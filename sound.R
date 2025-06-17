install.packages(c("stringdist", "phonics", "proxy"))
library(stringdist)  # pour la distance de Levenshtein
library(phonics)     # pour les algorithmes de phonétisation (Soundex, NYSIIS, etc.)
library(proxy)       # pour calculer des distances personnalisées

library(ggplot2)
library(ggspatial)
library(ggrepel)
library(ggnewscale)

# Supposons que dataVot contient les noms de communes
communes_sound <- unique(ch_aggregated_geolevels$NAME)
n <- length(communes_sound)

# Option 1 : Distance Levenshtein brute
dist_lev <- stringdistmatrix(communes_sound, communes_sound, method = "lv")
rownames(dist_lev) <- colnames(dist_lev) <- communes_sound

# Option 2 : Distance Levenshtein sur Soundex

# Convertir en ASCII de base
library(stringi)

# clean_name <- function(x) {
#   x <- tolower(x)                             # tout en minuscules
#   x <- stringi::stri_trans_general(x, "Latin-ASCII")  # retirer accents
#   x <- gsub("[^a-z]", "", x)                 # ne garder que les lettres a-z
#   return(x)
# }
clean_name <- function(x) {
  x <- as.character(x)                           # forcer en caractère
  x <- trimws(x)                                 # enlever espaces début/fin
  x <- gsub("\\s*\\([^\\)]*\\)\\s*$", "", x)     # retirer parenthèses finales + espaces
  x <- tolower(x)                                # tout en minuscules
  x <- stringi::stri_trans_general(x, "Latin-ASCII")  # retirer accents
  x <- gsub("[^a-z]", "", x)                     # ne garder que a-z
  return(x)
}


communes_clean <- sapply(communes_sound, clean_name)
nysiis_names <- nysiis(communes_clean)
any(grepl("[^A-Z]", nysiis(communes_clean)))  # devrait être FALSE si tout est bien nettoyé


dist_soundex_lev <- stringdistmatrix(soundex_names, soundex_names, method = "lv")
rownames(dist_soundex_lev) <- colnames(dist_soundex_lev) <- communes_sound

# Option 3 : Distance Levenshtein sur NYSIIS (plus adaptée au français)
nysiis_names <- nysiis(communes_sound)
dist_nysiis_lev <- stringdistmatrix(nysiis_names, nysiis_names, method = "lv")
rownames(dist_nysiis_lev) <- colnames(dist_nysiis_lev) <- communes_sound

# Option bonus : Phonétique + fréquence des bigrammes
# Pour aller plus loin, on peut construire des vecteurs de bigrammes (ou trigrammes) et calculer une distance cosinus :

get_bigrams <- function(word) {
  word <- paste0("_", word, "_")
  bigrams <- substring(word, 1:(nchar(word)-1), 2:nchar(word))
  table(factor(bigrams, levels = unique(unlist(sapply(communes_sound, get_bigrams)))))
}

bigram_matrix <- t(sapply(communes_sound, get_bigrams))
dist_bigram <- proxy::dist(bigram_matrix, method = "cosine")
rownames(dist_bigram) <- colnames(dist_bigram) <- communes_sound


### V2 #########################################################################
library(stringdist)
library(phonics)
library(proxy)
library(stringi)

# Nettoyage des noms (o)
# clean_name <- function(x) {
#   x <- tolower(x)
#   x <- stri_trans_general(x, "Latin-ASCII") # enlever accents
#   x <- gsub("[^a-z]", "", x)                # ne garder que les lettres
#   return(x)
# }

communes_sound <- unique(ch_aggregated_geolevels$NAME)
communes_clean <- sapply(communes_sound, clean_name)

# --- Option 1 : Levenshtein sur noms nettoyés
# Simple, directe, sensible à chaque caractère.
dist_lev <- stringdistmatrix(communes_clean, communes_clean, method = "lv")
rownames(dist_lev) <- colnames(dist_lev) <- communes_sound

# --- Option 2 : Levenshtein sur Soundex
# Regroupe les noms qui “se prononcent” pareil selon la logique anglaise.
soundex_names <- soundex(communes_clean)
dist_soundex_lev <- stringdistmatrix(soundex_names, soundex_names, method = "lv")
rownames(dist_soundex_lev) <- colnames(dist_soundex_lev) <- communes_sound

# --- Option 3 : Levenshtein sur NYSIIS
# Plus adapté aux sons romanes/françaises.
nysiis_names <- nysiis(communes_clean)
dist_nysiis_lev <- stringdistmatrix(nysiis_names, nysiis_names, method = "lv")
rownames(dist_nysiis_lev) <- colnames(dist_nysiis_lev) <- communes_sound

# --- Bonus : Distance cosinus sur bigrammes
# Très précis, capte les motifs fréquents comme "ens", "berg", "wil", etc.
get_bigrams <- function(word) {
  word <- paste0("_", word, "_")
  bigrams <- substring(word, 1:(nchar(word)-1), 2:nchar(word))
  return(bigrams)
}

# Construire la matrice de bigrammes
bigram_vocab <- unique(unlist(sapply(communes_clean, get_bigrams)))
bigram_matrix <- t(sapply(communes_clean, function(word) {
  tab <- table(factor(get_bigrams(word), levels = bigram_vocab))
  as.numeric(tab)
}))
rownames(bigram_matrix) <- communes_sound

# Calculer distance cosinus
dist_bigram <- proxy::dist(bigram_matrix, method = "cosine")
mat_bigram <- as.matrix(dist_bigram)
View(mat_bigram)

# Visualisation rapide des plus proches voisins :
sort(mat_bigram["Sullens", ])[1:10]  # les 10 noms les plus proches phonétiquement

# Carte
ch_aggregated_geolevels$ebikon = mat_bigram[,515]
515

# --- Etape 2 : MDS pondéré (ou non pondéré)
# MDS classique (non pondéré)
mds_bigram <- cmdscale(mat_bigram, k = 2)
plot(mds_bigram, type = "n", main = "Carte phonétique des communes (MDS sur bigrammes)")
text(mds_bigram, labels = rownames(mds_bigram), cex = 0.5)

# filtre pour ne pas tout afficher si trop dense :
sample_communes <- sample(rownames(mds_bigram), 200)
plot(mds_bigram[sample_communes, ], type = "n", main = "Carte phonétique (échantillon)")
text(mds_bigram[sample_communes, ], labels = sample_communes, cex = 0.7)

# --- Etape 3 : MDS pondéré (vecteur f des poids par commune)
library(ade4)

# Supposons que f contient les poids des communes (de même longueur que communes)
# et qu'on les associe aux noms :
weights <- data.frame(name = communes_sound, f = f)
row_weights <- weights[match(rownames(mat_bigram), weights$name), "f"]

# Analyse PCoA pondérée
pco_result <- dudi.pco(as.dist(mat_bigram), scannf = FALSE, nf = 2, row.w = row_weights)

# Affichage
s.label(pco_result$li, clab = 0.5)

# --- Formalisme François

# --- Sound distance
DS = mat_bigram
KS = -0.5 * diag(sqrt(f)) %*% H %*% DS %*% t(H) %*% diag(sqrt(f)) # kernel, sound


DeltaS = 0.5 * t(f) %*% DS %*% f # inertia 

f_nw = rep(1/n,n)
H_nw = diag(n) - rep(1, n) %*% t(f_nw) # centering matrix
KS_nw = -0.5 * diag(sqrt(f_nw)) %*% H_nw %*% DS %*% t(H_nw) %*% diag(sqrt(f_nw)) # political kernel

eigen_val_nw <- eigen(KS)
U_nw <- eigen_val_nw$vectors
lambda_nw <- eigen_val_nw$values

Y_nw <- diag(1/sqrt(f_nw)) %*% U_nw %*% diag(sqrt(lambda_nw))
as.data.frame(Y_nw[,1:2])

library(ggplot2)
library(ggtext)
mds_fun_S = function(fi, K, ch_aggregated_geolevels){
  
  eigen_val <- eigen(K)
  U <- eigen_val$vectors
  lambda <- eigen_val$values
  
  Y <- diag(1/sqrt(fi)) %*% U %*% diag(sqrt(lambda))
  
  # dist_nb = which(dist_types == letter)
  
  # if (!missing(conditionSample)) {
  # dataVotFun = dataVotFun[conditionSample,]
  #   fi = fi[conditionSample]
  # }
  
  # dataVot = dataVot[conditionSample,]
  # f = f[conditionSample]
  
  mds = as.data.frame(Y[,1:2])
  mds$V1 = Re(mds$V1)
  mds$V2 = Re(mds$V2)
  mds$commune = ch_aggregated_geolevels$NAME
  mds$langue = as.character(ch_aggregated_geolevels$language)
  # mds$langue = substring(mds$langue, 10)
  mds$f = f
  mds$voters = ch_aggregated_geolevels$swiss_data_muni.f
  mds$local_phonetic_dissimilarity = local_phonetic_dissimilarity
  mds$LAG_sound_diss_t1 = LAG_sound_diss_t1
  mds$Y_1_nw = Y_uni[,1]
  mds$Y_2_nw = Y_uni[,2]
  
  mds_filtered = mds[mds$f > sort(mds$f, decreasing = TRUE)[50], ]
  
  propDeltaPC = round(100*lambda / sum(lambda), digits = 1 )
  
  # print(propDeltaPC)
  # magnif = 0.2+0.5*(log(fi)-min(log(fi)))/(max(log(fi))-min(log(fi))) # defines a magnification factor for the object weights (here from 0.5 to 2)
  
  xlab = paste("Factor 3, inertia explained =",propDeltaPC[1],"%")
  ylab = paste("Factor 4, inertia explained =",propDeltaPC[2],"%")
  
  # magnif = magnif[conditionSample]
  mds_plot = ggplot() +
    geom_vline(xintercept = 0,linetype="dashed") +
    geom_hline(yintercept = 0, linetype="dashed") +
    # geom_point(aes(x = -mds[,1], y = -mds[,2], size=fi, color=mds$langue),
    #            alpha = magnif) +
    geom_point(aes(x = mds[,1], y = -mds[,2], color=mds$langue)) +
    geom_text(aes(x = mds_filtered[,1], y = -mds_filtered[,2], label = mds_filtered$commune)) +
    scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"),
                       labels = c("German", "French", "Italian","Romansh")) +
    labs(x = xlab, y = ylab) +
    # labs(size = paste0("Reg. weight ", expression(italic("f"))), color = "Language") +
    labs(size = "Reg. weight *f*", color = "Language") +
    scale_size_continuous(range = c(1, 8)) +
    theme_minimal() +
    theme(legend.title = element_markdown(lineheight = 1.2))
  
  return(list(eigen_val=eigen_val,mds=mds,mds_plot=mds_plot))
}

sound_MDS = mds_fun_S(f, KS, ch_aggregated_geolevels)
sound_MDS_nw = mds_fun_S(f_nw, KS_nw, ch_aggregated_geolevels)


### --- map

mds_df <- data.frame(NAME = rownames(mds_bigram),
                     phon1 = mds_bigram[,1],
                     phon2 = mds_bigram[,2])
communes_centres$latitude
# Fusionner avec les coordonnées géographiques
merged_df <- merge(communes_centres, mds_df, by = "NAME")
library(ggplot2)

ggplot(merged_df, aes(x = longitude, y = latitude)) +
  # geom_point(aes(color = phon1), size = 1) +
  geom_point(aes(color = phonetic_x), size = 1) +
  scale_color_viridis_c() +
  labs(title = "Carte géographique colorée par 1er axe phonétique",
       color = "Axe phonétique 1") +
  theme_minimal()

ggplot(merged_df, aes(x = longitude, y = latitude)) +
  # geom_point(aes(color = phon2), size = 1) +
  geom_point(aes(color = phonetic_y), size = 1) +
  scale_color_viridis_c() +
  labs(title = "Carte géographique colorée par 2e axe phonétique") +
  theme_minimal()

ggplot(merged_df) +
  # geom_segment(aes(x = longitude, y = latitude, xend = phon1*10000, yend = phon2*10000), alpha = 0.2) +
  geom_segment(aes(x = longitude, y = latitude, xend = phonetic_x*10000, yend = phonetic_y*10000), alpha = 0.2) +
  geom_point(aes(x = longitude, y = latitude), color = "blue", size = 0.3) +
  # geom_point(aes(x = phon1*10000, y = phon2*10000), color = "red", size = 0.3) +
  geom_point(aes(x = phonetic_x*10000, y = phonetic_y*10000), color = "red", size = 0.3) +
  labs(title = "Déformation géo ↔ phonétique", subtitle = "bleu = réel, rouge = phonétique") +
  theme_void()

# --- leaflet map
library(leaflet)
# maxData_sound = max(abs(mds_df$phon1))
maxData_sound = max(abs(Y[,1]))
maxData_sound = round(maxData_sound,1)

scale_range_sound <- c(-maxData_sound, maxData_sound)
# pal_sound <- scale_fill_viridis()
pal_sound <- colorNumeric("Spectral", domain = scale_range_sound)

pal_sound <- colorNumeric("Spectral", domain = Y[,1])

rev_pal_sound <- colorNumeric("Spectral", domain = scale_range_sound, reverse = TRUE)

leaflet(ch_aggregated) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  # addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lat=46.637785, lng=8.2 , zoom=7) %>%
  # municipality polygons
  addPolygons(
    # fillColor = ~pal_sound(mds_df$phon1),
    fillColor = ~pal_sound(Y[,1]),
    fillOpacity = 0.9,
    color = "white",
    weight = 0.1,
    opacity = 1,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE,
      sendToBack = TRUE),
    # label = paste0(ch_aggregated$NAME, ": ", mds_df$phon1),
    label = paste0(ch_aggregated$NAME, ": ", Y[,1]),
    smoothFactor = 0.2,
    # group = "MDS fac. 1",
  ) %>%
  addPolygons(data = lakes,
              weight = 0.7,
              fillColor = "#dddddd",
              fillOpacity = 1,
              color = "white",
              highlight = highlightOptions(
                weight = 1,
                color = "#666",
                fillOpacity = 1,
                bringToFront = TRUE),
              label = lakes$NAME,
              labelOptions = labelOptions(
                style = list(
                  "color" = "#666",
                  "font-style" = "italic"
                )),
              smoothFactor = 0.2,
              group = c("lakes")) %>%
  addLayersControl(
    # baseGroups = c("MDS fac. 1","MDS fac. 2"), # groups separated by municipality
    overlayGroups = c("lakes"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F)) %>%
  # addLegend(
  #   colors = c("#d47c17", "#d44317"),
  #   position = "bottomright",
  #   labels = c("Highways", "Voted projects"),
  #   opacity = 0.9
  # ) %>%
  # Ajout de la légende
  addLegend(
    # pal = rev_pal_sound,
    pal = pal_sound,
    # values = scale_range_sound,
    values = Y[,1],
    position = "bottomright",
    title = "MDS",
    # labFormat = labelFormat(suffix = "%"),
    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE), suffix = ""),
    
    opacity = 0.9
  )


# --- suffixes fréquents

# Suppose que communes_clean est déjà nettoyé (a-z uniquement)
suffix_freq <- function(names, n = 3) {
  suffixes <- substr(names, nchar(names) - n + 1, nchar(names))
  table(suffixes)
}

suffix_3 <- function(names, n = 3) {
  suffixes <- substr(names, nchar(names) - n + 1, nchar(names))
}
suffix_3_list <- suffix_3(communes_clean, n = 3)

# Suffixes de 3 lettres
suffix3_tab <- suffix_freq(communes_clean, n = 3)
suffix3_sorted <- sort(suffix3_tab, decreasing = TRUE)
head(suffix3_sorted, 20)

# Suffixes allemaniques
suffix3_de = suffix_freq(communes_clean[which(ch_aggregated_geolevels$language == 1)], n = 3)
suffix3_de <- sort(suffix3_de, decreasing = TRUE)
head(suffix3_de, 20)

# Créer une liste vide
resultats_suffixes <- list()

# Boucle sur les langues
for (lange in 1:4) {
  # Boucle sur la longueur du suffixe
  for (suff in 1:4) {
    # Calculer les suffixes pour la langue et longueur actuelle
    suffix_n_lan <- suffix_freq(communes_clean[which(ch_aggregated_geolevels$language == lange)], n = suff)
    suffix_n_lan <- sort(suffix_n_lan, decreasing = TRUE)
    top20 <- head(suffix_n_lan, 20)
    
    # Stocker le résultat dans la liste avec un nom clair
    resultats_suffixes[[paste0("Langue", lange, "_Suffixe", suff)]] <- top20
  }
}

# Dans un df
# Créer une liste vide pour stocker les petits tableaux
resultats_list <- list()

# Boucle sur les langues 1 à 4
for (lange in 1:4) {
  for (suff in 1:4) {
    suffix_n_lan <- suffix_freq(communes_clean[which(ch_aggregated_geolevels$language == lange)], n = suff)
    suffix_n_lan <- sort(suffix_n_lan, decreasing = TRUE)
    top20 <- head(suffix_n_lan, 20)
    
    # Stocker sous forme de data frame
    df_temp <- data.frame(
      langue = lange,
      longueur_suffixe = suff,
      suffixe = names(top20),
      frequence = as.numeric(top20),
      stringsAsFactors = FALSE
    )
    
    resultats_list[[length(resultats_list) + 1]] <- df_temp
  }
}

# Maintenant, pour toutes les langues ensemble (langue = 5)
for (suff in 1:4) {
  suffix_n_all <- suffix_freq(communes_clean, n = suff)
  suffix_n_all <- sort(suffix_n_all, decreasing = TRUE)
  top20_all <- head(suffix_n_all, 20)
  
  df_temp_all <- data.frame(
    langue = 5,  # 5 = toutes langues ensemble
    longueur_suffixe = suff,
    suffixe = names(top20_all),
    frequence = as.numeric(top20_all),
    stringsAsFactors = FALSE
  )
  
  resultats_list[[length(resultats_list) + 1]] <- df_temp_all
}

# Coller tous les petits data frames en un seul
resultats_suffixes_df <- do.call(rbind, resultats_list)

# Voir le résultat
head(resultats_suffixes_df)


# Préfixes de 3 lettres
prefix_freq <- function(names, n = 3) {
  prefix <- substr(names, 0, n)
  table(prefix)
}
prefix3_tab <- prefix_freq(communes_clean, n = 3)
prefix3_sorted <- sort(prefix3_tab, decreasing = TRUE)
head(prefix3_sorted, 20)

# Rechercher les communes par suffixe
# Exemple : les communes en "-ens"
target_suffix <- "ens"
selection <- grepl(paste0(target_suffix, "$"), communes_clean)

# Retrouver les communes et leurs coordonnées
selected_communes <- communes_sound[selection]
coord_sel <- communes_centres[communes_centres$NAME %in% selected_communes, ]

# Carte rapide
library(ggplot2)

ggplot() +
  geom_point(data = communes_centres, aes(x = longitude, y = latitude), color = "gray80", size = 0.5) +
  geom_point(data = coord_sel, aes(x = longitude, y = latitude), color = "red", size = 1.5) +
  labs(title = paste0("Répartition des communes en -", target_suffix)) +
  geom_sf(data=ch_aggregated_geolevels, size=0, alpha=0) +
  geom_sf(data=lacs, fill="gray80", size=0, alpha=1) +
  theme_minimal()

# map 2
library(ggplot2)
library(sf)

# Palette des langues (ordre: allemand, français, italien, romanche)
langue_colors <- c("1" = "#66C2A5", "2" = "#FC8D62", "3" = "#8DA0CB", "4" = "#E78AC3")

ggplot() +
  
  # Frontières des communes selon la langue
  geom_sf(data = ch_aggregated_geolevels, aes(color = as.factor(language)),
          fill = NA, size = 0.2, alpha = 1) +
  
  # Lacs en gris clair
  geom_sf(data = lacsWGS84, fill = "gray90", color = NA, alpha = 1) +
  
  # Toutes les communes (fond)
  geom_point(data = communes_centres, aes(x = longitude, y = latitude), 
             color = "gray80", size = 0.3) +
  
  # Communes avec suffixe ciblé (en rouge)
  geom_point(data = coord_sel, aes(x = longitude, y = latitude), 
             color = "red", size = 0.9) +
  
  # Légende et couleurs
  scale_color_manual(
    name = "Langue principale",
    values = langue_colors,
    labels = c("Allemand", "Français", "Italien", "Romanche")
  ) +
  
  labs(title = paste0("Répartition des communes en -", target_suffix)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
  ) + 
  
  labs(x = NULL, y = NULL)

ggsave("sound-o.jpg", width = 9, height = 8)


# --- Autocorrélation
# Étape 1 : extraire les suffixes et préparer la base
# Suffixes des noms (ici 3 lettres)
suffix_df <- data.frame(
  NAME = communes_sound,
  suffix = substr(communes_clean, nchar(communes_clean)-2, nchar(communes_clean))
)

# Ajouter les coordonnées
suffix_df <- merge(suffix_df, communes_centres, by = "NAME")

# Étape 2 : sélectionner les suffixes fréquents
# Compter les suffixes
suffix_counts <- table(suffix_df$suffix)
frequent_suffixes <- names(suffix_counts[suffix_counts >= 10])

# Étape 3 : autocorrélation spatiale (z-score ou Moran's I)
# (Option simple) : Score de Moran sur un vecteur binaire "1 si suffixe, 0 sinon"
library(spdep)

# Fonction pour calculer un Moran’s I pour un suffixe donné
compute_moran <- function(target_suffix, df_coords) {
  # Créer variable binaire
  df_coords$has_suffix <- ifelse(df_coords$suffix == target_suffix, 1, 0)
  
  # Construire voisins par k plus proches (k = 10)
  coords_mat <- as.matrix(df_coords[, c("longitude", "latitude")])
  nb <- knn2nb(knearneigh(coords_mat, k = 10))
  listw <- nb2listw(nb, style = "W")
  
  moran_test <- moran.test(df_coords$has_suffix, listw)
  return(c(suffix = target_suffix, moran_i = moran_test$estimate[1], p = moran_test$p.value))
}

# Appliquer à tous les suffixes fréquents
results <- do.call(rbind, lapply(frequent_suffixes, compute_moran, df_coords = suffix_df))
results_df <- as.data.frame(results)
results_df$moran_i <- as.numeric(results_df$moran_i)
results_df$p <- as.numeric(results_df$p)

# Trier par degré d'autocorrélation
results_sorted <- results_df[order(-results_df$moran_i), ]
head(results_sorted, 10)

# Étape 4 : cartographier les suffixes les plus régionalisés
# Une fois les suffixes les plus autocorrélés identifiés (par exemple ens, wil, dorf…), on peut les passer dans la carte qu’on a faite plus haut.

library(ggplot2)
library(sf)

# Palette des langues
langue_colors <- c("1" = "#66C2A5", "2" = "#FC8D62", "3" = "#8DA0CB", "4" = "#E78AC3")

# Fonction qui fait la carte pour un suffixe donné
plot_suffix_map <- function(target_suffix, communes_centres, coord_sf, lacs_sf, ch_aggregated_geolevels) {
  
  # Sélectionner les communes qui ont le suffixe cible
  selection <- grepl(paste0(target_suffix, "$"), communes_clean)
  selected_communes <- communes_sound[selection]
  coord_sel <- communes_centres[communes_centres$NAME %in% selected_communes, ]
  
  # Plot
  ggplot() +
    
    # Frontières des communes, colorées par langue
    geom_sf(data = ch_aggregated_geolevels, aes(color = as.factor(language)), fill = NA, size = 0.2) +
    
    # Lacs en fond
    geom_sf(data = lacs_sf, fill = "gray90", color = NA, alpha = 1) +
    
    # Toutes les communes, fond en gris clair
    # geom_point(data = communes_centres, aes(x = longitude, y = latitude), color = "gray80", size = 0.4) +
    
    # Communes avec le suffixe en rouge
    geom_point(data = coord_sel, aes(x = longitude, y = latitude), color = "red", size = 0.9) +
    
    # Personnalisation
    scale_color_manual(
      name = "Langue principale",
      values = langue_colors,
      labels = c("Allemand", "Français", "Italien", "Romanche")
    ) +
    # labs(title = paste0("Répartition des communes terminant par -", target_suffix)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
    ) +
    
    labs(x = NULL, y = NULL) +
    annotation_scale(style="ticks", location = "br")
}

# Exemple d'utilisation :
plot_suffix_map(target_suffix = "ens", 
                communes_centres = communes_centres, 
                coord_sf = ch_aggregated_geolevels, 
                lacs_sf = lacsWGS84, 
                ch_aggregated_geolevels = ch_aggregated_geolevels)
ggsave("heatmaps_suffixes/map_suffixe-ens.png", width = 9, height = 8)

# --- Heatmap
library(ggplot2)
library(sf)

plot_suffix_heatmap <- function(target_suffix, communes_centres, lacs_sf, ch_aggregated_geolevels) {
  
  # Sélectionner les communes avec le suffixe
  selection <- grepl(paste0(target_suffix, "$"), communes_clean)
  selected_communes <- communes_centres[communes_centres$NAME %in% communes_sound[selection], ]
  
  # Plot
  ggplot() +
    
    # Optionnel : contours des communes
    # geom_sf(data = ch_aggregated_geolevels, fill = NA, color = "gray90", size = 0.2) +
    
    # Lacs en fond
    geom_sf(data = lacs_sf, fill = "gray90", color = NA) +
    
    # Heatmap de densité
    stat_density_2d(data = selected_communes,
                    aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                    geom = "polygon", contour = TRUE, n = 200) +
    # geom_density2d_filled(data = selected_communes,
    #                 aes(x = longitude, y = latitude, alpha = 0.75)) +
    
    # stat_density_2d(
    #   data = selected_communes,
    #   aes(x = longitude, y = latitude, fill = ..density..),
    #   geom = "raster", contour = FALSE, n = 400
    # ) +
    
    # scale_fill_viridis_c(limits = c(0.001, 12), na.value = "transparent") +
    
    # Palette de chaleur
    # scale_fill_viridis_c(option = "inferno", name = "Densité", limits = c(0.2, 1), na.value = "transparent") + # raster
    scale_fill_viridis_c(option = "inferno", name = "Densité") +
    
    scale_alpha(range = c(0.3, 0.8), guide = "none") +
    
    # Titre et thème
    labs(title = paste0("Densité des communes en -", target_suffix)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "right"
    ) + 
    
    labs(x = NULL, y = NULL)
}
plot_suffix_heatmap("ins", communes_centres, lacsWGS84, ch_aggregated_geolevels)

# v2
plot_suffix_heatmap_filled <- function(target_suffix, communes_centres, lacs_sf, ch_aggregated_geolevels) {
  
  # Sélection des communes avec le suffixe
  selection <- grepl(paste0(target_suffix, "$"), communes_clean)
  selected_communes <- communes_centres[communes_centres$NAME %in% communes_sound[selection], ]
  
  d <- MASS::kde2d(communes_centres$longitude, communes_centres$latitude, lims = st_bbox(ch_aggregated_geolevels)[c(1, 3, 2, 4)])
  dens <- data.frame(expand.grid(x = d$x, y = d$y), z = as.vector(d$z))
  
  # Affichage
  ggplot() +
    # scale_fill_viridis_d(option = "inferno", name = "Densité\nrelative") +
    # Lacs en fond
    geom_sf(data = lacs_sf, fill = "gray80", color = NA) +
    
    # Contours des communes
    geom_sf(data = ch_aggregated_geolevels, fill = NA, color = "gray90", size = 0.2) +
    
    geom_contour_filled(data = dens, aes(x = x, y = y, z = z, 
                                         alpha = after_stat(level))) +
    
    scale_alpha_manual(values = c(0, rep(0.75, 10)), guide = "none") +
    
    # Heatmap remplie
    # geom_density_2d_filled(data = selected_communes,
    #                        aes(x = longitude, y = latitude),
    #                        contour_var = "density", n = 400, adjust = 1.5) +
    # geom_density_2d_filled(data = selected_communes,
    #                        aes(x = longitude, y = latitude)) +
    
    # Palette de couleursLa structure phonétique des suffixes suit donc clairement la répartition historique des langues nationales et de leurs variantes dialectales.
    scale_fill_viridis_d(option = "inferno", name = "Densité\nrelative") +
    
    # Titre et thème
    labs(title = paste0("Densité des communes terminant par -", target_suffix)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "right"
    )
}

plot_suffix_heatmap_filled("ens", communes_centres, lacsWGS84, ch_aggregated_geolevels)

# n = 200 → résolution de la grille : augmenter pour plus de finesse
# alpha → transparence de la couche de chaleur
# option = "inferno" → palette viridis; alternatives : "magma", "plasma", "cividis"

# --- boucle de heatmaps
library(ggplot2)
library(sf)

# Fonction de plot (identique à plus haut)
plot_suffix_heatmap <- function(target_suffix, communes_centres, lacs_sf, ch_aggregated_geolevels) {
  selection <- grepl(paste0(target_suffix, "$"), communes_clean)
  selected_communes <- communes_centres[communes_centres$NAME %in% communes_sound[selection], ]
  
  ggplot() +
    geom_sf(data = ch_aggregated_geolevels, fill = NA, color = "gray90", size = 0.2) +
    geom_sf(data = lacs_sf, fill = "gray90", color = NA) +
    stat_density_2d(data = selected_communes,
                    aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                    geom = "polygon", contour = TRUE, n = 200) +
    scale_fill_viridis_c(option = "inferno", name = "Densité") +
    scale_alpha(range = c(0.3, 0.8), guide = "none") +
    labs(title = paste0("Densité des communes terminant par -", target_suffix)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      legend.position = "right"
    ) +
    labs(x = NULL, y = NULL)
}

# Boucle automatique
# Créer un dossier pour les figures (si pas encore fait)
if (!dir.exists("heatmaps_suffixes")) dir.create("heatmaps_suffixes")

# Nombre de suffixes à visualiser (top 6 par exemple)
top_n <- 1
top_suffixes <- results_sorted$suffix[1:top_n]

# Générer et enregistrer les figures
for (suf in top_suffixes) {
  plot <- plot_suffix_heatmap(suf, communes_centres, lacsWGS84, ch_aggregated_geolevels)
  
  ggsave(filename = paste0("heatmaps_suffixes/heatmap_suffix2_", suf, ".png"),
         plot = plot, width = 7, height = 6, dpi = 300)
}


# --- Plusieurs suffixes
# Choix de 4 suffixes typiques ou fortement régionalisés
suffixes_cibles <- c("gen", "wil", "ach", "ens", "ier", "o")
colors_suffixes <- c("gen" = "#8dd3c7", "wil" = "#ffffb3", "ach" = "#bebada", "ens" = "#fb8072", "ier" = "#80b1d3", "o" = "#fdb462")
colors_suffixes <- c("gen" = "#f1eef6", "wil" = "#d0d1e6", "ach" = "#a6bddb", "ens" = "#74a9cf", "ier" = "#2b8cbe", "o" = "#045a8d")

# Attribuer un suffixe à chaque commune (si elle correspond à au moins un)
# Initialisation
communes_centres$suffix_category <- NA

# Parcourir les suffixes et affecter
for (suf in suffixes_cibles) {
  matching <- grepl(paste0(suf, "$"), communes_clean)
  communes_centres$suffix_category[matching & is.na(communes_centres$suffix_category)] <- suf
}

# Joindre avec géométries
# Ajouter la catégorie au shapefile
ch_aggregated_geolevels$NAME <- as.character(ch_aggregated_geolevels$NAME)
communes_centres$NAME <- as.character(communes_centres$NAME)

suffix_map_df <- merge(ch_aggregated_geolevels, 
                       communes_centres[, c("NAME", "suffix_category")],
                       by = "NAME", all.x = TRUE)
library("dplyr")
ch_langue <- ch_aggregated_geolevels %>% 
  group_by(language) %>%
  summarise(population = sum(EINWOHNERZ, na.rm = TRUE))

# Carte finale avec couleurs par suffixe
ggplot() +
  # geom_sf(data = suffix_map_df, aes(fill = suffix_category, color = as.character(language)), size = 0.1) +
  geom_sf(data = suffix_map_df, aes(fill = suffix_category, color = "gray90"), linewidth = 0.05) +
  geom_sf(data = ch_langue, aes(fill = NA, color = as.character(language), alpha = 1), linewidth = 0.3) +
  geom_sf(data = lacsWGS84, fill = "gray80", color = NA) +
  scale_fill_manual(
    name = "Suffixe",
    values = colors_suffixes,
    na.value = "white"
  ) +
  # Personnalisation
  scale_color_manual(
    name = "Langue principale",
    values = langue_colors,
    labels = c("Allemand", "Français", "Italien", "Romanche")
  ) +
  # labs(title = "Répartition géographique de suffixes toponymiques") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
    legend.position = "right"
  ) +
  guides(alpha="none")

ggsave("test_suffixe.png", width = 9, height = 8)



# --- MDS
# --- Centering matrix

f
H = diag(n) - rep(1, n) %*% t(f) # centering matrix
DS = mat_bigram
DS = as.matrix((mat_bigram + t(mat_bigram)) / 2) ^ 2

KS = -0.5 * diag(sqrt(f)) %*% H %*% DS %*% t(H) %*% diag(sqrt(f)) # kernel, sound
KS <- (KS + t(KS)) / 2 # forcer symmétrie de KS à cause des petits nombres
eigen_val <- eigen(KS)
U <- eigen_val$vectors
lambda <- eigen_val$values
# D is squared Euclidean iff lambda is non-negative (up to numerical approximations)
if (min(lambda) <= -1e-12){print("D is not squared Euclidean")}else{"D is squared Euclidean"}
lambda=pmax(lambda,0) # set possible (small) negative eigenvalues to zero
Y <- diag(1/sqrt(f)) %*% U %*% diag(sqrt(lambda))

# Scree plot political eigenvalues
values = lambda[1:20]
dimensions = seq(1, length(values))
ggplot(data = data.frame(dimensions, values), aes(x = dimensions, y = values)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(
    title = NULL,
    x = expression(paste("Dimension ", alpha, " = 1, ..., 20")),
    # x = expression(paste("dimension ", alpha, " = 1, ..., n-1")),
    # x = expression(paste("dimension ", alpha, " = 1190, ..., 1200")),
    # y = expression(paste("eigenvalue ", mu[alpha]))
    y = expression(paste("Valeur propre ", lambda[alpha]))
    # y = expression(bgroup("(", symbol(x) * "dot", ")")), bty="n")
  ) +
  theme_minimal()
ggsave("mds_sound_Lambda1-20.png", width = 6, height = 4)



# Poids uniformes
f_nw = rep(1/n, n)
H_uni = diag(n) - rep(1, n) %*% t(f_nw) # centering matrix
KS_uni = -0.5 * diag(sqrt(f_nw)) %*% H_uni %*% DS %*% t(H_uni) %*% diag(sqrt(f_nw)) # kernel, sound
KS_uni <- (KS_uni + t(KS_uni)) / 2 # forcer symmétrie de KS à cause des petits nombres
eigen_val_uni <- eigen(KS_uni)
U_uni <- eigen_val_uni$vectors
lambda_uni <- eigen_val_uni$values
# D is squared Euclidean iff lambda is non-negative (up to numerical approximations)
if (min(lambda_uni) <= -1e-12){print("D is not squared Euclidean")}else{"D is squared Euclidean"}
lambda_uni=pmax(lambda_uni,0) # set possible (small) negative eigenvalues to zero
Y_uni <- diag(1/sqrt(f_nw)) %*% U_uni %*% diag(sqrt(lambda_uni))

# 1. ✅ Avoir une matrice de distances phonétiques
# Exemple : distance cosinus sur bigrammes, déjà calculée
mat_phon <- as.matrix(dist_bigram)

# 2. MDS classique
mds_phon <- cmdscale(mat_phon, k = 2)
mds_phon <- Y[,1:2]
row.names(mds_phon) <- ch_aggregated_geolevels$NAME
mds_df <- data.frame(
  NAME = rownames(mds_phon),
  # phonetic_x = mds_phon[, 1],
  # phonetic_y = mds_phon[, 2]
  phonetic_x = Y[, 1],
  phonetic_y = Y[, 2]
)

# 3. 🧭 Joindre aux métadonnées (langue, canton, coordonnées géo…)
mds_df <- merge(mds_df, ch_aggregated_geolevels[, c("NAME", "language")], by = "NAME")

#4. 🖼️ Visualiser l’espace phonétique
ggplot(mds_df, aes(x = Y[,1], y = Y[,2])) +
# ggplot(mds_df, aes(x = phonetic_x, y = phonetic_y)) +
  geom_point(aes(color = as.factor(language)), alpha = 0.8, size = 1) +
  scale_color_manual(
    name = "Langue",
    values = c("1" = "#66C2A5", "2" = "#FC8D62", "3" = "#8DA0CB", "4" = "#E78AC3"),
    labels = c("Allemand", "Français", "Italien", "Romanche")
  ) +
  labs(title = "Carte MDS des communes selon leur consonance") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Bonus : nommer quelques communes
set.seed(42)
labelled <- mds_df[sample(nrow(mds_df), 50), ]  # 50 aléatoires

ggplot(mds_df, aes(x = Y[,1], y = Y[,2])) +
ggplot(mds_df, aes(x = phonetic_x, y = phonetic_y)) +
  geom_point(color = "gray60", size = 0.5) +
  geom_text(data = labelled, aes(label = NAME), size = 2, check_overlap = TRUE) +
  labs(title = "Espace phonétique (projection MDS)") +
  theme_minimal()

# v2 Romain
propDeltaPC = round(100*lambda_uni / sum(lambda_uni), digits = 1 )

# print(propDeltaPC)
# magnif = 0.+0.5*(log(f)-min(log(f)))/(max(log(f))-min(log(f))) # defines a magnification factor for the object weights (here from 0.5 to 2)
xlab = paste("Facteur 1, inertie expliquée =",propDeltaPC[1],"%")
ylab = paste("Facteur 2, inertie expliquée =",propDeltaPC[2],"%")

mds_sound_filtr = which(ch_aggregated_geolevels$language == 2)

library(ggtext)
# magnif = magnif[conditionSample]
ggplot() +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  # geom_point(aes(x = -mds[,1], y = -mds[,2], size=fi, color=mds$langue),
  #            alpha = magnif) +
  # geom_point(aes(x = Y[,1], y = Y[,2], color=as.character(ch_aggregated_geolevels$language), alpha = magnif, size = f)) +
  # geom_text(aes(x = Y[,1], y = Y[,2], label = suffix_3_list , color=as.character(ch_aggregated_geolevels$language))) +
  geom_text(aes(x = mds$Y_1_nw, y = mds$Y_2_nw, label = suffix_3_list , color=as.character(ch_aggregated_geolevels$language))) +
  
  # geom_text(aes(x = Y[mds_sound_filtr,1], y = Y[mds_sound_filtr,2],label = suffix_3_list[mds_sound_filtr])) +
  # geom_text(aes(x = mds_filtered[,1], y = mds_filtered[,2], label = mds_filtered$commune)) +
  # geom_text_repel(aes(x = mds_filtered[,1], y = mds_filtered[,2], label = mds_filtered$commune), max.overlaps = 50) +
  geom_text_repel(aes(x = mds_filtered$Y_1_nw, y = mds_filtered$Y_2_nw, label = mds_filtered$commune), max.overlaps = 50) + # version nw
  
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"),
                     labels = c("Allemand", "Français", "Italien","Romanche")) +
  labs(x = xlab, y = ylab) +
  # labs(size = paste0("Reg. weight ", expression(italic("f"))), color = "Language") +
  labs(size = "Poids rég. *f*", color = "Langue") +
  scale_size_continuous(range = c(1, 8)) +
  theme_minimal() +
  theme(legend.title = element_markdown(lineheight = 1.2)) +
  guides(alpha="none") 
  xlim(0.05, 0.13) +
  ylim(-0.07,0.1)
ggsave("heatmaps_suffixes/mds_sound_no_pond2.png", width = 9, height = 8)


# améliorer clareté
library(ggplot2)
library(ggtext)
library(ggrepel)

ggplot() +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_text_repel(aes(x = Y[,1], y = Y[,2], label = suffix_3_list, 
                      color = as.character(ch_aggregated_geolevels$language)),
                  size = 2.8, max.overlaps = 150, box.padding = 0.4, point.padding = 0.2,
                  segment.color = "grey70") +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"),
                     labels = c("Allemand", "Français", "Italien", "Romanche")) +
  labs(x = xlab, y = ylab, color = "Langue principale") +
  theme_minimal() +
  theme(legend.title = element_markdown(lineheight = 1.2),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5))


# --- 1. Calcul d'une autocorrélation spatiale sur l’espace phonétique

library(dplyr)
library(spdep)
A <- nb2mat(poly2nb(ch_aggregated_geolevels), style = "B", zero.policy = TRUE)
# --- Heat kernel
# t diffusion time
# lambda features et nu spatial

# f: regional weight vector
# A: binary (0-1) square symmetric matrix
# t: diffusion time, if t=0 -> identity matrix and if t=infinity, no longer depends on the initial state (Markov chain)
E1=function(f,A,t){
  LapA=diag(rowSums(A))-A
  Psi=(diag(sqrt(1/f))%*%LapA%*%diag(sqrt(1/f)))/(sum(A)-sum(diag(A)))    # normalised essentially positive generator
  U=eigen(Psi)$vectors
  Gamma=eigen(Psi)$values
  E=diag(sqrt(f))%*%U%*%diag(exp(-t*Gamma))%*%t(U)%*%diag(sqrt(f))
  return(E)}
f_nw = rep(1/n, n)
f
EX=E1(f,as.matrix(A),50)
EX2=E1(f_nw,as.matrix(A),1)

WX=diag(1/f)%*%EX
WX2=diag(1/f_nw)%*%EX2

local_phonetic_dissimilarity_centered <- local_phonetic_dissimilarity - mean(local_phonetic_dissimilarity, na.rm=TRUE)
LAG_sound_diss_t1 <- WX2 %*% local_phonetic_dissimilarity_centered # variable "laggée" (moyenne des voisins)

model <- lm(LAG_sound_diss_t1 ~ local_phonetic_dissimilarity_centered, weights = f)
summary(model) # pente estimation de du I de Moran


slope_lag <- coef(lm(LAG_sound_diss_t1 ~ local_phonetic_dissimilarity_centered, weights = f))[2]
# slope_lag <- coef(lm(lag_x_c ~ x_c, weights = f))[2]

intercept_lag <- coef(lm(LAG_sound_diss_t1 ~ local_phonetic_dissimilarity_centered, weights = f))[1]
# intercept_lag <-  coef(lm(lag_x_c ~ x_c, weights = f))[1]

xlab = paste("Dissimilarité phonique locale")
ylab = paste("Lag de la diss. phon. locale")
ylab = paste("Lag de la dissimilarité phonique locale")

mds = as.data.frame(Y[,1:2])
mds$V1 = Re(mds$V1)
mds$V2 = Re(mds$V2)
mds$commune = ch_aggregated_geolevels$NAME
mds$langue = as.character(ch_aggregated_geolevels$language)
# mds$langue = substring(mds$langue, 10)
mds$f = f
mds$voters = ch_aggregated_geolevels$swiss_data_muni.f
mds$local_phonetic_dissimilarity = local_phonetic_dissimilarity
mds$LAG_sound_diss_t1 = LAG_sound_diss_t1

mds$local_phonetic_dissimilarity_centered = local_phonetic_dissimilarity_centered

mds_filtered = mds[mds$f > sort(mds$f, decreasing = TRUE)[50], ]
# ch_aggregated_geolevels$x_c = x_c
# ch_aggregated_geolevels$lag_x_c = lag_x_c

ggplot() +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  # geom_point(data = Xmds, aes(x = Xmds1, y = lagged_first_coord, size=f, color=langue),
  #            alpha = magnif) +
  
  geom_point(data = ch_aggregated_geolevels, aes(x = local_phonetic_dissimilarity_centered, y = LAG_sound_diss_t1-intercept_lag, size=swiss_data_muni.f, color=as.character(language)),
             alpha = magnif) +
  
  # geom_point(data = ch_aggregated_geolevels, aes(x = x_c, y = lag_x_c, size=swiss_data_muni.f, color=as.character(language)),
  #            alpha = magnif) +
  # geom_text(data = Xmds_filtered, aes(x = Xmds1, y = lagged_first_coord, label = commune)) +
  geom_text_repel(data = mds_filtered, aes(x = local_phonetic_dissimilarity_centered, y = LAG_sound_diss_t1, label = commune), max.overlaps = 50) +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"),
                     labels = c("Allemand", "Français", "Italien","Romanche")) +
  labs(x = xlab, y = ylab) +
  labs(size = "Taille com.", color = "Langue princ.") +
  # scale_size_continuous(range = c(1, 8)) +
  # geom_abline(slope = slope_lag, intercept = intercept_lag) +
  geom_abline(slope = slope_lag, intercept = 0) +
  annotate(
    "text", 
    x = 0.05, y = -0.125, # Position du texte sur le graphique
    label = paste("Pente =", round(slope_lag, 2)), 
    # color = "blue",
    size = 3, 
    hjust = 0
  ) +
  theme_minimal()
ggsave("heatmaps_suffixes/moran_scat_sound_t50_f_nw2.jpg", width = 9, height = 8)
ggsave("heatmaps_suffixes/moran_scat_sound_t1_f_nw3.jpg", width = 8, height = 7)




# (a) Construire les voisins sur l’espace phonétique
# On peut utiliser, par exemple, k plus proches voisins :
library(spdep)

# Construire un voisinage 10-NN sur Y
coords_mds <- Y[, 1:2]
nb_mds <- knn2nb(knearneigh(coords_mds, k = 10))
listw_mds <- nb2listw(nb_mds, style = "W")

# (b) Créer une variable à tester
langue_vec <- as.numeric(ch_aggregated_geolevels$language)

# (c) Calculer le Moran's I
moran_test_result <- moran.test(langue_vec, listw_mds)
print(moran_test_result)


# --- 2. Construire la double carte (géographique vs phonétique)
# Code pour les 2 cartes côte à côte
library(patchwork) # Pour assembler deux ggplots facilement

# Carte géographique
geo_plot <- ggplot(communes_centres, aes(x = longitude, y = latitude)) +
  geom_point(aes(color = as.factor(ch_aggregated_geolevels$language)), size = 1) +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"),
                     labels = c("Allemand", "Français", "Italien", "Romanche")) +
  labs(title = "Carte géographique des communes",
       color = "Langue principale") +
  theme_minimal()

# Carte phonétique
phon_plot <- ggplot() +
  geom_point(aes(x = Y[,1], y = Y[,2], color = as.factor(ch_aggregated_geolevels$language)), size = 1) +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"),
                     labels = c("Allemand", "Français", "Italien", "Romanche")) +
  labs(title = "Espace phonétique MDS des suffixes",
       color = "Langue principale") +
  theme_minimal()

# Assembler
geo_plot + phon_plot + plot_layout(ncol = 2)

# --- Carte Moran
# 1. Construction du voisinage (géographique)
library(spdep)

coords_geo <- as.matrix(communes_centres[, c("longitude", "latitude")])
nb_geo <- knn2nb(knearneigh(coords_geo, k = 10))  # k=10 voisins géographiques
listw_geo <- nb2listw(nb_geo, style = "W")

# 2. Choix de la variable phonétique à autocorréler
var_phon <- Y[,1]

# 3. Calcul du Moran’s I local
local_moran_result <- localmoran(var_phon, listw_geo)

# Résultat : un tableau avec I_i, E(I_i), Var(I_i), z-score, p-value
head(local_moran_result)

# Extraire stat
communes_centres$local_moran_I <- local_moran_result[, "Ii"]
p_colname <- grep("^Pr", colnames(local_moran_result), value = TRUE)
communes_centres$local_moran_pvalue <- local_moran_result[, p_colname]

# 4. Visualisation propre
library(ggplot2)

ggplot(communes_centres) +
  geom_sf(data = ch_aggregated_geolevels, fill = "white", color = "grey80", size = 0.2) +
  geom_point(aes(x = longitude, y = latitude, color = local_moran_I), size = 1) +
  scale_color_gradient2(
    midpoint = 0, low = "blue", mid = "white", high = "red",
    name = "Moran's I local"
  ) +
  labs(title = "Autocorrélation locale sur l'axe phonétique 1",
       subtitle = "Commune par commune") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )





# --- Dissimilarité locale
library(spdep)

# Matrice des coordonnées géographiques
coords_mat <- as.matrix(communes_centres[, c("longitude", "latitude")])

# Construire un voisinage géographique basé sur les 10 plus proches voisins
nb_geo <- knn2nb(knearneigh(coords_mat, k = 10))

# Initialiser un vecteur vide pour stocker la dissimilarité locale
local_phonetic_dissimilarity <- numeric(length = length(nb_geo))

# Boucle sur chaque commune
for (i in seq_along(nb_geo)) {
  neighbors <- nb_geo[[i]]  # voisins géographiques de la commune i
  
  if (length(neighbors) > 0) {
    # Moyenne des distances phonétiques entre la commune i et ses voisins
    local_phonetic_dissimilarity[i] <- mean(DS[i, neighbors])
  } else {
    local_phonetic_dissimilarity[i] <- NA  # si pas de voisin
  }
}

# Ajouter au tableau des communes
communes_centres$local_phonetic_dissimilarity <- local_phonetic_dissimilarity

library(ggplot2)

ggplot(communes_centres) +
  geom_point(aes(x = longitude, y = latitude, color = local_phonetic_dissimilarity), size = 1.2) +
  scale_color_viridis_c(option = "plasma", name = "Dissimilarité phonétique\nlocale") +
  labs(title = "Dissimilarité phonétique locale des communes suisses") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Ajout de la variable au sf
ch_aggregated_geolevels$local_phonetic_dissimilarity <- local_phonetic_dissimilarity
ch_aggregated_geolevels$local_phonetic_dissimilarity_centered <- local_phonetic_dissimilarity_centered


# Carte
ggplot() +
  geom_sf(data = ch_aggregated_geolevels,
          aes(fill = local_phonetic_dissimilarity),
          color = "gray90", linewidth = 0.05) +
  geom_sf(data = lacsWGS84, fill = "gray80", color = NA) +
  scale_fill_viridis_c(name = "Dis. phon. locale", option = "plasma") +  # Choix de palette
  labs(x = NULL, y = NULL) +
  # scale_color_manual(
  #   name = "Langue principale",
  #   values = langue_colors,
  #   labels = c("Allemand", "Français", "Italien", "Romanche")
  # ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
    legend.position = "right",
    axis.ticks = element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank()
  ) +
  scale_x_continuous(breaks=NULL) +
  annotation_scale(style="ticks", location = "br") 
ggsave("heatmaps_suffixes/diff_phon_loc.png", width = 9, height = 8)









communes_centres$suffix3 = suffix_3_list
# --- Carte suffixes
ggplot(communes_centres, aes(x = longitude, y = latitude)) +
  # geom_point(aes(color = as.factor(ch_aggregated_geolevels$language)), size = 1) +
  geom_text(aes(color = as.factor(ch_aggregated_geolevels$language)), size = 2, label = communes_centres$suffix3) +
  # geom_text_repel(aes(color = as.factor(ch_aggregated_geolevels$language)), size = 2, label = communes_centres$suffix3, max.overlaps = 200) +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"),
                     labels = c("Allemand", "Français", "Italien", "Romanche")) +
  # labs(title = "Carte géographique des communes",
  #      color = "Langue principale") +
  labs(color = "Langue principale") +
  labs(x = NULL, y = NULL) +
  theme_minimal()



# --- Carte suffixes
ggplot(communes_centres, aes(x = longitude, y = latitude)) +
  # Lacs
  geom_sf(data = lacsWGS84, fill = "gray90", color = NA, alpha = 1) +
  # Texte suffixe
  geom_text(aes(color = as.factor(ch_aggregated_geolevels$language)), size = 2.5, label = paste0("-",communes_centres$suffix3)) +
  # geom_text_repel(aes(color = as.factor(ch_aggregated_geolevels$language)), size = 2.5, label = communes_centres$suffix3, max.overlaps = 50) +
  
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"),
                     labels = c("Allemand", "Français", "Italien", "Romanche")) +
  labs(color = "Langue principale") +
# 5. Habillage
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    # pas de légende
    legend.position="none",
  ) +
  
  xlim(6, 7.6) +
  
  # 5. Echelle
  annotation_scale(style="ticks", location = "br")
ggsave("heatmaps_suffixes/suffix_romand6.pdf", width = 13, height = 17)



# ggplot(communes_filtered, aes(x = longitude, y = latitude)) +
ggplot(communes_centres, aes(x = longitude, y = latitude)) +
  # geom_text(aes(color = as.factor(suffix3)), size = 2, label = communes_filtered$suffix3) +
  geom_text(aes(color = as.factor(suffix3)), size = 2, label = paste0("-",communes_centres$suffix3)) +
  labs(color = "Suffixe") +
  geom_sf(data = lacsWGS84, fill = "gray90", color = NA, alpha = 1) +
  # geom_sf(data = ch_langue, aes(fill = NA, color = as.character(language), alpha = 1), linewidth = 0.3) +
  scale_color_manual(
    name = "Langue principale",
    values = langue_colors,
    labels = c("Allemand", "Français", "Italien", "Romanche")
  ) +
  labs(x = NULL, y = NULL) +
  # theme(legend.position="none") +
  theme(
    legend.position="none",
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 7),
    
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    panel.grid.major = element_blank()
  ) +
  theme_minimal()

library(ggplot2)

ggplot(communes_filtered, aes(x = longitude, y = latitude)) +
  geom_text(aes(label = suffix3, color = suffix3), size = 2) +
  geom_sf(data = lacsWGS84, fill = "gray90", color = NA, alpha = 1) +
  # geom_sf(data = ch_langue, aes(color = as.character(language)), fill = NA, alpha = 1, linewidth = 0.3) +
  scale_color_manual(
    name = "Langue principale",
    values = langue_colors,
    labels = c("Allemand", "Français", "Italien", "Romanche")
  ) +
  # labs(x = NULL, y = NULL, color = "Suffixe") +
  # theme(
  #   legend.title = element_text(size = 8),
  #   legend.text = element_text(size = 7),
  #   axis.text.x=element_blank(),
  #   axis.text.y=element_blank(),
  #   panel.grid.major = element_blank()
  # ) +
  # theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    panel.grid.major = element_blank()
  ) +
  
  # 5. Echelle
  annotation_scale(style="ticks", location = "br")




library(ggnewscale)
library(RColorBrewer)
library(ggspatial)

suffixes <- unique(communes_filtered$suffix3)
suffix_colors <- setNames(brewer.pal(length(suffixes), "Paired"), suffixes)

ggplot(communes_filtered, aes(x = longitude, y = latitude)) +
  geom_sf(data = lacsWGS84, fill = "gray90", color = NA, alpha = 1) +
  geom_sf(data = ch_langue, aes(color = as.character(language)), fill = NA, alpha = 1, linewidth = 0.3) +
  scale_color_manual(name = "Langue principale", values = langue_colors,
                     labels = c("Allemand", "Français", "Italien", "Romanche")) +
  # geom_text(aes(label = suffix3, color = suffix3), size = 2) +
  geom_point(aes(color = suffix3), size = 1) +
  scale_color_manual(labels = paste0("-",sort(suffixes)), values = suffix_colors, name = "Suffixe") +
  # new_scale_color() +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_text(size=8),
        legend.text = element_text(size=7)) +
  annotation_scale(style="ticks", location = "br") +
  theme_minimal()
ggsave("heatmaps_suffixes/suffix_map10.png", width = 9, height = 8)

#### -début version clean
library(ggplot2)
library(ggnewscale)

suffixes <- unique(communes_filtered$suffix3)
suffixes <- c("ens", "ins", "wil", "gen")
suffix_colors <- setNames(brewer.pal(length(suffixes), "Set3"), suffixes)
suffix_colors[2] = "#FFFF73"
suffix_colors[9] = "#EED9DD"

suffix_colors[1:10] <- brewer.pal(n = 10, name = "Set3")

suffix_colors[2] = "#FDB462"

communes_filtered2 <- communes_sf2[which(communes_sf2$suffix3 %in% suffixes),]

ggplot(communes_filtered2, aes(x = longitude, y = latitude)) +

  # 1. Couche des frontières linguistiques avec une première échelle de couleur
  geom_sf(data = ch_langue, aes(color = as.character(language)), fill = NA, alpha = 1, linewidth = 0.3) +
  scale_color_manual(name = "Langue principale", values = langue_colors,
                     labels = c("Allemand", "Français", "Italien", "Romanche")) +
  
  # 2. Couche des lacs
  geom_sf(data = lacsWGS84, fill = "gray90", color = NA, alpha = 1) +
  
  # 3. Nouvelle échelle de couleur pour les suffixes
  new_scale_color() +
  
  # 4. Points colorés selon suffixe
  geom_point(aes(color = suffix3), size = 1) +
  scale_color_manual(name = "Suffixe", values = suffix_colors, labels = paste0("-", sort(suffixes))) +
  
  # 5. Habillage
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    panel.grid.major = element_blank()
  ) +
  
  # 5. Echelle
  annotation_scale(style="ticks", location = "br")
ggsave("heatmaps_suffixes/suffix_moran_top4_2.png", width = 9)

ggsave("heatmaps_suffixes/suffix_map10_34.png", width = 9)
#### -fin



library(sf)
library(dplyr)
library(ggplot2)
library(concaveman)

# 1. Convertir les points en sf
communes_sf2 <- st_as_sf(communes_centres, coords = c("longitude", "latitude"), crs = 4326)
communes_sf2$latitude <- communes_centres$latitude
communes_sf2$longitude <- communes_centres$longitude

# 2. Filtrer les suffixes fréquents (par exemple les 8 plus fréquents)
top_suffixes <- communes_centres %>%
  count(suffix3, sort = TRUE) %>%
  slice_max(n, n = 8) %>%
  pull(suffix3)

top_suffixes[9] = "ier"
top_suffixes[10] = "ano"

communes_filtered <- communes_sf2 %>% filter(suffix3 %in% top_suffixes)

# 3. Créer les zones par suffixe (polygones concaves)
zones_suffixes <- communes_filtered %>%
  group_by(suffix3) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("MULTIPOINT") %>%
  mutate(geometry = concaveman(geometry))

# 4. Tracer la carte
ggplot() +
  geom_sf(data = zones_suffixes, aes(fill = suffix3), alpha = 0.5, color = NA) +
  geom_sf(data = communes_filtered, aes(color = suffix3), size = 0.5) +
  scale_fill_viridis_d(name = "Suffixe (zone)") +
  scale_color_manual(values = rep("black", length(top_suffixes))) +  # Pour éviter surcharge
  theme_minimal() +
  labs(title = "Zones d'influence des suffixes toponymiques", x = NULL, y = NULL)

###
library(sf)
library(dplyr)
library(ggplot2)
library(concaveman)
library(purrr)

# 1. Convertir les points en sf
communes_sf2 <- st_as_sf(communes_centres, coords = c("longitude", "latitude"), crs = 4326)
communes_sf2$language = ch_aggregated_geolevels$language

# 2. Filtrer les suffixes fréquents
top_suffixes <- communes_centres %>%
  count(suffix3, sort = TRUE) %>%
  slice_max(n, n = 8) %>%
  pull(suffix3)

communes_filtered <- communes_sf2 %>% filter(suffix3 %in% top_suffixes)

# 3. Calcul des polygones concaves par suffixe
zones_suffixes <- communes_filtered %>%
  group_split(suffix3) %>%
  map_dfr(function(group) {
    suff <- unique(group$suffix3)
    if (nrow(group) >= 3) {
      poly <- concaveman(group)
      poly$suffix3 <- suff
      return(poly)
    } else {
      return(NULL)  # Ignore les groupes trop petits
    }
  })

# 4. Carte
ggplot() +
  geom_sf(data = zones_suffixes, aes(fill = suffix3), alpha = 0.5, color = NA) +
  geom_sf(data = communes_filtered, aes(color = suffix3), size = 0.5) +
  scale_fill_viridis_d(name = "Suffixe (zone)") +
  scale_color_manual(values = rep("black", length(top_suffixes))) +
  geom_sf(data = lacsWGS84, fill = "gray90", color = NA, alpha = 1) +
  geom_sf(data = ch_langue, aes(fill = NA, color = as.character(language), alpha = 1), linewidth = 0.3) +
  scale_color_manual(
    name = "Langue principale",
    values = langue_colors,
    labels = c("Allemand", "Français", "Italien", "Romanche")
  ) +
  theme_minimal() +
  labs(title = "Zones d'influence des suffixes toponymiques", x = NULL, y = NULL)

