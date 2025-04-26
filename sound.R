install.packages(c("stringdist", "phonics", "proxy"))
library(stringdist)  # pour la distance de Levenshtein
library(phonics)     # pour les algorithmes de phonétisation (Soundex, NYSIIS, etc.)
library(proxy)       # pour calculer des distances personnalisées

# Supposons que dataVot contient les noms de communes
communes_sound <- unique(ch_aggregated_geolevels$NAME)
n <- length(communes_sound)

# Option 1 : Distance Levenshtein brute
dist_lev <- stringdistmatrix(communes_sound, communes_sound, method = "lv")
rownames(dist_lev) <- colnames(dist_lev) <- communes_sound

# Option 2 : Distance Levenshtein sur Soundex

# Convertir en ASCII de base
library(stringi)

clean_name <- function(x) {
  x <- tolower(x)                             # tout en minuscules
  x <- stringi::stri_trans_general(x, "Latin-ASCII")  # retirer accents
  x <- gsub("[^a-z]", "", x)                 # ne garder que les lettres a-z
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

# Nettoyage des noms
clean_name <- function(x) {
  x <- tolower(x)
  x <- stri_trans_general(x, "Latin-ASCII") # enlever accents
  x <- gsub("[^a-z]", "", x)                # ne garder que les lettres
  return(x)
}

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
sort(mat_bigram["Ebikon", ])[1:100]  # les 10 noms les plus proches phonétiquement

# Carte
ch_aggregated_geolevels$ebikon = mat_bigram[,1643]
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

# --- Road distance
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
  mds$f = fi
  mds$voters = ch_aggregated_geolevels$swiss_data_muni.f
  
  mds_filtered = mds[mds$voters > sort(mds$voters, decreasing = TRUE)[50], ]
  
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
  geom_point(aes(color = phon1), size = 1) +
  scale_color_viridis_c() +
  labs(title = "Carte géographique colorée par 1er axe phonétique",
       color = "Axe phonétique 1") +
  theme_minimal()

ggplot(merged_df, aes(x = longitude, y = latitude)) +
  geom_point(aes(color = phon2), size = 1) +
  scale_color_viridis_c() +
  labs(title = "Carte géographique colorée par 2e axe phonétique") +
  theme_minimal()

ggplot(merged_df) +
  geom_segment(aes(x = longitude, y = latitude, xend = phon1*10000, yend = phon2*10000), alpha = 0.2) +
  geom_point(aes(x = longitude, y = latitude), color = "blue", size = 0.3) +
  geom_point(aes(x = phon1*10000, y = phon2*10000), color = "red", size = 0.3) +
  labs(title = "Déformation géo ↔ phonétique", subtitle = "bleu = réel, rouge = phonétique") +
  theme_void()

# --- leaflet map

maxData_sound = max(abs(mds_df$phon1))

scale_range_sound <- c(-maxData_sound, maxData_sound)
pal_sound <- colorNumeric("PRGn", domain = scale_range_sound)
rev_pal_sound <- colorNumeric("PRGn", domain = scale_range_sound, reverse = TRUE)

leaflet(ch_aggregated) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  # addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lat=46.637785, lng=8.2 , zoom=7) %>%
  # municipality polygons
  addPolygons(
    fillColor = ~pal_sound(mds_df$phon1),
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
    label = paste0(ch_aggregated$NAME, ": ", mds_df$phon1),
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
    pal = rev_pal_sound,
    values = scale_range_sound,
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

# Suffixes de 3 lettres
suffix3_tab <- suffix_freq(communes_clean, n = 3)
suffix3_sorted <- sort(suffix3_tab, decreasing = TRUE)
head(suffix3_sorted, 20)


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
    geom_point(data = communes_centres, aes(x = longitude, y = latitude), color = "gray80", size = 0.4) +
    
    # Communes avec le suffixe en rouge
    geom_point(data = coord_sel, aes(x = longitude, y = latitude), color = "red", size = 0.9) +
    
    # Personnalisation
    scale_color_manual(
      name = "Langue principale",
      values = langue_colors,
      labels = c("Allemand", "Français", "Italien", "Romanche")
    ) +
    labs(title = paste0("Répartition des communes terminant par -", target_suffix)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
    ) + 
    
    labs(x = NULL, y = NULL)
}

# Exemple d'utilisation :
plot_suffix_map(target_suffix = "ens", 
                communes_centres = communes_centres, 
                coord_sf = ch_aggregated_geolevels, 
                lacs_sf = lacsWGS84, 
                ch_aggregated_geolevels = ch_aggregated_geolevels)


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
plot_suffix_heatmap("gen", communes_centres, lacsWGS84, ch_aggregated_geolevels)

# v2
plot_suffix_heatmap_filled <- function(target_suffix, communes_centres, lacs_sf, ch_aggregated_geolevels) {
  
  # Sélection des communes avec le suffixe
  selection <- grepl(paste0(target_suffix, "$"), communes_clean)
  selected_communes <- communes_centres[communes_centres$NAME %in% communes_sound[selection], ]
  
  d <- MASS::kde2d(communes_centres$longitude, communes_centres$latitude, lims = st_bbox(ch_aggregated_geolevels)[c(1, 3, 2, 4)])
  dens <- data.frame(expand.grid(x = d$x, y = d$y), z = as.vector(d$z))
  
  # Affichage
  ggplot() +
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
    
    # Palette de couleurs
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

plot_suffix_heatmap_filled("gen", communes_centres, lacsWGS84, ch_aggregated_geolevels)

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
top_n <- 6
top_suffixes <- results_sorted$suffix[1:top_n]

# Générer et enregistrer les figures
for (suf in top_suffixes) {
  plot <- plot_suffix_heatmap(suf, communes_centres, lacsWGS84, ch_aggregated_geolevels)
  
  ggsave(filename = paste0("heatmaps_suffixes/heatmap_suffix_", suf, ".png"),
         plot = plot, width = 7, height = 6, dpi = 300)
}


# --- Plusieurs suffixes
# Choix de 4 suffixes typiques ou fortement régionalisés
suffixes_cibles <- c("ens", "wil", "dorf", "o", "y", "kon", "gen")
colors_suffixes <- c("ens" = "#a6cee3", "wil" = "#b2df8a", "dorf" = "#fb9a99", "o" = "#33a02c", "y" = "#fb9a99", "kon" = "#e31a1c", "gen" = "#1f78b4")

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

# Carte finale avec couleurs par suffixe
ggplot() +
  geom_sf(data = suffix_map_df, aes(fill = suffix_category, color = as.character(language)), size = 0.1) +
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
  labs(title = "Répartition géographique de suffixes toponymiques") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "right"
  )

