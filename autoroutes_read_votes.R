# Charger les bibliothèques nécessaires
library(jsonlite)
library(dplyr)
library(sf)

# https://dam-api.bfs.admin.ch/hub/api/dam/assets/32588879/master
# Charger le fichier JSON
file_path = "/Users/rloup/Downloads/sd-t-17-02-20240922-eidgAbstimmung.json"
json_data = fromJSON(file_path, flatten = TRUE)

# Extraire les informations au niveau de la Suisse
swiss_data = json_data$schweiz$vorlagen %>% 
  bind_rows()
# %>% 
#   select(vorlagenId, vorlagenTitel.text, resultat.jaStimmenInProzent, resultat.neinStimmenAbsolut,
#          resultat.stimmbeteiligungInProzent, resultat.eingelegteStimmzettel, resultat.gueltigeStimmen)

# Assembler les données par commune
swiss_data_muni = c()
for (i in 1:26) {
  swiss_data_muni = rbind(swiss_data_muni, (swiss_data[[11]][[1]])[[4]][[i]])
}
swiss_data_muni$geoLevelnummer = as.integer(swiss_data_muni$geoLevelnummer)

swiss_data_muni = swiss_data_muni %>%
  filter(.[[1]] <= 7000)
# Afficher le data frame résultant
View(swiss_data_muni)
dim(swiss_data_muni)
# write.csv(swiss_data_muni, "swiss_data_muni.csv", row.names = F)


dataVot = read.csv("/Users/rloup/Documents/r_projects/swiss_political_autocorrelation/votes2023/formatted_data/dataVot2023.csv")
View(dataVot)

setdiff(swiss_data_muni$geoLevelname, dataVot$municipality)
setdiff(dataVot$municipality, swiss_data_muni$geoLevelname)


#### --- Mutations
# Charger les données
detailed_language = read.csv("/Users/rloup/Documents/r_projects/swiss_political_autocorrelation/votes2023/detailed_language.csv")

communes_mutees = read_excel("Communes_mutees_2024.xlsx")
names(communes_mutees) = communes_mutees[1,]
communes_mutees = communes_mutees[-1,]
names(communes_mutees) = c("no_mut", "old_canton", "old_no_district", "old_no_muni",
                         "old_name_muni", "new_canton", "new_no_district", "new_no_muni", "new_name_muni", "date")
# Add berner mutual voters
communes_mutees = rbind(communes_mutees, c(9999,"BE",9999, 629, "Oberhünigen", "BE", 9999, 628, "Zäziwil", "2024-01-01"))
write.csv(communes_mutees, "communes_mutees_2024.csv", row.names = F)

# Charger les données
communes_mutees <- read.csv("communes_mutees_2024.csv")
detailed_language <- read.csv("detailed_language.csv")

# Fonction pour générer le dataframe detailed_language_année en fonction des mutations
generer_detailed_language <- function(annee = NULL) {
  # Filtrer les mutations par année si spécifiée
  if (!is.null(annee)) {
    communes_mutees_filtre <- communes_mutees[format(as.Date(communes_mutees$date), "%Y") <= annee, ]
  } else {
    communes_mutees_filtre <- communes_mutees
  }
  
  # Trier les mutations par date pour appliquer les changements chronologiquement
  communes_mutees_filtre <- communes_mutees_filtre[order(as.Date(communes_mutees_filtre$date)), ]
  
  # Création d'un dictionnaire pour garder le traçage final de chaque commune
  final_mapping <- list()
  final_names <- list()
  
  # Itérer à travers les mutations pour créer un chemin de mappage vers la version finale
  for (i in seq_len(nrow(communes_mutees_filtre))) {
    old_no <- communes_mutees_filtre$old_no_muni[i]
    new_no <- communes_mutees_filtre$new_no_muni[i]
    new_name <- communes_mutees_filtre$new_name_muni[i]
    
    # Assigner le nouveau numéro à toutes les occurrences anciennes dans final_mapping
    if (!is.null(final_mapping[[as.character(old_no)]])) {
      old_no <- final_mapping[[as.character(old_no)]]
    }
    
    # Mettre à jour le mapping pour que chaque ancienne commune pointe vers la version finale
    final_mapping[[as.character(old_no)]] <- new_no
    final_names[[as.character(new_no)]] <- new_name  # Mise à jour du nom final de la commune
  }
  
  # Fonction pour obtenir le BFS_n final et le nom final si disponible
  get_final_info <- function(commune, original_name) {
    # Trouver le BFS_n final
    final_no <- commune
    while (!is.null(final_mapping[[as.character(final_no)]])) {
      final_no <- final_mapping[[as.character(final_no)]]
    }
    
    # Trouver le nom final ou utiliser le nom original
    final_name <- if (!is.null(final_names[[as.character(final_no)]])) final_names[[as.character(final_no)]] else original_name
    
    return(list(final_no = as.integer(final_no), final_name = final_name))
  }
  
  # Appliquer le mappage final pour obtenir BFS_n final et le nom de la commune
  final_info <- mapply(get_final_info, detailed_language$BFS_n, detailed_language$municipality, SIMPLIFY = FALSE)
  
  # Extraire les informations finales dans le dataframe
  detailed_language$final_BFS_n <- sapply(final_info, `[[`, "final_no")
  detailed_language$municipality <- sapply(final_info, `[[`, "final_name")
  
  # Ajouter les nouvelles communes qui n'existent pas encore dans detailed_language
  nouvelles_communes <- setdiff(unique(communes_mutees_filtre$new_no_muni), unique(detailed_language$final_BFS_n))
  if (length(nouvelles_communes) > 0) {
    nouvelles_entrees <- data.frame(
      BFS_n = nouvelles_communes, 
      municipality = sapply(nouvelles_communes, function(bfs) final_names[[as.character(bfs)]]),
      german = NA, french = NA, italian = NA, other_lang = NA, romansh = NA, 
      final_BFS_n = nouvelles_communes
    )
    detailed_language <- rbind(detailed_language, nouvelles_entrees)
  }
  
  # Remplacer BFS_n par le numéro final et supprimer la colonne intermédiaire
  detailed_language$BFS_n <- detailed_language$final_BFS_n
  detailed_language <- detailed_language[, !names(detailed_language) %in% "final_BFS_n"]
  
  # Supprimer les doublons pour ne garder que la dernière version de chaque commune
  detailed_language_annee <- detailed_language[!duplicated(detailed_language$BFS_n), ]
  
  return(detailed_language_annee)
}

# Exemple d'utilisation : obtenir detailed_language_2024
detailed_language_2024 <- generer_detailed_language(annee = 2024)

# 4 Bernese communes vote in other electoral districts
# https://www.belex.sites.be.ch/app/fr/texts_of_law/141.111
for (i in 1:dim(detailed_language_2024)[1]) {
  # Oberhünigen -> Zäziwil
  if (detailed_language_2024$BFS_n[i] == 629) { # if the commune has no polling station, it is aggregated
    detailed_language_2024$BFS_n[i] <- 628
    detailed_language_2024$municipality[i] <- "Zäziwil"
  }
}

# Sum same lines
detailed_language_2024 <- detailed_language_2024 %>%
  group_by(BFS_n, municipality) %>%
  summarise(across(c(german, french, italian, other_lang, romansh), sum, na.rm = TRUE))

# Ajouter la colonne language_region
detailed_language_2024 <- detailed_language_2024 %>%
  mutate(language_region = case_when(
    german >= french & german >= italian & german >= romansh ~ 1,  # German-majority
    french >= german & french >= italian & french >= romansh ~ 2,  # French-majority
    italian >= german & italian >= french & italian >= romansh ~ 3, # Italian-majority
    romansh >= german & romansh >= french & romansh >= italian ~ 4 # Romansh-majority
  ))


# Sauvegarder le résultat si nécessaire
write.csv(detailed_language_2024, "detailed_language_2024.csv", row.names = FALSE)

# Vérifier
# View(cbind(detailed_language_2024[,1:2], swiss_data_muni[,1:2]))

### --- Shapefiles
ch = st_read("/Users/rloup/Documents/r_projects/spatial_autocorrelation_political_opinions/data/GIS_data/ch.shp")

# Charger le fichier des mutations
# communes_mutees <- read.csv("communes_mutees_2024.csv")

# Filtrer les mutations pertinentes pour 2024
# communes_mutees <- communes_mutees %>%
#   filter(as.Date(DATUM_AEND) <= as.Date("2024-12-31"))

# Joindre les mutations pour obtenir le BFS_NUMMER final
ch <- ch %>%
  left_join(communes_mutees %>% select(old_no_muni, new_no_muni, new_name_muni),
            by = c("BFS_NUMMER" = "old_no_muni")) %>%
  mutate(
    BFS_FINAL = ifelse(is.na(new_no_muni), BFS_NUMMER, new_no_muni),
    NAME = ifelse(is.na(new_name_muni), NAME, new_name_muni)  # Met à jour le nom si nécessaire
  )

# Agréger les surfaces et populations par BFS_NUMMER final
ch_aggregated <- ch %>%
  group_by(BFS_FINAL) %>%
  summarise(
    GEM_FLAECH = sum(GEM_FLAECH, na.rm = TRUE),  # Somme des surfaces
    EINWOHNERZ = sum(EINWOHNERZ, na.rm = TRUE),  # Somme des populations
    NAME = first(NAME),                          # Conserver un nom représentatif
    BEZIRKSNUM = first(BEZIRKSNUM),              # Conserver un numéro de district unique
    KANTONSNUM = first(KANTONSNUM),              # Conserver un numéro de canton unique
    geometry = st_union(geometry)               # Union géométrique des polygones
  )
# Vérification des résultats
dim(ch_aggregated)
print(ch_aggregated)
plot(ch_aggregated$geometry)
View(cbind(ch_aggregated, swiss_data_muni[,1:2]))

#### --- Communes centres
communes_centres = read.csv("communes_ch_centres2024.csv")
dim(communes_centres)
View(cbind(communes_centres, swiss_data_muni[,1:2]))


### --- Matrix fixing
# Road distance matrix
distance_mat = read.csv("/Users/rloup/Documents/r_projects/swiss_political_autocorrelation/distance_mat_2023.csv")
rownames(distance_mat) = distance_mat[, 1]  # Set first column as row names
distance_mat[, 1] = NULL  # Remove original column from data frame

# Road time matrix
time_mat = read.csv("/Users/rloup/Documents/r_projects/swiss_political_autocorrelation/time_mat_2023.csv")
rownames(time_mat) = time_mat[, 1]  # Set first column as row names
time_mat[, 1] = NULL  # Remove original column from data frame

elev_dist = read.csv("/Users/rloup/Documents/r_projects/spatial_autocorrelation_political_opinions/data/distances/elev_dist_2023.csv")
rownames(elev_dist) = time_mat[, 1]
colnames(elev_dist) = colnames(time_mat)


# # -- Load data to change
# distance_mat = read.csv("distance_mat_2023.csv")
# # set specific column as row names
# rownames(distance_mat) = distance_mat[,1]
# # remove original column from data frame
# distance_mat[,1] = NULL

matToChange = distance_mat
matToChange = time_mat
matToChange = elev_dist

# time_mat = read.csv("time_mat_2023.csv")
# # set specific column as row names
# rownames(time_mat) = time_mat[,1]
# # remove original column from data frame
# time_mat[,1] = NULL
# matToChange = time_mat

# set initial and final index and name

positionInit = which(dataVot$BFS_n==6775) # Bonfol
positionFin = 2132
newBFS = 6812

# --- Computation
# save column to move
cMat = matToChange[,positionInit]
# remove 0/1 which was in diagonal
cMat = cMat[-positionInit]

# save row to move
rMat = matToChange[positionInit,]
# remove 0/1 which was in diagonal
rMat = rMat[-positionInit]
rownames(rMat) = newBFS

# Remove misplaced column/row
matToChange = matToChange[-positionInit,-positionInit]

# if last row/column
matToChange = rbind(matToChange, rMat)
cMat[2132] = 0
matToChange = cbind(matToChange, cMat)
matToChange = as.data.frame(matToChange)
colnames(matToChange)[2132] = paste0("X", newBFS)
# end if last row/column


for (i in c(947, 993, 2456, 6773, 4042, 629)) {
  positionRemove = which(dataVot$BFS_n == i)
  matToChange = matToChange[-positionRemove,-positionRemove]
}
dim(matToChange)
# Save results
# write.csv(matToChange, "distance_mat_2024.csv")
# write.csv(matToChange, "time_mat_2024.csv")
# write.csv(matToChange, "elev_dist_2024.csv")

# ------------------------------------------------------------------------------
# Vote analysis
swiss_data[[3]][[1]]

# --- Weight f: "voix valides"
f = swiss_data_muni$resultat.gueltigeStimmen / sum(swiss_data_muni$resultat.gueltigeStimmen)
n = length(f) # 2126

# --- Centering matrix
H = diag(n) - rep(1, n) %*% t(f) # centering matrix

# --- Distances and kernels
DX = as.matrix(dist(swiss_data_muni$resultat.jaStimmenInProzent)^2) # political distances between municipalities
KX = -0.5 * diag(sqrt(f)) %*% H %*% DX %*% t(H) %*% diag(sqrt(f)) # political kernel

##### Only MDS

# Mixed kernel between politics and geographical distances
# Kr = -0.5 * diag(sqrt(f)) %*% H %*% D_corr %*% t(H) %*% diag(sqrt(f))
# Kn = -0.5 * diag(sqrt(f)) %*% H %*% Dn %*% t(H) %*% diag(sqrt(f))

eigen_val <- eigen(KX)
U <- eigen_val$vectors
lambda <- eigen_val$values
length(lambda[lambda>0]) # count positive lambda
lambda <- pmax(lambda,0)

# Weighted MDS
Y = diag(1/sqrt(f)) %*% U %*% diag(sqrt(lambda))
# Y = diag(1/rep(1/length(f), length(f))) %*% U %*% diag(sqrt(lambda)) # unweighted

# 2 dimensions of MDS data frame
mds = as.data.frame(Y[,1:2])
mds$V1 = Re(mds$V1)
mds$V2 = Re(mds$V2)
mds$commune = detailed_centres_2024$NAME
mds$langue = as.character(detailed_language_2024$language_region)
mds$f = f

# 15 bigggest cities
mds_filtered = mds[mds$f > sort(mds$f, decreasing = TRUE)[15], ]

# inertia explained in %
propDeltaPC = round(100*lambda[1:2]/sum(lambda), digits = 1)

magnif = 0.2+0.5*(log(f)-min(log(f)))/(max(log(f))-min(log(f))) # defines a magnification factor for the object weights (here from 0.5 to 2)
xlab = paste("Factor 1, inertia explained =",propDeltaPC[1],"%")
ylab = paste("Factor 2, inertia explained =",propDeltaPC[2],"%")
library(ggplot2)
library(ggtext)
mds_plot = ggplot() +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  # geom_point(aes(x = -mds[,1], y = -mds[,2], size=f, color=mds$langue),
  geom_point(aes(x = -mds[,1], y = -mds[,2], color=mds$langue),
             alpha = magnif) +
  geom_text(aes(x = -mds_filtered[,1], y = -mds_filtered[,2], label = mds_filtered$commune)) +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"),
                     labels = c("German", "French", "Italian","Romansh")) +
  labs(x = xlab, y = ylab) +
  labs(size = "Reg. weight *f*", color = "Language") +
  scale_size_continuous(range = c(1, 8)) +
  theme_minimal() +
  theme(legend.title = element_markdown(lineheight = 1.2))
mds_plot

# --- Unidimensional factor representation (for 1 factor MDS)
# Plot with outlier names
mds_outliers = mds %>%
  group_by(langue) %>%
  mutate(outlier = ifelse(V1 < quantile(V1, 0.25) - 1.5 * IQR(V1) |
                            V1 > quantile(V1, 0.75) + 1.5 * IQR(V1), TRUE, FALSE))

ggplot(mds_outliers, aes(x = langue, y = V1, fill = langue)) + 
  geom_boxplot() +
  geom_text(data = subset(mds_outliers, outlier == TRUE), 
            aes(label = commune), 
            size = 3, vjust = -0.5) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"),
                    labels = c("German", "French", "Italian","Romansh")) +
  theme_minimal() +
  labs(x = "Language", y = "", fill = "Language") +
  theme(legend.position = "top")
