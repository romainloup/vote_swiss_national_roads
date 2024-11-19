# Charger les bibliothèques nécessaires
library(jsonlite)
library(dplyr)

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
  # Niedermuhlern -> Wald
  if (detailed_language_2024$BFS_n[i] == 629) { # if the commune has no polling station, it is aggregated
    detailed_language_2024$BFS_n[i] <- 628
    detailed_language_2024$municipality[i] <- "Zäziwil"
  }
}
# Sum same lines
detailed_language_2024 <- detailed_language_2024 %>%
  group_by(BFS_n, municipality) %>%
  summarise(across(c(german, french, italian, other_lang, romansh), sum, na.rm = TRUE))

# Sauvegarder le résultat si nécessaire
write.csv(detailed_language_2024, "detailed_language_2024.csv", row.names = FALSE)
