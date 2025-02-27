# Chargement des bibliothèques
library(dplyr)

# Chargement des fichiers
communes_mutees_pend <- read.csv("communes_mutees_2018-2024.csv")
ts_x <- read.csv("ts-x-11.04.04.05-2018.csv", sep = ";")

# Correction des noms de colonnes pour ts_x
colnames(ts_x) <- c("PERSPECTIVE", "REF_YEAR", "GEO_CANT_RESID", "GEO_COMM_RESID", 
                    "GEO_CANT_WORK", "GEO_COMM_WORK", "VALUE")

# Conversion des colonnes numériques
communes_mutees_pend <- communes_mutees_pend %>%
  mutate(across(c(old_no_muni, new_no_muni), as.integer))
ts_x <- ts_x %>%
  mutate(across(c(GEO_COMM_RESID, GEO_COMM_WORK, VALUE), as.integer))

# Mises à jour des communes
while (TRUE) {
  # Identifie les mutations en cours
  mutations <- ts_x %>%
    inner_join(communes_mutees_pend, by = c("GEO_COMM_RESID" = "old_no_muni"))
  
  if (nrow(mutations) == 0) break
  
  # Agrégation des valeurs pour les nouvelles communes
  aggregated <- mutations %>%
    group_by(PERSPECTIVE, REF_YEAR, GEO_COMM_RESID = new_no_muni, GEO_COMM_WORK) %>%
    summarise(VALUE = sum(VALUE), .groups = "drop")
  
  # Met à jour les données avec les nouvelles communes
  ts_x <- ts_x %>%
    anti_join(mutations, by = c("PERSPECTIVE", "REF_YEAR", "GEO_COMM_RESID", "GEO_COMM_WORK")) %>%
    bind_rows(aggregated)
}

# Écriture du fichier final
write.csv(ts_x, "ts-x-updated.csv", row.names = FALSE)



# --- v2
# Chargement des bibliothèques
library(dplyr)

# Chargement des fichiers
communes_mutees <- read.csv("communes_mutees_2018-2024.csv")
ts_x <- read.csv("ts-x-11.04.04.05-2018.csv", sep = ";")

# Correction des noms de colonnes pour ts_x
colnames(ts_x) <- c("PERSPECTIVE", "REF_YEAR", "GEO_CANT_RESID", "GEO_COMM_RESID", 
                    "GEO_CANT_WORK", "GEO_COMM_WORK", "VALUE")

# Conversion des colonnes numériques
communes_mutees <- communes_mutees %>%
  mutate(across(c(old_no_muni, new_no_muni), as.integer))
ts_x <- ts_x %>%
  mutate(across(c(GEO_COMM_RESID, GEO_COMM_WORK, VALUE), as.integer))

# Fonction pour mettre à jour les communes
update_communes <- function(ts_x, communes_mutees) {
  # Créer une table de correspondance pour les mutations
  mapping <- communes_mutees %>%
    select(old_no_muni, new_no_muni) %>%
    distinct()
  
  # Mettre à jour GEO_COMM_RESID et GEO_COMM_WORK
  ts_x <- ts_x %>%
    left_join(mapping, by = c("GEO_COMM_RESID" = "old_no_muni")) %>%
    mutate(GEO_COMM_RESID = if_else(!is.na(new_no_muni), new_no_muni, GEO_COMM_RESID)) %>%
    select(-new_no_muni) %>%
    left_join(mapping, by = c("GEO_COMM_WORK" = "old_no_muni")) %>%
    mutate(GEO_COMM_WORK = if_else(!is.na(new_no_muni), new_no_muni, GEO_COMM_WORK)) %>%
    select(-new_no_muni)
  
  # Agréger les données pour les nouvelles communes
  ts_x <- ts_x %>%
    group_by(PERSPECTIVE, REF_YEAR, GEO_COMM_RESID, GEO_COMM_WORK) %>%
    summarise(VALUE = sum(VALUE), .groups = "drop")
  
  return(ts_x)
}

# Mise à jour des communes dans une boucle pour gérer les mutations multiples
while (TRUE) {
  updated_ts_x <- update_communes(ts_x, communes_mutees)
  if (nrow(updated_ts_x) == nrow(ts_x)) break
  ts_x <- updated_ts_x
  
  print(nrow(updated_ts_x))
}

# Écriture du fichier final
write.csv(ts_x, "ts-x-updated.csv", row.names = FALSE)
