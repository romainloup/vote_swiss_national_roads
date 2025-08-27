#--------------------------------
# Packages
#--------------------------------
library(dplyr)
library(readxl)

#--------------------------------
# Read data
#--------------------------------

# Nombre de contribuables par classe
special_class = read_xlsx("/Users/rloup/Downloads/statistik-dbst-np-gden-2020-normalfall-fr.xlsx", sheet=2, skip = 2)
# special_class = special_class[,-c(1:2)]


# sheet = 2 -> nb contribuables
# sheet = 3 -> revenu imposable
special_class = read_xlsx("/Users/rloup/Downloads/statistik-dbst-np-gden-2021-normalfall-fr.xlsx",
                          sheet = 2, skip = 2) %>%
  select(-c(1:2)) %>%
  mutate(across(.cols = -c(1:2), ~ ifelse(. == "-", 0, .))) %>%
  mutate(across(.cols = -c(1:2), as.numeric)) %>%
  filter(.[[1]] <= 7000) %>% # Filtrer les lignes où la 1ère colonne >= 7000 pour supprimer les "non communes"
  arrange(.[[1]])

names(special_class)[names(special_class) == 'Gemeinde ID'] <- 'BFS_n'
names(special_class)[names(special_class) == 'Gemeinde / Commune'] <- 'municipality'

# View(special_class)
# special_class = grouped_data

# Mutations
mutation_2023 = read_excel("/Users/rloup/Documents/r_projects/swiss_political_autocorrelation/votes2023/Communes_mutees_2023.xlsx")
mutation_2024 = read_excel("/Users/rloup/Documents/r_projects/vote_swiss_national_roads/Communes_mutees_2024.xlsx") # pour données 2024

names(mutation_2023) = mutation_2023[1,]
mutation_2023 = mutation_2023[-1,]
names(mutation_2023) = c("no_mut", "old_canton", "old_no_district", "old_no_muni",
                         "old_name_muni", "new_canton", "new_no_district", "new_no_muni", "new_name_muni", "date")
# View(mutation_2023)
names(mutation_2024) = mutation_2024[1,]
mutation_2024 = mutation_2024[-1,]
names(mutation_2024) = c("no_mut", "old_canton", "old_no_district", "old_no_muni",
                         "old_name_muni", "new_canton", "new_no_district", "new_no_muni", "new_name_muni", "date")

# mutation_2024 = mutation_2023
mutation_tot = rbind(mutation_2023, mutation_2024)
mutation_tot = unique(mutation_tot)
mutation_2023 = mutation_tot

# Merge data frames on columns BFS_n and municipality, and old_no_muni and old_name_muni
merged_data <- merge(special_class, mutation_2023, 
                     by.x = c("BFS_n", "municipality"), 
                     by.y = c("old_no_muni", "old_name_muni"), 
                     all.x = TRUE)

# Replace merged_data$BFS_n by merged_data$new_no_muni if merged_data$new_no_muni is not NA
merged_data$BFS_n[!is.na(merged_data$new_no_muni)] <- merged_data$new_no_muni[!is.na(merged_data$new_no_muni)]
merged_data$municipality[!is.na(merged_data$new_name_muni)] <- merged_data$new_name_muni[!is.na(merged_data$new_name_muni)]

# Delete unused columns
merged_data = merged_data[,1:13]

# Repeat the process for change of names
merged_data <- merge(merged_data, mutation_2023, 
                     by.x = c("BFS_n", "municipality"), 
                     by.y = c("old_no_muni", "old_name_muni"), 
                     all.x = TRUE)

# Replace merged_data$BFS_n by merged_data$new_no_muni if merged_data$new_no_muni is not NA
# deux lignes dessous pas obligatoires ???
merged_data$BFS_n[!is.na(merged_data$new_no_muni)] <- merged_data$new_no_muni[!is.na(merged_data$new_no_muni)]
merged_data$municipality[!is.na(merged_data$new_name_muni)] <- merged_data$new_name_muni[!is.na(merged_data$new_name_muni)]

merged_data = merged_data[,1:13]


# Pas besoin, juste check pour données 2023
dataVot = read.csv("/Users/rloup/Documents/r_projects/swiss_political_autocorrelation/votes2023/formatted_data/dataVot2023.csv")
# Chaeck if there are some differences
setdiff(dataVot$municipality, merged_data$municipality)
setdiff(merged_data$municipality,dataVot$municipality)

# 4 Bernese communes vote in other electoral districts
# https://www.belex.sites.be.ch/app/fr/texts_of_law/141.111/versions/2415
for (i in 1:dim(merged_data)[1]) {
  # Niedermuhlern -> Wald
  if (merged_data$BFS_n[i] == 877) { # if the commune has no polling station, it is aggregated
    merged_data$BFS_n[i] <- 888
    merged_data$municipality[i] <- "Wald (BE)"
  }
  # Deisswil bei Münchenbuchsee -> Wiggiswil
  if (merged_data$BFS_n[i] == 535) {
    merged_data$BFS_n[i] <- 553
    merged_data$municipality[i] <- "Wiggiswil"
  }
  # Hellsau -> Höchstetten
  if (merged_data$BFS_n[i] == 408) {
    merged_data$BFS_n[i] <- 410
    merged_data$municipality[i] <- "Höchstetten"
  }
  # Meienried -> Büren an der Aare
  if (merged_data$BFS_n[i] == 389) {
    merged_data$BFS_n[i] <- 383
    merged_data$municipality[i] <- "Büren an der Aare"
  }
  # Pour données 2024
  if (merged_data$BFS_n[i] == 629) {
    merged_data$BFS_n[i] <- 628
    merged_data$municipality[i] <- "Zäziwil"
  }
}

# If they exist, find municipalities with the same name but different BFS_n
duplicate_municipalities <- merged_data %>%
  group_by(municipality) %>%
  filter(n_distinct(BFS_n) > 1) %>%
  arrange(municipality, BFS_n)

# Print result
print(duplicate_municipalities)
# Correct false BFS numbers
# Correct false BFS numbers
merged_data$BFS_n[which(merged_data$BFS_n==30)] = "291"
merged_data$BFS_n[which(merged_data$BFS_n==3378)] = "3396"




# Create a grouped data set with a sum of merged municipalities
# grouped_data <- merged_data %>%
#   group_by(BFS_n, municipality) %>%
#   summarise(across(c(`0`, `1 - 30'000`, `30'001 - 40'000`, `40'001 - 50'000`, `50'001 - 75'000`,
#                      `75'001 - 100'000`, `100'001 - 200'000`, `200'001 - 500'000`,
#                      `500'001 - 1'000'000`, `1'000'001 - u.m./et plus`), sum, na.rm = TRUE))

grouped_data <- merged_data %>%
  group_by(BFS_n, municipality) %>%
  summarise(across(c(`0`, `1 - 30'000`, `30'001 - 40'000`, `40'001 - 50'000`, `50'001 - 75'000`,
                     `75'001 - 100'000`, `100'001 - 200'000`, `200'001 - 500'000`,
                     `500'001 - 1'000'000`, `1'000'001 - u.m./et plus`, `Total / Totale`), 
                   ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

dim(grouped_data)
# View(grouped_data)

# Make BFS numbers numeric
grouped_data$BFS_n = as.numeric(grouped_data$BFS_n)

# Reorder grouped_data by ascending BFS_n
grouped_data <- grouped_data %>%
  arrange(BFS_n)

# View(grouped_data)

# Check
length(unique(grouped_data$municipality)) # ok if = 2132

write.csv(grouped_data, "IFD_classes_revenu_net_normaux_nb_contribuables_2024.csv", row.names = F)
# write.csv(grouped_data, "IFD_classes_revenu_net_normaux_2024.csv", row.names = F)
