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
  swiss_data_muni = rbind(test, (swiss_data[[11]][[1]])[[4]][[i]])
}
# Afficher le data frame résultant
View(swiss_data_muni)
dim(swiss_data_muni)
