# Charger les bibliothèques nécessaires
library(jsonlite)
library(dplyr)

# https://dam-api.bfs.admin.ch/hub/api/dam/assets/32588879/master
# Charger le fichier JSON
file_path <- "/Users/rloup/Downloads/sd-t-17-02-20240922-eidgAbstimmung.json"
json_data <- fromJSON(file_path, flatten = TRUE)

# Extraire les informations au niveau de la Suisse
swiss_data <- json_data$schweiz$vorlagen %>% 
  bind_rows() %>% 
  select(vorlagenId, vorlagenTitel.text, resultat.jaStimmenInProzent, resultat.neinStimmenAbsolut,
         resultat.stimmbeteiligungInProzent, resultat.eingelegteStimmzettel, resultat.gueltigeStimmen)

test = c()
for (i in 1:26) {
  test = rbind(test, (swiss_data[[11]][[1]])[[4]][[i]])
}
test = rbind((swiss_data[[11]][[1:26]])[[4]][[1]])
# Afficher le data frame résultant
print(swiss_data)
