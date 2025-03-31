library(dplyr)
library(leaflet)
library(ggrepel)

yes = read.csv("yes_2024.csv")

# Filtrer les lignes où au moins une des colonnes est égale à 821
result_theme_road <- voteInfo %>%
  filter(theme1_id == 821 | theme2_id == 821 | theme3_id == 821) %>%
  select(vote_id) # Sélectionner uniquement la colonne vote_id

# Afficher les résultats
print(result_theme_road)

which(names(yes) == result_theme_road)

# Extraire les vote_ids de `result` (vecteur)
vote_ids <- result_theme_road$vote_id # Si result est un dataframe, sinon : vote_ids <- result

# Filtrer les colonnes de yes en fonction des noms des colonnes
selected_yes <- yes %>%
  select(all_of(intersect(colnames(yes), as.character(paste0("X",vote_ids)))))

# Afficher le dataframe résultant
View(selected_yes) # uniquement les votes avec thème "Construction des routes"

# Extraire les noms des colonnes de selected_yes
selected_vote_ids <- colnames(selected_yes)

# Filtrer voteInfo pour obtenir les `object_fr` correspondants
objects_fr <- voteInfo %>%
  filter(vote_id %in% selected_vote_ids) %>%
  select(vote_id, date, object_fr)

# Afficher les résultats
View(objects_fr)

# --- Distances and kernels
# DX = as.matrix(dist(swiss_data_muni$resultat.jaStimmenInProzent)^2) # political distances between municipalities 1 vote
DX_2024 = as.matrix(dist(yes[,3:14])^2) # political distances between municipalities, 2024 votes
DX_theme = as.matrix(dist(selected_yes)^2) # political distances between municipalities, road theme
DX = as.matrix(dist(yes[,3:dim(yes)[2]])^2) # political distances between municipalities, 381 votes

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


mds_plot = ggplot() +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(aes(x = -mds[,1], y = -mds[,2], size=f, color=mds$langue),
             alpha = magnif) +
  # geom_point(aes(x = -mds[,1], y = -mds[,2], color=mds$langue),
  #            alpha = magnif) +
  geom_text(aes(x = -mds_filtered[,1], y = -mds_filtered[,2], label = mds_filtered$commune)) +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"),
                     labels = c("German", "French", "Italian","Romansh")) +
  labs(x = xlab, y = ylab) +
  labs(size = "Reg. weight *f*", color = "Language") +
  scale_size_continuous(range = c(1, 8)) +
  theme_minimal() +
  theme(legend.title = element_markdown(lineheight = 1.2))
mds_plot

# Boxplot pondéré
library(ggplot2)
library(ggrepel)
library(dplyr)

mds_outliers = mds %>%
  group_by(langue) %>%
  mutate(outlier = ifelse(V1 < quantile(V1, 0.25) - 1.5 * IQR(V1) |
                            V1 > quantile(V1, 0.75) + 1.5 * IQR(V1), TRUE, FALSE))

swiss_data_muni$language_region = detailed_language_2024$language_region
swiss_data_muni$language_region = ch_aggregated_geolevels$Typologie.urbain.rural
swiss_data_muni$outlier = mds_outliers$outlier
swiss_data_muni$f = f

# Ajouter les coordonnées jitterées dans les données pour un alignement parfait
swiss_data_muni <- swiss_data_muni %>%
  mutate(jitter_x = jitter(as.numeric(language_region), amount = 0.3), # Jitter pour l'axe X
         # jitter_y = resultat.jaStimmenInProzent) # Les valeurs Y restent identiques
         jitter_y = residu_road) # Les valeurs Y restent identiques

library(ggrepel)
# ggplot(swiss_data_muni, aes(x = language_region, y = resultat.jaStimmenInProzent, fill = as.character(language_region))) +
ggplot(swiss_data_muni, aes(x = language_region, y = residu_road, fill = as.character(language_region))) +
  
  # Boxplot modernisé
  geom_boxplot(alpha = 0.2, color = "black", outlier.shape = NA, aes(weight = f)) + # alpha = 0.2 pour langue
  # Points individuels avec jitter et taille proportionnelle à 'f'
  geom_point(data = swiss_data_muni,
             aes(x = jitter_x, y = jitter_y, color = as.character(language_region), size = f), 
             alpha = 0.6) +
  # Labels pour les outliers alignés avec les points jitterés
  geom_text_repel(
    # data = subset(swiss_data_muni, outlier == TRUE & rank(-f) <= 10),
    data = subset(swiss_data_muni, outlier == TRUE | rank(-f) <= 15),
    aes(x = jitter_x, y = jitter_y, label = geoLevelname),
    size = 3,
    nudge_y = 2, # Légère élévation des labels
    point.padding = 0.3,
    box.padding = 0.4,
    segment.color = "gray50", # Couleur des lignes
    segment.size = 0.5, # Taille des segments
    max.overlaps = Inf
  ) +
  # Palette de couleurs pour le remplissage
  # scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"),
  #                   labels = c("German", "French", "Italian", "Romansh")) +
  scale_fill_manual(values = c("#8dd3c7", "#ffffb3", "#bebada"),
                    labels = c("Urban", "Intermediate", "Rural")) +
  
  # scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"), guide = "none") +
  scale_color_manual(values = c("#8dd3c7", "#ffffb3", "#bebada"), guide = "none") +
  
  # Échelle pour la taille des points
  scale_size_continuous(name = "Taille de la commune", range = c(1, 10)) +
  # Ligne de référence
  geom_hline(yintercept = 50, color = "red", linetype = "dashed", size = 0.7) +
  # Design moderne
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  labs(
    # title = "Résultats des votes par région linguistique",
    # subtitle = "Distribution des résultats en pourcentage de votes 'oui' par langue, pondérés par la taille des communes",
    # x = "Linguistic region",
    x = "Urban-rural BFS 3-degree typology",
    # y = "'yes' pourcentage",
    y = "Residuals of 'yes' pourcentage",
    # fill = "Spoken language"
    fill = "Typology"
  )
# + scale_x_discrete(labels = c("German", "French", "Italian", "Romansh"))

ggsave("/Users/rloup/Library/CloudStorage/OneDrive-UniversitédeLausanne/routes_nationales/images/weighted_boxplot_typology_residuals.png", width = 9, height = 8)


library(readxl)
#--------------------------------
# Interactive result map
#--------------------------------

# Read and transform main Swiss lakes GIS data
lakes = st_read("/Users/rloup/Documents/r_projects/swiss_political_autocorrelation/lakesSMV25/SMV25_lakes.shp")
lakes = st_transform(lakes, crs = 4326)  # Transform to Geodetic CRS

library(leaflet)
palDiv = colorNumeric("PRGn", NULL) # color palette

result_map = leaflet(ch_aggregated) %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( lat=46.637785, lng=8.2 , zoom=7) %>%
  # municipality polygons
  addPolygons(
    fillColor = palDiv(swiss_data_muni$resultat.jaStimmenInProzent),
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
    label = paste0(ch_aggregated$NAME, ": ", round(swiss_data_muni$resultat.jaStimmenInProzent, 2), "%"),
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
  # Ajout de la légende
  addLegend(
    pal = palDiv,
    values = swiss_data_muni$resultat.jaStimmenInProzent,
    position = "bottomright",
    title = "Oui (%)",
    labFormat = labelFormat(suffix = "%"),
    opacity = 0.9
  )
result_map

# --- kenel binaire projets autoroutes

projets_2024 = st_read("troncons/projets_2024.shp")
projets_2024 = st_transform(projets_2024, st_crs(ch_aggregated))

# Vérifiez les intersections entre les polygones et les lignes
intersections <- st_intersects(ch_aggregated, projets_2024, sparse = FALSE)

# Ajouter la colonne "road_project_indiv" avec l'id_proj correspondant s'il y a intersection
ch_aggregated$road_project_indiv <- apply(intersections, 1, function(row) {
  intersecting_ids <- projets_2024$id_proj[row] # Récupère les id_proj pour les intersections
  if (length(intersecting_ids) > 0) {
    return(paste(intersecting_ids, collapse = ", ")) # Concatène les IDs s'il y en a plusieurs
  } else {
    return(NA) # Pas d'intersection
  }
})
# Ajouter la colonne "road_project_indiv" avec un seul id_proj (le premier trouvé) pour chaque intersection
ch_aggregated$road_project_indiv <- apply(intersections, 1, function(row) {
  intersecting_ids <- projets_2024$id_proj[row] # Récupère les id_proj pour les intersections
  if (length(intersecting_ids) > 0) {
    return(intersecting_ids[1]) # Retourne le premier id trouvé
  } else {
    return(NA) # Pas d'intersection
  }
})

# Identifier les voisins pour chaque entité
neighbors <- st_touches(ch_aggregated)

# Ajouter la colonne "road_project_indiv_n"
ch_aggregated$road_project_indiv_n <- sapply(1:length(neighbors), function(i) {
  # Si la colonne "road_project_indiv" contient déjà une valeur, retourner NA
  if (!is.na(ch_aggregated$road_project_indiv[i])) {
    return(NA)
  }
  
  # Récupérer les indices des voisins
  neighbor_indices <- neighbors[[i]]
  
  if (length(neighbor_indices) > 0) {
    # Récupérer les valeurs des voisins (hors NA)
    neighbor_values <- na.omit(ch_aggregated$road_project_indiv[neighbor_indices])
    
    if (length(neighbor_values) > 0) {
      # Retourner une seule valeur des voisins (par exemple, la plus fréquente)
      return(as.integer(names(sort(table(neighbor_values), decreasing = TRUE))[1]))
    } else {
      return(NA) # Tous les voisins ont des NA
    }
  } else {
    return(NA) # Pas de voisins
  }
})

# Ajouter la colonne "road_project_indiv_n2"
ch_aggregated$road_project_indiv_n2 <- sapply(1:length(neighbors), function(i) {
  # Si la colonne "road_project_indiv_n" contient déjà une valeur, retourner NA
  if (!is.na(ch_aggregated$road_project_indiv_n[i])) {
    return(NA)
  }
  
  # Récupérer les indices des voisins
  neighbor_indices <- neighbors[[i]]
  
  if (length(neighbor_indices) > 0) {
    # Récupérer les valeurs des voisins (hors NA)
    neighbor_values <- na.omit(ch_aggregated$road_project_indiv_n[neighbor_indices])
    
    if (length(neighbor_values) > 0) {
      # Retourner une seule valeur des voisins (par exemple, la plus fréquente)
      return(as.integer(names(sort(table(neighbor_values), decreasing = TRUE))[1]))
    } else {
      return(NA) # Tous les voisins ont des NA
    }
  } else {
    return(NA) # Pas de voisins
  }
})

# Afficher les résultats
View(ch_aggregated)






# Ajouter la colonne "road_project" : 1 si intersection, 0 sinon
ch_aggregated$road_project <- apply(intersections, 1, function(row) as.integer(any(row)))

ch_aggregated$road_project_direct = ch_aggregated$road_project

# 2. Identifier les polygones voisins des intersections
# Trouver les indices des polygones avec une intersection
intersecting_indices <- which(ch_aggregated$road_project_direct == 1)

# Trouver les voisins de tous les polygones
neighbors <- st_touches(ch_aggregated, sparse = TRUE)

# Identifier les voisins des polygones avec intersection
neighboring_indices <- unique(unlist(neighbors[intersecting_indices]))

# Ajouter une colonne pour voisins des intersections (2) et intersections (1)
ch_aggregated$road_project_n1 <- ifelse(
  ch_aggregated$road_project_direct == 1,  # Si c'est une intersection
  1,                                # Attribuer 1
  ifelse(seq_len(nrow(ch_aggregated)) %in% neighboring_indices, 2, 0) # Sinon vérifier si voisin
)

### voisin des voisins etc

library(sf)

# Initialiser la colonne avec des 0
ch_aggregated$road_project_n <- 0

# Étape 1 : Attribuer 1 aux intersections directes
intersecting_indices <- which(ch_aggregated$road_project_direct == 1)
ch_aggregated$road_project_n[intersecting_indices] <- 1

# Étape 2 : Trouver les voisins directs (niveau 2)
neighbors <- st_touches(ch_aggregated, sparse = TRUE)
neighboring_indices <- unique(unlist(neighbors[intersecting_indices]))
neighboring_indices <- neighboring_indices[ch_aggregated$road_project_n[neighboring_indices] == 0] # Exclure les déjà marqués
ch_aggregated$road_project_n[neighboring_indices] <- 2

# Étape 3 : Trouver les voisins des voisins (niveau 3)
neighbors_of_neighbors <- unique(unlist(neighbors[neighboring_indices]))
neighbors_of_neighbors <- neighbors_of_neighbors[ch_aggregated$road_project_n[neighbors_of_neighbors] == 0] # Exclure les déjà marqués
ch_aggregated$road_project_n[neighbors_of_neighbors] <- 3

View(ch_aggregated)


# --- add id by project
library(sf)
library(dplyr)
library(igraph)

# Charger l'objet sf
# Remplacez 'your_sf_object' par le nom de votre objet sf
# sf_data <- st_read("path_to_your_file.shp")

# Créer les buffers de 100m autour de chaque entité
projets_2024 <- projets_2024 %>%
  mutate(buffer = st_buffer(geometry, dist = 100))
plot(projets_2024$buffer)
# Trouver les intersections des buffers
intersectionsBuff <- st_intersects(projets_2024$buffer)

# Créer un graphe pour regrouper les entités connectées
graph <- graph_from_adj_list(intersectionsBuff)

# Obtenir les composants connectés (groupes d'entités dans le même buffer)
components <- components(graph)$membership

# Ajouter une colonne 'id' basée sur les groupes
projets_2024 <- projets_2024 %>%
  mutate(id_proj = components)

# Supprimer la colonne temporaire 'buffer' si elle n'est plus nécessaire
projets_2024 <- projets_2024 %>%
  select(-buffer)

# Enregistrer ou visualiser les résultats
st_write(projets_2024, "output_file_with_ids.shp")



# --- Regression
# Régression pondérée
reg_model = lm(resultat.jaStimmenInProzent ~ mds$V1 + mds$V2, data = swiss_data_muni, weights = f) # faire aussi avec résidu à la place de resultat.jaStimmenInProzent 
summary(reg_model)

residu_road = swiss_data_muni$resultat.jaStimmenInProzent - predict(reg_model)

leaflet(ch_aggregated) %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( lat=46.637785, lng=8.2 , zoom=7) %>%
  # municipality polygons
  addPolygons(
    fillColor = palDiv(residu_road),
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
    label = paste0(ch_aggregated$NAME, ": ", residu_road),
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
  # Ajout de la légende
  addLegend(
    pal = palDiv,
    values = swiss_data_muni$resultat.jaStimmenInProzent,
    position = "bottomright",
    title = "Résidus",
    labFormat = labelFormat(suffix = "%"),
    opacity = 0.9
  )

# Diagramme des valeurs observées vs prédites
plot(swiss_data_muni$resultat.jaStimmenInProzent, predict(reg_model), 
     xlab = "Valeurs observées (pourcentage de oui)", 
     ylab = "Valeurs prédites",
     main = "Observé vs Prédit")
abline(0, 1, col = "red")

plot(predict(reg_model),residu_road, 
     xlab = "Valeurs prédites", 
     ylab = "Résidus")
abline(0, 1, col = "red")

reg_model_residus = lm(residu_road ~ as.factor(ch_aggregated$road_project), weights = f)
summary(reg_model_residus)

# Diagramme des valeurs observées vs prédites
plot(swiss_data_muni$resultat.jaStimmenInProzent, predict(reg_model_residus), 
     xlab = "Valeurs observées (pourcentage de oui)", 
     ylab = "Valeurs prédites",
     main = "Observé vs Prédit")
abline(0, 1, col = "red")

reg_model_binary = lm(resultat.jaStimmenInProzent ~ as.character(ch_aggregated$road_project_direct), data = swiss_data_muni, weights = f) # faire aussi avec résidu à la place de resultat.jaStimmenInProzent 
summary(reg_model_binary)
# Diagramme des valeurs observées vs prédites
plot(swiss_data_muni$resultat.jaStimmenInProzent, predict(reg_model_binary), 
     xlab = "Valeurs observées (pourcentage de oui)", 
     ylab = "Valeurs prédites",
     main = "Observé vs Prédit")
abline(0, 1, col = "red")

ch_aggregated$test = predict(reg_model_binary)

pal = colorNumeric("Reds", NULL) # color palette
leaflet(ch_aggregated) %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( lat=46.637785, lng=8.2 , zoom=7) %>%
  # municipality polygons
  addPolygons(
    fillColor = pal(ch_aggregated$test),
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
    label = paste0(ch_aggregated$NAME, ": ", round(swiss_data_muni$resultat.jaStimmenInProzent, 2), "%"),
    smoothFactor = 0.2,
    # group = "MDS fac. 1",
  )

View(yes)
yes$language = detailed_language_2024$language_region

lm_result_autoroutes = lm(yes$X6730 ~ as.factor(yes$language), weights = f*n)
summary(lm_result_autoroutes)

summary(lm_result_autoroutes)$r.squared

result_r_squared = c()
for (i in 3:(dim(yes)[2]-1)) {
  res_lm = summary(lm(yes[,i] ~ as.factor(yes$language), weights = f*n))$r.squared
  result_r_squared[i] = res_lm
}

hist(result_r_squared, breaks = 30)

which(result_r_squared > 0.7)



leaflet(ch_aggregated) %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( lat=46.637785, lng=8.2 , zoom=7) %>%
  # municipality polygons
  addPolygons(
    fillColor = palDiv(yes[,46]),
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
    label = paste0(ch_aggregated$NAME, ": ", round(yes[,46], 2), "%"),
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
  # Ajout de la légende
  addLegend(
    pal = palDiv,
    values = swiss_data_muni$resultat.jaStimmenInProzent,
    position = "bottomright",
    title = "Oui (%)",
    labFormat = labelFormat(suffix = "%"),
    opacity = 0.9
  )


# --- indicateur distance

distance_mat_2024

indicateur_dist = distance_mat_2024

seuil_dist = 5000
# Matrice proximité selon le seuil "seuil_dist"
indicateur_dist[distance_mat_2024 < seuil_dist] = 1
indicateur_dist[distance_mat_2024 > seuil_dist] = 0

# Vecteur de n x 1
pop_atteinte = as.matrix(indicateur_dist)%*%f
pop_atteinte = as.data.frame(pop_atteinte)
pop_atteinte$language = yes$language

plot(pop_atteinte$V1, yes$X6730, col =pop_atteinte$language)



test = lm(yes$X6540 ~ pop_X6740test = lm(yes$X6540 ~ pop_atteinte,  weights = f*n))

lm_pop = lm(residu_road ~ pop_atteinte, weights = f*n)
summary(lm_pop)
plot(pop_atteinte, residu_road)

revenu = read.csv("IFD_classes_revenu_net_normaux_24.csv")
grouped_data
View(revenu)
