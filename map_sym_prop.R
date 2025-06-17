library(ggplot2)
library(sf)
library(viridis)  # Pour une palette sympa
library(scales)   # Pour formater les pourcentages

# Préparation des données
dataSetCircles$vote_pc <- 100 * dataSetCircles$vote  # Pour afficher en %

# Base de la carte
ggplot() +
  # Fond des communes par langue (contours)
  geom_sf(data = ch_langue, aes(color = color), fill = NA, size = 0.2, show.legend = FALSE) +
  
  # Lacs
  geom_sf(data = lakes, fill = "grey90", color = "white", size = 0.3) +
  
  # Cercles proportionnels
  geom_point(
    data = dataSetCircles,
    aes(x = longitude, y = latitude, 
        size = f, 
        fill = vote_pc),
    shape = 21, color = "black", alpha = 0.95
  ) +
  
  # Échelles de couleur et de taille
  scale_fill_viridis(
    name = "Oui (%)", 
    option = "C", 
    direction = -1,
    labels = percent_format(scale = 1)
  ) +
  scale_size(
    name = "Population (f)",
    range = c(1, 15),  # Ajuste selon ton jeu de données
    breaks = c(1000, 10000, 50000),
    labels = comma_format()
  ) +
  
  # Thème et options
  coord_sf() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )



# V2

install.packages("maptiles")
install.packages("ggspatial")

library(maptiles)
library(terra)     # pour raster
library(sf)

# Télécharger le fond CartoDB Positron
basemap <- get_tiles(
  x = ch_langue,  # n'importe quel objet sf avec bbox
  provider = "CartoDB.Positron",
  crop = TRUE,
  zoom = 7
)

library(ggplot2)
library(ggspatial)
library(scales)

# Calculer vote en %
dataSetCircles$vote_pc <- 100 * dataSetCircles$vote

ggplot() +
  # Fond de carte
  layer_spatial(basemap) +
  
  # Cercles proportionnels
  geom_point(
    data = dataSetCircles,
    aes(x = longitude, y = latitude, 
        size = f, 
        fill = vote_pc),
    shape = 21, color = "black", alpha = 0.95
  ) +
  
  # Palette personnalisée
  scale_fill_gradientn(
    colors = my_palette(seq(0, 100, length.out = 100)),
    name = "Oui (%)",
    limits = c(0, 100),
    labels = percent_format(scale = 1)
  ) +
  
  scale_size(
    name = "Population (f)",
    range = c(1, 15),
    breaks = c(1000, 10000, 50000),
    labels = comma_format()
  ) +
  
  coord_sf(crs = 4326) +  # WGS84
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )


# V3

library(ggplot2)
library(sf)
library(maptiles)
library(ggspatial)
library(scales)
library(rgeoda)

natural_breaks(5, as.data.frame(f))

# ---- 1. Convertir ton jeu de cercles en sf ----
dataSetCircles_sf <- st_as_sf(dataSetCircles, coords = c("longitude", "latitude"), crs = 4326)

bb_box = st_bbox(dataSetCircles_sf)
bb_box[1:2] = bb_box[1:2]-0.1
bb_box[3:4] = bb_box[3:4]+0.1

# ---- 2. Télécharger le fond de carte CartoDB ----
basemap <- get_tiles(
  x = bb_box,
  # x = st_union(dataSetCircles_sf),  # bbox automatiquement calculée
  provider = "CartoDB.PositronNoLabels",
  zoom = 7,
  crop = TRUE
)

# ---- 3. Carte ggplot ----
ggplot() +
  # Fond de carte
  layer_spatial(basemap) +
  
  # 1. Couche des frontières linguistiques avec une première échelle de couleur
  geom_sf(data = ch_langue, aes(color = as.character(language)), fill = NA, alpha = 1, linewidth = 0.3) +
  scale_color_manual(name = "Main language", values = langue_colors,
                     labels = c("German", "French", "Italian", "Romansh")) +
  geom_sf(data=lacs, fill="gray80", size=0, alpha=1, stroke = 0.3, color = "gray80") +
  
  # Cercles proportionnels
  # geom_point(data = dataSetCircles, aes(x = longitude, y = latitude, size = f, color = canton)) +
  # geom_sf(
  #   data = dataSetCircles_sf,
  #   aes(size = f, fill = 100 * vote),
  #   shape = 21, color = "#555555", alpha = 1, linewidth = 10.5
  # ) +
  
  geom_point(
    data = dataSetCircles,
    aes(x = longitude, y = latitude, size = f, fill = 100 * vote),
    shape = 21,
    color = "#555555",     # Couleur du bord
    stroke = 0.1,          # <--- Épaisseur du contour ici
    alpha = 0.95
  ) +
  
  # Palette couleur personnalisée (à adapter si nécessaire)
  scale_fill_gradientn(
    # colors = my_palette(seq(0, 100, length.out = 100)),
    colors = my_palette(seq(10, 90, length.out = 80)),
    name = "Yes (%)",
    # limits = c(0, 100),
    limits = c(10, 90),
    labels = percent_format(scale = 1)
  ) +
  
  scale_size(
    name = "Population",
    range = c(0.1, 12),  # <- taille min et max des cercles
    breaks = natural_breaks(7, as.data.frame(f)),
  ) +
  
  # scale_size(
  #   name = "Population",
  #   range = c(1, 8),  # taille min/max (mm)
  #   # breaks = c(1000, 5000, 10000, 50000),
  #   # labels = scales::comma_format(),
  #   trans = "log10"   # <- transformation logarithmique base 10
  # ) +
  
  # Taille des cercles
  # scale_size(
  #   name = "Population",
  #   range = c(1, 15),
  #   breaks = c(1000, 10000, 50000),  # adapte à ton cas
  #   labels = comma_format()
  # ) +
  
  # Légende manuelle pour la taille des cercles
  # annotate("text", x = legend_x, y = legend_y + 0.25, label = "Population", hjust = 0, size = 3.5) +
  # annotate("point", x = legend_x, y = legend_y, size = radii[1], shape = 21, fill = "grey60", color = "black") +
  # annotate("text", x = legend_x + 0.15, y = legend_y, label = format(sizes_ref[1], big.mark = " "), hjust = 0, size = 3) +
  # annotate("point", x = legend_x, y = legend_y - 0.2, size = radii[2], shape = 21, fill = "grey60", color = "black") +
  # annotate("text", x = legend_x + 0.15, y = legend_y - 0.2, label = format(sizes_ref[2], big.mark = " "), hjust = 0, size = 3) +
  # annotate("point", x = legend_x, y = legend_y - 0.4, size = radii[3], shape = 21, fill = "grey60", color = "black") +
  # annotate("text", x = legend_x + 0.15, y = legend_y - 0.4, label = format(sizes_ref[3], big.mark = " "), hjust = 0, size = 3) +
  
  # Coordonnées et suppression axes
  coord_sf(crs = 4326) +
  
  # Échelle graphique
  annotation_scale(location = "br", style = "ticks", line_width = 0.5) +
  
  # Thème propre sans axes
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

ggsave("images/ch_symb_prop2.pdf", width = 9, height = 8)



# --- Map residuals
my_palette <- colorNumeric(
  palette = colorRampPalette(c("#af8dc3", "#7fbf7b"))(100), # Transition douce entre violet et vert
  domain = NULL
)
my_palette = scale_color_brewer(n = 11, palette = "PRGn")

rev_my_palette = colorNumeric(
  palette = colorRampPalette(c("#7fbf7b", "#af8dc3"))(100), # Transition douce entre vert et violet
  domain = NULL
)
# --- Regression
# Régression pondérée 2ers facteurs MDS sur toutes les votations
reg_model = lm(resultat.jaStimmenInProzent ~ mds$V1, data = swiss_data_muni, weights = f) # faire aussi avec résidu à la place de resultat.jaStimmenInProzent 
summary(reg_model)

reg_fac1 = lm(resultat.jaStimmenInProzent ~ mds$V1, data = swiss_data_muni, weights = f)
summary(reg_fac1)

reg_fac2 = lm(resultat.jaStimmenInProzent ~ mds$V2, data = swiss_data_muni, weights = f)
summary(reg_fac2)
# Résidus
residu_road = swiss_data_muni$resultat.jaStimmenInProzent - predict(reg_model)
ch_aggregated$residu_road = residu_road

autoroutes = st_read("/Users/rloup/Documents/r_projects/vote_swiss_national_roads/autoroutes.shp") # CRS: CH1903+
autoroutes = st_transform(autoroutes, crs = 4326)
autoroutes = st_zm(autoroutes, drop = T, what = "ZM") # delete Z for Leaflet

projets_2024 = st_read("/Users/rloup/Documents/r_projects/vote_swiss_national_roads/troncons/projets_2024.shp") # CRS: CH1903+
projets_2024 = st_transform(projets_2024, crs = 4326)
projets_2024 = st_zm(projets_2024, drop = T, what = "ZM") # delete Z for Leaflet

centre_entrees = st_read("/Users/rloup/Documents/r_projects/vote_swiss_national_roads/troncons/centre_entrees_corr.shp") # CRS: CH1903+
centre_entrees = st_transform(centre_entrees, crs = 4326)
centre_entrees = st_zm(centre_entrees, drop = T, what = "ZM") # delete Z for Leaflet

centre_entrees = st_zm(
  st_transform(
    st_read("/Users/rloup/Documents/r_projects/vote_swiss_national_roads/troncons/centre_entrees_corr.shp"),
    crs = 4326
  ),
  drop = TRUE,
  what = "ZM"
)

# Calcul des distances minimales
ch_aggregated$min_distance_to_highway <- st_distance(
  ch_aggregated,
  centre_entrees
) %>%
  apply(1, min) # Trouve la distance minimale pour chaque polygone

maxData = residu_road[which.max( abs(residu_road) )]

scale_range <- c(-maxData, maxData)
palDiv0 <- colorNumeric("PRGn", domain = scale_range)
rev_palDiv0 <- colorNumeric("PRGn", domain = scale_range, reverse = TRUE)


leaflet(ch_aggregated) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  # addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( lat=46.637785, lng=8.2 , zoom=7) %>%
  # municipality polygons
  addPolygons(
    fillColor = ~palDiv0(residu_road),
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
    label = paste0(ch_aggregated$NAME, ": ", round(residu_road, 2)),
    smoothFactor = 0.2,
    # group = "MDS fac. 1",
  ) %>%
  addPolylines(
    data = autoroutes,       # Fournissez ici un objet avec les autoroutes
    color = "#d47c17",          # Couleur des lignes
    weight = 1,              # Épaisseur des lignes
    opacity = 0.8,           # Opacité des lignes
    label = autoroutes$NAME  # Optionnel : ajoute un label si disponible
  ) %>%
  addPolylines(
    data = projets_2024,       # Fournissez ici un objet avec les autoroutes
    color = "#d44317",          # Couleur des lignes
    weight = 3,              # Épaisseur des lignes
    opacity = 0.9,           # Opacité des lignes
    label = projets_2024$NAME  # Optionnel : ajoute un label si disponible
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
  addLegend(
    colors = c("#d47c17", "#d44317"),
    position = "bottomright",
    labels = c("Highways", "Voted projects"),
    opacity = 0.9
  ) %>%
  # Ajout de la légende
  addLegend(
    pal = rev_palDiv0,
    values = residu_road,
    position = "bottomright",
    title = "Résidus",
    # labFormat = labelFormat(suffix = "%"),
    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE), suffix = "%"),
    
    opacity = 0.9
  )


# --- GGplot version

autoroutes$type <- "highway"
projets_2024$type <- "project"
routes <- rbind(autoroutes, projets_2024)

ggplot() +
  # Fond de carte
  layer_spatial(basemap) +
  
  # Données résidus
  geom_sf(data = ch_aggregated, aes(fill=residu_road), alpha = 1, lwd = 0.05, color = "white") +
  
  # Palette couleur personnalisée (à adapter si nécessaire)
  scale_fill_gradientn(
    colors = my_palette(seq(-35, 35, length.out = 70)),
    name = "residuals (%)",
    # limits = c(0, 100),
    limits = c(-35, 35),
    labels = percent_format(scale = 1)
  ) +
  
  # Couche des frontières linguistiques avec une première échelle de couleur
  geom_sf(data = ch_langue, aes(color = as.character(language)), fill = NA, alpha = 1, linewidth = 0.3) +
  scale_color_manual(name = "main language", values = langue_colors,
                     labels = c("German", "French", "Italian", "Romansh"),
                     guide = guide_legend(override.aes = list(linetype = "solid", size = 0.5))) +
  new_scale_color() + # ggnewscale pour mettre plusieurs légendes
  
  # Lacs
  geom_sf(data=lacs, fill="gray80", size=0, alpha=1, stroke = 0.3, color = "gray80") +
  
  # Autoroutes
  geom_sf(data = autoroutes, aes(colour = "highway")) +
  geom_sf(data = projets_2024, aes(colour = "project"), linewidth = 1) +
  scale_colour_manual(name = "road", values = c("highway" = "gray50", "project" = "black")) +
  
  # Coordonnées et suppression axes
  coord_sf(crs = 4326) +
  
  # Échelle graphique
  annotation_scale(location = "br", style = "ticks", line_width = 0.5) +
  
  # Thème propre sans axes
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

ggsave("images/ch_resid.pdf", width = 9, height = 8)
x