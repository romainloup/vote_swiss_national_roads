# autoroutes


min_time_to_highway = read.csv("min_time_to_highway_2024.csv")


min_time_to_highway$t_min = apply(min_time_to_highway[,2:dim(min_time_to_highway)[2]], 1, FUN = min)

View(min_time_to_highway)

pal = leaflet::colorNumeric(
  palette = "Reds",
  domain = round(min_time_to_highway$t_min/60),
  na.color = "#eeeeee")

pal_rev = leaflet::colorNumeric(
  palette = "Reds",
  domain = round(min_time_to_highway$t_min/60),
  reverse = TRUE,
  na.color = "#eeeeee")

# label for hover
labels3 = paste0(ch_aggregated$NAME, ": ", round(min_time_to_highway$t_min/60), " min", collapse = NULL)

leaflet(ch_aggregated) %>%
  # addProviderTiles("Stamen.TerrainBackground") %>%
  # addProviderTiles("Esri.WorldShadedRelief") %>%
  addProviderTiles("Esri.WorldTerrain") %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  # addProviderTiles("Esri.WorldGrayCanvas") %>%
  # addProviderTiles("Stadia.StamenTonerLabels") %>%
  setView( lat=46.637785, lng=8.2 , zoom=7) %>%
  # commune polygons
  addPolygons(
    fillColor = ~pal(round(min_time_to_highway$t_min/60)),
    fillOpacity = 0.6,
    color = "white",
    weight = 0.1,
    opacity = 1,
    
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE,
      sendToBack = TRUE),
    
    label = labels3,
    smoothFactor = 0.5
  ) %>%
  # canton polygons
  addPolygons(data=ch_canton,
              fill = FALSE,
              color = "#666",
              weight = 1,
              opacity = 1,
              smoothFactor = 0.5,
              highlight = highlightOptions(
                weight = 3,
                color = "#666",
                bringToFront = TRUE)
  ) %>%
  # addLegend(pal = factpal, values = ~language, opacity = 0.7, title = NULL,
  #           position = "bottomright")
  addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F)) %>%
  addLegend(
    colors = c("#d47c17", "#d44317"),
    position = "bottomright",
    labels = c("Highways", "Voted projects"),
    opacity = 0.9
  )%>%
  addLegend(pal = pal_rev,
            values = round(min_time_to_highway$t_min/60), 
            opacity = 0.7, 
            title = "Time [min]",
            position = "bottomright",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
  addPolygons(data = lakes,
              weight = 1,
              fillColor = "#dddddd",
              fillOpacity = 0.9,
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
              smoothFactor = 0.2) %>%
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
  )
