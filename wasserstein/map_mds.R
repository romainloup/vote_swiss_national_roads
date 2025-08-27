maxDataL = ch_aggregated_geolevels$residu_road_lang[which.max( abs(mds_wealth_ot$mds$V1) )]

maxDataL = max(abs(mds_wealth_ot$mds$V1))



scale_range2 <- c(-maxDataL, maxDataL)
palDiv2 <- colorNumeric("PRGn", domain = scale_range2)
rev_palDiv2 <- colorNumeric("PRGn", domain = scale_range2, reverse = TRUE)

leaflet(ch_aggregated_geolevels) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  # addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( lat=46.637785, lng=8.2 , zoom=7) %>%
  # municipality polygons
  addPolygons(
    fillColor = ~palDiv2(-mds_wealth_ot$mds$V1),
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
    label = paste0(ch_aggregated_geolevels$NAME, ": ", round(-mds_wealth_ot$mds$V1, 2)),
    smoothFactor = 0.2,
    # group = "MDS fac. 1",
  ) %>%
  # addPolylines(
  #   data = autoroutes,       # Fournissez ici un objet avec les autoroutes
  #   color = "#d47c17",          # Couleur des lignes
  #   weight = 1,              # Épaisseur des lignes
  #   opacity = 0.8,           # Opacité des lignes
  #   label = autoroutes$NAME  # Optionnel : ajoute un label si disponible
  # ) %>%
  # addPolylines(
  #   data = projets_2024,       # Fournissez ici un objet avec les autoroutes
  #   color = "#d44317",          # Couleur des lignes
  #   weight = 3,              # Épaisseur des lignes
  #   opacity = 0.9,           # Opacité des lignes
  #   label = projets_2024$NAME  # Optionnel : ajoute un label si disponible
  # ) %>%
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
    pal = rev_palDiv2,
    values = scale_range2,
    position = "bottomright",
    title = "MDS value",
    # labFormat = labelFormat(suffix = "%"),
    # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE), suffix = "%"),
    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
    
    opacity = 0.9
  )
