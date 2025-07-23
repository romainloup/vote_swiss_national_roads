
# --- Road time -> time_mat
time_mat = read.csv("/Users/rloup/Documents/r_projects/vote_swiss_national_roads/time_mat_2024.csv")
#set specific column as row names
rownames(time_mat) = time_mat[,1]
#remove original column from data frame
time_mat[,1] = NULL

# --- Road time
DZ = as.matrix((time_mat + t(time_mat)) / 2) ^ 2 # road time
KZ = -0.5 * diag(sqrt(f)) %*% H %*% DZ %*% t(H) %*% diag(sqrt(f)) # spatial kernel, time

dist_types = c("X", "Z", "f", "P", "w", "I", "S")

# --- Run RV function
list_RV_W = RV2(dist_types,f)
list_RV_W_S = RV2(dist_types,f)

# list_RV_W$Y_list[[2]] = -list_RV_W$Y_list[[2]]
# list_RV_W$Y_list[[3]] = -list_RV_W$Y_list[[3]]
# list_RV_W$Y_list[[5]] = -list_RV_W$Y_list[[5]]
# list_RV_W$Y_list[[6]] = -list_RV_W$Y_list[[6]]

# german, ..., 1:dim(ch_aggregated_geolevels)[1]
# --- German speakers vs french speakers
german = which(ch_aggregated_geolevels$language == 1)
french = which(ch_aggregated_geolevels$language == 2)
italian = which(ch_aggregated_geolevels$language == 3)
romansh = which(ch_aggregated_geolevels$language == 4)

dist_types_names = c("Pol", "Time", "Size", "Lang", "Wealth", "OT", "Sound")

# --- Parameters
# Type to compare
val_1 = 1 # x 
val_2 = 2
val_3 = 3
factor_1 = 1
factor_2 = 1
nb_muni = 50

select_lang = 1:dim(ch_aggregated_geolevels)[1] # Switzerland
select_lang = german
select_lang = french
select_lang = italian
select_lang = romansch
# --- end parameters


# Run graph
{
  filtered = as.data.frame(ch_aggregated_geolevels[select_lang,]$swiss_data_muni.f)
  names(filtered) = "f"
  filtered$x = list_RV_W_S$Y_list[[val_1]][select_lang,factor_1]
  # filtered$x = test$mds$V1
  filtered$y = list_RV_W_S$Y_list[[val_2]][select_lang,factor_2]
  
  # filtered$x = x
  # filtered$y = y
  
  filtered$municipality = ch_aggregated_geolevels[select_lang,]$NAME
  filtered = filtered[filtered$f > sort(filtered$f, decreasing = TRUE)[nb_muni+1], ]
  filtered = filtered[order(filtered$f, decreasing = TRUE),]
  
lambda_from_RV_1 = list_RV_W_S$eigen_val_list[[val_1]]$values
lambda_from_RV_2 = list_RV_W_S$eigen_val_list[[val_2]]$values

prop_expl_1 = round(100*lambda_from_RV_1 / sum(lambda_from_RV_1), digits = 1 )[factor_1]
prop_expl_1 = 49.9
prop_expl_2 = round(100*lambda_from_RV_2 / sum(lambda_from_RV_2), digits = 1 )[factor_2]

x_axis = list_RV_W_S$Y_list[[val_1]][select_lang,factor_1]
# x_axis = test$mds$V1
y_axis = list_RV_W_S$Y_list[[val_2]][select_lang,factor_2]

# x_axis = x
# y_axis = y

x_lab = dist_types_names[val_1]
x_lab = "Pol eco"
y_lab = dist_types_names[val_2]

# Graphique
ggplot() +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(aes(x = x_axis, y = y_axis, size=f[select_lang], color=mds_I$language[select_lang]),
             alpha = magnif[select_lang]) +
  ggrepel::geom_text_repel(aes(x = filtered$x, y = filtered$y, label = filtered$municipality),
                           box.padding = 0.5,   # Espace autour des étiquettes
                           point.padding = 0.3, # Espace autour des points
                           max.overlaps = Inf ) +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"),
                     labels = c("German", "French", "Italian","Romansh")) +
  labs(x = paste0(x_lab, ": fact 1: ",prop_expl_1,"%"), y = paste0(y_lab, ": fact 1: ",prop_expl_2,"%")) +
  labs(size = "Reg. weight *f*", color = "Language") +
  scale_size_continuous(range = c(1, 8)) +
  theme_minimal() +
  theme(legend.title = ggtext::element_markdown(lineheight = 1.2))
}

{
# --- Regression
# Régression pondérée 2ers facteurs MDS sur toutes les votations
reg_model_w = lm(x_axis ~ y_axis, weights = f[select_lang]) # faire aussi avec résidu à la place de resultat.jaStimmenInProzent 
summary(reg_model_w)

# Résidus
residuals_w = x_axis - predict(reg_model_w)

# Map residuals

maxData_w = residuals_w[which.max( abs(residuals_w) )]

scale_range_w <- c(-maxData_w, maxData_w)
palDiv_w <- colorNumeric("PRGn", domain = scale_range_w)
rev_palDiv_w <- colorNumeric("PRGn", domain = scale_range_w, reverse = TRUE)


leaflet(ch_aggregated[select_lang,]) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  # addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lat=46.637785, lng=8.2 , zoom=7) %>%
  # municipality polygons
  addPolygons(
    fillColor = ~palDiv_w(residuals_w),
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
    label = paste0(ch_aggregated$NAME[select_lang], ": ", round(residuals_w, 2)),
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
  # addLegend(
  #   colors = c("#d47c17", "#d44317"),
  #   position = "bottomright",
  #   labels = c("Highways", "Voted projects"),
  #   opacity = 0.9
  # ) %>%
  # Ajout de la légende
  addLegend(
    pal = rev_palDiv_w,
    values = residuals_w,
    position = "bottomright",
    title = "Résidus",
    # labFormat = labelFormat(suffix = "%"),
    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE), suffix = "%"),
    
    opacity = 0.9
  )
}

# Corrélation pondérée entre les résidus
weighted.cor <- function(u, v, w) {
  wm <- sum(w)
  mu_u <- sum(w * u) / wm
  mu_v <- sum(w * v) / wm
  cov_uv <- sum(w * (u - mu_u) * (v - mu_v)) / wm
  sd_u <- sqrt(sum(w * (u - mu_u)^2) / wm)
  sd_v <- sqrt(sum(w * (v - mu_v)^2) / wm)
  cov_uv / (sd_u * sd_v)
}


# dist_types_names = c("Pol", "Time", "Size", "Lang", "Wealth", "OT")
# Régression pondérée 2ers facteurs MDS sur toutes les votations
z = list_RV_W_S$Y_list[[val_3]][select_lang,factor_1]
x = list_RV_W_S$Y_list[[val_1]][select_lang,factor_1]
reg_model_x = lm(x ~ z, weights = f[select_lang]) # faire aussi avec résidu à la place de resultat.jaStimmenInProzent 
# Résidus
residuals_x = x - predict(reg_model_x)

# Reg 2
y = list_RV_W_S$Y_list[[val_2]][select_lang,factor_1]
reg_model_y = lm(y ~ z, weights = f[select_lang])
# Résidus
residuals_y = y - predict(reg_model_y)

partial_cor <- weighted.cor(residuals_x, residuals_y, f[select_lang])
partial_cor

weighted.cor(x, y, f[select_lang])

corr_mat_w = matrix(data = NA, 7,7)
for (i in 1:7) {
  x = list_RV_W_S$Y_list[[i]][select_lang,factor_1]
  for (j in 1:6) {
    # if (i!=j) {
      y = list_RV_W_S$Y_list[[j]][select_lang,factor_1]
    # }
    corr_mat_w[i,j] = weighted.cor(x, y, f[select_lang])
  }
}
rownames(corr_mat_w) = dist_types_names
colnames(corr_mat_w) = dist_types_names

plot(x,y)

# mettre dans matrice de corrélation 2 à 2 -> faire ACP là-dessus voir si les variables sont dépendantes entre elles
# reg multiple
# prédire 1er fact pol
# WLS pondérée