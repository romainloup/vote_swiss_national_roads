library(ggplot2)

# Initialisation des variables
road_time_values <- seq(1, 100, by = 3) # Incréments de 10
mean_values <- data.frame(
  RoadTime = numeric(),
  Group = character(),
  Mean = numeric(),
  PValue = numeric()
)

# Boucle pour calculer les moyennes et p-values
for (road_time in road_time_values) {
  # Groupes basés sur road_time
  group1 <- residu_road[which(min_time_to_highway$t_min < road_time * 60)]
  group2 <- residu_road[which(min_time_to_highway$t_min > road_time * 60)]
  
  # Tests t
  t_test_group1 <- t.test(group1, mu = 0)
  t_test_group2 <- t.test(group2, mu = 0)
  
  # Stockage des résultats
  mean_values <- rbind(mean_values,
                       data.frame(
                         RoadTime = road_time,
                         Group = "Less than road_time",
                         Mean = t_test_group1$estimate,
                         PValue = t_test_group1$p.value
                       ),
                       data.frame(
                         RoadTime = road_time,
                         Group = "Greater than road_time",
                         Mean = t_test_group2$estimate,
                         PValue = t_test_group2$p.value
                       ))
}

# Création du graphique ggplot
ggplot(mean_values, aes(x = RoadTime, y = Mean, color = Group)) +
  geom_line(size = 1) +
  geom_point(aes(size = -log10(PValue))) + # Taille des points selon la p-value
  scale_size_continuous(name = "Significance (-log10 p-value)") +
  labs(title = "Mean of Residuals vs Road Time Threshold",
       x = "Road Time Threshold (minutes)",
       y = "Mean of Residuals",
       color = "Group") +
  theme_minimal()






# --- V2
library(ggplot2)
library(gridExtra) # Pour combiner les graphiques

# Initialisation des variables
road_time_values <- seq(1, 146, by = 5) # Incréments de 10
mean_values <- data.frame(
  RoadTime = numeric(),
  Group = character(),
  Mean = numeric(),
  PValue = numeric()
)

# Boucle pour calculer les moyennes et p-values
for (road_time in road_time_values) {
  # Groupes basés sur road_time
  group1 <- residu_road[which(min_time_to_highway$t_min < road_time * 60)]
  group2 <- residu_road[which(min_time_to_highway$t_min > road_time * 60)]
  
  # Tests t
  t_test_group1 <- t.test(group1, mu = 0)
  t_test_group2 <- t.test(group2, mu = 0)
  
  # Stockage des résultats
  mean_values <- rbind(mean_values,
                       data.frame(
                         RoadTime = road_time,
                         Group = "Less than road time",
                         Mean = t_test_group1$estimate,
                         PValue = t_test_group1$p.value
                       ),
                       data.frame(
                         RoadTime = road_time,
                         Group = "Greater than road time",
                         Mean = t_test_group2$estimate,
                         PValue = t_test_group2$p.value
                       ))
}

# Premier graphique : Mean of Residuals
plot_mean <- ggplot(mean_values, aes(x = RoadTime, y = Mean, color = Group)) +
  geom_line(size = 1) +
  labs(title = "Mean of Residuals vs Road Time Threshold",
       x = "Road Time Threshold (minutes)",
       y = "Mean of Residuals",
       color = "Group") +
  theme_minimal()

# Deuxième graphique : P-values
plot_pvalue <- ggplot(mean_values, aes(x = RoadTime, y = PValue, color = Group)) +
  geom_line(size = 1) +
  labs(title = "P-values vs Road Time Threshold",
       x = "Road Time Threshold (minutes)",
       y = "P-value",
       color = "Group") +
  geom_hline(yintercept = 0.0001, linetype="dashed", color = "red") +
  # annotate("text", x = 0, y = 0.0003, label="0.0001", color="red") +
  theme_minimal() +
  scale_y_continuous(trans = "log10", labels = scales::scientific) + # Échelle logarithmique pour p-values
  labs(y = "P-value (log scale)")

# Combinaison des graphiques
grid.arrange(plot_mean, plot_pvalue, ncol = 1)

gA <- ggplotGrob(plot_mean)
gB <- ggplotGrob(plot_pvalue)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))

ggsave("/Users/rloup/Library/CloudStorage/OneDrive-UniversitédeLausanne/routes_nationales/images/two_lines.png", width = 9, height = 8)


# --- v3 loess

library(ggplot2)

# Création du dataframe
data_loess <- data.frame(
  Distance = min_time_to_highway$t_min / 60, # Conversion en heures si nécessaire
  Residuals = residu_road,
  Language = ch_aggregated_geolevels$language,
  f = ch_aggregated_geolevels$swiss_data_muni.f
)

# Graphique avec régression LOESS pondérée
plot_loess <- ggplot(data_loess, aes(x = Distance, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") + # Points avec transparence
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "red", fill = "pink") +
  labs(
    title = "Régression LOESS des résidus en fonction de la distance parcourue",
    x = "Distance parcourue (minutes)",
    y = "Résidus"
  ) +
  theme_minimal()

# Affichage
print(plot_loess)


# --- v4 loess
library(ggplot2)

# Création du dataframe
data_loess <- data.frame(
  Distance = min_time_to_highway$t_min / 60, # Conversion en heures si nécessaire
  Residuals = residu_road,
  Language = factor(ch_aggregated_geolevels$language, 
                    levels = c(1, 2, 3, 4), 
                    labels = c("German", "French", "Italian", "Romansh")), # Attribution des labels
  f = ch_aggregated_geolevels$swiss_data_muni.f
)

magnif = 0.2+0.5*(log(f)-min(log(f)))/(max(log(f))-min(log(f))) # defines a magnification factor for the object weights (here from 0.5 to 2)

# Graphique avec régression LOESS pondérée
plot_loess <- ggplot(data_loess, aes(x = Distance, y = Residuals, color = Language, size = magnif)) +
  geom_point(alpha = 0.6) + # Points avec transparence
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "black", fill = "grey70") + # Courbe LOESS en noir
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"), 
                     labels = c("German", "French", "Italian", "Romansh")) +
  scale_size_continuous(range = c(1, 6)) + # Ajuste l'échelle des tailles de points
  labs(
    title = "Régression LOESS des résidus en fonction de la distance parcourue",
    x = "Distance parcourue (minutes)",
    y = "Résidus",
    color = "Language",
    size = "Population (f)"
  ) +
  theme_minimal()

# Affichage
print(plot_loess)


# --- v5 loess pondéré
library(ggplot2)

# Création du dataframe
data_loess <- data.frame(
  Distance = min_time_to_highway$t_min / 60, # Conversion en heures si nécessaire
  Residuals = residu_road,
  Language = factor(ch_aggregated_geolevels$language, 
                    levels = c(1, 2, 3, 4), 
                    labels = c("German", "French", "Italian", "Romansh")), # Attribution des labels
  f = ch_aggregated_geolevels$swiss_data_muni.f # Poids des points
)

# Régression LOESS pondérée
loess_model <- loess(Residuals ~ Distance, data = data_loess, weights = f, span = 0.75) # span: the parameter alpha which controls the degree of smoothing.

# Prédictions pour dessiner la courbe
grid_distance <- seq(min(data_loess$Distance), max(data_loess$Distance), length.out = 300)
loess_pred <- predict(loess_model, newdata = data.frame(Distance = grid_distance), se = TRUE)

# Dataframe pour la courbe LOESS avec intervalle de confiance
loess_df <- data.frame(
  Distance = grid_distance,
  Fit = loess_pred$fit,
  Lower = loess_pred$fit - 1.96 * loess_pred$se.fit, # Intervalle de confiance 95%
  Upper = loess_pred$fit + 1.96 * loess_pred$se.fit
)

# Graphique avec LOESS pondérée
plot_loess <- ggplot() +
  # Points colorés selon la langue et taille selon f
  geom_point(data = data_loess, aes(x = Distance, y = Residuals, color = Language, size = f), alpha = 0.6) +
  # Intervalle de confiance de la LOESS
  geom_ribbon(data = loess_df, aes(x = Distance, ymin = Lower, ymax = Upper), fill = "grey60", alpha = 0.4) +
  # Courbe LOESS pondérée
  geom_line(data = loess_df, aes(x = Distance, y = Fit), color = "black", size = 1.2) +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"), 
                     labels = c("German", "French", "Italian", "Romansh")) +
  scale_size_continuous(range = c(1, 6)) + # Ajuste l'échelle des tailles de points
  labs(
    title = "Weighted LOESS regression of residuals as a function of distance travelled",
    x = "Travel time (minutes)",
    y = "Residuals (%)",
    color = "Language",
    size = "Population (f)"
  ) +
  geom_hline(yintercept = 0.0001, linetype="dashed", color = "red") +
  theme_minimal()

# Affichage du graphique
print(plot_loess)
ggsave("/Users/rloup/Library/CloudStorage/OneDrive-UniversitédeLausanne/routes_nationales/images/loess_reg.png", width = 9, height = 8)

# Zoom
plot_loess + coord_cartesian(xlim = c(0, 25), ylim = c(-5, 5))
ggsave("/Users/rloup/Library/CloudStorage/OneDrive-UniversitédeLausanne/routes_nationales/images/loess_reg_zoom.png", width = 9, height = 8)
