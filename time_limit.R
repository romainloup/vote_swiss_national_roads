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
road_time_values <- seq(1, 141, by = 10) # Incréments de 10
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
  theme_minimal() +
  scale_y_continuous(trans = "log10", labels = scales::scientific) + # Échelle logarithmique pour p-values
  labs(y = "P-value (log scale)")

# Combinaison des graphiques
grid.arrange(plot_mean, plot_pvalue, ncol = 1)

ggsave("/Users/rloup/Library/CloudStorage/OneDrive-UniversitédeLausanne/routes_nationales/images/two_lines.png", width = 9, height = 8)

