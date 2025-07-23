library(tidyverse)

# Exemple de données
set.seed(1)
votes <- tibble(
  id = 1:381,
  x = runif(381),
  y = runif(381),
  theme1 = sample(1:12, 381, replace = TRUE),
  theme2 = sample(c(NA, 1:12), 381, replace = TRUE),
  theme3 = sample(c(NA, 1:12), 381, replace = TRUE)
)

# Palette de 12 couleurs
theme_colors <- setNames(RColorBrewer::brewer.pal(12, "Set3"), as.character(1:12))

# Base du graphique
plot_base <- ggplot(votes, aes(x = x, y = y)) +
  theme_minimal()

# Fonction corrigée : chaque couche thématique
plot_layer <- function(votes, theme_col, size = 4, dx = 0, dy = 0) {
  votes %>%
    filter(!is.na(.data[[theme_col]])) %>%
    mutate(col = as.character(.data[[theme_col]])) %>%
    ggplot2::geom_point(
      mapping = aes(x = x + dx, y = y + dy, color = col),
      size = size,
      show.legend = TRUE
    )
    
}

# Superposition des trois couches
p <- plot_base +
  plot_layer(votes, "theme1", size = 2, dx = -0.002, dy = 0.002) +
  plot_layer(votes, "theme2", size = 2, dx = 0.002, dy = 0.002) +
  plot_layer(votes, "theme3", size = 2, dx = 0, dy = -0.002) +
  scale_color_manual(values = theme_colors) +
  scale_fill_discrete(labels=theme_names)

# Affichage
print(p)

# main themes
themeVotesNames[which(is.na(themeVotesNames$Parent)),]$Name_en


votes = data.frame(test$mds$commune, test$mds$V1, test$mds$V2, floor(vote_meta[1:381,]$theme1_id/100), floor(vote_meta[1:381,]$theme2_id/100), floor(vote_meta[1:381,]$theme3_id/100))

names(votes) = c("id", "x", "y", "theme1", "theme2", "theme3")

# Vecteur de correspondance
theme_names <- c(
  "State organisation",
  "Foreign policy",
  "Security policy",
  "Economy",
  "Agriculture",
  "Public finance",
  "Energy",
  "Transport and infrastructure",
  "Environment and living space",
  "Social policy",
  "Education and research",
  "Culture, religion and media"
)

# Ajouter les colonnes avec les labels
votes <- votes %>%
  mutate(
    theme1_label = theme_names[theme1],
    theme2_label = theme_names[theme2],
    theme3_label = theme_names[theme3]
  )

# votes = tibble(votes)





# ------ V2

plot_layer <- function(votes, theme_col, size = 2, dx = 0, dy = 0) {
  theme_names <- c(
    "State organisation", "Foreign policy", "Security policy", "Economy",
    "Agriculture", "Public finance", "Energy", "Transport and infrastructure",
    "Environment and living space", "Social policy", "Education and research",
    "Culture, religion and media"
  )
  
  votes %>%
    filter(!is.na(.data[[theme_col]])) %>%
    mutate(
      col = theme_names[.data[[theme_col]]]
    ) %>%
    ggplot2::geom_point(
      mapping = aes(x = x + dx, y = y + dy, color = col),
      size = size,
      show.legend = TRUE
    )
}


library(RColorBrewer)
theme_names <- c(
  "State organisation", "Foreign policy", "Security policy", "Economy",
  "Agriculture", "Public finance", "Energy", "Transport and infrastructure",
  "Environment and living space", "Social policy", "Education and research",
  "Culture, religion and media"
)

# 12 couleurs, noms explicites
theme_colors_named <- setNames(
  RColorBrewer::brewer.pal(12, "Set3"),
  theme_names
)

scale_color_manual(
  values = theme_colors_named,
  breaks = theme_names,          # assure l’ordre de la légende
  name = "Theme"
)

p <- ggplot() +
  plot_layer(votes, "theme1", dx = -0.002, dy = 0.002) +
  plot_layer(votes, "theme2", dx = 0.002, dy = 0.002) +
  plot_layer(votes, "theme3", dx = 0, dy = -0.002) +
  scale_color_manual(
    values = theme_colors_named,
    breaks = theme_names,
    name = "Theme"
  ) +
  labs(x = "Factor 1, inertia explained = 60.9 %", y = "Factor 2, inertia explained = 12.8 %") +
  theme_minimal()
p
ggsave(paste0("wasserstein/mds_vote_theme.pdf"), width = 11, height = 8)
