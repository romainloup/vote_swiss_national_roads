# Charger le package nécessaire pour les corrélations pondérées
if (!require("weights")) install.packages("weights") # Pour utiliser wtd.cor
library(weights)

# Nom de la colonne cible
target <- "X6730"

# Initialisation d'un vecteur pour stocker les résultats
result_correlations <- numeric(ncol(yes[,3:383]) - 1) # Pour stocker les corrélations
p_values <- numeric(ncol(yes[,3:383]) - 1) # Pour stocker les p-valeurs
col_names <- colnames(yes[,3:383])[colnames(yes[,3:383]) != target] # Colonnes à tester

# Boucle sur toutes les colonnes sauf la cible
for (i in seq_along(col_names)) {
  col <- col_names[i]
  # Calcul de la corrélation pondérée entre la colonne cible et la colonne actuelle
  corr_result <- wtd.cor(yes[,3:383][[target]], yes[,3:383][[col]], weight = f)
  result_correlations[i] <- corr_result[1, 1] # Stocker la corrélation
  p_values[i] <- corr_result[4]           # Stocker la p-valeur
}

# Créer un tableau de résultats
results <- data.frame(
  Votation = col_names,
  Correlation = result_correlations,
  P_value = p_values
)

# Trier par corrélation décroissante
results <- results[order(-abs(results$Correlation)), ]

# Afficher les résultats
head(results) # Les 6 votations les plus corrélées
