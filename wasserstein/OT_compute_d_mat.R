# Charger les données
richesse_data <- read.csv("richesse_data.csv")
payer_data <- read.csv("/Users/rloup/Documents/r_projects/vote_swiss_national_roads/IFD_classes_revenu_net_normaux_nb_contribuables_2024.csv")

# Supprimer la première colonne (ID)
# richesse_mat <- as.matrix(richesse_data[, -1])
richesse_mat <- as.matrix(richesse_data)
payer_mat <- as.matrix(payer_data[, -c(1,2,13)])
# payer_mat <- payer_mat/rowSums(payer_mat)


# Municipality wealth
richesse_vect <- richesse_mat/payer_mat

richesse_vect[is.nan(richesse_vect)] <- 0
richesse_vect <- rowSums(richesse_vect)

# richesse_vect <- richesse_vect/rowSums(richesse_vect)


# Distance L1 : d_{ij}^{wealth_1} = |x_i - x_j|
D_wealth1 <- abs(outer(richesse_vect, richesse_vect, "-"))

# Distance L2 : d_{ij}^{wealth_2} = (x_i - x_j)^2
D_wealth2 <- (outer(richesse_vect, richesse_vect, "-"))^2

# Transformer en proportions (chaque ligne = distribution)
row_sums <- rowSums(richesse_mat)
g <- richesse_mat / row_sums
# g <- f

# Définir les milieux des classes
mid_y <- c(0, 15000, 35000, 45000, 62500, 87500, 150000, 350000, 750000, 2000000)

# Fonction cumulative
cumsum_mat <- t(apply(g, 1, cumsum))

# Nombre de communes
n <- nrow(g)

# Initialiser les matrices de distances
d_OT1 <- matrix(0, n, n)
d_OT2 <- matrix(0, n, n)

# Boucle principale
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    # Différence des fonctions de répartition
    diff_cdf <- cumsum_mat[i, ] - cumsum_mat[j, ]
    
    # Wasserstein-1
    d1 <- sum(abs(diff_cdf) * c(diff(mid_y), 0))  # Attention : diff(mid_y) = longueurs entre classes
    # Wasserstein-2
    d2 <- sum((diff_cdf^2) * c(diff(mid_y), 0))
    
    d_OT1[i, j] <- d_OT1[j, i] <- d1
    d_OT2[i, j] <- d_OT2[j, i] <- d2
    
    # print(paste("j",j))
  }
  print(paste("i",i))
}

# --- Version calcul matriciel
# On suppose que `cumsum_mat` est déjà défini comme suit :
# cumsum_mat <- t(apply(g, 1, cumsum))

# Définir les longueurs entre classes (delta y)
dy <- c(diff(y), 0)  # On ajoute un zéro pour que dy ait longueur 10

# Centrer les poids selon dy
weighted_diff <- function(cdf_mat, dy, power = 1) {
  # (n x 10 x n) tableau implicite de toutes les paires de lignes
  n <- nrow(cdf_mat)
  # Calculs matriciels pairés (via broadcasting)
  # => On étend les matrices (via réplication) pour faire des différences pairées
  
  # Étend les cdf (3D implicite)
  A <- array(rep(t(cdf_mat), times = n), dim = c(10, n, n))     # A[ , i, j] = cdf[i, ]
  B <- array(rep(cdf_mat, each = n), dim = c(n, 10, n))         # B[ i, , j] = cdf[j, ]
  
  diff_array <- A - aperm(B, c(2,1,3))  # Dimensions: [10, n, n]
  diff_array <- aperm(diff_array, c(2,1,3))  # Dimensions: [n, 10, n]
  
  # Appliquer la pondération dy et l'exposant
  dy_mat <- matrix(dy, nrow = 1)
  if (power == 1) {
    w_dist <- apply(abs(diff_array), c(1,3), function(x) sum(x * dy))
  } else if (power == 2) {
    w_dist <- apply(diff_array^2, c(1,3), function(x) sum(x * dy))
  }
  
  return(w_dist)
}

# Calculer les matrices de distance
d_OT1 <- weighted_diff(cumsum_mat, dy, power = 1)
d_OT2 <- weighted_diff(cumsum_mat, dy, power = 2)

# S'assurer que les diagonales soient bien nulles
diag(d_OT1) <- 0
diag(d_OT2) <- 0


D_OT1 = d_OT1
D_OT2 = d_OT2
rm(d_OT1)
rm(d_OT2)
K_OT1 = -0.5 * diag(sqrt(f)) %*% H %*% D_OT1 %*% t(H) %*% diag(sqrt(f)) # wealth kernel OT1
K_OT2 = -0.5 * diag(sqrt(f)) %*% H %*% D_OT2 %*% t(H) %*% diag(sqrt(f)) # wealth kernel OT2

K_wealth1 = -0.5 * diag(sqrt(f)) %*% H %*% D_wealth1 %*% t(H) %*% diag(sqrt(f)) # wealth kernel OT1
K_wealth2 = -0.5 * diag(sqrt(f)) %*% H %*% D_wealth2 %*% t(H) %*% diag(sqrt(f)) # wealth kernel OT2

