## ============================================================
## Version "base" de ton algorithme, stabilisée mais inchangée
## dans l'esprit: forces politiques -> directions géo normalisées
## + ancrage vers positions initiales
## ============================================================

## --- Distances euclidiennes en WGS84 (comme dans ton script)
euclidean_distance <- function(p1, p2) {
  sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)
}

euclidean_dist_matrix <- function(coords) {
  as.matrix(dist(coords))   # en degrés (WGS84), fidèle à ta version
}

## --- Ajustement des positions (version très proche de la tienne)
adjust_positions_basic_stable <- function(coord_toy, dist_pol,
                                          alpha        = 1,      # même sens que chez toi
                                          iterations   = 10,
                                          k_spring     = 0.001,  # force des ressorts politiques
                                          k_init       = 0.05,   # ancrage vers origine (plus doux par défaut)
                                          center_each_iter = TRUE,  # enlève la translation globale
                                          max_step_per_iter = NA,   # en "degrés" (optionnel; ex 0.02 ~ qq km)
                                          verbose      = TRUE) {
  
  X0 <- as.matrix(coord_toy)               # positions d'origine
  X  <- X0                                 # positions courantes
  n  <- nrow(X)
  eps <- 1e-12
  
  ## 1) Normaliser la matrice politique comme tu faisais
  dist_pol <- as.matrix(dist_pol)
  if (!isSymmetric(dist_pol, tol = 1e-12)) {
    dist_pol <- (dist_pol + t(dist_pol)) / 2
  }
  diag(dist_pol) <- 0
  # Normalisation 0-1 sur l’OFF-diagonale
  off <- dist_pol[upper.tri(dist_pol)]
  rng <- range(off, na.rm = TRUE)
  if (diff(rng) > 0) {
    dist_pol_norm <- (dist_pol - rng[1]) / (rng[2] - rng[1])
  } else {
    dist_pol_norm <- dist_pol * 0  # cas dégénéré
  }
  diag(dist_pol_norm) <- 0
  dist_mean <- mean(dist_pol_norm[upper.tri(dist_pol_norm)], na.rm = TRUE)
  
  for (iter in 1:iterations) {
    ## 2) Distances géo courantes (en degrés, comme chez toi)
    D_geo <- euclidean_dist_matrix(X)
    diag(D_geo) <- 0
    # éviter divisions par 0
    D_geo[D_geo < eps] <- eps
    
    ## 3) Boucle sur i (fidèle à ton schéma)
    # On accumule les nouvelles positions dans X_new pour éviter d’utiliser des i déjà mis à jour
    X_new <- X
    
    for (i in 1:n) {
      # lignes i
      dist_geo_i    <- D_geo[i, ]                 # n
      dist_target_i <- dist_pol_norm[i, ]         # n
      
      # ta force politique (centrée par dist_mean)
      force_i <- k_spring * (dist_target_i - dist_mean)  # n
      
      # direction: (x_j - x_i) / d_ij   (même logique que chez toi; avec protections)
      direction_ij <- sweep(X, 2, X[i, ], FUN = "-")     # n x 2
      direction_ij <- direction_ij / dist_geo_i          # division par vecteur (recyclage par lignes)
      direction_ij[!is.finite(direction_ij)] <- 0
      
      # ajustement "politique" (signe et alpha comme chez toi)
      adjustment_mat <- alpha * (force_i * direction_ij) # n x 2 (force_i recyclé par colonnes)
      adjustment_mat[!is.finite(adjustment_mat)] <- 0
      
      # déplacement total dû aux autres j (colSums comme chez toi)
      delta_i <- -colSums(adjustment_mat, na.rm = TRUE)  # 1 x 2
      
      # ancrage vers la position d'origine (mêmes formules que chez toi)
      d0      <- euclidean_distance(X[i, ], X0[i, ])
      if (d0 > eps) {
        dir0   <- (X0[i, ] - X[i, ]) / d0
      } else {
        dir0   <- c(0, 0)
      }
      adjustment_init <- k_init * dir0 * (d0^2)  # exactement ton terme : k_init * direction * d0^2
      
      # déplacement proposé
      move_i <- delta_i + adjustment_init
      
      # cap optionnel (en "degrés")
      if (is.finite(max_step_per_iter) && !is.na(max_step_per_iter)) {
        mnorm <- sqrt(sum(move_i^2))
        if (mnorm > max_step_per_iter) {
          move_i <- move_i * (max_step_per_iter / mnorm)
        }
      }
      
      # appliquer
      X_new[i, ] <- X[i, ] + move_i
    }
    
    # 4) Centrer (supprime la translation globale parasite)
    if (center_each_iter) {
      X_new <- sweep(X_new, 2, colMeans(X_new), "-")
      # recentrer autour du centroïde d'origine pour rester proche de la Suisse
      X_new <- X_new + matrix(colMeans(X0), n, 2, byrow = TRUE)
    }
    
    X <- X_new
    
    if (verbose && (iter %% 10 == 0)) {
      cat(sprintf("It %d / %d\n", iter, iterations))
    }
  }
  
  # sortie data.frame comme chez toi
  out <- as.data.frame(X)
  names(out) <- names(coord_toy)
  out
}

## ============================================================
## EXEMPLE D’APPEL (adapte les objets/paramètres)
## ============================================================

# coord_toy = data.frame(longitude = ..., latitude = ...)  # WGS84
# dist_pol  = DX

# Réglage "conservateur" pour retrouver ta belle carte :
# - alpha modéré
# - ancrage doux (ajuste-le)
# - cap de déplacement (optionnel)
coord_toy_new4 <- adjust_positions_basic_stable(
  coord_toy  = coord_toy,
  dist_pol   = dist_pol,
  alpha      = 0.6,
  iterations = 50,       # augmente si nécessaire
  k_spring   = 0.001,
  k_init     = 0.5,     # si tu veux ta valeur d’origine: 0.5 (mais ça bouge fort)
  center_each_iter = TRUE,
  max_step_per_iter = NA,  # par ex. 0.02 pour limiter à ~ qq km en lat/long
  verbose    = TRUE
)

## --- Visualisation simple (flèches)
plot(coord_toy$longitude, coord_toy$latitude,
     main = "Carte initiale (bleu) vs ajustée (rouge)",
     col = "blue", pch = 16,
     xlim = range(c(coord_toy$longitude, coord_toy_new4$longitude)),
     ylim = range(c(coord_toy$latitude,  coord_toy_new4$latitude)))
points(coord_toy_new4$longitude, coord_toy_new4$latitude, col = "red", pch = 16)
segments(coord_toy$longitude, coord_toy$latitude,
         coord_toy_new4$longitude, coord_toy_new4$latitude,
         col = adjustcolor("gray30", alpha.f = 0.4))
legend("topright", legend = c("Initial", "Ajusté"), col = c("blue","red"), pch = 16)



# Nice map
coord_toy_new4$language = ch_aggregated_geolevels$language
ggplot() +
  # geom_point(data = coord_toy, aes(x = longitude, y = latitude, color = "Initial", size = f)) +
  geom_sf(ch,mapping=aes(geometry=geometry),
          fill=alpha("#dfdfdf",0.75),color=alpha("white",0.4)) +
  geom_sf(lakes,mapping=aes(geometry=geometry),
          fill=alpha("#c1c1ce",0.75),color=alpha("white",0.2)) +
  geom_segment(aes(x = coord_toy$longitude, y = coord_toy$latitude, 
                   xend = coord_toy_new4$longitude, yend = coord_toy_new4$latitude, 
                   alpha = f), 
               colour = "red") +
  # geom_segment(aes(x = coord_toy$longitude, y = coord_toy$latitude, xend = coord_toy_new4$longitude, yend = coord_toy_new4$latitude, colour = "segment", alpha = f)) +
  geom_point(data = coord_toy_new4, aes(x = longitude, y = latitude, color = as.character(language), size = f)) +
  scale_color_manual(values = c(
    "1" = "#66C2A5",
    "2" = "#FC8D62",
    "3" = "#8DA0CB",
    "4" = "#E78AC3"),
    name = "Language", 
    labels = c("German", "French", "Italian", "Romansh")
  ) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("wasserstein/deformation_carte.pdf", width = 10, height = 7)
