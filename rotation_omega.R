rotate_points <- function(x, y, omega) {
  theta <- omega * pi / 180  # Conversion en radians
  R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2) # Matrice de rotation
  
  coords <- rbind(x, y)  # Regroupement des coordonnées en une matrice 2 x N
  rotated_coords <- R %*% coords  # Application de la rotation
  
  return(list(x = rotated_coords[1, ], y = rotated_coords[2, ]))  # Renvoie les nouvelles coordonnées
}

# Exemple d'utilisation
x <- c(1, 2, 3)
y <- c(1, 2, 3)
omega <- 45  # Rotation de 45 degrés

rotated <- rotate_points(mds$V1, mds$V2, -acos(Omega[1,1])*180/pi)
rotated = as.data.frame(rotated)
mds$V1_rot = rotated$x
mds$V2_rot = rotated$y

# Tracer les points avant et après la rotation
plot(mds$V1, mds$V2, col = "red", pch = 16)
points(rotated$x, rotated$y, col = "blue", pch = 16)
arrows(x, y, rotated$x, rotated$y, col = "gray", length = 0.1)  # Indique le déplacement

plot(rotated$x, rotated$y, col = "blue", pch = 16)
