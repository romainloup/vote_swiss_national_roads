dist_types = c("_cup_XS")
# a letter should precede the 'D' distance and 'K' kernel, e.g. 'DX' and 'KX'

RV2 <- function(dist_types, f) {
  # Get list of kernels and distances
  K_list <- lapply(paste0("K", dist_types), get)
  
  D_list <- vector(mode = "list", length = length(dist_types))
  for (i in 1:length(dist_types)) {
    if (exists(paste0("D", dist_types[i]))) {
      D_list[[i]] <- get(paste0("D", dist_types[i]))
    }
  }
  
  # D_list <- lapply(paste0("D", dist_types), get)
  
  # Number of distances
  n_dist <- length(dist_types)
  
  # Compute CV_11 and nu_1 for each kernel
  CV_11 <- sapply(K_list, function(K) sum(diag(K %*% K)))
  nu_1 <- sapply(K_list, function(K) sum(diag(K))^2 / sum(diag(K %*% K)))
  
  # Initialize matrices and lists
  RVi <- E_RV <- Var_RV <- Z_RV <- matrix(NA, nrow = n_dist, ncol = n_dist)
  Delta_list <- Y_list <- eigen_val_list <- vector("list", length = n_dist)
  Y_list <- eigen_val_list <- vector("list", length = n_dist)
  nb_lambda_pos <- vector("list", length = n_dist)
  
  
  n <- dim(K_list[[1]])[1]  # Assuming all kernels have same dimension
  
  # Compute upper triangular indices
  upper_tri_indices <- upper.tri(matrix(1:(n_dist^2), n_dist, n_dist))
  
  # Compute also diag
  diag(upper_tri_indices) <- TRUE
  
  # Loop through upper triangular indices
  for (k in which(upper_tri_indices)) {
    i <- (k - 1) %/% n_dist + 1
    j <- k %% n_dist
    if (j == 0) j <- n_dist
    
    # Calculate CV/RV indices
    CV_22 <- sum(diag(K_list[[j]] %*% K_list[[j]]))
    CV_12 <- sum(diag(K_list[[i]] %*% K_list[[j]]))
    
    RV_12 <- CV_12 / sqrt(CV_11[i] * CV_22)
    
    nu_2 <- sum(diag(K_list[[j]]))^2 / sum(diag(K_list[[j]] %*% K_list[[j]]))
    
    # Calculate moments
    E_RV_12 <- sqrt(nu_1[i] * nu_2) / (n - 1)
    Var_RV_12 <- (2 * (n - 1 - nu_1[i]) * (n - 1 - nu_2)) / ((n - 2) * (n - 1)^2 * (n + 1))
    Z_12 <- (RV_12 - E_RV_12) / sqrt(Var_RV_12)
    
    # Fill matrices
    E_RV[i, j] <- E_RV_12
    Var_RV[i, j] <- Var_RV_12
    Z_RV[i, j] <- Z_12
    RVi[i, j] <- RV_12
    
    # Diagonals
    # RV_11 <- CV_11[k] / sqrt(CV_11[k] * CV_11[k])
    # E_RV[k, k] <- sqrt(nu_1[k] * nu_1[k]) / (n - 1)
    # Var_RV[k, k] <- (2 * (n - 1 - nu_1[k]) * (n - 1 - nu_1[k])) / ((n - 2) * (n - 1)^2 * (n + 1))
    # Z_RV[k, k] <- (RV_11 - E_RV[k, k]) / sqrt(Var_RV[k, k])
    
  }
  # Loop calculates Y matrix for n_dist K
  for (i in 1:n_dist) {
    eigen_val <- eigen(K_list[[i]])
    U <- eigen_val$vectors
    lambda <- eigen_val$values
    nb_lambda_pos[[i]] = length(lambda[lambda>0]) # count positive lambda
    lambda <- pmax(lambda,0)
    
    if (!is.null(D_list[[i]])) {
      Delta <- 0.5 * t(f) %*% D_list[[i]] %*% f
      Delta_list[[i]] <- Delta
    } else {Delta_list[[i]] <- sum(lambda)}
    
    Y <- diag(1/sqrt(f)) %*% U %*% diag(sqrt(lambda))
    
    # Fill lists
    # Delta_list[[i]] <- Delta
    Y_list[[i]] <- Y
    eigen_val_list[[i]] <- eigen_val
    # nb_lambda_pos[[i]] <- nb_lambda_pos
    print(paste("it",i))
  }
  # Replace NA values with transposed values
  E_RV[upper_tri_indices] <- t(E_RV)[upper_tri_indices]
  Var_RV[upper_tri_indices] <- t(Var_RV)[upper_tri_indices]
  Z_RV[upper_tri_indices] <- t(Z_RV)[upper_tri_indices]
  RVi[upper_tri_indices] <- t(RVi)[upper_tri_indices]
  
  # Set column and row names
  colnames(RVi) <- rownames(RVi) <- colnames(E_RV) <- rownames(E_RV) <- colnames(Var_RV) <- rownames(Var_RV) <- colnames(Z_RV) <- rownames(Z_RV) <- paste0("D", dist_types)
  
  return(list(Y_list = Y_list, 
              Delta_list = Delta_list,
              RVi = RVi, E_RV = E_RV, Var_RV = Var_RV, Z_RV = Z_RV, eigen_val_list = eigen_val_list,
              nb_lambda_pos = nb_lambda_pos))
}
list_RV = RV2(dist_types,f)
dist_types_test = "X"
test = RV2(dist_types_test,f)
test$Z_RV

dist_types = c("X", "Z", "f", "P", "_wealth1", "_wealth2", "_OT1", "_OT2")