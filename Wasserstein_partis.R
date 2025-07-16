# -------------------- 0. Setup --------------------
# install.packages(c("readr", "dplyr", "tidyr", "stringr"))  # if needed
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# -------------------- 1. File paths --------------------
reco_path  <- "recommandations_partis.csv"  # update if stored elsewhere
vote_path  <- "voteInfo.csv"

# -------------------- 2. Read files --------------------
reco_raw  <- read_delim(reco_path, delim = ";",
                        locale = locale(encoding = "UTF-8"), 
                        trim_ws = TRUE, show_col_types = FALSE)

voteinfo  <- read_csv(vote_path, show_col_types = FALSE)

# -------------------- 3. Reshape recommendation table --------------------
# All columns except the first ('parti') are vote IDs
reco_long <- reco_raw %>% 
  pivot_longer(-parti,
               names_to  = "vote_id",
               values_to = "rec_code",
               values_transform = list(rec_code = as.numeric)) %>% 
  # Ignore missing cells (parties that did not issue a recommendation)
  filter(!is.na(rec_code))

# -------------------- 4. Map codes to numeric positions --------------------
#  1 = YES  -> +1
#  2 = NO   -> -1
#  4/5 = neutral (blank / free vote) -> 0
reco_long <- reco_long %>% 
  mutate(rec_score = case_when(
    rec_code == 1 ~  1,
    rec_code == 2 ~ -1,
    rec_code %in% c(4, 5) ~  0,
    TRUE ~ NA_real_
  ))

# -------------------- 5. Compute polarisation index --------------------
polarisation <- reco_long %>% 
  group_by(vote_id) %>% 
  summarise(
    n_parties   = n(),                    # parties that took any position
    mean_score  = mean(rec_score, na.rm = TRUE),
    polar_index = mean(abs(rec_score - mean_score), na.rm = TRUE), # dispersion
    .groups = "drop"
  ) %>% 
  # Tagging categories (tweak thresholds to taste)
  mutate(polar_cat = case_when(
    polar_index < 0.25 ~ "Consensus",
    polar_index < 0.50 ~ "Moderate",
    TRUE               ~ "Polarised"
  ))

# -------------------- 6. Merge with vote metadata --------------------
# voteinfo has a numeric column 'vote_id'; ensure same type
vote_meta <- voteinfo %>% 
  mutate(vote_id = as.character(vote_id)) %>% 
  left_join(polarisation, by = "vote_id")

# -------------------- 7. Ajouter les étiquettes thématiques (niveau 1 + précis) --------------------
theme_labels <- read_csv("themeVotesNames.csv", show_col_types = FALSE)
theme_labels <- themeVotesNames

# 1. On garde les colonnes utiles et renomme pour clarté
theme_labels <- theme_labels %>%
  select(Code, Parent, Level, theme_label_fr = Name_fr)

# 2. On extrait les thèmes de niveau 1
level1_themes <- theme_labels %>%
  filter(Level == 1) %>%
  select(main_theme_code = Code, main_theme_label_fr = theme_label_fr)

# 3. On ajoute à chaque code son thème principal (niveau 1)
theme_lookup_full <- theme_labels %>%
  left_join(level1_themes, by = c("Parent" = "main_theme_code")) %>%
  mutate(main_theme_label_fr = ifelse(Level == 1, theme_label_fr, main_theme_label_fr))


# 4. On applique cela à vote_meta via theme1_id
theme_lookup_full <- theme_lookup_full %>%
  mutate(Code = as.numeric(Code))

vote_meta <- vote_meta %>%
  rowwise() %>%
  mutate(primary_theme_id = first(na.omit(c(theme1_id, theme2_id, theme3_id)))) %>%
  ungroup() %>%
  left_join(theme_lookup_full, by = c("primary_theme_id" = "Code"))

# 5. Résultat : deux colonnes supplémentaires
# - theme_label_fr         → nom du thème précis
# - main_theme_label_fr    → grand domaine


# -------------------- 8. Inspect --------------------
print(vote_meta %>% 
        select(vote_id, date, object_fr, polar_index, polar_cat, theme_label_fr) %>% 
        head())

# -------------------- 9. (Optional) save to disk --------------------
# write_csv(vote_meta, "vote_meta_polarisation.csv")


# Étape 1 – Filtrage des votations par thème
library(dplyr)

# Exemple : isoler les votes sur la "Politique économique"
votes_test <- vote_meta %>%
  filter(main_theme_label_fr == "Économie") %>%
  pull(vote_id)

votes_test <- vote_meta %>%
  filter(theme1_id == 1031 | theme2_id == 1031 | theme3_id == 1031) %>%
  pull(vote_id)

nb1 = 99
nb2 = 200
for (i in 1:12) {
  
# votes_econ <- vote_meta %>%
#   filter(if_any(starts_with("theme"), ~ .x > 399 & .x < 500)) %>%
#   pull(vote_id)

votes_econ <- vote_meta %>%
  filter(if_any(starts_with("theme"), ~ .x > nb1 & .x < nb2)) %>%
  pull(vote_id)

nb1 = nb1 + 100
nb2 = nb2 + 100

# --- K par thématique ---

# paste0("X", votes_econ)

yes_subset <- yes %>% select(intersect(names(yes), paste0("X", votes_econ)))


# P_rel for X
DX_theme = as.matrix(dist(yes_subset)^2) # political distances between municipalities
# DeltaX_theme = 0.5 * t(f) %*% DX_theme %*% f # inertia 
# AX=exp(-2*DX)
# AX=1/(1+DX^0.5) # affinity politique
KX_theme = -0.5 * diag(sqrt(f)) %*% H %*% DX_theme %*% t(H) %*% diag(sqrt(f)) # political kernel


test = mds_fun_Y2(f, KX_theme, ch_aggregated_geolevels)
ggsave(paste0("wasserstein/mds_theme", i, ".png"), width = 8, height = 9)
}


library(ggtext)
mds_fun_Y2 = function(fi, K, ch){
  
  eigen_val <- eigen(K)
  U <- eigen_val$vectors
  lambda <- eigen_val$values
  
  Y <- diag(1/sqrt(fi)) %*% U %*% diag(sqrt(lambda))
  
  # dist_nb = which(dist_types == letter)
  
  # if (!missing(conditionSample)) {
  # dataVotFun = dataVotFun[conditionSample,]
  #   fi = fi[conditionSample]
  # }
  
  # dataVot = dataVot[conditionSample,]
  # f = f[conditionSample]
  
  mds = as.data.frame(Y[,1:2])
  mds$V1 = Re(mds$V1)
  mds$V2 = Re(mds$V2)
  mds$commune = ch$NAME
  mds$langue = as.character(ch$language)
  mds$f = fi
  mds$voters = ch$swiss_data_muni.f
  
  mds_filtered = mds[mds$voters > sort(mds$voters, decreasing = TRUE)[1000], ]
  
  propDeltaPC = round(100*lambda / sum(lambda), digits = 1 )
  
  # print(propDeltaPC)
  magnif = 0.2+0.5*(log(fi)-min(log(fi)))/(max(log(fi))-min(log(fi))) # defines a magnification factor for the object weights (here from 0.5 to 2)
  xlab = paste("Factor 3, inertia explained =",propDeltaPC[1],"%")
  ylab = paste("Factor 4, inertia explained =",propDeltaPC[2],"%")
  
  # magnif = magnif[conditionSample]
  mds_plot = ggplot() +
    geom_vline(xintercept = 0,linetype="dashed") +
    geom_hline(yintercept = 0, linetype="dashed") +
    geom_point(aes(x = -mds[,1], y = -mds[,2], size=fi, color=mds$langue),
               alpha = magnif) +
    # geom_point(aes(x = mds[,1], y = -mds[,2], color=mds$langue)) +
    geom_text(aes(x = -mds_filtered[,1], y = -mds_filtered[,2], label = mds_filtered$commune)) +
    scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"),
                       labels = c("German", "French", "Italian","Romansh")) +
    labs(x = xlab, y = ylab) +
    # labs(size = paste0("Reg. weight ", expression(italic("f"))), color = "Language") +
    labs(size = "Reg. weight *f*", color = "Language") +
    scale_size_continuous(range = c(1, 8)) +
    theme_minimal() +
    theme(legend.title = element_markdown(lineheight = 1.2))
  
  return(list(eigen_val=eigen_val,mds=mds,mds_plot=mds_plot))
}







Z_pre_canton = data.frame(1:n, as.factor(ch_aggregated_geolevels$KANTONSNUM))
names(Z_pre_canton) = c("commune", "canton")
Z_pre_canton = Z_pre_canton %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = canton, values_from = value, values_fill = list(value = 0), names_prefix = "canton")
Z_canton = as.matrix(Z_pre_canton[,-1])
colnames(Z_canton) = unique(geoLevels$Canton)

rho_canton = t(Z_canton)%*%f
R = diag(as.vector(rho_canton))
S = diag(as.vector(sqrt(1/rho_canton)))%*%t(Z_canton)%*%diag(sqrt(f))
S%*%t(S)

# K_tild = S%*%KX%*%t(S)

KX_tild = S%*%KX%*%t(S)

# --- import and compute vote distance kernel

national_23 = read.csv2("wasserstein/conseil_national_2023.csv")

# remove NA columns
national_23_p = national_23[,colSums(is.na(national_23)) < nrow(national_23)]
# rowsum and divide to have a percentage
national_23_p = as.data.frame(t(apply(national_23[,-c(1:2)], 1, function(x) x / sum(x, na.rm = TRUE))))

# Distance de Hellinger
hellinger <- function(p1, p2) {
  ok <- !is.na(p1) & !is.na(p2)
  if (sum(ok) == 0) return(NA)
  d <- sqrt(sum((sqrt(p1[ok]) - sqrt(p2[ok]))^2)) / sqrt(2)
  return(d)
}

# n <- nrow(national_23_p)   # 2126
dist_mat <- matrix(NA, n, n)

for (i in 1:n) {
  for (j in i:n) {
    d <- hellinger(national_23_p[i, ], national_23_p[j, ])
    dist_mat[i, j] <- d
    dist_mat[j, i] <- d
  }
  print(i)
}

View(dist_mat)

# faster
library(parallel)

hellinger <- function(p1, p2) {
  ok <- !is.na(p1) & !is.na(p2)
  if (sum(ok) == 0) return(NA)
  sqrt(sum((sqrt(p1[ok]) - sqrt(p2[ok]))^2)) / sqrt(2)
}

n <- nrow(national_23_p)
pairs <- combn(n, 2, simplify = FALSE)

# Parallélisation
dist_vals <- mclapply(pairs, function(idx) {
  i <- idx[1]; j <- idx[2]
  hellinger(national_23_p[i, ], national_23_p[j, ])
}, mc.cores = detectCores() - 1)

# Construction de la matrice symétrique
dist_mat <- matrix(0, n, n)
for (k in seq_along(pairs)) {
  i <- pairs[[k]][1]
  j <- pairs[[k]][2]
  d <- dist_vals[[k]]
  dist_mat[i, j] <- d
  dist_mat[j, i] <- d
  print(i)
}
diag(dist_mat) <- 0
write.csv(dist_mat, "national_vote_distance.csv", row.names = F)

KX_nat = -0.5 * diag(sqrt(f)) %*% H %*% dist_mat %*% t(H) %*% diag(sqrt(f)) # political kernel


# --- second distance ---
# correlation kernel between votes
View(yes[,3:383])

# Fonction de corrélation pondérée
weighted_cor <- function(x, y, w) {
  mx <- sum(w * x) / sum(w)
  my <- sum(w * y) / sum(w)
  cov_xy <- sum(w * (x - mx) * (y - my)) / sum(w)
  sd_x <- sqrt(sum(w * (x - mx)^2) / sum(w))
  sd_y <- sqrt(sum(w * (y - my)^2) / sum(w))
  return(cov_xy / (sd_x * sd_y))
}

# Matrice de corrélation pondérée
cols <- ncol(yes[,3:383])
cor_mat <- matrix(NA, nrow = cols, ncol = cols)
colnames(cor_mat) <- names(yes[,3:383])
rownames(cor_mat) <- names(yes[,3:383])

for (i in 1:cols) {
  for (j in i:cols) {
    cor_ij <- weighted_cor(yes[,3:383][[i]], yes[,3:383][[j]], f)
    cor_mat[i, j] <- cor_ij
    cor_mat[j, i] <- cor_ij
  }
}

cor_mat <- as.data.frame(cor_mat)



rho_vote = rep(1/cols,cols)
R_vote = diag(as.vector(rho_vote))
S = diag(as.vector(sqrt(1/rho_vote)))%*%t(Z_canton)%*%diag(sqrt(f))
S%*%t(S)

# K_tild = S%*%KX%*%t(S)

KX_tild = S%*%KX%*%t(S)