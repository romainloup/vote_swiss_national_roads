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
    polar_index = mean(abs(rec_score - mean_score), na.rm = TRUE),
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

votes_econ <- vote_meta %>%
  filter(if_any(starts_with("theme"), ~ .x > 399 & .x < 500)) %>%
  pull(vote_id)
