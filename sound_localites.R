
# Lire localités
localites = read.csv2("/Users/rloup/Downloads/AMTOVZ_CSV_WGS84/AMTOVZ_CSV_WGS84.csv")

# Liste noms localités
localites_sound <- unique(localites$Ortschaftsname)
localites_sound <- localites$Ortschaftsname


n_l = dim(localites)[1]

cantons_abbr = unique(localites$Kantonskürzel)
cantons_abbr = paste0(" ", cantons_abbr)

cantons_abbr <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR",
                  "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG",
                  "TI", "VD", "VS", "NE", "GE", "JU")

clean_name <- function(x) {
  x <- as.character(x)                                 # forcer en caractère
  x <- trimws(x)                                       # enlever espaces début/fin
  x <- gsub("\\s*\\([^\\)]*\\)\\s*$", "", x)           # retirer parenthèses finales
  x <- gsub(paste0("\\b(", paste(cantons_abbr, collapse = "|"), ")\\b"), "", x)  # supprimer abbr
  x <- trimws(x)                                       # re-trim en cas d'espace résiduel
  x <- tolower(x)                                      # tout en minuscules
  x <- stringi::stri_trans_general(x, "Latin-ASCII")   # retirer accents
  x <- gsub("[^a-z]", "", x)                           # ne garder que les lettres
  return(x)
}

localites_clean <- sapply(localites_sound, clean_name)
# localites_clean <- unique(localites_clean)

suffix_3_list_loc <- suffix_3(localites_clean, n = 3)
localites$suffix3 = suffix_3_list_loc


# Suffixes de 3 lettres
suffix3_tab_loc <- suffix_freq(localites_clean, n = 3)
suffix3_sorted_loc <- sort(suffix3_tab_loc, decreasing = TRUE)
head(suffix3_sorted_loc, 20)


# Suffixes de 3 lettres
suffix3_tab <- suffix_freq(communes_clean, n = 3)
suffix3_sorted <- sort(suffix3_tab, decreasing = TRUE)
head(suffix3_sorted, 20)


# Dans un df
# Créer une liste vide pour stocker les petits tableaux
resultats_list_loc <- list()
resultats_list
# Boucle sur les langues 1 à 4
for (lange in 1:4) {
  for (suff in 1:4) {
    suffix_n_lan <- suffix_freq(communes_clean[which(ch_aggregated_geolevels$language == lange)], n = suff)
    suffix_n_lan <- sort(suffix_n_lan, decreasing = TRUE)
    top20 <- head(suffix_n_lan, 20)
    
    # Stocker sous forme de data frame
    df_temp <- data.frame(
      langue = lange,
      longueur_suffixe = suff,
      suffixe = names(top20),
      frequence = as.numeric(top20),
      stringsAsFactors = FALSE
    )
    
    resultats_list[[length(resultats_list) + 1]] <- df_temp
  }
}

# Maintenant, pour toutes les langues ensemble (langue = 5)
for (suff in 1:4) {
  suffix_n_all <- suffix_freq(communes_clean, n = suff)
  suffix_n_all <- sort(suffix_n_all, decreasing = TRUE)
  top20_all <- head(suffix_n_all, 20)
  
  df_temp_all <- data.frame(
    langue = 5,  # 5 = toutes langues ensemble
    longueur_suffixe = suff,
    suffixe = names(top20_all),
    frequence = as.numeric(top20_all),
    stringsAsFactors = FALSE
  )
  
  resultats_list[[length(resultats_list) + 1]] <- df_temp_all
}

# Coller tous les petits data frames en un seul
resultats_suffixes_df <- do.call(rbind, resultats_list)



library(ggplot2)
library(ggnewscale)

suffixes <- unique(communes_filtered$suffix3)
suffixes <- c("ens", "ins", "wil", "gen")
suffixes <- c("ern")
suffix_colors <- setNames(brewer.pal(length(suffixes), "Set3"), suffixes)
suffix_colors[2] = "#FDB462"

# communes_filtered2 <- communes_sf2[which(communes_sf2$suffix3 %in% suffixes),]

communes_filtered3 <- localites[which(localites$suffix3 %in% suffixes),]
communes_filtered3$E = as.numeric(communes_filtered3$E)
communes_filtered3$N = as.numeric(communes_filtered3$N)

ggplot() +
  
  # 1. Couche des frontières linguistiques avec une première échelle de couleur
  geom_sf(data = ch_langue, aes(color = as.character(language)), fill = NA, alpha = 1, linewidth = 0.3) +
  scale_color_manual(name = "Langue principale", values = langue_colors,
                     labels = c("Allemand", "Français", "Italien", "Romanche")) +
  
  # 2. Couche des lacs
  geom_sf(data = lacsWGS84, fill = "gray90", color = NA, alpha = 1) +
  
  
  # 3. Nouvelle échelle de couleur pour les suffixes
  new_scale_color() +
  
  # 4. Points colorés selon suffixe
  geom_point(data = communes_filtered3, aes(x = E, y = N, color = suffix3), size = 1) +
  scale_color_manual(name = "Suffixe", values = suffix_colors, labels = paste0("-", sort(suffixes))) +
  
  # 5. Habillage
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank()
  ) +
  
  # 6. Echelle
  annotation_scale(style = "ticks", location = "br")

ggsave("heatmaps_suffixes/suffix_moran_ern_localite.png", width = 9, height = 8)

# --- Moran's I
# Appliquer à tous les suffixes fréquents
localites$longitude = as.numeric(localites$E)
localites$latitude = as.numeric(localites$N)

results_loc <- do.call(rbind, lapply(frequent_suffixes, compute_moran, df_coords = localites))
results_df_loc <- as.data.frame(results_loc)
results_df_loc$moran_i <- as.numeric(results_df_loc$moran_i)
results_df_loc$p <- as.numeric(results_df_loc$p)

# Trier par degré d'autocorrélation
results_sorted_loc <- results_df_loc[order(-results_df_loc$moran_i), ]
head(results_sorted_loc, 25)
