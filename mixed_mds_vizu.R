

mixedMDS = as.data.frame(list_results$Y_list[[1]][,1])
mixedMDS$B = list_results$Y_list[[4]][,1]
names(mixedMDS) = c("pol", "income")
mixedMDS$municipality = incomes$municipality
mixedMDS$f = f

mds_filtered_mixed = mixedMDS[mixedMDS$f > sort(mixedMDS$f, decreasing = TRUE)[101], ]
mds_filtered_mixed = mds_filtered_mixed[order(mds_filtered_mixed$f, decreasing = TRUE),]


# Plot
propDeltaPC_A = round(100*list_results$eigen_val_list[[1]]$values / sum(list_results$eigen_val_list[[1]]$values), digits = 1 )
propDeltaPC_B = round(100*list_results$eigen_val_list[[4]]$values / sum(list_results$eigen_val_list[[4]]$values), digits = 1 )

magnif = 0.2+0.5*(log(f)-min(log(f)))/(max(log(f))-min(log(f))) # defines a magnification factor for the object weights (here from 0.5 to 2)

xlab = paste("Factor 1, politic, inertia explained =",propDeltaPC_A[1],"%")
ylab = paste("Factor 1, MDS incomes, inertia explained =",propDeltaPC_B[1],"%")

ggplot() +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(aes(x = list_results$Y_list[[1]][,1], y = list_results$Y_list[[4]][,1], size=f, color=mds_I$language),
             alpha = magnif) +
  # geom_point(aes(x = yes$X6730, y = list_results$Y_list[[4]][,1], size=f, color=mds_I$language),
  #            alpha = magnif) +
  ggrepel::geom_text_repel(aes(x = mds_filtered_mixed[,1], y = mds_filtered_mixed[,2], label = mds_filtered_mixed$municipality),
                           box.padding = 0.5,   # Espace autour des étiquettes
                           point.padding = 0.3, # Espace autour des points
                           max.overlaps = Inf ) +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"),
                     labels = c("German", "French", "Italian","Romansh")) +
  labs(x = xlab, y = ylab) +
  labs(size = "Reg. weight *f*", color = "Language") +
  scale_size_continuous(range = c(1, 8)) +
  theme_minimal() +
  theme(legend.title = ggtext::element_markdown(lineheight = 1.2))


# Incomes vs vote
mixedMDS = as.data.frame(list_results$Y_list[[1]][,1])
mixedMDS$B = list_results$Y_list[[4]][,1]
names(mixedMDS) = c("pol", "income")
mixedMDS$municipality = incomes$municipality
mixedMDS$f = f
mixedMDS$vote = yes$X6730

mds_filtered_mixed = mixedMDS[mixedMDS$f > sort(mixedMDS$f, decreasing = TRUE)[31], ]
mds_filtered_mixed = mds_filtered_mixed[order(mds_filtered_mixed$f, decreasing = TRUE),]

ggplot() +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  # geom_point(aes(x = list_results$Y_list[[1]][,1], y = list_results$Y_list[[4]][,1], size=f, color=mds_I$language),
  #            alpha = magnif) +
  geom_point(aes(x = yes$X6730, y = list_results$Y_list[[4]][,1], size=f, color=mds_I$language),
             alpha = magnif) +
  ggrepel::geom_text_repel(aes(x = mds_filtered_mixed$vote, y = mds_filtered_mixed[,2], label = mds_filtered_mixed$municipality),
                           box.padding = 0.5,   # Espace autour des étiquettes
                           point.padding = 0.3, # Espace autour des points
                           max.overlaps = Inf ) +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"),
                     labels = c("German", "French", "Italian","Romansh")) +
  labs(x = xlab, y = ylab) +
  labs(size = "Reg. weight *f*", color = "Language") +
  scale_size_continuous(range = c(1, 8)) +
  theme_minimal() +
  theme(legend.title = ggtext::element_markdown(lineheight = 1.2))





# Omega rotation

mds$vote = yes$X6730
mds$vote = incomes$municipality
mds$v1_incomes = list_results$Y_list[[4]][,1]

propDeltaPC_A = round(100*eigen(K_trunc_SX)$values / sum(eigen(K_trunc_SX)$values), digits = 1 )

mds_filtered_omega = mds[mds$f > sort(mds$f, decreasing = TRUE)[31], ]
mds_filtered_omega = mds_filtered_omega[order(mds_filtered_omega$f, decreasing = TRUE),]

xlab = "First MDS political factor, adjusted on municipality's size"

ggplot() +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  # geom_point(aes(x = list_results$Y_list[[1]][,1], y = list_results$Y_list[[4]][,1], size=f, color=mds_I$language),
  #            alpha = magnif) +
  geom_point(aes(x = mds$V1_rot, y = mds$v1_incomes, size=f, color=mds$langue),
             alpha = magnif) +
  ggrepel::geom_text_repel(aes(x = mds_filtered_omega$V1_rot, y = mds_filtered_omega$v1_incomes, label = mds_filtered_omega$commune),
                           box.padding = 0.5,   # Espace autour des étiquettes
                           point.padding = 0.3, # Espace autour des points
                           max.overlaps = Inf ) +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB",  "#E78AC3"),
                     labels = c("German", "French", "Italian","Romansh")) +
  labs(x = xlab, y = ylab) +
  labs(size = "Reg. weight *f*", color = "Language") +
  scale_size_continuous(range = c(1, 8)) +
  theme_minimal() +
  theme(legend.title = ggtext::element_markdown(lineheight = 1.2))
ggsave("mds_mixed_pol_incomes.png", width = 9, height = 8)
