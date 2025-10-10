##############################
##############################

### Accuracy plotting
### Nik Verweel
### 01/09/2025

##############################
##############################

# Import functions
source("AHNProcessingFunctions.R")

# Check required packages
CheckPackages(c('sf', 'dplyr', 'terra', 'lidR', 'rgl', 'stringr', 'ggplot2', 'readxl',
'ggspatial', 'patchwork', 'viridis', 'ForestTools', 'tidyr', 'ggnewscale', 'RANN', 'data.table'))

# Load in the metrics
plotMetrics <- read.csv('../Data/Accuracy/AccuracyMetricsPlot.csv')
treeMetrics <- read.csv('../Data/Accuracy/AccuracyMetricsTree.csv')

# Reshape
plotMetricsLong <- plotMetrics %>%
    pivot_longer(cols = c(n_trees_field, n_trees_itd),
                 names_to = "source",
                 values_to = "n_trees")

# Set order of forests
plotMetricsLong$forestType <- factor(plotMetricsLong$forestType,
                                     levels = c("Coniferous", "Deciduous", "Mixed"),
                                     labels = c("Coniferous", "Deciduous", "Mixed"))

### Plotting - Tree count ###
combined_plot <- ggplot(plotMetricsLong, aes(x = forestType, y = n_trees, fill = source)) +
  geom_violin(position = position_dodge(width = 0.8), trim = TRUE, alpha = 0.9, width = 1) +
  stat_boxplot(geom = "errorbar", 
               position = position_dodge(width = 0.8), 
               width = 0.1) +
  geom_boxplot(position = position_dodge(width = 0.8),
               outlier.shape = 21,
               outlier.size = 2,
               outlier.stroke = 0.3,
               width = 0.1) +
  scale_fill_manual(values = c("n_trees_field" = "#31688e",
                               "n_trees_itd"   = "#6ece58"),
                    labels = c("Field", "ITD")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) +
  labs(x = "Forest type", 
       y = "Number of trees",
       fill = "Source:",
       title = "Distribution of field-observed and ITD-derived tree counts per forest type") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.4),
    panel.grid.minor = element_line(color = "grey90", size = 0.2),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave('../Figures/Results/SRO3/n_tree_comparison.png', combined_plot, width = 8, height = 8, dpi = 150)

# Split data
plotConMetrics <- plotMetrics[plotMetrics$forestType == 'Coniferous',]
plotDecMetrics <- plotMetrics[plotMetrics$forestType == 'Deciduous',]
plotMixMetrics <- plotMetrics[plotMetrics$forestType == 'Mixed',]

# Determine the percentage of times n_trees_itd is within a 5% margin.
sum(abs(plotConMetrics$n_trees_itd - plotConMetrics$n_trees_field) <= 0.05 * plotConMetrics$n_trees_field) / count(plotConMetrics) * 100
sum(abs(plotDecMetrics$n_trees_itd - plotDecMetrics$n_trees_field) <= 0.05 * plotDecMetrics$n_trees_field) / count(plotDecMetrics) * 100
sum(abs(plotMixMetrics$n_trees_itd - plotMixMetrics$n_trees_field) <= 0.05 * plotMixMetrics$n_trees_field) / count(plotMixMetrics) * 100

# Test statistical significance
wilcox.test(plotConMetrics$n_trees_itd, plotConMetrics$n_trees_field, paired = TRUE)
wilcox.test(plotDecMetrics$n_trees_itd, plotDecMetrics$n_trees_field, paired = TRUE)
wilcox.test(plotMixMetrics$n_trees_itd, plotMixMetrics$n_trees_field, paired = TRUE)

# RMSEs
rmse <- sqrt(mean((plotConMetrics$n_trees_itd - plotConMetrics$n_trees_field)^2))
nrmse <- (rmse / mean(plotConMetrics$n_trees_field)) * 100
rmse <- sqrt(mean((plotDecMetrics$n_trees_itd - plotDecMetrics$n_trees_field)^2))
nrmse <- (rmse / mean(plotDecMetrics$n_trees_field)) * 100
rmse <- sqrt(mean((plotMixMetrics$n_trees_itd - plotMixMetrics$n_trees_field)^2))
nrmse <- (rmse / mean(plotMixMetrics$n_trees_field)) * 100

### Distances
combined <- ggplot(treeMetrics, aes(x = forestType, y = dist_to_nearest_itd)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.9, trim = TRUE,
              fill = "#31688e") +
  stat_boxplot(geom = "errorbar", 
               position = position_dodge(width = 0.8), 
               width = 0.2) +
  geom_boxplot(position = position_dodge(width = 0.8),
               outlier.shape = 21, outlier.size = 2, outlier.stroke = 0.3,
               fill = "#31688e",
               width = 0.2) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) +
  labs(x = "Forest type", y = "Distance to nearest ITD tree (m)",
       title = "Distribution of distances between field observed tree\nand ITD tree per forest type") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.4),
    panel.grid.minor = element_line(color = "grey90", size = 0.2),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave('../Figures/Results/SRO3/dist_comparison.png', combined, width = 8, height = 8, dpi = 150)

# Check means
treeConMetrics <- treeMetrics[treeMetrics$forestType == 'Coniferous',]
treeDecMetrics <- treeMetrics[treeMetrics$forestType == 'Deciduous',]
treeMixMetrics <- treeMetrics[treeMetrics$forestType == 'Mixed',]

mean(treeConMetrics$dist_to_nearest_itd)
mean(treeDecMetrics$dist_to_nearest_itd)
mean(treeMixMetrics$dist_to_nearest_itd)

# Height difference considerations
count(treeMetrics[treeMetrics$dist_to_nearest_itd < 1,]) / count(treeMetrics) * 100

count(treeConMetrics[treeConMetrics$dist_to_nearest_itd < 1,]) / count(treeConMetrics) * 100
count(treeDecMetrics[treeDecMetrics$dist_to_nearest_itd < 1,]) / count(treeDecMetrics) * 100
count(treeMixMetrics[treeMixMetrics$dist_to_nearest_itd < 1,]) / count(treeMixMetrics) * 100

# Find the 'correct' trees

nearbyTrees <- treeMetrics[treeMetrics$dist_to_nearest_itd < 1,]
nearbyTrees$height_diff_with_nearest[is.na(nearbyTrees$height_diff_with_nearest)] <- 0
correctTrees <- subset(nearbyTrees, height_diff_with_nearest >= -3 & height_diff_with_nearest <= 3)

# Determine accuracy
count(correctTrees) / count(treeMetrics) * 100
count(correctTrees[correctTrees$forestType == 'Coniferous',]) / count(treeMetrics[treeMetrics$forestType == 'Coniferous',]) * 100
count(correctTrees[correctTrees$forestType == 'Deciduous',]) / count(treeMetrics[treeMetrics$forestType == 'Deciduous',]) * 100
count(correctTrees[correctTrees$forestType == 'Mixed',]) / count(treeMetrics[treeMetrics$forestType == 'Mixed',]) * 100

### Height distribution plot ###
plotMetricsLong <- plotMetrics %>%
    pivot_longer(cols = c(avg_height_field, avg_height_itd),
                 names_to = "source",
                 values_to = "avg_height") %>%
    mutate(n_trees = case_when(source == "avg_height_field" ~ n_trees_field,
                               source == "avg_height_itd" ~ n_trees_itd,
                               TRUE ~ NA_integer_))

# Set order of forests
plotMetricsLong$forestType <- factor(plotMetricsLong$forestType,
                                     levels = c("Coniferous", "Deciduous", "Mixed"),
                                     labels = c("Coniferous", "Deciduous", "Mixed"))

# Get tree counts
tree_counts_by_source <- plotMetricsLong %>%
  group_by(forestType, source) %>%
  summarise(total_n_trees = sum(n_trees, na.rm = TRUE), y_position = max(avg_height, na.rm = TRUE) * 1.01) %>%
  ungroup()

# Plotting
p <- ggplot(plotMetricsLong, aes(x = forestType, y = avg_height, fill = source)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.9, trim = TRUE) +
  geom_text(data = tree_counts_by_source,
            aes(x = forestType, y = y_position,
                label = paste0("n = ", total_n_trees),
                group = source),
            position = position_dodge(width = 0.8),
            inherit.aes = FALSE,
            vjust = 0,
            size = 4) +
  scale_fill_manual(values = c("avg_height_field" = "#31688e",
                               "avg_height_itd"   = "#6ece58"),
                    labels = c("Field", "ITD")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) +
  labs(x = "Forest type", y = "Average tree height (m)",
       fill = "Source:",
       title = "Distribution of field vs ITD average tree height per plot by forest type",
       subtitle = paste0("n = total number of trees")) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.4),
    panel.grid.minor = element_line(color = "grey90", size = 0.2),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave('../Figures/Results/SRO3/avg_height_distribution.png', p, width = 8, height = 8, dpi = 150)

# Checking statistical signifance of height differences
wilcox.test(plotConMetrics$avg_height_itd, plotConMetrics$avg_height_field, paired = TRUE)
wilcox.test(plotDecMetrics$avg_height_itd, plotDecMetrics$avg_height_field, paired = TRUE)
wilcox.test(plotMixMetrics$avg_height_itd, plotMixMetrics$avg_height_field, paired = TRUE)
