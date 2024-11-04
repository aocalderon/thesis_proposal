library(tidyverse)
library(ggpubr)

dataCreationMainUS <- read_tsv("mainus_creation_times.tsv")

p = ggplot(dataCreationMainUS, aes(x = as.factor(partitions), y = time, group = tree)) +
  geom_line(aes(linetype = tree, color = tree)) + 
  geom_point(aes(shape = tree, color = tree), size = 3) +
  labs(x="Number of requested partitions (x1000)", y="Construction Tree Time(s)", caption = "(a)") +
  scale_color_discrete("Tree") +
  scale_shape_discrete("Tree") +
  guides(linetype = "none") +
  theme_bw() +
  theme(legend.position="top", plot.caption = element_text(hjust = 0.5, size = 12))

dataCreationGADM <- read_tsv("gadm_creation_times.tsv")

q = ggplot(dataCreationGADM, aes(x = as.factor(partitions), y = time, group = tree)) +
  geom_line(aes(linetype = tree, color = tree)) + 
  geom_point(aes(shape = tree, color = tree), size = 3) +
  labs(x="Number of requested partitions (x1000)", y="Creation Tree Time(s)", caption = "(b)") +
  scale_color_discrete("Tree") +
  scale_shape_discrete("Tree") +
  guides(linetype = "none") +
  theme_bw() +
  theme(legend.position="top", plot.caption = element_text(hjust = 0.5, size = 12))

W = 8
H = 4
ggarrange(p, q, ncol=2, nrow=1, common.legend = TRUE, legend="top")
ggsave(paste0("K_Creation.pdf"), width = W, height = H)

################################################################################

dataPartitioningMainUS <- read_tsv("mainus_partitioning_times.tsv")

p = ggplot(dataPartitioningMainUS, aes(x = as.factor(partitions), y = time, group = tree)) +
  geom_line(aes(linetype = tree, color = tree)) + 
  geom_point(aes(shape = tree, color = tree), size = 3) +
  labs(x="Number of requested partitions (x1000)", y="Partitioning Time(s)", caption = "(a)") +
  scale_color_discrete("Tree") +
  scale_shape_discrete("Tree") +
  guides(linetype = "none") +
  theme_bw() +
  theme(legend.position="top", plot.caption = element_text(hjust = 0.5, size = 12))


dataPartitioningGADM <- read_tsv("gadm_partitioning_times.tsv")

q = ggplot(dataPartitioningGADM, aes(x = as.factor(partitions), y = time, group = tree)) +
  geom_line(aes(linetype = tree, color = tree)) + 
  geom_point(aes(shape = tree, color = tree), size = 3) +
  labs(x="Number of requested partitions (x1000)", y="Partitioning Time(s)", caption = "(b)") +
  scale_color_discrete("Tree") +
  scale_shape_discrete("Tree") +
  guides(linetype = "none") +
  theme_bw() +
  theme(legend.position="top", plot.caption = element_text(hjust = 0.5, size = 12))

ggarrange(p, q, ncol=2, nrow=1, common.legend = TRUE, legend="top")
ggsave(paste0("K_Partitioning.pdf"), width = W, height = H)

################################################################################

dataOverlayMainUS <- read_tsv("mainus_overlay_times.tsv")

p = ggplot(dataOverlayMainUS, aes(x = as.factor(partitions), y = time, group = tree)) +
  geom_line(aes(linetype = tree, color = tree)) + 
  geom_point(aes(shape = tree, color = tree), size = 3) +
  labs(x="Number of requested partitions (x1000)", y="Overlay Time(s)", caption = "(a)") +
  scale_color_discrete("Tree") +
  scale_shape_discrete("Tree") +
  guides(linetype = "none") +
  theme_bw() +
  theme(legend.position="top", plot.caption = element_text(hjust = 0.5, size = 12))

dataOverlayGADM <- read_tsv("gadm_overlay.tsv")

q = ggplot(dataOverlayGADM, aes(x = as.factor(partitions), y = time, group = tree)) +
  geom_line(aes(linetype = tree, color = tree)) + 
  geom_point(aes(shape = tree, color = tree), size = 3) +
  labs(x="Number of requested partitions (x1000)", y="Overlay Time(s)", caption = "(b)") +
  scale_color_discrete("Tree") +
  scale_shape_discrete("Tree") +
  guides(linetype = "none") +
  theme_bw() +
  theme(legend.position="top", plot.caption = element_text(hjust = 0.5, size = 12))

ggarrange(p, q, ncol=2, nrow=1, common.legend = TRUE, legend="top")
ggsave(paste0("K_Overlay.pdf"), width = W, height = H)

################################################################################

dataSpaceMainUS <- read_tsv("mainus_overlay_space.tsv")

p = ggplot(dataSpaceMainUS, aes(x = as.factor(partitions), y = nodes, group = tree)) +
  geom_line(aes(linetype = tree, color = tree)) + 
  geom_point(aes(shape = tree, color = tree), size = 3) +
  labs(x="Number of requested partitions (x1000)", y="Space (number of nodes)", caption = "(a)") +
  scale_color_discrete("Tree") +
  scale_shape_discrete("Tree") +
  guides(linetype = "none") +
  theme_bw() +
  theme(legend.position="top", plot.caption = element_text(hjust = 0.5, size = 12))

dataSpaceGADM <- read_tsv("gadm_overlay_space.tsv")

q = ggplot(dataSpaceGADM, aes(x = as.factor(partitions), y = nodes, group = tree)) +
  geom_line(aes(linetype = tree, color = tree)) + 
  geom_point(aes(shape = tree, color = tree), size = 3) +
  labs(x="Number of requested partitions (x1000)", y="Space (number of nodes)", caption = "(b)") +
  scale_color_discrete("Tree") +
  scale_shape_discrete("Tree") +
  guides(linetype = "none") +
  theme_bw() +
  theme(legend.position="top", plot.caption = element_text(hjust = 0.5, size = 12))

ggarrange(p, q, ncol=2, nrow=1, common.legend = TRUE, legend="top")
ggsave(paste0("K_Space.pdf"), width = W, height = H)
