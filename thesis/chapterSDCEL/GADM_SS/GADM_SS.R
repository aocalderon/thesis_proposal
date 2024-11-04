library(tidyverse)
library(ggpubr)

data0 = enframe(read_lines("GADM_speedup_v01.txt"), value="line")

data1 = data0 %>%
  filter(str_detect(line, 'TIME')) 

fields1 = c("ts","start","appId","time","tag","stage","data")
fields2 = c("dataset", "tolerance", "overlay_method", "overlay_level", "partitions", "nodes", "run")
data2 = data1 %>% 
  separate(sep = "\\|", col = "line", into = fields1, extra = "drop") %>%
  separate(sep = "_"  , col = "data", into = fields2, extra = "drop") %>%
  filter(stage == "layer1" | stage == "layer2" | stage == "overlay") %>%
  select(time, stage, nodes) %>%
  mutate(time = as.numeric(time) / 1000.0, nodes = as.factor(nodes))

nodes_labels = c("3", "6", "9", "12")
data3 = data2 %>%
  group_by(nodes, stage) %>% summarise(time = mean(time))  %>%
  mutate(nodes = fct_relevel(nodes, nodes_labels))

write_tsv(data3, "GADM_speedup.tsv")

data4 = read_tsv("GADM_speedup_v01.tsv")
stage.labs <- c("Layer A", "Layer B", "Overlay")
names(stage.labs) <- c("layer1", "layer2","overlay")

data5 = data4 |> filter(nodes!="9")

p = ggplot(data5, aes(x = as.factor(nodes), y = time, fill="blue")) + 
  geom_col(width = 0.7, position="dodge") + 
  labs(x="Number of nodes", y="Time [s]", caption = "(a)") +
  guides(fill = F) +
  theme_bw()  + 
  theme(legend.position="top", 
        plot.caption = element_text(hjust = 0.5, size = 12)) +
  facet_wrap(~ stage, labeller = labeller(stage = stage.labs))
plot(p)

W = 5
H = 4
ggsave(paste0("GADM_speedup.pdf"), width = W, height = H)
data0 = enframe(read_lines("GADM_scaleup_v01.txt"), value="line")

data1 = data0 %>%
  filter(str_detect(line, 'TIME')) 

fields1 = c("ts","start","appId","time","tag","stage","data")
fields2 = c("dataset", "tolerance", "overlay_method", "overlay_level", "partitions", "nodes", "run")
fields3 = c(NA, NA, "size")
data2 = data1 %>% 
  separate(sep = "\\|", col = "line", into = fields1, extra = "drop") %>%
  separate(sep = "_"  , col = "data", into = fields2, extra = "drop") %>%
  separate(sep = "/"  , col = "dataset", into = fields3, extra = "drop") %>%
  filter(stage == "layer1" | stage == "layer2" | stage == "overlay") %>%
  select(time, stage, size) %>%
  mutate(time = as.numeric(time) / 1000.0, size = as.factor(size)) %>%
  mutate(size = recode_factor(size, S0 = "8M", S1 = "16M", S2 = "24M", S3 = "32M"))

size_labels = c("8M", "16M", "24M", "32M")
data3 = data2 %>%
  group_by(size, stage) %>% summarise(time = mean(time))  %>%
  mutate(size = fct_relevel(size, size_labels))

write_tsv(data3, "GADM_scaleup.tsv")

data4 = data3 |> filter(size!="24M")

stage.labs <- c("Layer A", "Layer B", "Overlay")
names(stage.labs) <- c("layer1", "layer2","overlay")

q = ggplot(data4, aes(x = size, y = time, fill="blue")) + 
  geom_col(width = 0.7, position="dodge") + 
  labs(x="Size [number of edges]", y="Time [s]", caption = "(b)") +
  theme_bw()  + 
  theme(legend.position="top", 
        plot.caption = element_text(hjust = 0.5, size = 12)) +
  guides(fill = F) +
  facet_wrap(~ stage, labeller = labeller(stage = stage.labs))
plot(q)

ggsave(paste0("GADM_scaleup.pdf"), width = W, height = H)

ggarrange(p, q, ncol=2, nrow=1, common.legend = TRUE, legend="top")
ggsave("GADM_SS.pdf", width = 8, height = 4)
