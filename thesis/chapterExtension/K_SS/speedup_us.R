library(tidyverse)

data1 <- enframe(read_lines("speedup_us.txt"), value = "line") |>
  filter(str_detect(line, 'TIME')) |>
  separate(col = line, into = c("ts", "epoch", "appId", "time", "tag", "stage", "size"), sep = "\\|") |>
  mutate(time = as.numeric(time) / 1000.0) |>
  select(size, stage, time) |>
  filter(stage == "layer1" | stage == "layer2" | stage == "overlay") |>
  mutate(size = recode_factor(size, S1 = "3", S2 = "6", S4 = "12"))

data2 <- data1 |> group_by(size, stage) |> summarise(time = mean(time))

stage.labs <- c("Layer A", "Layer B", "Overlay")
names(stage.labs) <- c("layer1", "layer2","overlay")
p = ggplot(data2, aes(x = size, y = time)) + 
  geom_col(width = 0.7, position="dodge") + 
  labs(x="Nodes", y="Time [s]") +
  facet_wrap(~ stage, labeller = labeller(stage = stage.labs))
plot(p)

W = 5
H = 4
ggsave(paste0("US_speedup.pdf"), width = W, height = H)
