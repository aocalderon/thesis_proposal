library(tidyverse)
library(ggpubr)

data0 = enframe(read_lines("Unbalance_Tester01.txt"), value="line")

data1 = data0 %>% filter(str_detect(line, 'INFO')) %>% filter(str_detect(line, 'END')) 

fields = c("tag","ts","runId","size1","size2","method","stage","time")
data2 = data1 %>% 
  separate(sep = "\\t", col = "line", into = fields, extra = "drop") %>%
  select(method, size2, time) %>%
  mutate(time = as.numeric(time) / 1000.0) %>%
  mutate(method = str_replace(method, "Sweeping", "Filter by sweep"))

data3 = data2 %>% group_by(method, size2) %>% summarise(time = mean(time)) %>%
  mutate(method = as.factor(method), size = as.factor(size2)) %>%
  mutate(size = recode(size,
                       "6K"  = "2x",
                       "9K"  = "3x",
                       "12K" = "4x",
                       "15K" = "5x",
                       "18K" = "6x",
                       "21K" = "7x")) 
size_order = factor(data3$size, level = c('2x','3x','4x','5x','6x','7x'))

p = ggplot(data3, aes(x = size_order, y = time, fill = method)) + 
  geom_col(width = 0.7, position="dodge") + 
  labs(x="Dataset Size", y="Time [s]", caption = "(a)") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, size = 12)
        ) +
  guides(fill=guide_legend(title="Method"))
plot(p)

W = 6
H = 4
ggsave(paste0("Unbalance_Tester01.pdf"), width = W, height = H)

data0 = enframe(read_lines("Unbalance_Tester02.txt"), value="line")

data1 = data0 %>% filter(str_detect(line, 'INFO')) %>% filter(str_detect(line, 'END')) 

fields = c("tag","ts","runId","perc","file","method","stage","time","p","n1","n2")
data2 = data1 %>% 
  separate(sep = "\\t", col = "line", into = fields, extra = "drop") %>%
  select(method, perc, time) %>%
  mutate(time = as.numeric(time) / 1000.0, perc = as.factor(as.numeric(perc) / 100.0)) %>%
  mutate(method = str_replace(method, "Sweeping", "Filter by sweep"))

data3 = data2 %>% group_by(method, perc) %>% summarise(time = mean(time)) 

q = ggplot(data3, aes(x = perc, y = time, fill = method)) + 
  geom_col(width = 0.7, position="dodge") + 
  labs(x="Percentage difference between datasets", y="Time [s]", caption = "(b)") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, size = 12)
  ) +
  guides(fill=guide_legend(title="Method"))
plot(q)

W = as.numeric(Sys.getenv("R_WIDTH"))
H = as.numeric(Sys.getenv("R_HEIGHT"))
ggsave(paste0("Unbalance_Tester02.pdf"), width = W, height = H)

ggarrange(p, q, ncol=2, nrow=1, common.legend = TRUE, legend="top")
ggsave("Unbalance_Tester.pdf", width = 8, height = 4)
