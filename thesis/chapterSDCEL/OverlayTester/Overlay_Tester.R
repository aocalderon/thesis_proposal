library(tidyverse)

data0 = enframe(read_lines("Overlay_Tester_v01.txt"), value="line")
data1 = data0 %>% filter(str_detect(line, 'TIME')) 

fields1 = c("ts","start","appId","time","tag","stage","data")
fields2 = c("dataset", "tolerance", "overlay_option", "overlay_level", "partitions", "run")
fields3 = c(NA, NA, "tag")
data2 = data1 %>% 
  separate(sep = "\\|", col = "line",    into = fields1, extra = "drop") %>%
  separate(sep = "_",   col = "data",    into = fields2, extra = "drop") %>%
  separate(sep = "/",   col = "dataset", into = fields3, extra = "drop") %>%
  filter(stage == "overlay" | stage == "overlayMaster" | stage == "overlayByLevel") %>%
  select(time, stage, tag, overlay_option, overlay_level, appId) %>%
  mutate(time = as.numeric(time) / 1000.0) %>%
  filter(as.numeric(overlay_level) < 11)

tag_labels = c("CA","TX","NC","TN","GA","VA","PA")
method_labels = c("By Label", 
                  "Master", 
                  "Level [4]", 
                  "Level [5]", 
                  "Level [6]", 
                  "Level [7]", 
                  "Level [8]", 
                  "Level [9]", 
                  "Level [10]")
data3 = data2 %>%
  group_by(overlay_option, overlay_level, tag, stage) %>% summarise(time = mean(time)) %>%
  mutate(method = as.factor(if(overlay_option == 0){ "By Label" } 
                            else { 
                              if(overlay_option == 1){ "Master" } 
                              else { paste0("Level [", overlay_level, "]") } 
                            }
  )) %>%
  mutate(method = fct_relevel(method, method_labels)) %>%
  mutate(tag = as.factor(tag)) %>%
  mutate(tag = fct_relevel(tag, tag_labels)) %>%
  select(method, time, tag, overlay_option, overlay_level, stage)

write_tsv(data3, "Overlay_Tester.tsv") 

p = ggplot(data3, aes(x = method, y = time, fill="blue")) + 
  geom_col(width = 0.7, position="dodge") + 
  labs(x="Method of overlay", y="Time [s]") + 
  facet_wrap(~tag, ncol = 4) +
  guides(fill = F) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
plot(p)

W = 9
H = 5
ggsave(paste0("Overlay_Tester.pdf"), width = W, height = H)
