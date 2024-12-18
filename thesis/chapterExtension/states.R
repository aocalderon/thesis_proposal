library(tidyverse)

states = c("NC", "GA", "TN", "VA", "TX", "CA")
values = c(111, 150, 212, 401, 472, 538)

data = data.frame(states=states, time=values)

p = ggplot(data, aes(x = states, y = time, fill="blue")) + 
  geom_col(width = 0.7, position="dodge") + 
  labs(x="State", y="Time [s]") +
  guides(fill=F)
plot(p)

W = 5
H = 4
ggsave(paste0("states.pdf"), width = W, height = H)
