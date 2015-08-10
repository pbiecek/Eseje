library(dplyr)
library(ggplot2)

df <- iris %>%
  mutate(gr = cut(Sepal.Length, seq(4,8,0.5))) %>%
  group_by(gr) %>%
  summarise(min = min(Petal.Length),
            max = max(Petal.Length))

# range
pl1 <- ggplot(df, aes(x=gr, ymin=min, ymax=max)) + 
  geom_linerange() + theme_bw()






ggsave("stats_range.png", pl1, width = 7, height = 7)

