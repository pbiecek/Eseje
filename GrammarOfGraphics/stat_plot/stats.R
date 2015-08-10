library(dplyr)
library(tidyr)
library(ggplot2)

df <- iris %>%
  mutate(gr = cut(Sepal.Length, seq(4,8,0.5))) %>%
  group_by(gr) %>%
  summarise(min = min(Petal.Length),
            max = max(Petal.Length))

# range
pl1 <- ggplot(df, aes(x=gr, ymin=min, ymax=max)) + 
  geom_linerange() + theme_bw()

df2 <- df %>%
  gather(min_max, value, min, max)
  
# points
pl2 <- ggplot(df2, aes(x=gr, y=value)) + 
  geom_point() + theme_bw()

# line
pl3 <- ggplot(df2, aes(x=gr, y=min_max)) + 
  geom_line() + theme_bw()





ggsave("stats_range.png", pl1, width = 7, height = 7)
ggsave("stats_point.png", pl2, width = 7, height = 7)

