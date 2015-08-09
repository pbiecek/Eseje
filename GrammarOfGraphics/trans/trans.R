#
# Bez transformacji
#
pl0 <- ggplot(iris, aes(Sepal.Length, Petal.Length)) + 
  geom_point() + 
  geom_smooth(se=FALSE, method="lm") +
  theme_bw()

#
# Transformacje układu współrzędnych
# po wyznaczeniu stats
#
pl1 <- ggplot(iris, aes(Sepal.Length, Petal.Length)) + 
  geom_point() + 
  coord_trans(ytrans="log", xtrans="log") +
  geom_smooth(se=FALSE, method="lm") +
  theme_bw()

#
# Transformacje skal robione przed stats
#
pl2 <- ggplot(iris, aes(Sepal.Length, Petal.Length)) + 
  geom_point() + 
  scale_y_log10(breaks=1:10) + 
  scale_x_log10(breaks=1:10) +
  geom_smooth(se=FALSE, method="lm") +
  theme_bw()

#
# Transforamcja zmiennych robiona przed stats
#
pl3 <- ggplot(iris, aes(log10(Sepal.Length), log10(Petal.Length))) + 
  geom_point() + 
  geom_smooth(se=FALSE, method="lm") +
  theme_bw()


pl0
pl1
pl2
pl3


ggsave(pl0, filename = "trans_0.png")
ggsave(pl1, filename = "trans_coords.png")
ggsave(pl2, filename = "trans_scales.png")
ggsave(pl3, filename = "trans_vars.png")
