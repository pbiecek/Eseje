#
# Transformacje układu współrzędnych
# po wyznaczeniu stats
#
ggplot(iris, aes(Sepal.Length, Petal.Length)) + 
  geom_point() + 
  coord_trans(ytrans="log", xtrans="log") +
  geom_smooth(se=FALSE, method="lm")

#
# Transformacje skal robione przed stats
#
ggplot(iris, aes(Sepal.Length, Petal.Length)) + 
  geom_point() + 
  scale_y_log10(breaks=1:10) + 
  scale_x_log10(breaks=1:10) +
  geom_smooth(se=FALSE, method="lm")

#
# Transforamcja zmiennych robiana przed stats
#
ggplot(iris, aes(log10(Sepal.Length), log10(Petal.Length))) + 
  geom_point() + 
  geom_smooth(se=FALSE, method="lm")
