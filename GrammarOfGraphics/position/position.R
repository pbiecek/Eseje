library(ggplot2)

ggplot(iris, aes(x=Species, y=Sepal.Length)) +
  geom_point()

ggplot(iris, aes(x=Species, y=Sepal.Length)) +
  geom_point(position = position_dodge())

ggplot(iris, aes(x=Species, y=Sepal.Length)) +
  geom_point(position = position_stack(0.1, 0))

ggplot(iris, aes(x=Species, y=Sepal.Length)) +
  geom_point(position = position_fill())

ggplot(iris, aes(x=Species, y=Sepal.Length)) +
  geom_point(position = position_jitter(0.1, 0))

ggplot(iris, aes(x=Species, fill=Species, y=Sepal.Length)) +
  geom_point(position = position_jitterdodge(0.3, 0))
