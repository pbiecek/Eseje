library(ggplot2)

ggplot(iris, aes(x=Species, y=Sepal.Length)) +
  geom_point()

ggplot(iris, aes(x=Species, y=Sepal.Length)) +
  geom_point(position = position_dodge())

ggplot(iris, aes(x=Species, y=Sepal.Length, ymax=Sepal.Length+0.2)) +
  geom_point(position = position_stack(0.1, 0))

ggplot(iris, aes(x=Species, y=Sepal.Length)) +
  geom_point(position = position_fill(0.1, 0.1))

ggplot(iris, aes(x=Species, y=Sepal.Length)) +
  geom_point(position = position_jitter(0.1, 0))

ggplot(iris, aes(x=Species, fill=Species, y=Sepal.Length)) +
  geom_point(position = position_jitterdodge(0.3, 0))


#
# factor

# jednak nie jest zaimplementowane

ggplot(iris, aes(x=Species, y=factor(Sepal.Length), group=factor(Sepal.Length))) +
  geom_point(position = position_dodge(width = 0.2, 0))
