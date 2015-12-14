library(openxlsx)
library(SmarterPoland)
library(ggthemes)

plD <- ggplot(na.omit(countries), aes(x = birth.rate, y = death.rate)) +
  geom_point(size=2, color="black") +
  theme_bw()

pl <- list()

pl[[1]] <- plD + ggtitle("default")
pl[[2]] <- plD + scale_x_tufte() + scale_y_tufte() + ggtitle("_tufte")
pl[[3]] <- plD + scale_x_sqrt() + scale_y_sqrt() + ggtitle("_sqrt")
pl[[4]] <- plD + scale_x_reverse() + scale_y_reverse() + ggtitle("_reverse")
pl[[5]] <- plD + scale_x_log10() + scale_y_log10() + ggtitle("_log10")
pl[[6]] <- plD + scale_x_continuous(trans="log2") + scale_y_continuous(trans="log2") + ggtitle("_log2")



library(gridExtra)

pdf("scales6.pdf", width = 8, height = 8)
do.call(grid.arrange, pl)
dev.off()









plD <- ggplot(na.omit(countries), aes(x = birth.rate, y = death.rate, shape=continent, color=continent)) +
  geom_point(size=5) +
  theme_bw()

pl <- list()

pl[[1]] <- plD + ggtitle("default")
pl[[2]] <- plD + scale_shape_stata() + ggtitle("_stata")
pl[[3]] <- plD + scale_shape_calc() + ggtitle("_calc")
pl[[4]] <- plD + scale_shape_tableau() + ggtitle("_tableau")
pl[[5]] <- plD + scale_shape_cleveland() + ggtitle("_cleveland")
pl[[6]] <- plD + scale_shape_manual(values=LETTERS) + ggtitle("_manual")

pdf("scales6b.pdf", width = 8, height = 8)
do.call(grid.arrange, pl)
dev.off()







