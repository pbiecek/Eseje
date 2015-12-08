library(openxlsx)
library(SmarterPoland)
library(ggthemes)

plD <- ggplot(na.omit(countries), aes(x = birth.rate, y = death.rate)) +
  geom_point(size=2, color="black") +
  scale_x_sqrt() 

pl <- list()

pl[[1]] <- plD +   theme_bw() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_bw")

pl[[2]] <- plD +   theme_calc() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_calc")

pl[[3]] <- plD +   theme_classic() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_classic")

pl[[4]] <- plD +   theme_economist() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_economist")

pl[[5]] <- plD +   theme_economist_white() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_economist_white")

pl[[6]] <- plD +   theme_excel() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_excel")

pl[[7]] <- plD +   theme_few() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_few")

pl[[8]] <- plD +   theme_fivethirtyeight() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_fivethirtyeight")

pl[[9]] <- plD +   theme_foundation() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_foundation")

pl[[10]] <- plD +   theme_gray() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_gray")

pl[[11]] <- plD +   theme_map() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_map")

pl[[12]] <- plD +   theme_pander() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_pander")

pl[[13]] <- plD +   theme_solarized() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_solarized")

pl[[14]] <- plD +   theme_stata() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_stata")

pl[[15]] <- plD +   theme_wsj() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_wsj")

pl[[16]] <- plD +   theme_tufte() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_tufte")

pl[[17]] <- plD +   theme_solarized_2() +  theme(legend.position="none") +
  theme(text=element_text(size=20)) + ggtitle("theme_solarized_2")

library(gridExtra)

pdf("themes9.pdf", width = 16, height = 12)
do.call(grid.arrange, pl[c(1,10,11,  8,17,14,  4,5, 6)])
dev.off()

