library(openxlsx)
library(SmarterPoland)
library(maps)

plD <- ggplot(na.omit(countries), aes(x = birth.rate, y = death.rate)) +
  stat_ellipse(color="red4")+
  geom_point(data=countries[,-5],size=1,color="grey") +
  geom_point(size=2, color="red") +
  theme_bw() + theme(legend.position="none") +
  theme(text=element_text(size=20)) +
  scale_x_sqrt() + scale_y_sqrt()


pdf("facet.pdf", width = 12, height = 7)
plD + facet_wrap(~continent)
dev.off()


pdf("facet2.pdf", width = 5, height = 4)
ggplot(na.omit(countries), aes(x = birth.rate, y = death.rate, color=continent)) +
  stat_ellipse()+
  geom_point(size=2) +
  theme_bw() + theme(legend.position="none") +
  theme(text=element_text(size=20)) +
  scale_x_sqrt() + scale_y_sqrt()
dev.off()





require(proto)

StatEllipse <- proto(ggplot2:::Stat,
                     {
                       required_aes <- c("x", "y")
                       default_geom <- function(.) GeomPath
                       objname <- "ellipse"
                       
                       calculate_groups <- function(., data, scales, ...){
                         .super$calculate_groups(., data, scales,...)
                       }
                       calculate <- function(., data, scales, level = 0.75, segments = 51,...){
                         dfn <- 2
                         dfd <- length(data$x) - 1
                         if (dfd < 3){
                           ellipse <- rbind(c(NA,NA))	
                         } else {
                           require(MASS)
                           v <- cov.trob(cbind(data$x, data$y))
                           shape <- v$cov
                           center <- v$center
                           radius <- sqrt(dfn * qf(level, dfn, dfd))
                           angles <- (0:segments) * 2 * pi/segments
                           unit.circle <- cbind(cos(angles), sin(angles))
                           ellipse <- t(center + radius * t(unit.circle %*% chol(shape)))
                         }
                         
                         ellipse <- as.data.frame(ellipse)
                         colnames(ellipse) <- c("x","y")
                         return(ellipse)
                       }
                     }
)

stat_ellipse <- function(mapping=NULL, data=NULL, geom="path", position="identity", ...) {
  StatEllipse$new(mapping=mapping, data=data, geom=geom, position=position, ...)
}