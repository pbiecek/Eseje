library(SmarterPoland)
library(ggplot2)

pl1 <- ggplot(maturaExam[maturaExam$rok == 2015,], 
       aes(podstawowy.matematyka, podstawowy.j.polski)) + 
  geom_point() + theme_bw()


pl2 <- ggplot(maturaExam[maturaExam$rok == 2015,], 
              aes(podstawowy.matematyka, podstawowy.j.polski)) + 
  geom_jitter() + theme_bw()


pl3 <- ggplot(maturaExam[maturaExam$rok == 2015,], 
              aes(podstawowy.matematyka, podstawowy.j.polski)) + 
  geom_smooth(se=FALSE) + theme_bw() + ylim(0,74)


pl4 <- ggplot(maturaExam[maturaExam$rok == 2015,], 
              aes(factor(podstawowy.matematyka), podstawowy.j.polski)) + 
  geom_boxplot(coef=5) + theme_bw() + ylim(0,74)




