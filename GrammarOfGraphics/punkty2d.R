library(SmarterPoland)
library(ggplot2)

pl1 <- ggplot(maturaExam[maturaExam$rok == 2015,], 
       aes(podstawowy.matematyka, podstawowy.j.polski)) + 
  geom_point() + theme_bw()+
  xlab("Punkty, matura z j. polskiego, poziom podstawowy") +
  ylab("Punkty, matura z matematyki, poziom podstawowy") 


pl2 <- ggplot(maturaExam[maturaExam$rok == 2015,], 
              aes(podstawowy.matematyka, podstawowy.j.polski)) + 
  geom_jitter() + theme_bw()+
  xlab("Punkty, matura z j. polskiego, poziom podstawowy") +
  ylab("Punkty, matura z matematyki, poziom podstawowy") 


pl3 <- ggplot(maturaExam[maturaExam$rok == 2015,], 
              aes(podstawowy.matematyka, podstawowy.j.polski)) + 
  geom_smooth(se=FALSE) + theme_bw() + ylim(0,74)+
  xlab("Punkty, matura z j. polskiego, poziom podstawowy") +
  ylab("Punkty, matura z matematyki, poziom podstawowy") 


pl4 <- ggplot(maturaExam[maturaExam$rok == 2015,], 
              aes(factor(podstawowy.matematyka), podstawowy.j.polski)) + 
  geom_boxplot(coef=5) + theme_bw() + ylim(0,74) +
  xlab("Punkty, matura z j. polskiego, poziom podstawowy") +
  ylab("Punkty, matura z matematyki, poziom podstawowy") 


ggsave(pl1, filename = "geomMatura1.png", width = 7, height = 7)
ggsave(pl2, filename = "geomMatura2.png", width = 7, height = 7)
ggsave(pl4, filename = "geomMatura4.pdf", width = 7, height = 7, useDingbats=FALSE)

