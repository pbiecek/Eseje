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

maturaExam2 <- maturaExam[maturaExam$rok == 2015,] %>%
  group_by(podstawowy.matematyka) %>%
  summarise(podstawowy.j.polski=median(podstawowy.j.polski))

pl5 <- ggplot(maturaExam[maturaExam$rok == 2015,], 
       aes(factor(podstawowy.matematyka), podstawowy.j.polski)) + 
  geom_boxplot(fill="grey80",color="grey80", coef=5) + 
  geom_jitter(size=1, alpha=0.05) + theme_bw() + ylim(0,74) +
  geom_line(data=maturaExam2, aes(factor(podstawowy.matematyka), podstawowy.j.polski, group=1), color="red3", size=3) +
  xlab("Punkty, matura z j. polskiego, poziom podstawowy") +
  ylab("Punkty, matura z matematyki, poziom podstawowy") +
  scale_x_discrete(labels=c("0", rep("",9), "10", rep("",9), "20", rep("",9), "30", rep("",9), "40", rep("",9), "50"))


ggsave(pl1, filename = "geomMatura1.png", width = 7, height = 7)
ggsave(pl2, filename = "geomMatura2.png", width = 7, height = 7)
ggsave(pl4, filename = "geomMatura4.pdf", width = 7, height = 7, useDingbats=FALSE)
ggsave(pl5, filename = "geomMatura5.png", width = 7, height = 7)

