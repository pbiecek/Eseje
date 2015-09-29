# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)
library(SmarterPoland)

head(countries)


# boxplot
plB <- ggplot(countries, aes(x = continent, y = birth.rate)) +
  geom_boxplot(fill="grey", coef = 3) +
  theme_bw()

ggsave(plB, filename = "geomBoxplot.pdf", width = 7, height = 7, useDingbats=FALSE)


# crossbar
plC <- ggplot(countries, aes(x = continent, y = birth.rate)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", width = 0.3)  +
  theme_bw()

ggsave(plC, filename = "geomCrossbar.pdf", width = 7, height = 7, useDingbats=FALSE)

# strzalki
library(grid)
cq <- countries %>%
  group_by(continent) %>%
  summarise(q1 = quantile(birth.rate, 0.25, na.rm=TRUE),
            q2 = quantile(birth.rate, 0.5, na.rm=TRUE),
            q3 = quantile(birth.rate, 0.75, na.rm=TRUE))
cq13 <- cq %>%
  gather(key, value, -continent)

pq1 <- ggplot(cq13, aes(x=continent, y=value, group=continent)) +
  geom_path(arrow=arrow(ends = "both")) + 
  geom_point(data=cq13[cq13$key == "q2",], aes(x=continent, y=value), size=4) +
  theme_bw() + xlab("") + ylab("Kwartyle i mediana dla wsp. urodzin")

pq2 <- ggplot(cq, aes(x=continent, y=q2)) +
  geom_boxplot(aes(ymin=q1, lower=q1, middle=q2, upper=q3, ymax=q3), stat="identity") +
  theme_bw() + xlab("") + ylab("Kwartyle i mediana dla wsp. urodzin")

pq3 <- ggplot(cq, aes(x=continent, y=q2)) +
  geom_errorbar(aes(ymin=q1, ymax=q3), stat="identity", width=0.3) +
  geom_text(label="*", size=15) + 
  theme_bw() + xlab("") + ylab("Kwartyle i mediana dla wsp. urodzin")

ggsave(pq1, filename = "statQ1.pdf", width = 7, height = 7, useDingbats=FALSE)
ggsave(pq2, filename = "statQ2.pdf", width = 7, height = 7, useDingbats=FALSE)
ggsave(pq3, filename = "statQ3.pdf", width = 7, height = 7, useDingbats=FALSE)











# stat_binhex
plBH <- ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  stat_binhex(bins = 9) + scale_fill_gradient(low = "white", high = "black") + 
  theme_bw()

ggsave(plBH, filename = "geomBinHex.pdf", width = 7, height = 7, useDingbats=FALSE)


# geomRug
plR <- ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  geom_rug() + 
  theme_bw()

ggsave(plR, filename = "geomRug.pdf", width = 7, height = 7, useDingbats=FALSE)


