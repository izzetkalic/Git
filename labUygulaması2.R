# SPSS ten veri aktarma
library(foreign)
Spss <- read.spss("C:/Dosya yolu/dosya.sav",to.data.frame = TRUE)

y <- c(1,4,8,11,14,16,18,20,21,24,26)
x <- c(0,2,4,6,8,10,12,14,16,18,20)
cozum <- lm(y~x)
# Anova, Katsayilar, Belirleme Katsayilari
summary(cozum)
anova(cozum)