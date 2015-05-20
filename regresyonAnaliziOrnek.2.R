# SPSS ten veri aktarma
library(foreign)
Spss <- read.spss("C:/Dosya yolu/dosya.sav",to.data.frame = TRUE)

y <- c(3,4,5,5,5,6,7,7,8,10)
x1 <- c(5,5,6,6,7,7,7,8,9,10)
x2 <- c(1,1,3,4,5,6,6,5,3,6)
degiskenler <- data.frame(y,x1,x2)
cozum <- lm(degiskenler)
summary(cozum)
anova(cozum)

# Korelasyon katsayilari;
library(polycor)
library(ppcor)
cor(degiskenler)

 # Pearson Korelason
zeroOrder <- hetcor(degiskenler)

 # Kisim Korelasyon
partial <- pcor(degiskenler, method = "pearson")

 # Kismi Korelasyon
part <- spcor(degiskenler, method = "pearson")

# Guven Araliklari (alfa = 0,05 icin)
confint(cozum)
