# SPSS ten veri aktarma
library(foreign)
Spss <- read.spss("C:/Dosya yolu/dosya.sav",to.data.frame = TRUE)

y <- c(2,8,16,22,28,32,36,40,42,48,50,52)
x1 <- c(0,4,8,12,16,20,24,28,32,30,40,40)
x2 <- c(22,20,10,12,8,8,6,6,8,4,1,0)
degiskenler <- data.frame(y,x1,x2)
cozum <- lm(degiskenler)

# Belirleme katsayisi, duzeltimis belirleme kat sayisi icin;
summary(cozum)

anovaTablosu <- anova(cozum)
guvenAraligi <- confint(cozum)
# Korelasyon katsayilari;
library(polycor)
library(ppcor)
cor(degiskenler)
zeroOrder <- hetcor(degiskenler)
partial <- pcor(degiskenler, method = "pearson")
part <- spcor(degiskenler, method = "pearson")

# Gorsel korelasyon icin;
library(corrgram)
corrgram(degiskenler,upper.panel=panel.pie)
corrgram(degiskenler,lower.panel=panel.ellipse,
         +          upper.panel=panel.pts, text.panel=panel.txt,
         +          diag.panel=panel.minmax)