# SPSS ten veri aktarma
library(foreign)
Spss <- read.spss("C:/Dosya yolu/dosya.sav",to.data.frame = TRUE)

# Dogrusal Model
y.dogrusal <- c(1,4,8,11,14,16,18,20,21,24,26)
x.dogrusal <- c(0,2,4,6,8,10,12,14,16,18,20)
cozum.dogrusal <- lm(y.dogrusal~x.dogrusal)
## Anova, Katsayilar, Belirleme Katsayilari
summary(cozum.dogrusal)
anova(cozum.dogrusal)

# Egrisel Model
y.egrisel <- c(4,10,14,20,22,25,20,18,9,5)
x.egrisel <- c(1,2,4,7,6,7,8,12,11,12)
cozum.egrisel <- lm(y.egrisel~x.egrisel+I(x.egrisel^2))
## Anova, Katsayilar, Belirleme Katsayilari
summary(cozum.egrisel)
anova(cozum.egrisel)