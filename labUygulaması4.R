y <- c(1,4,8,11,14,16,18,20,21,24,26)
x1 <- c(0,2,4,6,8,10,12,14,16,18,20)
x2 <- c(0,3,2,6,9,8,11,14,16,15,18)
x3 <- c(10,6,6,6,4,4,4,3,3,3,2)
x4 <- c(16,12,10,9,12,8,7,6,5,4,1)
x5 <- c(12,10,11,14,12,10,8,6,9,10,5)
x6 <- c(2,6,6,5,5,3,7,7,5,8,8)
cozum <- lm(y~x1+x2+x3+x4+x5+x6)

# Grafikler
layout(matrix(c(1,2,3,4),2,2))
plot(cozum)

# Ileriye Dogru secme ve Geriye Dogru Eleme Yontemleri
library(MASS)
stepAIC(cozum, direction="backward")