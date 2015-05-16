## Kitap Ornek 4.1 R Kodlari

# Degisken Tanimlari
b<-c("b0","b1","b2")
X0 <- c(1,1,1,1,1,1,1,1,1)
X1 <- c(5,5,6,7,7,7,8,8,10)
X2 <- c(2,2,8,3,6,4,2,5,4)
y <- c(4,5,5,6,8,8,8,10,9)
G <- c(X0,X1,X2)

# Matris Tanimlari ve Islemler
X = matrix(G, nrow=9, ncol=3)
Y = matrix(y)

TraX <- t(X)
TraXX <- TraX %*% X
DetXX <- det(TraXX)
# Ko-faktorler Matrisi Icýn Algoritma (Bkz. http://stackoverflow.com/questions/16757100/get-adjoint-matrix-in-r)
minor <- function(TraXX, i, j) det( TraXX[-i,-j] )
cofactor <- function(TraXX, i, j) (-1)^(i+j) * minor(TraXX,i,j)
adjoint <- function(TraXX) {
  n <- nrow(TraXX)
  B <- matrix(NA, n, n)
  for( i in 1:n )
    for( j in 1:n )
      B[j,i] <- cofactor(TraXX, i, j)
  B
}
KoFakXX <- adjoint(TraXX)
TersXX <- KoFakXX/DetXX
TraXY <- TraX %*% Y
b <- TersXX %*% TraXY
