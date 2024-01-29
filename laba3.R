library(readr)
library(stats)
library(forecast)
library(tseries)
## Задание №1 ----
AR_ARCH <- function(n,tetta,a,p,q){
  sigma = c(a[1])
  X = c(sqrt(sigma[1]) * rnorm(1, 0, 1))
  for(i in 2:n){
    len = length(X)
    e=rnorm(1,0,1)
    Q = 0
    q_ = q
    if(q>len){
      q_ = len
    }
    for(j in 1:q_-1){
      Q = Q + a[j+1]*X[q_-j]*X[q_-j]
    }
    sigma = c(sigma,sqrt(a[1]+Q))
    if(p>=len){
      p_ = len
     X = c(X,t(tetta[length(tetta)-len:1])%*%X[1]+sigma[i]*e)
    }
    else{
      X = c(X,t(tetta)%*%X[c(len:(len-p+1))]+sigma[i]*e)
    }
    
  }
  return (X)
}
AR_ARCH2 <- function(n,tetta,a,p,q){
  
  sigma = c(a[1])
  X = c(sqrt(sigma[1]) * rnorm(1, 0, 1))
  sigma = c(sigma,a[1] + a[2]*X[1]*X[1])
  X = c(X,sqrt(sigma[2]) * rnorm(1, 0, 1))
  sigma = c(sigma,a[1] + a[2]*X[2]*X[2] + a[3]*X[1]*X[1])
  X = c(X,sqrt(sigma[3]) * rnorm(1, 0, 1))
  Q = 0
  for(i in 4:n){
    e=rnorm(1,0,1)
    Q = 0
    for(j in 1:q){
      Q = Q + a[j+1]*X[i-j]*X[i-j]
    }
    sigma = c(sigma,sqrt(a[1]+Q))
    X = c(X,t(tetta)%*%X[c((i-1):(i-p))]+sigma[i]*e)
    
  }
  
  
  return (X)
}
MNK <- function(learn){
  est = arima(learn, order = c(2, 0, 0), include.mean = F)
  tetta_est = c(est$coef);tetta_est
  learn1 = c(learn[1])
  learn1 = c(learn1,learn[2] - tetta_est[1] * learn[1])
  for(i in 3:length(learn)){
    learn1 = c(learn1,learn[i] - t(tetta_est)%*%c(learn[i-1], learn[i-2]))
  }
  A_est = garch(learn1, c(0, 3))
  print(A_est$coef[1])
  print(A_est$coef[2])
  print(A_est$coef[3])
  print(A_est$coef[4])
  return(c(A_est$coef[1],A_est$coef[2],A_est$coef[3],A_est$coef[4]))
}
tetta = c(-0.3,0.4)
a = c(1,0.2,0.1,0.2)
c(a[5:(2+1)])
p = 2
q = 3
X = AR_ARCH2(2100,tetta,a,p,q)
plot(X,type = c("l"), col="557799")


## Задание №2 ----
learn = X[1:2000]
test = X[2001:2100]


## Задание №3 ----
X = AR_ARCH2(2100,tetta,a,p,q)
MNK(X)

## Задание №4 ----
prognoz <- function(n,X,p,q){
  est = arima(X, order = c(2, 0, 0), include.mean = F)
  tetta_est = c(est$coef);tetta_est
  x_p = c(X[1])
  x_p = c(x_p,X[2])
  for(i in 3:n){
    x_p = c(x_p,t(tetta_est)%*%X[c((i-1):(i-2))])
  }
  plot(X,type = c("l"), col="557799", lwd = 2)
  lines(x_p,type = c("p"), col="black" )
  a_est = MNK(X)
  sigma = c(sqrt(a_est[1]))
  sigma = c(sigma,sqrt(a_est[1] + a_est[2]*x_p[1]*x_p[1]))
  sigma = c(sigma,sqrt(a_est[1] + a_est[2]*x_p[2]*x_p[2] + a_est[3]*x_p[1]*x_p[1]))
  
  Q = 0
  for(i in 4:n){
    Q = 0
    for(j in 1:q){
      Q = Q + a_est[j+1]*x_p[i-j]*x_p[i-j]
    }
    sigma = c(sigma,sqrt(a[1]+Q))
  }
  border_top = c()
  border_bottom = c()
  for(i in 1:n){
    border_top = c(border_top,x_p[i]+sigma[i])
    border_bottom = c(border_bottom,x_p[i]-sigma[i])
  }
  lines(border_top,type = c("l"), col="red", lty = 2,xlim = c(min(border_top), max(border_top)))
  lines(border_bottom,type = c("l"), col="red", lty = 2, xlim = c(min(border_bottom), max(border_bottom)))
  return(x_p)
}
n = 1100
a = c(0.1,0.2,0.3,0.001)
X = AR_ARCH2(n,tetta,a,p,q)
learn = X[1:1000]
test = X[1001:1100]
prognoz(100,test,p,q)

## Задание №5 ----
#Экспорт данных Аэрофлот

## Задание №6 ----
table=read.csv2("C:/Users/Валентина/Desktop/Сдать/AR/labs/AFLT.csv", sep= ";")
data = as.numeric(sub(",", ".", table$VOL, fixed = TRUE))
data

## Задание №7 ----
plot(table$LAST,type="l",col="purple")

## Задание №8 ----
z = c(0)
for(i in 2:length(data)){
  z = c(z,(data[i]-data[i-1])/data[i-1])
}

## Задание №9 ----
plot(z,type="l",col="purple")
length(z)

## Задание №10 ----
learn = z[1:604]
test = z[605:654]

test
MNK(test)
prognoz(50,test,2,3)

