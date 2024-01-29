library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)
library(gifski)
library(RColorBrewer)
library(stats)
## Задание №1 ----
AR <- function(n,tetta){
  X = c()
  X = c(X,runif(1, min = 0, max = 1))
  for(i in 2:n){
    e=rnorm(1,0,1)
    X = c(X,X[i-1]*tetta+e)
  }
  return (X)
}
AR2 <- function(n,tetta){
  X = c()
  X = c(X,runif(1, min = 0, max = 1))
  X = c(X,runif(1, min = 0, max = 1))
  for(i in 3:n){
    e=rnorm(1,0,1)
    X = c(X,X[i-2]*tetta[1] + X[i-1]*tetta[2]+e)
  }
  return (X)
}
Animation<- function(AR,n){
  
  y = c(1:n);y
  AR <- data.frame(REG = X, index = y);AR
  my_plot = ggplot(data = AR, aes(x = index , y = REG, color = REG)) + geom_line()
  Plot1 = my_plot + transition_reveal(index)+ ease_aes()+scale_color_gradient(low="blue", high="red")
  final_animation<-animate(Plot1,100,fps = 20,duration = 30, renderer = gifski_renderer())
  anim_save("suicide_animate.gif",animation=final_animation)
  final_animation
}
MNK<- function(AR,n){
  t = 0
  S1 = 0
  S2 = 0;
  for(i in 2:n){
    S1 = S1 + AR[i]*AR[i-1]
    S2 = S2 + AR[i-1]*AR[i-1]
  }
  t = S1/S2
  return (t)
}
n = 500
tetta = runif(1, min= -1, max = 1)
X=AR(n,tetta)
plot(X, main = "AR",type = c("l"), col="557799")
##Animation(X,n)

n=500
tetta1 = runif(1, min= -5, max = 5)
X1=AR(n,tetta1)
plot(X1, main = "AR",type = c("l"))
tetta2 = 0


n=500
tetta2=-1
X2=AR(n,tetta2)
plot(X2, main = "AR",type = c("l"),col="purple")


##Animation(X2,n)

## Задание №2 ----
n = 500
tetta = runif(1, min= -1, max = 1)
tetta
X=AR(n,tetta)
t = 0
S1 = 0
S2 = 0;
for(i in 2:n){
  S1 = S1 + X[i]*X[i-1]
  S2 = S2 + X[i-1]*X[i-1]
}
t = S1/S2;t;tetta

## Задание №3 ----
MP = function(tt) {
  sum = 0
  for (i in 2 : 100) {
    sum = sum + (X[i] - tt * X[i-1])**2
  }
  return (sum)
}
interval = c(-10,10)
tetta_est <- optimize(f = MP, lower = min(interval), upper = max(interval),
                      maximum = FALSE)$minimum
print("Tetta:")
tetta
print("MNK:")
t
print("MP:")
tetta_est 

## Задание №4 ----
n = 1000
tetta = 0.7
AR1 = AR(n,tetta)
MNK(AR1,n)
nabl = array(dim = n-10)
nabl[1] = MNK(AR1,10)
for(i in 11:n){
  nabl[i-9] = MNK(AR1,i)
}
plot(nabl,type = c("l"), col="red")

## Задание №5 ----
m = 100
TETTA = c(0.2,0.4);TETTA
Y = AR2(m,TETTA)
D = TETTA[1]*TETTA[1] + 4*TETTA[2];D
lamda1 = (TETTA[1]+sqrt(D))*0.5;lamda1
lamda2 = (TETTA[1]-sqrt(D))*0.5;lamda2
plot(Y, main = "AR",type = c("l"), col="557799")
##Animation(Y,m)

## Задание №6 ---- 
arima(Y, order = c(2, 0, 0), include.mean = FALSE)

