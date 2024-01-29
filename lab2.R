library(ggplot2)
library(tidyr)
library(gganimate)
library(gifski)
library(RColorBrewer)
library(stats)
library(tseries)
## Задание №1 ----
GARCH1 <- function(n,p,q,a,b){
  sigma = c(0)
  h = c(0)
  for(i in 2:n){
    P = 0
    if(p>0){
      P = P+a[2]*h[i-1]*h[i-1]
    }
    Q = 0
    if(q>0){
      Q = Q + b[1]*sigma[i-1]*sigma[i-1]
    }
    sigma = c(sigma,sqrt(a[1]+P+Q))
    
    e=rnorm(1,0,1)
    h = c(h,e*sigma[i])
    
  }
  return (data.frame(h,sigma))
}

GARCH3 <- function(n,p,q,a,b){
  sigma = c(0)
  h = c(0)
  for(i in 2:n){
    P = 0
    if(p>0){
      count = p
      if(p>length(h)){
        count = length(h)
      }
      for(j in 1:count){
        P = P+a[j+1]*h[i-j]*h[i-j]
      }
    }
    Q = 0
    if(q>0){
      count = q
      if(q>length(sigma)){
        count = length(sigma)
      }
      for(j in 1:count){
        Q = Q + b[j]*sigma[i-j]*sigma[i-j]
      }
    }
    sigma = c(sigma,sqrt(a[1]+P+Q))
    e=rnorm(1,0,1)
    h = c(h,e*sigma[i])
    
  }
  return (h)
}
Animation<- function(X,n){
  
  y = c(1:n);y
  AR <- data.frame(REG = X, index = y);AR
  my_plot = ggplot(data = AR, aes(x = index , y = REG, color = REG)) + geom_line()
  Plot1 = my_plot + transition_reveal(index)+ ease_aes()+scale_color_gradient(low="blue", high="red")
  final_animation<-animate(Plot1,100,fps = 20,duration = 30, renderer = gifski_renderer())
  anim_save("suicide_animate.gif",animation=final_animation)
  final_animation
}
a = c(0.6,0.2)
b = c(0.4)
Data = GARCH1(1000,1,0,a,b)
plot(Data$h,type="l",col="blue")
plot(Data$sigma,type="l",col="blue")

## Задание №2 ----
MNK = function(garch, n) {
  params = c()
  sum1 = 0
  sum2 = 0
  a1 = 0
  a0 = 0
  for (i in 2 : n) {
    sum1 = sum1 + (garch[i]**2)*((garch[i-1]**2) - 1)
    sum2 = sum2 + (garch[i-1]**4)-(garch[i-1]**2)
  }
  a0 = sum1/sum2
  sum = 0
  for (i in 2 : n) {
    sum = sum + garch[i]**2 - a1*garch[i-1]**2
  }
  a1 = sum/n
  params = c(params,a0)
  params = c(params,a1)
  return (params)
}
a = c(0.7,0.1)
b = c(0.4)
Data = GARCH1(1000,1,0,a,b)
MNK(Data$h,1000)

## Задание №3 ----

garch(Data$h,order = c(0,1))

## Задание №4 ----

a = c(0.1,0.3,0.1,0.4)
Data = GARCH3(500,3,0,a,b)
plot(Data,type = c("l"), col="557799")

garch(Data,order=c(3,0),start=a)$coef

##Animation(Data,1000)


## Задание №5 ----
a = c(0.7,0.1)
b = c(0.4)
Data = GARCH1(1000,1,1,a,b)
garch(Data$h,order = c(1,1), start = c(a[1],a[2],b[1]))



