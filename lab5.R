## Задание №2 ----
B = function(delta,n,B0){
  B = c(B0)
  k_delta = c(delta)
  for(k in 2:n){
    e = rnorm(1,0,sqrt(delta))
    B = c(B,B[k-1]+e)
    k_delta = c(k_delta,k*delta)
  }
  return(data.frame(k_delta,B))
}
n = 1000 ##К сожалению, не могу сгенерировать больше. Ноут не тянет :( 
delta = 0.0001
B0 = 0
Brownian=B(delta,n,B0)
plot(Brownian$k_delta,Brownian$B,type="l",col="blue")
## Задание №3 ----
get_random_color = function(){
  r = runif(1,0,1)
  g = runif(1,0,1)
  b = runif(1,0,1)
  color = rgb(r,g,b,1)
  return (color)
}
B_line = function(delta,n){
  Brownian=B(delta,n,B0)
  return(lines(Brownian$k_delta,Brownian$B,type = "l",col=get_random_color()))
}
plot(Brownian$k_delta,Brownian$B,type="l",col="blue",ylim = c(-1.5,1.5))
for(i in 1:199){
  B_line(delta,n)
}
## Задание №4 ----
b = 3*sqrt(Brownian$k_delta)
a = -b
lines(Brownian$k_delta,b,col = "red",type = "l", lwd = 2)
lines(Brownian$k_delta,a,col = "red",type = "l",lwd = 2)

## Задание №5 ----
S = function(delta,n,S0,sigma,a,B0){
  Brownian=B(delta,n,B0)
  S = c(S0)
  for(k in 2:n){
    ex = (a-(sigma**2)/2)*k*delta+sigma*Brownian$B[k]
    S = c(S,S0*exp(ex))
  }
  return (data.frame(k_d = Brownian$k_delta,S))
}
S0 = 1
B0 = 0
a = 0.5
sigma = 0.9
n = 1000
delta = 0.0001
s = S(delta,n,S0,sigma,a,B0)
plot(s$k_d,s$S,type = "l",col="red")

## Задание №6 ----
S_line = function(delta,n,S0,sigma,a,B0){
  s = S(delta,n,S0,sigma,a,B0)
  return(lines(s$k_d,s$S,type = "l",col=get_random_color()))
}
plot(s$k_d,s$S,type = "l",col="red",ylim = c(-0.5,2.5))
for(i in 1:199){
  S_line(delta,n,S0,sigma,a,B0)
}

