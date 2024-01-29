## Задание №1 ----
n = 1000
t = 50
lambda = 2
T = c(0)
Nt = c()
for(i in 1:n){
  tay = c()
  count = 0
  while(sum(tay)<=t){
      tay = c(tay,rexp(1,lambda))
      count = count+1;
  }
  Nt = c(Nt,count)
}

Nt
## Задание №2 ----
Nt_sorted = sort(Nt)
hist(Nt,freq=FALSE,col="green")
y = c()
for(i in 1:n){
  y = c(y,(lambda*t)^Nt_sorted[i]/factorial(Nt_sorted[i])*exp(-lambda*t))
}
lines(Nt_sorted,y,type="l",lwd=3,col="blue")
## Задание №3 ----
U = function(t_max,U0,lambda,mu,c){
  temp = rexp(1,lambda)
  tay = c(temp)
  T = c(temp)
  X = c(rexp(1,1/mu))
  U = c(U0)
  k = 1
  while(sum(tay)<=t_max){
    k = k + 1
    tay = c(tay,rexp(1,lambda))
    T = c(T,T[k-1]+tay[k])
    X = c(X,rexp(1,1/mu))
    U = c(U,U[k-1]+c*tay[k]-X[k])
  }
  return(data.frame(T,U))
}
U0 = 50
t_max = 50

## Задание №3.a ----
c = 3
mu = 2
lambda = 1
c/(lambda*mu) - 1
u = U(t_max,U0,lambda,mu,c)
plot(u$T,u$U,type="l")

## Задание №3.b ----
c = 1
mu = 5
lambda = 0.5
c/(lambda*mu) - 1
u = U(t_max,U0,lambda,mu,c)
plot(u$T,u$U,type="l")

## Задание №4 ----
n = 1000
U0 = 100
t_max = 1000
c = 1
mu = 3
lambda = 0.3
count = 0
for(i in 1:n){
  u = U(t_max,U0,lambda,mu,c)
  if(min(u$U)<0) {
    count = count + 1
  }  b
}
psi_est = (1/n)*count
psi_est
r = c/(lambda*mu) - 1;r
psi2 = exp(-1/m*r/(1+r)*U0)
psi2
