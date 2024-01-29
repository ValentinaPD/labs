## Задание №1 ----
n = 200
r = 0.01
B = c(1)
for (i in 2:n) {
  B = c(B,(1+r)*B[i-1])
}
plot(B,type="l")
## Задание №2 ----
n = 200
a = -0.3
b = 0.8
p = 0.4
S = c(1)
p_n = c(a)
for (i in 2:n) {
  temp = rbinom(1,1,p)
  if(temp == 1){
    p_n = c(p_n,b)
  }
  else{
    p_n = c(p_n,a)
  }
  S = c(S,(1+p_n[i])*S[i-1])
}
plot(S,type="l")
## Задание №3 ----
B = function(j, N, p){
  temp = 0
  for(k in j:N){
    temp = temp + (factorial(N)/(factorial(k)*factorial(N-k)))*(p**k)*(1-p)**(N-k)
  }
  return(temp)
}

n = 10
a = -0.3
b = 0.8
r = 0.2
K = 100
S0 = 100
p1 = (r-a)/(b-a);p1 ## p с волной
p2 = ((1+b)/(1+r))*p1;p2 ## p со звездой
K0 = 1 + (log(K/(S0*(1+a)**n)))*(log((1+b)/(1+a)))**(-1);K0
C = S0*B(K0,n,p2) - K*(1+r)**(-n)*B(K0,n,p1);C
