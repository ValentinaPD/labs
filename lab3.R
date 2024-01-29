## Лабораторная 3.1----
n=200;
X1 = rnorm(n,0,1)
r1 = 0.175
r2 = 0.797
X2 = r1*X1 + sqrt(1-r1*r1)*rnorm(n,0,1)
X3 = r2*X1 + sqrt(1-r2*r2)*rnorm(n,0,1)
##   par(mar = rep(2, 4))
plot(X1,X2, col="green")
plot(X2,X3, col="blue")
plot(X1,X3, col="red")

cor.test(X1,X2)
cor.test(X1,X3)
cor.test(X2,X3)

cor.test(X1,X2,method = "spearman")
cor.test(X1,X3,method = "spearman")
cor.test(X2,X3,method = "spearman")

r1_est = cor(X1,X2)
r2_est = cor(X1,X3)
r3_est = cor(X2,X3)

z1_1 = (1/2)*log((1+r1_est)/(1-r1_est))-(qnorm(0.975,0,1)/sqrt(n-3))-(r1_est/(2*(n-1)))
z1_2 = (1/2)*log((1+r1_est)/(1-r1_est))+(qnorm(0.975,0,1)/sqrt(n-3))-(r1_est/(2*(n-1)))

tanh(z1_1)
tanh(z1_2)

z2_1 = (1/2)*log((1+r2_est)/(1-r2_est))-(qnorm(0.975,0,1)/sqrt(n-3))-(r2_est/(2*(n-1)))
z2_2 = (1/2)*log((1+r2_est)/(1-r2_est))+(qnorm(0.975,0,1)/sqrt(n-3))-(r2_est/(2*(n-1)))

tanh(z2_1)
tanh(z2_2)

z3_1 = (1/2)*log((1+r3_est)/(1-r3_est))-(qnorm(0.975,0,1)/sqrt(n-3))-(r3_est/(2*(n-1)))
z3_2 = (1/2)*log((1+r3_est)/(1-r3_est))+(qnorm(0.975,0,1)/sqrt(n-3))-(r3_est/(2*(n-1)))

tanh(z3_1)
tanh(z3_2)


## Лабораторная 3.2----
library(rcompanion)
n = 200
p = 0.31
q1 = 0.79
q2 = 0.35

n1 = rbinom(1,n,p);n1
n2 = n - n1;n2
n11 = rbinom(1,n1,q1);n11
n12 = n1 - n11;n12
n21 = rbinom(1,n2,q2);n21
n22 = n2 - n21;n22

tab = cbind(c(n11,n12),c(n21,n22));tab
test = chisq.test(tab);test
cramerV(tab)
cramerV(tab, ci=TRUE)


## Лабораторная 3.3----

data1=read.csv2("C:/Users/Валентина/Desktop/R/Rent.csv", sep= ";" )
#rent = data1[c(2)]
#s = data1[c(3)]
#floor = data1[c(4)]
#rooms = data1[c(5)]
#walls = data1[c(6)]
#district = data1[c(7)]

tab1 = table(data1$walls,data1$district);tab1
chisq.test(tab1)
tab2 = data.frame(data1$rent,data1$s,data1$floor)
cor.test(data1$rent,data1$s)
cor(tab2,method = "spearman")
cor.test(data1$rent,data1$floor,method = "spearman")
cor.test(data1$s,data1$floor,method = "spearman")
