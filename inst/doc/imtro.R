## ------------------------------------------------------------------------
x=1:30   
y=matrix(x,nrow=6)
y

## ------------------------------------------------------------------------
m=seq(1,5.5,0.5)  
l=matrix(m,nrow=2)
colnames(l)=c("a","b","c","d","e")
knitr::kable(l) 

## ------------------------------------------------------------------------
n=rnorm(10)    
z=1:10
plot(n~z)

## ----Code chunk1, echo = FALSE-------------------------------------------
x=c(0,1,2,3,4)
p=c(0.1,0.2,0.2,0.2,0.3)
cp=cumsum(p)
 m <- 1e3
r=numeric(m)
r <- x[findInterval(runif(m),cp)+1]
ct <- as.vector(table(r))
t=ct/sum(ct)
q=ct/sum(ct)/p
k=rbind(t,p,q)
colnames(k)=c("x=0","x=1","x=2","x=3","x=4")
rownames(k)=c("the empirical distribution","the theoretical distribution","the rate")
k

## ----Code chunk1_, eval = FALSE------------------------------------------
#  x=c(0,1,2,3,4)
#  p=c(0.1,0.2,0.2,0.2,0.3)
#  cp=cumsum(p)
#   m <- 1e3
#  r=numeric(m)
#  r <- x[findInterval(runif(m),cp)+1]
#  ct <- as.vector(table(r))
#  t=ct/sum(ct)
#  q=ct/sum(ct)/p
#  k=rbind(t,p,q)
#  colnames(k)=c("x=0","x=1","x=2","x=3","x=4")
#  rownames(k)=c("the empirical distribution","the theoretical distribution","the rate")
#  k

## ----Code chunk2, echo = FALSE-------------------------------------------
f<-function(a,b,n){
  j=w=0
  y=numeric(n)
  while(w<n){
    u <- runif(1)
    j <- j + 1 
    x1 <- runif(1)
    if (x1^(a-1) * (1-x1)^(b-1) > u) {
      w <- w + 1
      y[w] <- x1
    }
  }
  y
}
y=f(3,2,1000)
hist(y)
curve(dbeta(x,3,2)*1000/12,add=T,col="green")

## ----Code chunk2_, eval = FALSE------------------------------------------
#  f<-function(a,b,n){
#    j=w=0
#    y=numeric(n)
#    while(w<n){
#      u <- runif(1)
#      j <- j + 1
#      x1 <- runif(1)
#      if (x1^(a-1) * (1-x1)^(b-1) > u) {
#        w <- w + 1
#        y[w] <- x1
#      }
#    }
#    y
#  }
#  y=f(3,2,1000)
#  hist(y)
#  curve(dbeta(x,3,2)*1000/12,add=T,col="green")

## ---- echo = FALSE-------------------------------------------------------
n=1000
r=4
beta1=2
t=rgamma(n,r,beta1)
x=rexp(n,t)
hist(x)
barplot(x)

## ---- eval = FALSE-------------------------------------------------------
#  n=1000
#  r=4
#  beta1=2
#  t=rgamma(n,r,beta1)
#  x=rexp(n,t)
#  hist(x)
#  barplot(x)

## ---- echo = FALSE-------------------------------------------------------
f=function(x){
  m=10000
  t=runif(m,min=0,max=x)
  th=mean(1/beta(3,3)*t^(2)*(1-t)^(2)*x)
}
x=seq(0.1,0.9,by=0.1)
MC=numeric(9)
for(i in 1:9){
  result=f(x[i])
  MC[i]=result[[1]]
}
pBeta=pbeta(x,3,3)
m=rbind(MC,pBeta)
knitr::kable(m,col.names=x)
matplot(x,cbind(MC,pBeta),col=1:2,pch=1:2,xlim=c(0,1))
legend("topleft", inset=.05,legend=c("MC","pbeta"),col=1:2,pch=1:2)

## ---- eval = FALSE-------------------------------------------------------
#   f=function(x){
#    m=10000
#    t=runif(m,min=0,max=x)
#    th=mean(1/beta(3,3)*t^(2)*(1-t)^(2)*x)
#  }
#  x=seq(0.1,0.9,by=0.1)
#  MC=numeric(9)
#  for(i in 1:9){
#    result=f(x[i])
#    MC[i]=result[[1]]
#  }
#  pBeta=pbeta(x,3,3)
#  m=rbind(MC,pBeta)
#  knitr::kable(m,col.names=x)

## ---- echo = FALSE-------------------------------------------------------
f1=function(n,sigma,p){
  x1=runif(n)
  if(p==1){
  x2=1-x1
  }
  else{
    x2=runif(n)
  }
  y1=sqrt(-2*sigma^2*log(1-x1))
  y2=sqrt(-2*sigma^2*log(1-x2))
  z=cbind(y1,y2)
  y=rowMeans(z)
  return(y)
}

n=1000
g1=f1(n,2,1)
g2=f1(n,2,0)
hist(g1)
z=c(sd(g1),sd(g2),sd(g1)/sd(g2))
names(z)=c("sd(g1)","sd(g2)","sd(g1)/sd(g2)")
z

## ---- eval = FALSE-------------------------------------------------------
#  f1=function(n,sigma){
#    x1=runif(n/2)
#    x2=1-x1
#    x=c(x1,x2)
#    y=sqrt(-2*sigma^2*log(1-x))
#    return(y)
#  }
#  f2=function(n,sigma){
#    x=runif(n)
#    y=sqrt(-2*sigma^2*log(1-x))
#    return(y)
#  }
#  n=10000
#  g1=f1(n,2)
#  g2=f2(n,2)
#  hist(g1)
#  qqplot(g1,g2)
#  z=c(sd(g1),sd(g2),sd(g1)/sd(g2))
#  names(z)=c("sd(g1)","sd(g2)","sd(g1)/sd(g2)")
#  z

## ---- echo = FALSE-------------------------------------------------------
n=1000
f=function(x){
  x^2/sqrt(2*pi)*exp(-(x^2)/2)
}
f1=function(x){
  exp(-x+1)
}
f2=function(x){
   1/x^2
}
x=seq(1,5,0.1)
plot(x,f(x),col=1,type="o",ylim=c(0,1),lty=1,ylab="y")
points(x,f1(x),col=2,type="o",lty=1)
lines(x,f2(x),col=3,type="o",lty=1)
legend("topright",inset=.05,legend=c("f(x)","f1(x)","f2(x)"),lty=1,col=1:3,horiz=FALSE)
  X1=rexp(n,rate=1)+1
  theta1=mean(f(X1)/f1(X1))
  sd1=sd(f(X1)/f1(X1))/n
  X2=runif(n)
  X2<-1/(1-X2)
  theta2=mean(f(X2)/f2(X2))
  sd2=sd(f(X2)/f2(X2))/n
  a=c(theta1,sd1,theta2,sd2)
  a=matrix(a,nrow=2)
  colnames(a)=c("f1","f2")
  rownames(a)=c("theta","sd")
  a
  print("s1<s2,so f1 is better than f2")

## ---- eval = FALSE-------------------------------------------------------
#  n=1000
#  f=function(x){
#    x^2/sqrt(2*pi)*exp(-(x^2)/2)
#  }
#  f1=function(x){
#    exp(-x+1)
#  }
#  f2=function(x){
#     1/x^2
#  }
#  x=seq(1,5,0.1)
#  plot(x,f(x),col=1,type="o",ylim=c(0,1),lty=1,ylab="y")
#  points(x,f1(x),col=2,type="o",lty=1)
#  lines(x,f2(x),col=3,type="o",lty=1)
#  legend("topright",inset=.05,legend=c("f(x)","f1(x)","f2(x)"),lty=1,col=1:3,horiz=FALSE)
#    X1=rexp(n,rate=1)+1
#    theta1=mean(f(X1)/f1(X1))
#    sd1=sd(f(X1)/f1(X1))/n
#    X2=runif(n)
#    X2<-1/(1-X2)
#    theta2=mean(f(X2)/f2(X2))
#    sd2=sd(f(X2)/f2(X2))/n
#    a=c(theta1,sd1,theta2,sd2)
#    a=matrix(a,nrow=2)
#    colnames(a)=c("f1","f2")
#    rownames(a)=c("theta","sd")
#    a

## ---- echo = FALSE-------------------------------------------------------
n=1000
f=function(x){
  x^2/sqrt(2*pi)*exp(-(x^2)/2)
}
f1=function(x){
  exp(-x+1)
}
  X1=rexp(n,rate=1)+1
  theta1=mean(f(X1)/f1(X1))
  sd1=sd(f(X1)/f1(X1))/n
  a=c(theta1,sd1)
  a=matrix(a,ncol=1)
  colnames(a)=c("f1")
  rownames(a)=c("theta","sd")
  a

## ----eval = FALSE--------------------------------------------------------
#  n=1000
#  f=function(x){
#    x^2/sqrt(2*pi)*exp(-(x^2)/2)
#  }
#  f1=function(x){
#    exp(-x+1)
#  }
#    X1=rexp(n,rate=1)+1
#    theta1=mean(f(X1)/f1(X1))
#    sd1=sd(f(X1)/f1(X1))/n
#    a=c(theta1,sd1)
#    a=matrix(a,ncol=1)
#    colnames(a)=c("f1")
#    rownames(a)=c("theta","sd")
#    a

## ---- echo = FALSE-------------------------------------------------------
f<-function(x){
  n<-length(x)
  a<-seq(1-n,n-1,2)
  i<-sort(x)
  y<-sum(a*i)/(n*n*mean(x))
  return(y)
}
n=500
m<-1000
t1<-numeric(m)
for(i in 1:m){
  q<-rnorm(n)
  p<-exp(q)
  t1[i]<-f(p)
}
result1<-c(mean(t1),quantile(t1,probs=c(0.5,0.1)))
names(result1)<-c("mean","median","deciles")
print(result1)
hist(t1)
t2<-numeric(m)
for(i in 1:m){
  x<-runif(n) # then x is uniform
  t2[i]<-f(x)
}
result2<-c(mean(t2),quantile(t2,probs=c(0.5,0.1)))
names(result2)<-c("mean","median","deciles")
print(result2)
hist(t2)
t3<-numeric(m)
for(i in 1:m){
  x<-rbinom(n,1,0.1) # then x is Bernoulli(0.1)
  t3[i]<-f(x)
}
result3<-c(mean(t3),quantile(t3,probs=c(0.5,0.1)))
names(result3)<-c("mean","median","deciles")
print(result3)
hist(t3)

## ---- eval = FALSE-------------------------------------------------------
#  f<-function(x){
#    n<-length(x)
#    a<-seq(1-n,n-1,2)
#    i<-sort(x)
#    y<-sum(a*i)/(n*n*mean(x))
#    return(y)
#  }
#  n=500
#  m<-1000
#  t1<-numeric(m)
#  for(i in 1:m){
#    q<-rnorm(n)
#    p<-exp(q)
#    t1[i]<-f(p)
#  }
#  result1<-c(mean(t1),quantile(t1,probs=c(0.5,0.1)))
#  names(result1)<-c("mean","median","deciles")
#  print(result1)
#  hist(t1)
#  t2<-numeric(m)
#  for(i in 1:m){
#    x<-runif(n) # then x is uniform
#    t2[i]<-f(x)
#  }
#  result2<-c(mean(t2),quantile(t2,probs=c(0.5,0.1)))
#  names(result2)<-c("mean","median","deciles")
#  print(result2)
#  hist(t2)
#  t3<-numeric(m)
#  for(i in 1:m){
#    x<-rbinom(n,1,0.1) # then x is Bernoulli(0.1)
#    t3[i]<-f(x)
#  }
#  result3<-c(mean(t3),quantile(t3,probs=c(0.5,0.1)))
#  names(result3)<-c("mean","median","deciles")
#  print(result3)
#  hist(t3)

## ---- echo = FALSE-------------------------------------------------------
n=500
m=1000
G1<-function(n){
  y<-rnorm(n,1,1)
  x<-exp(y)
  G.sample<-f(x)
  return(G.sample)
}
G.sp<-numeric(m)
for(i in 1:m){
  G.sp[i]<-G1(n)
}
CI<-c(mean(G.sp)-sd(G.sp)*qt(0.975,m-1),mean(G.sp)+sd(G.sp)*qt(0.975,m-1))
print(CI)
cover.rate<-sum(I(G.sp>CI[1]&G.sp<CI[2]))/m
print(cover.rate)

## ---- eval = FALSE-------------------------------------------------------
#  n=500
#  m=1000
#  G1<-function(n){
#    y<-rnorm(n,1,1)
#    x<-exp(y)
#    G.sample<-f(x)
#    return(G.sample)
#  }
#  G.sp<-numeric(m)
#  for(i in 1:m){
#    G.sp[i]<-G1(n,1,1)
#  }
#  CI<-c(mean(G.sp)-sd(G.sp)*qt(0.975,m-1),mean(G.sp)+sd(G.sp)*qt(0.975,m-1))
#  print(CI)
#  cover.rate<-sum(I(G.sp>CI[1]&G.sp<CI[2]))/m
#  print(cover.rate)

## ---- echo = FALSE-------------------------------------------------------
library(mvtnorm)
power.test<-function(method,cor){
  n1<-500
  n2<-1e4
  mean<-c(0,0)
  sigma<-matrix(c(1,cor,cor,1),nrow=2)
  p<-mean(replicate(n2,expr={
    x<-rmvnorm(n1,mean,sigma)
    cor.test(x[,1],x[,2],method=method)$p.value<=0.05
  }))
  p
}
power<-data.frame(method=c("pearson","kendall","spearman"),power=c(power.test("pearson",0.1),power.test("kendall",0.1),power.test("spearman",0.1)))
knitr::kable(power,format="markdown",align="c")

## ---- eval = FALSE-------------------------------------------------------
#  library(mvtnorm)
#  power.test<-function(method,cor){
#    n1<-500
#    n2<-1e4
#    mean<-c(0,0)
#    sigma<-matrix(c(1,cor,cor,1),nrow=2)
#    p<-mean(replicate(n2,expr={
#      x<-rmvnorm(n1,mean,sigma)
#      cor.test(x[,1],x[,2],method=method)$p.value<=0.05
#    }))
#    p
#  }
#  power<-data.frame(method=c("pearson","kendall","spearman"),power=c(power.test("pearson",0.1),power.test("kendall",0.1),power.test("spearman",0.1)))
#  knitr::kable(power,format="markdown",align="c")

## ---- echo = FALSE-------------------------------------------------------
c1=c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
c2=c(339,330,281,303,344,307,300,343,336,313,312,274,276,288,296)
x=matrix(c(c1,c2),ncol=2)
b.cor <- function(x,i) cor(x[i,1],x[i,2])
n <- 15
theta.hat <- b.cor(x,1:n)
theta.jack <- numeric(n)
for(i in 1:n){
  theta.jack[i] <- b.cor(x,(1:n)[-i])
}
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(original=theta.hat,bias=bias.jack, se=se.jack),3)

## ---- eval = FALSE-------------------------------------------------------
#  c1=c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
#  c2=c(339,330,281,303,344,307,300,343,336,313,312,274,276,288,296)
#  x=matrix(c(c1,c2),ncol=2)
#  b.cor <- function(x,i) cor(x[i,1],x[i,2])
#  n <- 15
#  theta.hat <- b.cor(x,1:n)
#  theta.jack <- numeric(n)
#  for(i in 1:n){
#    theta.jack[i] <- b.cor(x,(1:n)[-i])
#  }
#  bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
#  se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2))
#  round(c(original=theta.hat,bias=bias.jack, se=se.jack),3)

## ---- echo = FALSE-------------------------------------------------------
library(boot)
aircondit
b.mean=function(x,i)mean(x[i])
mean.boot=boot(data=aircondit$hours,statistic=b.mean,R=1000)
mean.boot
CI=boot.ci(mean.boot,type=c("norm","basic","perc","bca"))
CI

## ---- eval = FALSE-------------------------------------------------------
#  library(boot)
#  aircondit
#  b.mean=function(x,i)mean(x[i])
#  mean.boot=boot(data=aircondit$hours,statistic=b.mean,R=1000)
#  mean.boot
#  CI=boot.ci(mean.boot,type=c("norm","basic","perc","bca"))
#  CI

## ---- echo = FALSE-------------------------------------------------------
library(bootstrap)
scor
n=nrow(scor)
sigma.hat=cov(scor)*(n-1)/n
eigenvalues.hat=eigen(sigma.hat)$values
theta.hat=eigenvalues.hat[1]/sum(eigenvalues.hat)
theta.jack=numeric(n)
for(i in 1:n){
  sigma.jack=cov(scor[-i,])*(n-1)/n
  eigenvalues.jack=eigen(sigma.jack)$values
  theta.jack[i]=eigenvalues.jack[1]/sum(eigenvalues.jack)
}
bias.jack=(n-1)*(mean(theta.jack)-theta.hat)
bias.jack
se.jack=sd(theta.jack)*sqrt((n-1)^2/n)
se.jack

## ---- eval = FALSE-------------------------------------------------------
#  library(bootstrap)
#  scor
#  n=nrow(scor)
#  sigma.hat=cov(scor)*(n-1)/n
#  eigenvalues.hat=eigen(sigma.hat)$values
#  theta.hat=eigenvalues.hat[1]/sum(eigenvalues.hat)
#  theta.jack=numeric(n)
#  for(i in 1:n){
#    sigma.jack=cov(scor[-i,])*(n-1)/n
#    eigenvalues.jack=eigen(sigma.jack)$values
#    theta.jack[i]=eigenvalues.jack[1]/sum(eigenvalues.jack)
#  }
#  bias.jack=(n-1)*(mean(theta.jack)-theta.hat)
#  bias.jack
#  se.jack=sd(theta.jack)*sqrt((n-1)^2/n)
#  se.jack

## ---- echo = FALSE-------------------------------------------------------
library(DAAG)
attach(ironslag)
n <- length(magnetic)
e1 <- e2 <- e3 <- e4 <- cbind(rep(0,n*(n-1)/2),rep(0,n*(n-1)/2))
k=1
for(i in 1:(n-1)){
  for(j in (i+1):n ){
    y <- magnetic[-c(i,j)]
    x <- chemical[-c(i,j)]
    J1 <- lm(y ~ x)
    yhat11 <- J1$coef[1] + J1$coef[2] * chemical[i]
    yhat12 <- J1$coef[1] + J1$coef[2] * chemical[j]
    e1[k,1]=yhat11-magnetic[i]
    e1[k,2]=yhat12-magnetic[j]

    J2 <- lm(y ~ x + I(x^2))
    yhat21 <- J2$coef[1] + J2$coef[2] * chemical[i] +J2$coef[3] * chemical[i]^2
    yhat22 <- J2$coef[1] + J2$coef[2] * chemical[j] +J2$coef[3] * chemical[j]^2
    e2[k,1]=yhat21-magnetic[i]
    e2[k,2]=yhat22-magnetic[j]
    J3 <- lm(log(y) ~ x)
    logyhat31 <- J3$coef[1] + J3$coef[2] * chemical[i]
    yhat31 <- exp(logyhat31)
    logyhat32 <- J3$coef[1] + J3$coef[2] * chemical[j]
    yhat32 <- exp(logyhat32)
    e3[k,1] <-  yhat31-magnetic[i]
    e3[k,2] <-  yhat32-magnetic[j]
    J4 <- lm(log(y) ~ log(x))
    logyhat41 <- J4$coef[1] + J4$coef[2] * log(chemical[i])
    logyhat42 <- J4$coef[1] + J4$coef[2] * log(chemical[j])
    yhat41 <- exp(logyhat41)
    yhat42 <- exp(logyhat42)
    e4[k,1] <-  yhat41 -magnetic[i]
    e4[k,1] <-  yhat42 -magnetic[j]
    k=k+1
  }
}

c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## ---- eval = FALSE-------------------------------------------------------
#  library(DAAG)
#  attach(ironslag)
#  n <- length(magnetic)
#  e1 <- e2 <- e3 <- e4 <- cbind(rep(0,n*(n-1)/2),rep(0,n*(n-1)/2))
#  k=1
#  for(i in 1:(n-1)){
#    for(j in (i+1):n ){
#      y <- magnetic[-c(i,j)]
#      x <- chemical[-c(i,j)]
#      J1 <- lm(y ~ x)
#      yhat11 <- J1$coef[1] + J1$coef[2] * chemical[i]
#      yhat12 <- J1$coef[1] + J1$coef[2] * chemical[j]
#      e1[k,1]=yhat11-magnetic[i]
#      e1[k,2]=yhat12-magnetic[j]
#  
#      J2 <- lm(y ~ x + I(x^2))
#      yhat21 <- J2$coef[1] + J2$coef[2] * chemical[i] +J2$coef[3] * chemical[i]^2
#      yhat22 <- J2$coef[1] + J2$coef[2] * chemical[j] +J2$coef[3] * chemical[j]^2
#      e2[k,1]=yhat21-magnetic[i]
#      e2[k,2]=yhat22-magnetic[j]
#      J3 <- lm(log(y) ~ x)
#      logyhat31 <- J3$coef[1] + J3$coef[2] * chemical[i]
#      yhat31 <- exp(logyhat31)
#      logyhat32 <- J3$coef[1] + J3$coef[2] * chemical[j]
#      yhat32 <- exp(logyhat32)
#      e3[k,1] <-  yhat31-magnetic[i]
#      e3[k,2] <-  yhat32-magnetic[j]
#      J4 <- lm(log(y) ~ log(x))
#      logyhat41 <- J4$coef[1] + J4$coef[2] * log(chemical[i])
#      logyhat42 <- J4$coef[1] + J4$coef[2] * log(chemical[j])
#      yhat41 <- exp(logyhat41)
#      yhat42 <- exp(logyhat42)
#      e4[k,1] <-  yhat41 -magnetic[i]
#      e4[k,1] <-  yhat42 -magnetic[j]
#      k=k+1
#    }
#  }
#  
#  c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## ---- echo = FALSE-------------------------------------------------------
f <- function(x1,x2){
  Fx1<-ecdf(x1)
  Fx2<-ecdf(x2)
  n<-length(x1)
  m<-length(x2)
  w1<-sum((Fx1(x1)-Fx2(x1))^2)+sum((Fx1(x2)-Fx2(x2))^2)
  w2<-w1*m*n/((m+n)^2)
  return(w2)
}
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)
t=f(x,y)
m=c(x,y)
z <- numeric(1000)
m1=length(m)
m2=length(x)
q=1:m1
for (i in 1:1000) {
  d= sample(q, size = m2, replace = FALSE)
  q1 <- m[d]
  q2 <- m[-d]
  z[i] <- f(q1,q2)
}
p <- mean(abs(c(t, z)) >= abs(t))
p
hist(z, main = "", freq = FALSE, xlab = "Cramer-von Mises statistic", breaks = "scott")

points(t, 0)

## ---- eval = FALSE-------------------------------------------------------
#  f <- function(x1,x2){
#    Fx1<-ecdf(x1)
#    Fx2<-ecdf(x2)
#    n<-length(x1)
#    m<-length(x2)
#    w1<-sum((Fx1(x1)-Fx2(x1))^2)+sum((Fx1(x2)-Fx2(x2))^2)
#    w2<-w1*m*n/((m+n)^2)
#    return(w2)
#  }
#  attach(chickwts)
#  x <- sort(as.vector(weight[feed == "soybean"]))
#  y <- sort(as.vector(weight[feed == "linseed"]))
#  detach(chickwts)
#  t=f(x,y)
#  m=c(x,y)
#  z <- numeric(1000)
#  m1=length(m)
#  m2=length(x)
#  q=1:m1
#  for (i in 1:1000) {
#    d= sample(q, size = m2, replace = FALSE)
#    q1 <- m[d]
#    q2 <- m[-d]
#    z[i] <- f(q1,q2)
#  }
#  p <- mean(abs(c(t, z)) >= abs(t))
#  p
#  hist(z, main = "", freq = FALSE, xlab = "Cramer-von Mises statistic", breaks = "scott")
#  points(t, 0)

## ---- echo = FALSE-------------------------------------------------------
pi<-3.1415926
f<-function(x,u,lamada){
  t=lamada/(pi*(lamada^2+(x-u)^2))
  return(t)
}
g<-function(n,sigma,x0,u,lamada){
  x<-NULL
  x[1]<-x0
  e=runif(n)
  k<-0
  for(i in 2:n){
    y<-rnorm(1,x[i-1],sigma)
    if(e[i]<=(f(y,u,lamada)/f(x[i-1],u,lamada))){
      x[i]<-y
    }
    else
    {
      x[i]<-x[i-1]
      k<-k+1
    }
  }
  return(list(x=x,k=k))
}
z=g(n=10000,sigma=0.5,x0=0,u=0,lamada=1)
p=z$k/9000
p
hist(z$x[1001:10000],freq=F)
curve(f(x,0,1),add=TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  pi<-3.1415926
#  f<-function(x,u,lamada){
#    t=lamada/(pi*(lamada^2+(x-u)^2))
#    return(t)
#  }
#  g<-function(n,sigma,x0,u,lamada){
#    x<-NULL
#    x[1]<-x0
#    e=runif(n)
#    k<-0
#    for(i in 2:n){
#      y<-rnorm(1,x[i-1],sigma)
#      if(e[i]<=(f(y,u,lamada)/f(x[i-1],u,lamada))){
#        x[i]<-y
#      }
#      else
#      {
#        x[i]<-x[i-1]
#        k<-k+1
#      }
#    }
#    return(list(x=x,k=k))
#  }
#  z=g(n=10000,sigma=0.5,x0=0,u=0,lamada=1)
#  p=z$k/9000
#  p
#  hist(z$x[1001:10000],freq=F)
#  curve(f(x,0,1),add=TRUE)

## ---- echo = FALSE-------------------------------------------------------

f=function(t){
  if (t < 0 || t >= 1){
    y=0
  }
  else{
    y=(1/2+t/4)^125 * ((1-t)/4)^18 * ((1-t)/4)^20 * (t/4)^34
  }
  return(y)
}
t1=runif(1000)
t2=runif(1000,-0.25,0.25)
x <- numeric(1000)
x[1]=0.25
for (i in 2:1000) {
  z = x[i-1] + t2[i]
  k= f(z) / f(x[i-1])
  if (t1[i] <= k)
    x[i]=z
  else
    x[i]=x[i-1]
}
z=x[100:1000]
p=mean(z)
p

## ---- eval = FALSE-------------------------------------------------------
#  
#  f=function(t){
#    if (t < 0 || t >= 1){
#      y=0
#    }
#    else{
#      y=(1/2+t/4)^125 * ((1-t)/4)^18 * ((1-t)/4)^20 * (t/4)^34
#    }
#    return(y)
#  }
#  t1=runif(1000)
#  t2=runif(1000,-0.25,0.25)
#  x <- numeric(1000)
#  x[1]=0.25
#  for (i in 2:1000) {
#    z = x[i-1] + t2[i]
#    k= f(z) / f(x[i-1])
#    if (t1[i] <= k)
#      x[i]=z
#    else
#      x[i]=x[i-1]
#  }
#  z=x[100:1000]
#  p=mean(z)
#  p

## ---- echo = FALSE-------------------------------------------------------
mys<-function(a,k){
  M=sqrt(a^2*k/(k+1-a^2))
  return(pt(M,df=k,lower.tail = FALSE))
}
myf<-function(a,k){
  mys(a=a,k=(k-1))-mys(a=a,k=k)
}
kc=c(4:25,100,500,1000)
A=rep(0,length(kc))
for(i in 1:length(kc)){
  A[i]=uniroot(myf,c(0.5,sqrt(kc[i]-1)),k=kc[i])$root
}
cbind(kc,A)
plot(kc,A,type="o")

## ---- eval = FALSE-------------------------------------------------------
#  mys<-function(a,k){
#    M=sqrt(a^2*k/(k+1-a^2))
#    return(pt(M,df=k,lower.tail = FALSE))
#  }
#  myf<-function(a,k){
#    mys(a=a,k=(k-1))-mys(a=a,k=k)
#  }
#  kc=c(4:25,100,500,1000)
#  A=rep(0,length(kc))
#  for(i in 1:length(kc)){
#    A[i]=uniroot(myf,c(0.5,sqrt(kc[i]-1)),k=kc[i])$root
#  }
#  cbind(kc,A)
#  plot(kc,A,type="o")

## ---- echo = FALSE-------------------------------------------------------
f<-function(t,m,n){
1/(m*3.141592653*(1+((t-n)/m)^2))
}

pdf<-function(x,m,n,lower.tail=TRUE){
 if(lower.tail) res<-integrate(f,lower = -Inf,upper = x,rel.tol=.Machine$double.eps^0.25,m=m,n=n)
 else res<-integrate(f,lower = x,upper = Inf,rel.tol=.Machine$double.eps^0.25,m=m,n=n)
  return(res$value)
}
pdf(x=0,m = 1,n = 0)
pcauchy(0,location = 0,scale = 1)
pdf(x=3,m = 2,n =1,lower.tail = F )
pcauchy(3,location = 1,scale = 2,lower.tail = F)

## ---- eval = FALSE-------------------------------------------------------
#  f<-function(t,m,n){
#  1/(m*3.141592653*(1+((t-n)/m)^2))
#  }
#  
#  pdf<-function(x,m,n,lower.tail=TRUE){
#   if(lower.tail) res<-integrate(f,lower = -Inf,upper = x,rel.tol=.Machine$double.eps^0.25,m=m,n=n)
#   else res<-integrate(f,lower = x,upper = Inf,rel.tol=.Machine$double.eps^0.25,m=m,n=n)
#    return(res$value)
#  }
#  pdf(x=0,m = 1,n = 0)
#  pcauchy(0,location = 0,scale = 1)
#  pdf(x=3,m = 2,n =1,lower.tail = F )
#  pcauchy(3,location = 1,scale = 2,lower.tail = F)

## ----echo=FALSE----------------------------------------------------------
        dat <- rbind(Genotype=c('AA','BB','OO','AO','BO','AB'),
                     Frequency=c('p2','q2','r2','2pr','2qr','2pq',1),
                     Count=c('nAA','nBB','nOO','nAO','nBO','nAB','n'))
    knitr::kable(dat,format='markdown',caption = "Comparation of them",align = "c")

## ---- echo = FALSE-------------------------------------------------------
library(nloptr)
f1 <- function(x,x1,n.A=28,n.B=24,nOO=41,nAB=70) {
  return(sum(x)-1)
}

f2 <- function(x,x1,n.A=28,n.B=24,nOO=41,nAB=70) {
  r1<-1-sum(x1)
  nAA<-n.A*x1[1]^2/(x1[1]^2+2*x1[1]*r1)
  nBB<-n.B*x1[2]^2/(x1[2]^2+2*x1[2]*r1)
  r<-1-sum(x)
  return(-2*nAA*log(x[1])-2*nBB*log(x[2])-2*nOO*log(r)-
           (n.A-nAA)*log(2*x[1]*r)-(n.B-nBB)*log(2*x[2]*r)-nAB*log(2*x[1]*x[2]))
}

opts <- list("algorithm"="NLOPT_LN_COBYLA",
             "xtol_rel"=1.0e-5)
mle<-c()
r<-matrix(0,1,2)
r<-rbind(r,c(0.2,0.35))
j<-2
while (sum(abs(r[j,]-r[j-1,]))>1e-5) {
res <- nloptr( x0=c(0.3,0.25),eval_f=f2,lb = c(0,0), ub = c(1,1), eval_g_ineq = f1, opts = opts,x1=r[j,],n.A=28,n.B=24,nOO=41,nAB=70 )
j<-j+1
r<-rbind(r,res$solution)
mle<-c(mle,f2(x=r[j,],x1=r[j-1,]))
}
r  
mle 

## ---- eval = FALSE-------------------------------------------------------
#  library(nloptr)
#  f1 <- function(x,x1,n.A=28,n.B=24,nOO=41,nAB=70) {
#    return(sum(x)-1)
#  }
#  
#  f2 <- function(x,x1,n.A=28,n.B=24,nOO=41,nAB=70) {
#    r1<-1-sum(x1)
#    nAA<-n.A*x1[1]^2/(x1[1]^2+2*x1[1]*r1)
#    nBB<-n.B*x1[2]^2/(x1[2]^2+2*x1[2]*r1)
#    r<-1-sum(x)
#    return(-2*nAA*log(x[1])-2*nBB*log(x[2])-2*nOO*log(r)-
#             (n.A-nAA)*log(2*x[1]*r)-(n.B-nBB)*log(2*x[2]*r)-nAB*log(2*x[1]*x[2]))
#  }
#  
#  opts <- list("algorithm"="NLOPT_LN_COBYLA",
#               "xtol_rel"=1.0e-5)
#  mle<-c()
#  r<-matrix(0,1,2)
#  r<-rbind(r,c(0.2,0.35))
#  j<-2
#  while (sum(abs(r[j,]-r[j-1,]))>1e-5) {
#  res <- nloptr( x0=c(0.3,0.25),eval_f=f2,lb = c(0,0), ub = c(1,1), eval_g_ineq = f1, opts = opts,x1=r[j,],n.A=28,n.B=24,nOO=41,nAB=70 )
#  j<-j+1
#  r<-rbind(r,res$solution)
#  mle<-c(mle,f2(x=r[j,],x1=r[j-1,]))
#  }
#  r
#  mle

## ---- echo = FALSE-------------------------------------------------------
attach(mtcars)
f <- list( mpg ~ disp, mpg ~ I(1 / disp), mpg ~ disp + wt, mpg ~ I(1 / disp) + wt )
x <- vector("list", length(f))
for (i in seq_along(f)) { x[[i]] <-lm(f[[i]]) }
x
lapply(f,lm)

## ---- eval = FALSE-------------------------------------------------------
#  attach(mtcars)
#  f <- list( mpg ~ disp, mpg ~ I(1 / disp), mpg ~ disp + wt, mpg ~ I(1 / disp) + wt )
#  x <- vector("list", length(f))
#  for (i in seq_along(f)) { x[[i]] <-lm(f[[i]]) }
#  x
#  lapply(f,lm)

## ---- echo = FALSE-------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ] })
for(i in seq_along(bootstraps)){
  print(lm(mpg~disp,data =bootstraps[[i]]))
}
lapply(bootstraps,lm,formula=mpg~disp)

## ---- eval = FALSE-------------------------------------------------------
#  bootstraps <- lapply(1:10, function(i) {
#    rows <- sample(1:nrow(mtcars), rep = TRUE)
#    mtcars[rows, ] })
#  for(i in seq_along(bootstraps)){
#    print(lm(mpg~disp,data =bootstraps[[i]]))
#  }
#  lapply(bootstraps,lm,formula=mpg~disp)

## ---- echo = FALSE-------------------------------------------------------
rsq <- function(mod) summary.lm(mod)$r.squared
for (i in seq_along(f)) {
 print( rsq(lm(f[[i]])))
  }
lapply(lapply(f,lm),rsq)
for(i in seq_along(bootstraps)){
  print(rsq(lm(mpg~disp,data =bootstraps[[i]])))
}
lapply(lapply(bootstraps,lm,f=mpg~disp),rsq)

## ---- eval = FALSE-------------------------------------------------------
#  rsq <- function(mod) summary.lm(mod)$r.squared
#  for (i in seq_along(f)) {
#   print( rsq(lm(f[[i]])))
#    }
#  lapply(lapply(f,lm),rsq)
#  for(i in seq_along(bootstraps)){
#    print(rsq(lm(mpg~disp,data =bootstraps[[i]])))
#  }
#  lapply(lapply(bootstraps,lm,f=mpg~disp),rsq)

## ---- echo = FALSE-------------------------------------------------------
trials <- replicate( 100, t.test(rpois(10, 10), rpois(7, 10)), simplify = FALSE )
p_value<-function(mod) mod$p.value
sapply(trials, p_value)

## ---- eval = FALSE-------------------------------------------------------
#  trials <- replicate( 100, t.test(rpois(10, 10), rpois(7, 10)), simplify = FALSE )
#  p_value<-function(mod) mod$p.value
#  sapply(trials, p_value)

## ---- echo = FALSE-------------------------------------------------------
x1=c(rep(1,3),rep(2,4))
x2=c(rep(3,4),rep(4,5))
x=list(x1,x2)
lapplyf=function(f,x){
  t=Map(f,x)
   n<-length(t[[1]])
   y=vapply(t,as.vector,numeric(n))
   return(y)
}
lapplyf(mean,x)
lapplyf(quantile,x)

## ---- eval = FALSE-------------------------------------------------------
#  x1=c(rep(1,3),rep(2,4))
#  x2=c(rep(3,4),rep(4,5))
#  x=list(x1,x2)
#  lapplyf=function(f,x){
#    t=Map(f,x)
#     n<-length(t[[1]])
#     y=vapply(t,as.vector,numeric(n))
#     return(y)
#  }
#  lapplyf(mean,x)
#  lapplyf(quantile,x)

## ---- echo = FALSE-------------------------------------------------------
library("microbenchmark")
my.chisq.test<-function(x,y){
if(!is.vector(x) && !is.vector(y))
stop("at least one of 'x' and 'y' is not a vector")
if(typeof(x)=="character" || typeof(y)=="character")
stop("at least one of 'x' and 'y' is not a numeric vector")
if(any(x<0) || anyNA(x)) 
stop("all entries of 'x' must be nonnegative and finite")
if(any(y<0) || anyNA(y)) 
stop("all entries of 'y' must be nonnegative and finite")
if((n<-sum(x))==0) 
stop("at least one entry of 'x' must be positive")
if((n<-sum(x))==0) 
stop("at least one entry of 'x' must be positive")
if(length(x)!=length(y)) 
stop("'x' and 'y' must have the same length")
DNAME<-paste(deparse(substitute(x)),"and",deparse(substitute(y)))
METHOD<-"Pearson's Chi-squared test"
x<-rbind(x,y)
nr<-as.integer(nrow(x));nc<-as.integer(ncol(x))
sr<-rowSums(x);sc<-colSums(x);n<-sum(x)
E<-outer(sr,sc,"*")/n
STATISTIC<-sum((x - E)^2/E)
names(STATISTIC)<-"X-squared"
structure(list(statistic=STATISTIC,method=METHOD,data.name=DNAME),class="htest")
}
a<-c(365,435,527);b<-c(231,389,453)    
my.chisq.test(a,b)        
chisq.test(rbind(a,b))
microbenchmark(t1=my.chisq.test(a,b),t2=chisq.test(rbind(a,b)))  

## ---- eval = FALSE-------------------------------------------------------
#  library("microbenchmark")
#  my.chisq.test<-function(x,y){
#  if(!is.vector(x) && !is.vector(y))
#  stop("at least one of 'x' and 'y' is not a vector")
#  if(typeof(x)=="character" || typeof(y)=="character")
#  stop("at least one of 'x' and 'y' is not a numeric vector")
#  if(any(x<0) || anyNA(x))
#  stop("all entries of 'x' must be nonnegative and finite")
#  if(any(y<0) || anyNA(y))
#  stop("all entries of 'y' must be nonnegative and finite")
#  if((n<-sum(x))==0)
#  stop("at least one entry of 'x' must be positive")
#  if((n<-sum(x))==0)
#  stop("at least one entry of 'x' must be positive")
#  if(length(x)!=length(y))
#  stop("'x' and 'y' must have the same length")
#  DNAME<-paste(deparse(substitute(x)),"and",deparse(substitute(y)))
#  METHOD<-"Pearson's Chi-squared test"
#  x<-rbind(x,y)
#  nr<-as.integer(nrow(x));nc<-as.integer(ncol(x))
#  sr<-rowSums(x);sc<-colSums(x);n<-sum(x)
#  E<-outer(sr,sc,"*")/n
#  STATISTIC<-sum((x - E)^2/E)
#  names(STATISTIC)<-"X-squared"
#  structure(list(statistic=STATISTIC,method=METHOD,data.name=DNAME),class="htest")
#  }
#  a<-c(365,435,527);b<-c(231,389,453)
#  my.chisq.test(a,b)
#  chisq.test(rbind(a,b))
#  microbenchmark(t1=my.chisq.test(a,b),t2=chisq.test(rbind(a,b)))

## ---- echo = FALSE-------------------------------------------------------
my.table<-function(...,dnn = list.names(...),deparse.level = 1){
    list.names <- function(...) {
        l <- as.list(substitute(list(...)))[-1L]
        nm <- names(l)
        fixup <- if (is.null(nm)) 
            seq_along(l)
        else nm == ""
        dep <- vapply(l[fixup], function(x) switch(deparse.level + 
            1, "", if (is.symbol(x)) as.character(x) else "", 
            deparse(x, nlines = 1)[1L]), "")
        if (is.null(nm)) 
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    args <- list(...)
    if (!length(args)) 
        stop("nothing to tabulate")
    if (length(args) == 1L && is.list(args[[1L]])) {
        args <- args[[1L]]
        if (length(dnn) != length(args)) 
            dnn <- if (!is.null(argn <- names(args))) 
                argn
            else paste(dnn[1L], seq_along(args), sep = ".")
    }
    bin <- 0L
    lens <- NULL
    dims <- integer()
    pd <- 1L
    dn <- NULL
    for (a in args) {
        if (is.null(lens)) 
            lens <- length(a)
        else if (length(a) != lens) 
            stop("all arguments must have the same length")
        fact.a <- is.factor(a)
        if (!fact.a) {
            a0 <- a
            a <- factor(a)
        }
        ll <- levels(a)
        a <- as.integer(a)
        nl <- length(ll)
        dims <- c(dims, nl)
        dn <- c(dn, list(ll))
        bin <- bin + pd * (a - 1L)
        pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin)) 
        bin <- bin + 1L
    y <- array(tabulate(bin, pd), dims, dimnames = dn)
    class(y) <- "table"
    y
}
a=b=c(1,seq(1,5))            
my.table(a,b)
table(a,b)
microbenchmark(t1=my.table(a,b),t2=table(a,b))    

## ---- eval = FALSE-------------------------------------------------------
#  my.table<-function(...,dnn = list.names(...),deparse.level = 1){
#      list.names <- function(...) {
#          l <- as.list(substitute(list(...)))[-1L]
#          nm <- names(l)
#          fixup <- if (is.null(nm))
#              seq_along(l)
#          else nm == ""
#          dep <- vapply(l[fixup], function(x) switch(deparse.level +
#              1, "", if (is.symbol(x)) as.character(x) else "",
#              deparse(x, nlines = 1)[1L]), "")
#          if (is.null(nm))
#              dep
#          else {
#              nm[fixup] <- dep
#              nm
#          }
#      }
#      args <- list(...)
#      if (!length(args))
#          stop("nothing to tabulate")
#      if (length(args) == 1L && is.list(args[[1L]])) {
#          args <- args[[1L]]
#          if (length(dnn) != length(args))
#              dnn <- if (!is.null(argn <- names(args)))
#                  argn
#              else paste(dnn[1L], seq_along(args), sep = ".")
#      }
#      bin <- 0L
#      lens <- NULL
#      dims <- integer()
#      pd <- 1L
#      dn <- NULL
#      for (a in args) {
#          if (is.null(lens))
#              lens <- length(a)
#          else if (length(a) != lens)
#              stop("all arguments must have the same length")
#          fact.a <- is.factor(a)
#          if (!fact.a) {
#              a0 <- a
#              a <- factor(a)
#          }
#          ll <- levels(a)
#          a <- as.integer(a)
#          nl <- length(ll)
#          dims <- c(dims, nl)
#          dn <- c(dn, list(ll))
#          bin <- bin + pd * (a - 1L)
#          pd <- pd * nl
#      }
#      names(dn) <- dnn
#      bin <- bin[!is.na(bin)]
#      if (length(bin))
#          bin <- bin + 1L
#      y <- array(tabulate(bin, pd), dims, dimnames = dn)
#      class(y) <- "table"
#      y
#  }
#  a=b=c(1,seq(1,5))
#  my.table(a,b)
#  table(a,b)
#  microbenchmark(t1=my.table(a,b),t2=table(a,b))

