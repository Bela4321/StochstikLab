#Aufgabe 1
#a) Birthday Problem

sharedBdayOdds = function(n){
  temp =1
  for (i in 1:(n-1)){
    temp = temp*(365-i)/365
  }
    return(1-temp)
}
for (i in 1:50){
  print(sharedBdayOdds(i))
}

#b) sim Birthday Problem
simSharedBdayOdds = function(n){
  bdays = sample(1:365,n,replace=TRUE)
  return(length(unique(bdays))<n)
}

#run 10000 times
print(mean(sapply(rep(23,10000),simSharedBdayOdds)))

#c)sim Birthday Problem with gaussian distribution
simSharedBdayOddsGauss = function(n){
  bdays = rnorm(n,mean=182,sd=90)
  bdays=round(bdays)
  bdays[bdays<1]=1
  bdays[bdays>365]=365
  return(length(unique(bdays))<n)
}

#run 10000 times with gaussian distribution
print(mean(sapply(rep(23,10000),simSharedBdayOddsGauss)))

#Aufgabe 2
#a) plot distributions

#gamma function of 0.5*N
gamma = function(x){
  if (x%%1==0){
    return (factorial(x-1))
  }
  if (x%%1!=0.5){
  return (NA)
  }
  prod=pi
  while(x>1){
    prod=prod*(x-1)
    x=x-1
  }
  return(prod)
}

#chi2 distribution
chi2 = function(x,k){
  return(1/(2^k))
}

#euler beta function
beta = function(x,y){
  return(gamma(x)*gamma(y)/gamma(x+y))
}


#i) chi2 distribution
chi2 = function(x,k){
    if (x<0){
      return(0)
    }
    return( (exp(-x/2)* x^(k/2-1))/(2^(k/2)*gamma(k/2)))
}
#vector wrapper
chi2vec = function(x,k){
  return(sapply(x,chi2,k))
}
#plot for k=c(1:7,10,15) with rainbow colorscale
plot(seq(0,20,0.1),sapply(seq(0,20,0.1),chi2,k=1),type="l",col=rainbow(15)[1],xlab="x",ylab=sprintf("chi2(x,%s)",setK),main="chi2 distribution")
for (setK in c(2:7,10,15)){
  curve(chi2vec(x,setK),from=0,to=20,col=rainbow(15)[setK],add=TRUE)
}
legend("topright",legend=c(1:7,10,15),col=rainbow(15)[c(1:7,10,15)],lty=1)

#ii)t distribution
t = function(x,k){
  return(gamma((k+1)/2)/(sqrt(k*pi)*gamma(k/2)) * (1+x^2/k)^(-(k+1)/2))
}
#vector wrapper
tvec = function(x,k){
  return(sapply(x,t,k))
}
#plot for k=c(1:5,10) with rainbow colorscale
plot(seq(-5,5,0.1),sapply(seq(-5,5,0.1),t,k=1),type="l",col=rainbow(15)[1],xlab="x",ylab=sprintf("t(x,%s)",setK),ylim = c(0,0.5),main="t distribution")
for (setK in c(2:5,10)){
  curve(tvec(x,setK),from=-5,to=5,col=rainbow(15)[setK],add=TRUE)
}
legend("topright",legend=c(1:5,10),col=rainbow(15)[c(1:5,10)],lty=1)


#iii) F distribution
F = function(x,r,s){
  if (x<=0){
    return(0)
  }
  return((r/s)^(r/2) * x^(r/2-1) / (beta(r/2,s/2)* (1+r/s*x)^((r+s)/2)))
}
#vector wrapper
Fvec = function(x,r,s){
  return(sapply(x,F,r,s))
}
#plot for r=c(2,5,10) and s=c(2,5,10) with rainbow colorscale
plot(seq(0,5,0.1),sapply(seq(0,5,0.1),F,r=2,s=2),type="l",col=rainbow(10)[1],xlab="x",ylab=sprintf("F(x,r,s)"),main="F distribution")
for (temp in 1:9){
  setR = c(2,5,10)[(temp-1)%%3+1]
  setS = c(2,5,10)[floor((temp-1)/3)+1]
  curve(Fvec(x,setR,setS),from=0,to=5,col=rainbow(10)[temp],add=TRUE)
}
legend("topright",legend=c("r=2,s=2","r=2,s=5","r=2,s=10","r=5,s=2","r=5,s=5","r=5,s=10","r=10,s=2","r=10,s=5","r=10,s=10"),col=rainbow(10)[1:9],lty=1)


#b)
#i)plot convergence of chi2 distribution to normal distribution

for (setK in seq(10,100,10)){
  plot(seq(-setK/sqrt(2*setK),setK/sqrt(2*setK),2*setK/(100*sqrt(2*setK))),(sapply(seq(0,2*setK,2*setK/100),chi2,k=setK))*sqrt(2*setK),type="l",col="red",xlab="x",ylab=sprintf("chi2(x,%s)",setK),main="chi2 distribution")
  curve(dnorm(x,mean=0,sd=1),from=-setK/sqrt(2*setK),to=setK/sqrt(2*setK),col="blue",add=TRUE)
}

#ii)plot convergence of t distribution to normal distribution

for (setK in c(seq(2,20,2),100)){
  plot(seq(-5,5,0.1),(sapply(seq(-5,5,0.1),t,k=setK))/2,type="l",col="red",xlab="x",ylab=sprintf("t(x,%s)",setK),main="t distribution")
  curve(dnorm(x,mean=0,sd=1),from=-5,to=5,col="blue",add=TRUE)
}

#Aufgabe 3
#a) wahrscheinlichkeit durch integration
for (k in 1:5){
  print(sprintf("k=%s: %s",k,integrate(dnorm,mean=3,sd=2,3-k*2,3+k*2)$value))
}

#b)visualize sigma and 2 sigma intervall
plot(seq(3-2*4,3+2*4,0.1),dnorm(seq(3-2*4,3+2*4,0.1),mean=3,sd=2),type="l",col="blue",xlab="x",ylab="p(x)",main="normal distribution")
segments(3-2*2,0,3-2*2,dnorm(3-2*2,mean=3,sd=2),col="red")
segments(3+2*2,0,3+2*2,dnorm(3+2*2,mean=3,sd=2),col="red")
segments(3-2*1,0,3-2*1,dnorm(3-2*1,mean=3,sd=2),col="#7FFFD4")
segments(3+2*1,0,3+2*1,dnorm(3+2*1,mean=3,sd=2),col="#7FFFD4")
legend("topright",legend=c("2 sigma","sigma"),col=c("red","#7FFFD4"),lty=1)


#c)sigma intervall wahrscheinlichkeiten sind unabhaengig von mean und sd


#Aufgabe 4
#a)sim dice


#b)
relative6=c()
n=300
sim = sample(1:6,n,replace=TRUE)
for (i in 1:n){
  relative6[i] = sum(sim[1:i]==6)/i
}
plot(1:n,relative6,type="l",col="blue",xlab="n",ylab="h(6)",main="relative frequency of 6")
curve(0*x+1/6,from=1,to=n,col="red",add=TRUE)


#c)
n=1000
sim = sample(1:6,n,replace=TRUE)
for (i in 1:n){
  relative6[i] = sum(sim[1:i]==6)/i
}
plot(1:n,relative6,type="l",col="blue",xlab="n",ylab="h(6)",main="relative frequency of 6")
curve(0*x+1/6,from=1,to=n,col="red",add=TRUE)



#Aufgabe 5
#a)zentraler Grenzwertsatz Bernoulli
p=0.5
for (n in c(10,50,100)){
  var=n*p*(1-p)
  plot((1:n-n*p)/sqrt(var),dbinom(1:n,n,p)*sqrt(var),type="l")
  curve(dnorm(x,mean=0,sd=1),col="red",add=TRUE)
}
#b)zentraler Grenzwertsatz exponential verteilte variablen
dexp = function(x,lambda){
  return(lambda*exp(-lambda*x))
}
lambda=1
