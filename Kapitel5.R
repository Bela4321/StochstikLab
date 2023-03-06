
#Aufgabe 1
#a) read in tax.dat
tax = read.table("Datensaetze/tax.dat",header=TRUE)
summary(tax)

#b) pie chart
par(mfrow=c(2,2))
for (i in 1:ncol(tax)){
  pie(tax[1:(length(tax[,1])-1),i],labels=rownames(tax),main=colnames(tax)[i])
}
#c)relative tax
for (i in c(1,3)){
  for (j in 1:nrow(tax)){
    pie(c(tax[j,i]-tax[j,i+1],tax[j,i+1]),labels=c("after tax","tax"),main=paste(rownames(tax)[j],colnames(tax)[i],"\nProzent: ",round(tax[j,i+1]/tax[j,i]*100,1),"%"))
  }
}

#d) barplot
par(mfrow=c(1,1))
barplot(c(rbind(c(tax[,1]-tax[,2],tax[,3]-tax[,4]),c(tax[,2],tax[,4]))) ,col=rainbow(12)[floor(seq(1,13,0.5))] , beside=TRUE,legend=c(sapply(rownames(tax),paste,...="1974"),sapply(rownames(tax),paste,...="1978")),main="incomes and taxes")

barplot(c(rbind(tax[,2],tax[,4])) ,col=rainbow(6)[floor(seq(1,7,0.5))] , beside=TRUE,legend=rownames(tax),main="taxes 1974 vs 78")
#bevorzuge Kreisdiagramme, da sie die relativen Anteile besser darstellen

#e) progressive tax, von 1974 zu 1978 haben viele Leute ihr Einkommen erhöht, (vermutlich) weniger Leute in niedriger Einkommensgruppe.


#Aufgabe 2
#a) #x~U(1,2)
# P(y<=1.4)=0.4
# Dichte bei x=1.4 =>1
# unteres Quartil = 1.25
# oberes Quartil = 1.75

#b)
emp.quantile = function(x,p=seq(0,1,0.25)){
  return(sort(x)[floor(p*length(x))+1])
}

for (i in 1:50){
  x= sample(1:1000,125,replace=TRUE)
  p= seq(0,1,0.07324)
  if (any(emp.quantile(x,p)!=quantile(x,p,type=2))){
    print("false")
  }
}

#c)
x=runif(20,1,2)
p=c(0.25,0.75)
print(emp.quantile(x,p))
for (n in c(100,1000)){
  x=runif(n,1,2)
  print(emp.quantile(x,p))
}

#d)
qa= function(x){
  return(emp.quantile(x,0.75)-emp.quantile(x,0.25))
}

for (n in c(100,1000)){
  x=runif(n,1,2)
  print(qa(x))
}

approxNormalSd = function(x){
  emp.sd= sd(x)
  IQR = qa(x)
  IQR.sd = IQR/(0.6744*2)
  weight=0.5

  return(weight*emp.sd+(1-weight)*IQR.sd)
}

simNorm = function(n){
  x=rnorm(n,mean=0,sd=1)
  approxNormalSd(x)
}

schaetzer= c()
for (k in 1:10000){
  schaetzer[k]= simNorm(100)
}
print(paste("Mittelwert: ",mean(schaetzer),"\nVarianz: ",var(schaetzer)))
hist(schaetzer)


#Aufgabe 3
#a)
n=100
x= rnorm(n,1,2)
hist(x,freq = F)
curve(dnorm(x,1,2),add=T,col="red")
lines(density(x),col="blue",type="p")

#b)
simChiSq = function(n,k){
  result = c()
  for (i in 1:n){
    x= rnorm(k)
    result[i]= sum(x^2)
  }
  return(result)
}

simt = function(n,k){
  result = c()
  for (i in 1:n){
    x= rnorm(k)
    result[i]= rnorm(1)/sqrt(sum(x^2)/n)
  }
  return(result)
}

for (k in c(3,5,7)){
  x= simChiSq(1000,k)
  hist(x,freq = F,main=paste("Chi^2 mit k=",k))
  curve(dchisq(x,k),add=T,col="red")
  meanChiSq = mean(x)
  Mom2ChiSq = mean(x^2)
  theoMean = k
  theoMom2ChiSq = 2*k+k^2
  print(paste("Chi2 mit k=",k,":"))
  print(paste("TheoMean: ",theoMean," Mean: ",meanChiSq))
  print(paste("TheoMom2: ",theoMom2ChiSq," Mom2: ",Mom2ChiSq))
}

for (k in c(1,3,5,7)){
  x= simt(1000,k)
  hist(simt(1000,k),freq = F,main=paste("Chi^2 mit k=",k))
  curve(dt(x,k),add=T,col="red")
  meant= mean(x)
  Mom2t = mean(x^2)
  theoMean = 0
  theoMom2t = k/(k-2)
  print(paste("t mit k=",k,":"))
  print(paste("TheoMean: ",theoMean," Mean: ",meant))
  print(paste("TheoMom2: ",theoMom2t," Mom2: ",Mom2t))
}
#t verteilung wird falsch simuliert??

#c)
xnorm= rnorm(50,1,1)
xuni = runif(50,1-sqrt(3),1+sqrt(3))
xexp = rexp(50)

boxplot(xnorm,xuni,xexp,main="Boxplot",names=c("Normal","Uniform","Exponential"))

#d)
#i)
for (n in c(5,15)){
  qqnorm(rnorm(n),main=paste("Normal mit n=",n))
}

#ii)
par(mfrow=c(2,2))
for (n in c(20,40,100)){
  qqnorm(rnorm(n),main=paste("Normal mit n=",n))
  qqnorm(runif(n),main=paste("Uniform mit n=",n))
  qqnorm(rexp(n),main=paste("Exponential mit n=",n))
  qqnorm(rt(n,2),main=paste("t mit n=",n))
}


#e)
library("MASS")
hills
plot(sort(hills$climb))
qqplot(rexp(35),sort(hills$climb))

plot(sort(hills$dist))
qqplot(rexp(35),sort(hills$dist))

plot(sort(hills$time))
qqplot(rexp(35),sort(hills$time))





library(ggplot2)

#qplot Aufgaben
#3b)
simChiSq = function(n,k){
  result = c()
  for (i in 1:n){
    x= rnorm(k)
    result[i]= sum(x^2)
  }
  return(result)
}

simt = function(n,k){
  result = c()
  for (i in 1:n){
    x= rnorm(k)
    result[i]= rnorm(1)/sqrt(sum(x^2)/n)
  }
  return(result)
}

for (k in c(3,5,7,11)){
  x= simChiSq(1000,k)
  ggplot(data.frame(x),aes(x)) +
    geom_histogram(aes(y=..density..),binwidth=0.5) +
    ggtitle(paste("Chi^2 mit k=",k))+
    stat_function(fun = dchisq, args = list(df = k),col="red")

  meanChiSq = mean(x)
  Mom2ChiSq = mean(x^2)
  theoMean = k
  theoMom2ChiSq = 2*k+k^2
  print(paste("Chi2 mit k=",k,":"))
  print(paste("TheoMean: ",theoMean," Mean: ",meanChiSq))
  print(paste("TheoMom2: ",theoMom2ChiSq," Mom2: ",Mom2ChiSq))
}

for (k in c(1,3,5,7)){
  x= simt(1000,k)
    ggplot(data.frame(x),aes(x)) +
        geom_histogram(aes(y=..density..),binwidth=0.5) +
        ggtitle(paste("t mit k=",k))+
        stat_function(fun = dt, args = list(df = k),col="red")
  meant= mean(x)
  Mom2t = mean(x^2)
  theoMean = 0
  theoMom2t = k/(k-2)
  print(paste("t mit k=",k,":"))
  print(paste("TheoMean: ",theoMean," Mean: ",meant))
  print(paste("TheoMom2: ",theoMom2t," Mom2: ",Mom2t))
}




#c)
xnorm= rnorm(50,1,1)
xuni = runif(50,1-sqrt(3),1+sqrt(3))
xexp = rexp(50)
df = data.frame(xnorm,xuni,xexp)

#3 boxplot in 3 fascets
ggplot(df) +
  geom_boxplot(aes(x=1,y=xnorm)) +
  geom_boxplot(aes(x=2,y=xuni)) +
  geom_boxplot(aes(x=3,y=xexp)) +
  scale_x_continuous(breaks=c(1,2,3),labels=c("Normal","Uniform","Exponential")) +
  ggtitle("Boxplot")

#d)
#i)
for (n in c(5,15)){
    ggplot(data.frame(rnorm(n)),aes(sample=rnorm(n))) +
        stat_qq() +
        ggtitle(paste("Normal mit n=",n))
}

#ii)
par(mfrow=c(1,1))
for (n in c(20,40,100)){
  xnorm = rnorm(n)
  xuni = runif(n)
  xexp = rexp(n)
  xt = rt(n,2)
  df = data.frame(xnorm,xuni,xexp,xt)
  ggplot(df) +
    stat_qq(aes(sample=xnorm,color="red")) +
    stat_qq(aes(sample=xuni, color="blue")) +
    stat_qq(aes(sample=xexp,color = "#800080")) +
    stat_qq(aes(sample=xt,color= "#00FF00")) +
    ggtitle(paste("Normal mit n=",n))
}

