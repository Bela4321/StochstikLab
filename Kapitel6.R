#Aufgabe 1
#a+b)
m=1000
n=12
p=0.7
#i) konstanter Schaetzer
constschaetzer = function(n,p){
  schaetzerliste= c()
  for (i in 1:m){
    x=rbinom(n,1,p)
    schaetzer=0.9
    schaetzerliste[i]=schaetzer
  }
  return(schaetzerliste)
}

#ii) produkt schaetzer
prodSchaetzer = function (n,p){
  schaetzerliste= c()
  for (i in 1:m){
    x=rbinom(n,1,p)
    schaetzer=prod(x)
    schaetzerliste[i]=schaetzer
  }
  return(schaetzerliste)
}

#iii)mean+4 schaetzer
mean4Schaetzer = function (n,p){
  schaetzerliste= c()
  for (i in 1:m){
    x=rbinom(n,1,p)
    schaetzer=(sum(x)+2)/(n+4)
    schaetzerliste[i]=schaetzer
  }
  return(schaetzerliste)
}

#iv)mean schaetzer
meanSchaetzer = function (n,p){
  schaetzerliste= c()
  for (i in 1:m){
    x=rbinom(n,1,p)
    schaetzer=mean(x)
    schaetzerliste[i]=schaetzer
  }
  return(schaetzerliste)
}


#v) first+last quater mean schaetzer
firstlastSchaetzer = function (n,p){
  schaetzerliste= c()
  for (i in 1:m){
    x=rbinom(n,1,p)
    schaetzer=(mean(x[1:floor(n/4)])+mean(x[(n-floor(n/4)):n]))/2
    schaetzerliste[i]=schaetzer
  }
  return(schaetzerliste)
}

hist(constschaetzer(n,p),breaks = seq(0,1,0.01), main="Histogramm der konstanten Schaetzer", xlab="Schaetzer", ylab="Haeufigkeit")
hist(prodSchaetzer(n,p),breaks = seq(0,1,0.01), main="Histogramm der produkt Schaetzer", xlab="Schaetzer", ylab="Haeufigkeit")
hist(mean4Schaetzer(n,p),breaks = seq(0,1,0.01), main="Histogramm der mean+4 Schaetzer", xlab="Schaetzer", ylab="Haeufigkeit")
hist(meanSchaetzer(n,p),breaks = seq(0,1,0.01), main="Histogramm der mean Schaetzer", xlab="Schaetzer", ylab="Haeufigkeit")
hist(firstlastSchaetzer(n,p),breaks = seq(0,1,0.01), main="Histogramm der first+last quater mean Schaetzer", xlab="Schaetzer", ylab="Haeufigkeit")


#c)
#konstanter Schaetzer
s = constschaetzer(n,p)
sMean = mean(s)
sVar = var(s)
print("konstanter Schaetzer")
print(paste("Mittelwert: ", sMean))
print(paste("Varianz: ", sVar))

#produkt Schaetzer
s = prodSchaetzer(n,p)
sMean = mean(s)
sVar = var(s)
print("produkt Schaetzer")
print(paste("Mittelwert: ", sMean))
print(paste("Varianz: ", sVar))

#mean+4 Schaetzer
s = mean4Schaetzer(n,p)
sMean = mean(s)
sVar = var(s)
print("mean+4 Schaetzer")
print(paste("Mittelwert: ", sMean))
print(paste("Varianz: ", sVar))

#mean Schaetzer
s = meanSchaetzer(n,p)
sMean = mean(s)
sVar = var(s)
print("mean Schaetzer")
print(paste("Mittelwert: ", sMean))
print(paste("Varianz: ", sVar))

#first+last quater mean Schaetzer
s = firstlastSchaetzer(n,p)
sMean = mean(s)
sVar = var(s)
print("first+last quater mean Schaetzer")
print(paste("Mittelwert: ", sMean))
print(paste("Varianz: ", sVar))


#mean schaetzer wirkt am geeignetsten

    #d)
for (n in c(24,100)){
  print("")
  print(paste("n=",n))
  hist(constschaetzer(n, p), breaks = seq(0, 1, 0.01), main = "Histogramm der konstanten Schaetzer", xlab = "Schaetzer", ylab = "Haeufigkeit")
  hist(prodSchaetzer(n, p), breaks = seq(0, 1, 0.01), main = "Histogramm der produkt Schaetzer", xlab = "Schaetzer", ylab = "Haeufigkeit")
  hist(mean4Schaetzer(n, p), breaks = seq(0, 1, 0.01), main = "Histogramm der mean+4 Schaetzer", xlab = "Schaetzer", ylab = "Haeufigkeit")
  hist(meanSchaetzer(n, p), breaks = seq(0, 1, 0.01), main = "Histogramm der mean Schaetzer", xlab = "Schaetzer", ylab = "Haeufigkeit")
  hist(firstlastSchaetzer(n, p), breaks = seq(0, 1, 0.01), main = "Histogramm der first+last quater mean Schaetzer", xlab = "Schaetzer", ylab = "Haeufigkeit")

  s = constschaetzer(n, p)
  sMean = mean(s)
  sVar = var(s)
  print("konstanter Schaetzer")
  print(paste("Mittelwert: ", sMean))
  print(paste("Varianz: ", sVar))

  #produkt Schaetzer
  s = prodSchaetzer(n, p)
  sMean = mean(s)
  sVar = var(s)
  print("produkt Schaetzer")
  print(paste("Mittelwert: ", sMean))
  print(paste("Varianz: ", sVar))

  #mean+4 Schaetzer
  s = mean4Schaetzer(n, p)
  sMean = mean(s)
  sVar = var(s)
  print("mean+4 Schaetzer")
  print(paste("Mittelwert: ", sMean))
  print(paste("Varianz: ", sVar))

  #mean Schaetzer
  s = meanSchaetzer(n, p)
  sMean = mean(s)
  sVar = var(s)
  print("mean Schaetzer")
  print(paste("Mittelwert: ", sMean))
  print(paste("Varianz: ", sVar))

  #first+last quater mean Schaetzer
  s = firstlastSchaetzer(n, p)
  sMean = mean(s)
  sVar = var(s)
  print("first+last quater mean Schaetzer")
  print(paste("Mittelwert: ", sMean))
  print(paste("Varianz: ", sVar))
}


#e+f)
simNormSchaetzer= function(m,n){
  schaetzerliste=c()
  for (i in 1:m){
    x= rnorm(n,5,1)
    schaetzer = mean(x)
    schaetzerliste[i]=schaetzer
  }
  hist(schaetzerliste, main = paste("Mean Schaetzer mit m=",m," und n=",n), xlab = "Schaetzer", ylab = "Haeufigkeit",freq = F)
  lines(density(schaetzerliste), col="red")
}
simNormSchaetzer(100,5)

#g)
for (m in c(100,200)){
  for (n in c(5,10)){
    print("--------------------------------")
    print(paste("m=",m,"n=",n))
    simNormSchaetzer(m,n)
  }
}

#Aufgabe 2
#a)
summary(faithful)

#b)
faunder = faithful[faithful$waiting<67,]
faover = faithful[faithful$waiting>=67,]

print(paste("Mean unter 67 Minuten: ",mean(faunder$waiting)))
print(paste("Mean ueber 67 Minuten: ",mean(faover$waiting)))
print(paste("St dv unter 67 Minuten: ",sd(faunder$waiting)))
print(paste("St dv ueber 67 Minuten: ",sd(faover$waiting)))

qqnorm(faunder$waiting)
qqnorm(faover$waiting)
qqnorm(faithful$waiting)
qqline(faithful$waiting)
plot(density(faithful$waiting), col="red")

#ja, ist sinnvoll


#c)
twoCompModelLkelyhood = function(par){
  mu1 = par[1]
  mu2 = par[2]
  sigma1 = par[3]
  sigma2 = par[4]
  p = par[5]
  x=faithful$waiting
  #plotModel(par)
  return(sum(log((p*dnorm(x, mu1, sigma1) + (1-p)*dnorm(x, mu2, sigma2)))))
}
#find ML-Schaetzer
ML=optim(par=c(20,100,10,10,0.3), fn=twoCompModelLkelyhood,
         method="L-BFGS-B",
         lower = c(0.001,0.001,0.001,0.001,0.2),
         upper = c(1000,1000,1000,1000,0.8),
         control = list("fnscale"=-1))

modellDensity = function(x,param){
    mu1 = param[1]
    mu2 = param[2]
    sigma1 = param[3]
    sigma2 = param[4]
    p = param[5]
    return(p*dnorm(x, mu1, sigma1) + (1-p)*dnorm(x, mu2, sigma2))
}

plotModel = function (param) {
  hist(faithful$waiting, freq = F, main = "Histogramm der waiting times", xlab = "waiting time", ylab = "Haeufigkeit")
  lines(1:120, modellDensity(1:120,param=param), col="red")
}
plotModel(ML$par)

integrate(modellDensity, lower = 0, upper = 120, param = ML$par)


#Aufgabe 4
#a+b)
x= rnorm(30,3,sqrt(2))
mean= mean(x)
MSE = sum((x-mean)^2)/30
evar = var(x)

#c+d)
meanlist = c()
MSElist = c()
evarlist = c()
for (i in 1:1000){
  x= rnorm(30,3,sqrt(2))
  mean= mean(x)
  MSE = sum((x-mean)^2)/30
  evar = var(x)
  meanlist[i]=mean
  MSElist[i]=MSE
  evarlist[i]=evar
}
hist(meanlist, main = paste("Histogramm der Mittelwerte\n mean=",mean(meanlist)," Variance=",var(meanlist)), xlab = "Mittelwert", ylab = "Haeufigkeit",freq = F)
curve(dnorm(x,3,sqrt(2/30)), add = T, col="red")
hist(MSElist, main = paste("Histogramm der MSE\n mean=",mean(MSElist)," Variance=",var(MSElist)), xlab = "MSE", ylab = "Haeufigkeit",freq = F)
curve(dchisq(x,29,30/2), add = T, col="red")
hist(evarlist, main = paste("Histogramm der Varianzen\n mean=",mean(evarlist)," Variance=",var(evarlist)), xlab = "Varianz", ylab = "Haeufigkeit",freq = F)







