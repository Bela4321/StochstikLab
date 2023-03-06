#Aufgabe 2
#a)
simTest0 = numeric(100000)
simTest1 = numeric(100000)

simTest0 = rnorm(100000,1,2)
simTest1 = rnorm(100000,4,2)

TruePositive = sum(simTest0<=2)/100000
TrueNegative = sum(simTest1>2)/100000
FalsePositive = sum(simTest0>2)/100000
FalseNegative = sum(simTest1<=2)/100000

ggplot() + geom_histogram(aes(x=simTest0), bins=100, fill="blue", alpha=0.5) +
  geom_histogram(aes(x=simTest1), bins=100, fill="red", alpha=0.5) +
  geom_vline(xintercept=2, color="black", linetype="dashed", size=1) +
  theme_bw()

#b)
#i)
simTest0 = numeric(100000)

simTest0 = rexp(100000,2)

for (c in seq(0,100,0.0001)){
  FalseNegative = sum(simTest0>c)/100000
    if (FalseNegative<0.05){
        print(c)
        break
    }
}

#c = 1.504

#ii)
beta = numeric(979)
for (theta in seq(2,100,0.1)){
  simTest0 = rexp(1000,theta)
  beta[theta*10-19] = sum(simTest0<=.504)/1000
}
ggplot() + geom_line(aes(x=seq(2,100,0.1),y=beta), color="blue") +
  theme_bw()


#c)
#i)
simTest = numeric(100000)
for (i in 1:100000){
  x= mean(rnorm(9,40,6))
  if (x<36||x>44){
    simTest[i] = 1
  }
  else{
    simTest[i] = 0
  }
}
beta = sum(simTest)/100000
print(paste("Beta: ",beta))


#ii)
for (i in 1:100000){
  x= mean(rnorm(36,40,6))
  if (x<38||x>42){
    simTest[i] = 1
  }
  else{
    simTest[i] = 0
  }
}
beta = sum(simTest)/100000
print(paste("Beta: ",beta))


betaVec = numeric(199)
for (theta in seq(30,50,0.1)){
  simTest = numeric(1000)
  for (i in 1:1000){
    x= mean(rnorm(theta,40,6))
    if (x<38||x>42){
      simTest[i] = 1
    }
    else{
      simTest[i] = 0
    }
  }
  betaVec[theta*10-299] = sum(simTest)/1000
}
ggplot() + geom_line(aes(x=seq(30,50,0.1),y=betaVec), color="blue") +
  theme_bw()

#d)
mu= 10
sigma = 2


for (n in c(10,50,100)){
  print(n)
  means = numeric(10000)
  sigmas = numeric(10000)
  for (i in 1:10000){
    x = rnorm(n,mu,sigma)
    means[i] = mean(x)
    sigmas[i] = sum((x-means[i])^2)/n
  }
  #plot means distribution
  graphic = ggplot(data.frame(means)) + geom_histogram(aes(x=means), bins=100, fill="blue", alpha=0.5, ) +
    stat_function(fun = dnorm, args = list(mean = mu, sd = sigma/sqrt(n)), color="red") +
    labs(title = paste("Means distribution for n = ", n)) +
    theme_bw()

  print(graphic)

  #plot sigmas distribution
  sigmas = sigmas^2*n/sigma^2
  graphic = ggplot(data.frame(sigmas)) + geom_histogram(aes(x=sigmas), bins=100, fill="blue", alpha=0.5) +
    stat_function(fun = dchisq, args = list(df=n-1), color="red") +
    labs(title = paste("Sigma distribution for n = ", n)) +
    theme_bw()

  print(graphic)
}



#Aufgabe 3

#a) H0= neue Behandlung hat Risiko =0.2
#   H1= neue Behandlung hat Risiko <0.2
# FalsePositive->Neue Behandlung ist besser als alte, es wird aber angenommen, dass sie nicht besser ist (kein Umstieg, obwohl es besser ist)
# FalseNegative->Neue Behandlung ist nicht besser als alte, es wird aber angenommen, dass sie besser ist (Umstieg, obwohl es nicht besser ist)

#b)
#teststatistik: #Komp ist Bin vert. mit p=0.2, n=10 bei p-Wert 0.25:
qbinom(0.25,10,0.2)
#c)
qbinom(0.1,10,0.2)
#wenn 0 Komplikationen auftreten, verwerfen wir die Nullhypothese und H1 wird angenommen

#d)
?binom.test()
binom.test(0,10,0.2,alternative="less")

#e)
notfoundp0.05 = T
for (n in 1:1000){
  test = binom.test(0,n,0.2,alternative="less")
  if (test$p.value<0.05&&notfoundp0.05){
    print(paste("n for p<=0.05: ",n))
    notfoundp0.05 = F
  }
  if (test$p.value<0.01){
    print(paste("n for p<=0.01: ",n))
    break
  }
}

library(MASS)

#a)
?birthwt
dataWhite = birthwt[birthwt$race==1,]
dataBlack = birthwt[birthwt$race==2,]
summary(dataWhite$bwt)
summary(dataBlack$bwt)
#for dataWhite:
ggplot(dataWhite) + geom_histogram(aes(x=bwt,y = ..density..), bins=50, fill="blue", alpha=0.5) +
  geom_density(aes(x=bwt), color="red") +
  theme_bw()
#for dataBlack:
ggplot(dataBlack) + geom_histogram(aes(x=bwt,y = ..density..), bins=50, fill="blue", alpha=0.5) +
  geom_density(aes(x=bwt), color="red") +
  theme_bw()

ggplot() + geom_boxplot(aes(x=1,y=dataWhite$bwt)) +
  geom_boxplot(aes(x=2,y=dataBlack$bwt)) +
  labs(title = "Boxplot of birthweight: left=white, right=black") +
  theme_bw()

#qqnorm
ggplot() + geom_qq(aes(sample=dataWhite$bwt), color="blue") +
  geom_qq(aes(sample=dataBlack$bwt), color="red") +
  labs(title = "QQ-Plot of birthweight: blue=white, red=black") +
  theme_bw()


#c) test for equal variances
var.test(dataWhite$bwt,dataBlack$bwt)

#d)
#t-test
t.test(dataWhite$bwt,dataBlack$bwt, var.equal = T)

#Welch-Test
t.test(dataWhite$bwt,dataBlack$bwt, var.equal = F)

#U-Test
wilcox.test(dataWhite$bwt,dataBlack$bwt)


#e)draw empirical cdf
ggplot() + stat_ecdf(aes(x=dataWhite$bwt), color="blue") +
  stat_ecdf(aes(x=dataBlack$bwt), color="red") +
    labs(title = "Empirical CDF of birthweight: blue=white, red=black") +
    theme_bw()

#kolmogorov-smirnov test
ks.test(dataWhite$bwt,dataBlack$bwt)