#Aufgabe 1
#a)
library(MASS)
data = forbes

plot(data$bp,data$pres)
fit.forbes = lm(pres~bp, data = data)
abline(fit.forbes)
summary(fit.forbes)
#R^2 = 0.9944 -> sehr guter fit

#b)
plot(fit.forbes$fitted.values,fit.forbes$residuals)
library(ggplot2)
#plot residuals in comparison to normal distribution
ggplot(data = as.data.frame(fit.forbes$residuals))+
  geom_histogram(aes(x = fit.forbes$residuals,y = ..density..), bins = 20) +
  stat_function(fun = dnorm, args = list(mean = mean(fit.forbes$residuals), sd = sd(fit.forbes$residuals)), size = 2, col = "red") +
  labs(title = "Residuals in comparison to normal distribution", x = "Residuals", y = "Frequency")


#c)
#log(pres) = log(beta0) + beta1*bp + error
fit.forbes.log = lm(log(pres)~bp, data = data)
summary(fit.forbes.log)
beta0 = exp(fit.forbes.log$coefficients[1])
beta1 = fit.forbes.log$coefficients[2]
errorSD = sqrt(fit.forbes.log$sigma)

#d)
plot(fit.forbes$fitted.values,fit.forbes$residuals)

ggplot(data = as.data.frame(fit.forbes$residuals))+
  geom_histogram(aes(x = fit.forbes$residuals,y = ..density..), bins = 20) +
  stat_function(fun = dnorm, args = list(mean = mean(fit.forbes$residuals), sd = sd(fit.forbes$residuals)), size = 2, col = "red") +
  labs(title = "Residuals in comparison to normal distribution", x = "Residuals", y = "Frequency")

plot(fit.forbes.log$fitted.values,fit.forbes.log$residuals)

ggplot(data = as.data.frame(fit.forbes.log$residuals))+
  geom_histogram(aes(x = fit.forbes.log$residuals, y = ..density..), bins = 20) +
  stat_function(fun = dnorm, args = list(mean = mean(fit.forbes.log$residuals), sd = sd(fit.forbes.log$residuals)), size = 2, col = "red") +
  labs(title = "Residuals in comparison to normal distribution", x = "Residuals", y = "Frequency")
#Modell passt gut
qqnorm(fit.forbes.log$residuals)
#e)
#get max argument
maxArg = which.max(fit.forbes.log$residuals)
print(data[maxArg,])
plot(data$bp,data$pres)
points(data[maxArg,]$bp,data[maxArg,]$pres, col = "red", pch = 16)

#f)
#remove max argument
data2 = data[-maxArg,]
fit.forbes.log2 = lm(log(pres)~bp, data = data2)
summary(fit.forbes.log2)

plot(fit.forbes.log2$fitted.values,fit.forbes.log2$residuals)

ggplot(data = as.data.frame(fit.forbes.log2$residuals))+
  geom_histogram(aes(x = fit.forbes.log2$residuals, y = ..density..), bins = 10) +
  stat_function(fun = dnorm, args = list(mean = mean(fit.forbes.log2$residuals), sd = sd(fit.forbes.log2$residuals)), size = 2, col = "red") +
  labs(title = "Residuals in comparison to normal distribution", x = "Residuals", y = "Frequency")

qqnorm(fit.forbes.log2$residuals)
qqline(fit.forbes.log2$residuals)



#Aufgabe 2
#a)
data = read.table("Datensaetze/softdrink.txt", header = TRUE)

fit.soft = lm(y~x1+x2, data = data)
summary(fit.soft)

plot(data$x1)
plot(data$x2)
#meisten liegen in einem Intervall, einige Ausreißer, die *2 wert haben.

#c)
#i)
plot(fit.soft$residuals)
#ii)
plot(fit.soft$fitted.values,fit.soft$residuals)

#d)
soft.X = data.frame(rep(1, length(data$x1)), data$x1, data$x2)
mat.X = as.matrix(soft.X)
soft.hut = mat.X%*%solve(t(mat.X)%*%mat.X)%*%t(mat.X)

library(matlib)
limit = 2*tr(soft.hut)/length(data$x1)
for (i in 1:length(data$x1)){
  if (soft.hut[i,i] > limit){
    print(i)
  }
}
#9 und 22 sind über dem limit

#e)
epsilon.schlange = fit.soft$residuals/sd(fit.soft$residuals)

plot(epsilon.schlange)
#i) ja, ein Ausreißer, #9

#ii)
r_i = epsilon.schlange/sqrt(1-diag(soft.hut))
quotient = 1/sqrt(1-diag(soft.hut))
plot(quotient)
#ausreißer wie d)

#f)

jackknive.residuals = c()
for (i in 1:length(data$x1)){
  jackknive.residuals[i] = fit.soft$residuals[i]/(sd(fit.soft$residuals[-i])*sqrt(1-soft.hut[i,i]))
}
sigmas = c()
for (i in 1:length(data$x1)){
  sigmas[i] = sd(fit.soft$residuals[-i])
}

#g)
plot(jackknive.residuals/r_i)
#1,4,9,20 fallen auf

#h)
PRESS.residuals = c()
data = as.data.frame(data)
for (i in 1:length(data$x1)){
  PRESS.model = lm(y~x1+x2, data = data[-i,])
  PRESS.residuals[i] = data[i,]$y - predict(PRESS.model, data[i,])
}

#i)
qqnorm(fit.soft$residuals)
qqline(fit.soft$residuals)

#j)
Cook.D = tr(soft.hut)^-1*r_i^2*(diag(soft.hut)/(1-diag(soft.hut)))
plot(Cook.D)
#9 sticht klar heraus, 22 etwas

#k)
library(stats)
#i)
hatvalues(fit.soft)
diag(soft.hut)

#ii)
rstandard(fit.soft)
r_i

#iii)
rstudent(fit.soft)
jackknive.residuals

#iv)
lm.influence(fit.soft)$sigma
sigmas

#v)
cooks.distance(fit.soft)
Cook.D




#visualize data
scale = colorRampPalette(c("cyan", "deeppink3"), space = "rgb")(100)
log.y = log(data$y)
colors = scale[(log.y-min(log.y))/(max(log.y)-min(log.y))*100]
plot(data$x1,data$x2,col = colors, pch = 16)
