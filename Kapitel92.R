#Aufgabe 2.1
#a)
df = data.frame(list(Intervall = 1:15,noOfBacteria = 
            c( 355 ,211 ,197 ,166, 142, 106, 104 ,60 ,56 ,38 ,36 ,32 ,21, 19 ,15)))
library(lattice)
xyplot(noOfBacteria ~ Intervall, data = df, 
       xlab = "Intervall", ylab = "Anzahl Bakterien", 
       main = "Bakterienwachstum")

#nein, eher exponentieller Zerfall

#b) transformieren : log-->Anzahl Bakterien
df$logBacteria = log(df$noOfBacteria)
xyplot(logBacteria ~ Intervall, data = df, 
       xlab = "Intervall", ylab = "Anzahl Bakterien", 
       main = "Bakterienwachstum")

#c)linear regrression
fit = lm(logBacteria ~ Intervall, data = df)
summary(fit)
percentloss = exp(-0.218425)
# %loss = 20%

#Aufgabe 2.2
#a)
renv::install("gamair")
library("gamair")

data("hubble")

df = hubble
summary(df)
head(df)
#b)
xyplot(y~x, data = df)
# spricht für einen linearen zusammenhang, aber mit recht größer varianz

#c)
fit = lm(y~0+x, data = df)
summary(fit)
# geschaetztes beta = 76.581
# Einheit von Beta : (km/s)/MPc =m/(s*kPc) = (30856776*10^9)^-1*s^-1

#d)
fit$coefficients
plot(df$x,df$y)
abline(fit, col = "red")

#e)
#get outlier indices
outliers = which(abs(fit$residuals)>500)
#remove outliers
cleandf = df[-outliers,]
#f)
fit = lm(y~0+x, data = cleandf)
plot(cleandf$x,cleandf$y)
abline(fit, col = "red")

#g)
age1 = 1/fit$coefficients*30856776*10^9
age2 = 1/76.581*30856776*10^9


#Aufgabe 2.3
#a)
# es werden zufallsverteilte daten generiert mit beta0 = 1 und beta1 = 1/2
# anschließend wird eine lineare regression durchgeführt

#b)
simulate = function(n){
    x = seq(-1,1,0.05)
    beta0 = c()
    beta1 = c()
    for (i in 1:n){
        y = 1 + 0.5*x + rnorm(length(x),0,1)
        fit = lm(y~x)
        fitbeta0[i] = fit$coefficients[1]
        beta1[i] = fit$coefficients[2]
    }

    return (list(beta0=beta0,beta1 = beta1))
}
Simulation = simulate(1000)
mean(Simulation$beta0)
mean(Simulation$beta1)
#c)
histogram(Simulation$beta0, main = "Histogramm von beta0", xlab = "beta0")
histogram(Simulation$beta1, main = "Histogramm von beta1", xlab = "beta1")

#d)
var(Simulation$beta0)
var(Simulation$beta1)

#e)
x = seq(-1,1,0.05)
y = 1 + 0.5*x + rnorm(length(x),0,1)
fit = lm(y~x)
summary(fit)

sd(Simulation$beta0)
sd(Simulation$beta1)
#sd +-0.01





#Aufgabe 2.4
#a)
x= c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
y1 = c(1.40, 1.49, 1.43, 1.59, 1.94, 2.00, 1.97, 2.71, 2.55, 3.06)
fit1 = lm(y1~x)
plot(x,y1)
abline(fit1, col = "red")
plot(fit1$residuals,fit1$fitted.values)

#b)
y2 = c(1.66, 1.66, 1.50, 1.56, 1.81, 1.77, 1.64, 2.28, 2.02, 4.26)
fit2 = lm(y1~x)
plot(x,y1)
abline(fit2, col = "red")
plot(fit2$residuals,fit2$fitted.values)


y3 = c(1.95, 1.67, 1.34, 1.32, 1.58, 1.64, 1.70, 2.62, 2.73, 3.61)
fit3 = lm(y1~x)
plot(x,y1)
abline(fit3, col = "red")
plot(fit3$residuals,fit3$fitted.values)

#c)
summary(fit1)
summary(fit2)
summary(fit3)

#d)?


#Aufgabe 2.5
#a+b)
wine = read.table("Datensaetze/wine.txt", header = TRUE)
head(wine)

#c)
fit = lm(wine$price~wine$temp+wine$h.rain+wine$w.rain)
summary(fit)
# temperatur: positiven Einfluss auf Preis
# h.rain: negativen Einfluss auf Preis
# w.rain: positiven Einfluss auf Preis

#d)
X = matrix(c(rep(1,length(wine$temp)),wine$temp,wine$h.rain,wine$w.rain),ncol=4)
HutMatrix = X%*%solve(t(X)%*%X)%*%t(X)
predictedValues = HutMatrix%*%wine$price
#e)
fit$fitted.values

plot(fit$fitted.values, wine$temp)
plot(predictedValues, wine$temp)
abline(fit, col = "red")

