
# Aufagabe 1
## $\chi^2$-Anpassungstest

  set.seed(123)
  #Diese Funktion simuliert die Multinomialverteilung und berechnet die Chi2-Teststatistik
  simulateAnpassungstest <- function(){
    n <- 1000
    p <- c(1/8, 1/4, 1/2, 1/8)
    a <- rmultinom(1, n, p)
    sum((a - n*p)^2/(n*p))
  }
  
  #Wir speichern die Ergebnisse in results
  results <- c()
  #Wir berechnen 300-mal die Teststatistik und speichern sie in results
  for (i in 1:300){
    results =c(results ,simulateAnpassungstest())
  }


  #Wir plotten die Verteilung der Teststatistik 
  #und vergleichen sie mit der Chi2-Verteilung mit 3 Freiheitsgraden
  hist(results, freq=FALSE, main = "Histogramm der Chi2 Werte", xlab = "Chi2-Wert",
       ylab = "relative Häufigkeit", ylim = c(0, 0.25))
  curve(dchisq(x, 3), add = TRUE, col = "red")

## $t-$Test mit unbekannter Varianz

set.seed(123)
mu0 <- 2
sigma <- 4
data <- rnorm(1000, mu0, sigma)
alpha <- 0.05

#Berechne mean und varianz der Daten
dataVar <- var(data)
dataMean <- mean(data)

#Berechne Teststatistik
T <- sqrt(length(data))*(dataMean-mu0)/sqrt(dataVar)

#Teststatistik
print(paste("T = ", T))

#p-Wert von T
print(paste("p-Wert von T = ", pt(T, length(data)-1, lower.tail = FALSE)))

#Testresultat
if (pt(T, length(data)-1, lower.tail = FALSE) > 1-alpha){
  print("Nullhypothese wird verworfen")
} else {
  print("Nullhypothese wird nicht verworfen")
}


#Verteilung der Teststatistik

set.seed(123)
Tdata = c()

#Berechne Teststatistik 300-mal
for (i in 1:300){
  data <- rnorm(1000, mu0, sigma)
  mu0 <- 2
  alpha <- 0.05

  T <- sqrt(length(data))*(mean(data)-mu0)/sqrt(var(data))
  Tdata = c(Tdata, T)
}
#Histogramm der Teststatistik, vergleiche mit t-Verteilung
hist(Tdata, freq=FALSE, main = "Histogramm der T Werte",
     xlab = "T-Wert", ylab = "relative Häufigkeit")
curve(dt(x, length(data)-1), add = TRUE, col = "red")


#Kolmogorov-Smirnoff-Test
print(ks.test(Tdata, "pt", length(data)-1))

#QQ-Plot:

qqnorm(Tdata)
qqline(Tdata)



## $t-$Test mit bekannter Varianz

set.seed(123)
mu0 <- 2
sigma <- 4
data <- rnorm(1000, mu0, sigma)
alpha <- 0.05

dataMean <- mean(data)

T <- sqrt(length(data))*(dataMean-mu0)/sigma

#Teststatistik
print(paste("T = ", T))

#p-Wert von T unter Normalverteilung
print(paste("p-Wert von T = ", pnorm(T, lower.tail = FALSE)))

#Testresultat
if (pnorm(T, lower.tail = FALSE) > 1-alpha){
  print("Nullhypothese wird verworfen")
} else {
  print("Nullhypothese wird nicht verworfen")
}


#Verteilung der Teststatistik

set.seed(123)
Tdata = c()

for (i in 1:300){
  data <- rnorm(1000, mu0, sigma)
  alpha <- 0.05

  T <- sqrt(length(data))*(mean(data)-mu0)/sigma
  Tdata = c(Tdata, T)
}
hist(Tdata, freq=FALSE, main = "Histogramm der T Werte", xlab = "T-Wert", ylab = "relative Häufigkeit")
curve(dnorm(x), add = TRUE, col = "red")

#Kolmogorov-Smirnoff-Test
print(ks.test(Tdata, "pnorm", lower.tail = FALSE))

#QQ-Plot:

qqnorm(Tdata)
qqline(Tdata)



# Aufgabe 2

## Daten einlesen
data <- read.table("Datensaetze/wine.txt", header = TRUE)

## Überblick über die Daten
head(data)
summary(data)

## Lineares Modell erstellen
model <- lm(price ~ temp + h.rain + w.rain, data = data)
summary(model)

## Angepasste Werte berechnen
X = matrix(c(rep(1,length(data$temp)),data$temp,data$h.rain,data$w.rain),ncol=4)
HutMatrix = X%*%solve(t(X)%*%X)%*%t(X)
fittedValuesByHand = HutMatrix%*%data$price

#Vergleichen mit den aus dem linearen Modell berechneten Werten:
plot(fittedValuesByHand, model$fitted.values)
if (all((fittedValuesByHand - model$fitted.values)<0.0001)){
  print("Die beiden Werte sind annähernd gleich")
} else {
  print("Die beiden Werte sind nicht annähernd gleich")
}


## Bestimmtheitsmaß
R2 = 1 - sum((data$price - fittedValuesByHand)^2)/sum((data$price - mean(data$price))^2)
print(paste("R2 = ", R2))

  
  
  
## zufälliger Fehler
sigma.hat.sq = sum((data$price - fittedValuesByHand)^2)/(length(data$price)-4)
print(paste("sigma.hat= ", sqrt(sigma.hat.sq)))


## Erweiterung des linearen Modells
model2 <- lm(price ~ temp + h.rain + w.rain + year, data = data)
summary(model2)


## Vergleich der Güte der Modelle
print(paste("R2 Vergleich: ","old model: ", summary(model)$r.squared,
                            " new model: ", summary(model2)$r.squared))
print(paste("Sigma Vergleich: ","old model: ", summary(model)$sigma,
                              " new model: ", summary(model2)$sigma))

## Residuenanalyse
plot(model$fitted.values, model$residuals, xlab = "fitted values", ylab = "residuals", main = "Residuenanalyse")


## Lineares Log Modell
model3 <- lm(log(price) ~ temp + h.rain + w.rain + year, data = data)
summary(model3)

true.fitted.values = exp(model3$fitted.values)
model3.sigma.hat.sq = sum((data$price - true.fitted.values)^2)/(length(data$price)-4)
print(paste("sigma.hat= ", sqrt(model3.sigma.hat.sq)))

model3.R2=1-sum((data$price-true.fitted.values)^2)/sum((data$price-mean(data$price))^2)
print(paste("R2= ", model3.R2))



### Residuenanalyse
plot(exp(model3$fitted.values), exp(model3$residuals), xlab = "fitted values", ylab = "residuals", main = "Residuenanalyse")


## Vergleich der 3 Modelle
Vergleichstabelle <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  R2 = c(summary(model)$r.squared, summary(model2)$r.squared, model3.R2),
  Adjusted.R2 = c(summary(model)$adj.r.squared, summary(model2)$adj.r.squared,
                  1-(1-model3.R2)*(length(data$price)-1)/(length(data$price)-4)),
  Sigma = c(summary(model)$sigma, summary(model2)$sigma, sqrt(model3.sigma.hat.sq))
)
knitr::kable(Vergleichstabelle)


