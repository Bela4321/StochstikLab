#Aufgabe 4
#a)
library(alr4)
install.packages("alr4")

challeng = read.table("Datensaetze/challeng.txt", header = TRUE)
summary(challeng$Temp)

summary(challeng$Temp[challeng$Fail >0])
print(challeng[challeng$Fail >0,])

plot(challeng$Temp, challeng$Fail, xlab = "Temperatur", ylab = "Fehlerrate", main = "Fehlerrate nach Temperatur")
#unter 65°F passiert nur Quatsch-> sollte eig. nicht starten

#b)
challeng["y"] = as.numeric(challeng$Fail>0)
plot(challeng$Temp, challeng$y, xlab = "Temperatur", ylab = "Fehler aufgetreten", main = "Fehler nach Temperatur")
#siehe a) -> Quatsch

#c)
fit.challeng =  lm(y ~ Temp, data = challeng)
summary(fit.challeng)
plot(challeng$Temp, challeng$y, col = rev(rainbow(2))[challeng$y+1], xlab = "Temperatur", ylab = "Fehler aufgetreten", main = "Fehler nach Temperatur")
abline(fit.challeng, col = "red")
abline(0.5, 0, col = "blue")
#draw vertical line at intercection of red and blue line
v = (0.5 - fit.challeng$coefficients[1])/fit.challeng$coefficients[2]
abline(v = v, col = "green")
#da 0.00538 < 0.01 ist, ist die Temperatur signifikant zu 1% Niveau
# rundet man den regessionswert, sehr guter fit.

#d+e)
logist.fit.challeng = glm(y ~ Temp, data = challeng, family = binomial(link = "binomial"))
summary(logist.fit.challeng)
plot(challeng$Temp, challeng$y, col = rev(rainbow(2))[challeng$y+1], xlab = "Temperatur", ylab = "Fehler aufgetreten", main = "Fehler nach Temperatur")
#plot logistic regression line
curve(1/(1+exp(-logist.fit.challeng$coefficients[1] - logist.fit.challeng$coefficients[2]*x)), add = TRUE, col = "red")
abline(0.5, 0, col = "blue")
abline(v = 64.8, col = "green")
#draw vertical line at intercection of red and blue line
#get probability for each temperature
predict(logist.fit.challeng, data.frame(Temp=31), type = "response")

#f)
#i)
probit.fit.challeng = glm(y ~ Temp, data = challeng, family = binomial(link = "probit"))
summary(probit.fit.challeng)
plot(challeng$Temp, challeng$y, col = rev(rainbow(2))[challeng$y+1], xlab = "Temperatur", ylab = "Fehler aufgetreten", main = "Fehler nach Temperatur")
#plot logistic regression line
curve(1/(1+exp(-probit.fit.challeng$coefficients[1] - probit.fit.challeng$coefficients[2]*x)), add = TRUE, col = "red")
abline(0.5, 0, col = "blue")
abline(v = 64.94, col = "green")

#ii)
komploglog.fit.challeng = glm(y ~ Temp, data = challeng, family = binomial(link = "cloglog"))
summary(komploglog.fit.challeng)
plot(challeng$Temp, challeng$y, col = rev(rainbow(2))[challeng$y+1], xlab = "Temperatur", ylab = "Fehler aufgetreten", main = "Fehler nach Temperatur")
#plot logistic regression line
curve(1/(1+exp(-komploglog.fit.challeng$coefficients[1] - komploglog.fit.challeng$coefficients[2]*x)), add = TRUE, col = "red")
abline(0.5, 0, col = "blue")
abline(v = 62.825, col = "green")
