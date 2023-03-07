#Aufgabe 1
#a)
library(MASS)
data = hills

# Model: time = beta0 +beta1*dist+beta2*climb + epsilon
# epsilon ~ N(0,sigma^2), unabhängig

fit.hills = lm(time~dist+climb, data = data)
summary(fit.hills)

#b)
# by hand
# Coefficients:
fit.function = function(beta){
    residual = sum((hills$time - beta[1] - beta[2]*hills$dist - beta[3]*hills$climb)^2)
    return(residual)
}
start.par.hills = c(0,0,0)
optim.hills = optim(start.par.hills, fit.function, method = "L-BFGS-B")

X = matrix(c(rep(1,length(hills$dist)), hills$dist, hills$climb), ncol = 3)

#calc y hat
y.hat = X%*%optim.hills$par





#residuals stats
residuals.hills = (hills$time - optim.hills$par[1] - optim.hills$par[2]*hills$dist - optim.hills$par[3]*hills$climb)^2
summary(residuals.hills)

#Coefficients matrix
#Std. Error
Fehlervarianz = 1/(length(hills$dist)-3)*sum(residuals.hills)

se.beta0 = sqrt(Fehlervarianz*solve(t(X)%*%X)[1,1])
se.beta1 = sqrt(Fehlervarianz*solve(t(X)%*%X)[2,2])
se.beta2 = sqrt(Fehlervarianz*solve(t(X)%*%X)[3,3])
Std.Error = c(se.beta0, se.beta1, se.beta2)

#t value
t.beta0 = optim.hills$par[1]/se.beta0
t.beta1 = optim.hills$par[2]/se.beta1
t.beta2 = optim.hills$par[3]/se.beta2
t.value = c(t.beta0, t.beta1, t.beta2)

#Pr(>|t|)
p.beta0 = 2*pt(-abs(t.beta0), df = length(hills$dist)-3)
p.beta1 = 2*pt(-abs(t.beta1), df = length(hills$dist)-3)
p.beta2 = 2*pt(-abs(t.beta2), df = length(hills$dist)-3)
p.value = c(p.beta0, p.beta1, p.beta2)

print(data.frame(Std.Error, t.value, p.value))

#Residual standard error
residual.std.error = sqrt(Fehlervarianz)
print(paste("Residual standard error: ", residual.std.error, "on ", length(hills$dist)-3, "degrees of freedom"))

# multiple R^2
R2 = 1- sum(residuals.hills)/sum((hills$time - mean(hills$time))^2)

# adjusted R^2
R2.adjusted = 1-(length(hills$dist)-1)/(length(hills$dist)-3)*(1-R2)


# F statistic
F = (sum((y.hat-mean(hills$time))^2)*(length(hills$dist)-3))/((3-1)*sum(residuals.hills))
#DF = c(3-1,length(hills$dist)-3)
F.p.value = pf(F, df1 = 3-1, df2 = length(hills$dist)-3, lower.tail = FALSE)









#Aufgabe 3
#a) 
df.study = data.frame(list(Lernzeit=c(20, 16, 34, 23, 27, 32, 18, 22),Punkte = c(64, 61, 84, 70, 88, 92, 72, 77)))

fit.study = lm(Punkte~Lernzeit, data = df.study)
summary(fit.study)
plot(df.study$Lernzeit,df.study$Punkte)
abline(fit.study, col = "red")

#b)
summary(fit.study)
#Pr(>|t|) <0.01 -> Lernzeit ist zu 1% Niveau signifikant


#c)
y.30.hat = predict(fit.study, data.frame(Lernzeit = 30))

#d) 
# predict(fit.study, data.frame(Lernzeit = 30), interval = "confidence", level = 0.9)

Fehlervarianz.study = 1/(length(df.study$Lernzeit)-2)*sum((df.study$Punkte - fit.study$fitted.values)^2)

study.X = matrix(c(rep(1,length(df.study$Lernzeit)), df.study$Lernzeit), ncol = 2)

radius.conf = qt(0.95,df = length(df.study$Lernzeit)-2)*
    sqrt(Fehlervarianz.study*c(1,30)%*%solve(t(study.X)%*%study.X)%*%c(1,30))

lwr.bound.conf = y.30.hat-radius.conf
upr.bound.conf = y.30.hat+radius.conf

# Prädikationsintervall
# predict(fit.study, data.frame(Lernzeit = 30), interval = "prediction", level = 0.9)

radius.präd = qt(0.95,df = length(df.study$Lernzeit)-2)*
    sqrt(Fehlervarianz.study*(1+c(1,30)%*%solve(t(study.X)%*%study.X)%*%c(1,30)))

lwr.bound.präd = y.30.hat-radius.präd
upr.bound.präd = y.30.hat+radius.präd
