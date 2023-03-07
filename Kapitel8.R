library(lattice)
library(MASS)

#Aufgabe 2
n= 500
x05 = mvrnorm(n, mu=c(0,0), Sigma=matrix(c(1,0.5,0.5,1),2,2))
xyplot(x05[,1]~x05[,2],type="p",pch=19)
cor(x05[,1],x05[,2],method="pearson")

n=20
n=50
x0 = mvrnorm(n, mu=c(0,0), Sigma=matrix(c(1,0,0,1),2,2))
x0 = cbind(0,x0)
x03 = mvrnorm(n, mu=c(0,0), Sigma=matrix(c(1,0.3,0.3,1),2,2))
x03 = cbind(rep(0.3,n),x03)
x09 = mvrnorm(n, mu=c(0,0), Sigma=matrix(c(1,0.9,0.9,1),2,2))
x09 = cbind(0.9,x09)
xn07 = mvrnorm(n, mu=c(0,0), Sigma=matrix(c(1,-0.7,-0.7,1),2,2))
xn07 = cbind(0.7,xn07)

x = rbind(x0,x03,x09,xn07)
#chage header
colnames(x) = c("rho","X","Y")
x = as.data.frame(x)
#plot
xyplot(Y~X|rho,data=x)
#get empirical correlation
print("Actual Cor = 0, Empirical correlation:")
cor(x0[,2],x0[,3],method="pearson")
print("Actual Cor = 0.3, Empirical correlation:")
cor(x03[,2],x03[,3],method="pearson")
print("Actual Cor = 0.9, Empirical correlation:")
cor(x09[,2],x09[,3],method="pearson")
print("Actual Cor = -0.7, Empirical correlation:")
cor(xn07[,2],xn07[,3],method="pearson")


#Aufgabe 1
#a)
data = Titanic
mosaicplot(~Class+Survived+Sex,data=data)

df = as.data.frame(data)
#barchart by age and survived, colored by sex
barchart(Class~Freq|Sex+Age, data = df, groups = Survived,
         stack = T, layout = c(4,1), autokey = list(title = "Survived", columns = 2),
         scales = list(x="free"))

#b)

library(mlmRev)
library(lme4)
library(Matrix)

head(Chem97)

xyplot(score~gcsecnt,data = Chem97,groups = gender,pch=19,alpha=0.1)

densityplot(~gcsecnt|score,data = Chem97,layout= c(1,6))
histogram(~gcsecnt|score,data = Chem97,layout= c(1,6))

#plot densities by score in same graph
densityplot(~gcsecnt,data = Chem97,groups = score, auto.key = T, plot.points = F)


#c)
head(barley)
dotplot(yield~variety|site,data = barley,groups = year,auto.key = T)
#nix

#d)
head(oats)
?oats

xyplot(Y~N|B+V,data = oats,auto.key = T)



#Aufgabe 3
#a)
#read in csv
data = read.csv("Datensaetze/wuermer.csv",header=TRUE,sep=",")
#Blei
xyplot(Pb~Ind.Masse,data=data)
cor(data$Pb,data$Ind.Masse,method="pearson")
#Cadium
xyplot(Cd~Ind.Masse,data=data)
cor(data$Cd,data$Ind.Masse,method="pearson")

#Blei nach Fanggebiet
xyplot(Pb~Ind.Masse|Gruppe,data=data)
#get correlation for each group
print("Blei nach Fanggebiet")
print("Group A:")
cor(data$Pb[data$Gruppe=="A"],data$Ind.Masse[data$Gruppe=="A"],method="pearson")
print("Group B:")
cor(data$Pb[data$Gruppe=="B"],data$Ind.Masse[data$Gruppe=="B"],method="pearson")
print("Group C:")
cor(data$Pb[data$Gruppe=="C"],data$Ind.Masse[data$Gruppe=="C"],method="pearson")
print("Group D:")
cor(data$Pb[data$Gruppe=="D"],data$Ind.Masse[data$Gruppe=="D"],method="pearson")
#Cadium nach Fanggebiet
xyplot(Cd~Ind.Masse|Gruppe,data=data)
#get correlation for each group
print("Cadium nach Fanggebiet")
print("Group A:")
cor(data$Cd[data$Gruppe=="A"],data$Ind.Masse[data$Gruppe=="A"],method="pearson")
print("Group B:")
cor(data$Cd[data$Gruppe=="B"],data$Ind.Masse[data$Gruppe=="B"],method="pearson")
print("Group C:")
cor(data$Cd[data$Gruppe=="C"],data$Ind.Masse[data$Gruppe=="C"],method="pearson")
print("Group D:")
cor(data$Cd[data$Gruppe=="D"],data$Ind.Masse[data$Gruppe=="D"],method="pearson")

#Bei Blei gibt es Fanggebietintern für A,C keine Korrelationen, bei B,C leichte.
#Auf alle Gruppen bezogen, lässt sich aber fälschnicherweise eine leichte Korrelation vermuten.

#bei Cadium kann man auf alle Gruppen bezogen von einer mittelstarken Korrelation ausgehen,
#nach gruppen aufgezteilt zeigt sich eine starke Korrelation in Gruppen B,D, aber keine in Gruppen A,C.


#b)
n1=200

library(MASS)
x_function=mvrnorm(n1, mu=c(0,0), Sigma=matrix(c(1,-0.2,-0.2,1),2,2))

#x_manual
N1= rnorm(n1,0,1)
N2= rnorm(n1,0,1)

a = sqrt(1-0.2^2)
x_manual=cbind(N1,a*N2-0.2*N1)

#plot x_function
xyplot(x_function[,1]~x_function[,2],type="p",pch=19)
#plot x_manual
xyplot(x_manual[,1]~x_manual[,2],type="p",pch=19)


v1 = mvrnorm(200, mu=c(0,0), Sigma=matrix(c(1,-0.2,-0.2,1),2,2))
v2 = mvrnorm(200, mu=c(3,3), Sigma=matrix(c(1,-0.2,-0.2,1),2,2))

vGes = rbind(v1,v2)
cor(vGes[,1],vGes[,2],method="pearson")
cor(vGes[,1],vGes[,2],method="spearman")
cor.test(vGes[,1],vGes[,2],method="pearson")
cor.test(vGes[,1],vGes[,2],method="spearman")

#resultat: Korrelation = 0 sehr unwahrscheinlich, Trugschluss durch 2 verschiedene Verteilungen

#c)

data = read.csv("Datensaetze/storch.csv",header=TRUE,sep=",")
#i)
xyplot(Geburtenrate~Stoerche,data=data)
cor(data$Geburtenrate,data$Stoerche,method="pearson")
cor.test(data$Geburtenrate,data$Stoerche,method="pearson")# korrelation = 0 unwahrscheinlich
# Es gibt einige Ausreißer, die Sehr hohe Storchanzahl und Geburtensrate haben, so entsteht eine hohe Korrelation.
#ii)
cor(data$Geburtenrate,data$Stoerche,method="spearman")
cor.test(data$Geburtenrate,data$Stoerche,method="spearman")# korrelation = 0 möglich
# Hier werden Ausreißer nicht so stark gewichtet, sodass eine Korrelation von 0 möglich ist.

#iii)
GebPerPeop = data$Geburtenrate/data$Menschen
StoePerFlae = data$Stoerche/data$Flaeche
xyplot(GebPerPeop~StoePerFlae)
cor(GebPerPeop,StoePerFlae,method="pearson")
#Keine nennenswerte Korrelation.
# Aus i) hatten wir Ausreißer, diese sind große Länder-> mehr Fläche-> mehr Störche
#                                         große Länder-> mehr Menschen-> mehr Geburten


#d)
x1 = mvrnorm(200, mu=c(0,0), Sigma=matrix(c(1,-0.2,-0.2,1),2,2))
x2 = mvrnorm(100, mu=c(5,5), Sigma=matrix(c(1,-0.2,-0.2,1),2,2))

x= rbind(x1,x2)
cor(x[,1],x[,2],method="pearson")
cor(x[,1],x[,2],method="spearman")

cor.test(x[,1],x[,2],method="pearson")
cor.test(x[,1],x[,2],method="spearman")
# wieder einige extreme Ausreißer
#-> flasche Korrelation, durch 2 verschiedene Verteilungen


#Aufgabe 4
#a)
data = HairEyeColor
mosaicplot(~Sex+Hair+Eye,color = rainbow(4), data=data)
mosaicplot(~Hair+Eye,color = rainbow(4), data=data)

#b)
prob = data/sum(data)
dataComb = apply(data,c(1,2),sum)
probHair = apply(prob,c(1,3),sum)
probEye = apply(prob,c(2,3),sum)

probHairComb = apply(probHair,1,sum)
probEyeComb = apply(probEye,1,sum)

#c)
#Erwartet: #(Hair = a, Eye = b) = #Ges*P(Hair = a) * P(Eye = b)

#d)
chisq = 0
for(hair in rownames(prob)){
  for(eye in colnames(prob)){
    chisq = chisq + (dataComb[hair,eye]-sum(data)*probHairComb[hair]*probEyeComb[eye])^2/(sum(data)*probHairComb[hair]*probEyeComb[eye])
  }
}
#e)
#test+ 0.95-Quantile
chisq.test(dataComb)
chisq.test(data[,,"Male"])
chisq.test(data[,,"Female"])
pchisq(0.95,df=2)
