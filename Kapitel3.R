#Aufgabe 1
#a) Palindrome
is.zeichenpalindrome=function(s){
  #lowercase+remove special chars
  s = tolower(s)
  s = str_replace_all(s,"[^a-z]","")
  for (i in 1:floor(nchar(s)/2)){
    if (substr(s,i,i)!=substr(s,nchar(s)-i+1,nchar(s)-i+1)){
      return(FALSE)
    }
  }
    return(TRUE)
}

is.zeichenpalindrome("Able was I ere I saw Elba")

#b)zahlpalindrome
is.zahlpalindrome=function(n){
  n = as.character(n)
  for (i in 1:floor(nchar(n)/2)){
    if (substr(n,i,i)!=substr(n,nchar(n)-i+1,nchar(n)-i+1)){
      return(FALSE)
    }
  }
  return(TRUE)
}

is.zahlpalindrome(12321)


#c) Palindrome level
palindrome.level=function(n){
  if (length(n)>53/7*2){
    return(NA)
  }
  #trim leading zeros
    while (n[1]==0){
        n=n[2:length(n)]
    }
  if (all(n==rev(n))){
    return(0)
  }
  n= n+rev(n)
  for (i in 1:length(n)){
    if (n[i]>9){
      n[i]=n[i]-10
      n[i+1]=vec_4 <- ifelse(is.na(n[i+1]), 1, n[i+1]+1)
    }
  }
    return(1+palindrome.level(rev(n)))
}

palindrome.level(c(1,2,3))

palLevel = 1:200
for (i in 1:200){
  palLevel[i]=palindrome.level(c(floor(i/100),floor(i/10)%%10,i%%10))
}


#Aufgabe 2
makeratioApprox=function(x,depth){
  if (x<0){
    return(c(-1*makeratioApprox(-1*x,depth)))
  }
  if (depth<=0){
    return(c())
  }
  if ((x-floor(x))<=0.5){
    sequence = c(floor(x),makeratioApprox(1/(x-floor(x)),depth-1))
  } else {
    sequence = c(ceiling(x),-1*makeratioApprox(1/(ceiling(x)-x),depth-1))
  }
  return(sequence)
}

makeratioApprox(pi,3)


gcd = function(a,b){
  if (b==0){
    return(a)
  }
  return(gcd(b,a%%b))
}

turnApproxToFraction=function(koeff){
  num = 1
  denom = koeff[length(koeff)]
    for (i in (length(koeff)-1):1){
      num = num + denom*koeff[i]
      temp = num
      num = denom
      denom = temp
    }
  #simplify
    g = gcd(num,denom)
    num = num/g
    denom = denom/g
    return(c(denom,num))
}
Approximator=function(x,depth) {
  turnApproxToFraction(makeratioApprox(x,depth))
}
Approximator(pi,3)


#Aufgabe 3
# a)
library(MASS)
summary(cars)
plot(cars)
plot(c(),c(),xlim =c(min(cars$speed),max(cars$speed)),ylim =c(min(cars$dist),max(cars$dist)),xlab="speed",ylab="dist")
for (row in rownames(cars)){
    points(cars[row,"speed"],cars[row,"dist"])
}

#b)
Animals
summary(Animals)
plot(Animals)
#make log scale+name labels
AnimalsLog = log(Animals)
plot(AnimalsLog,xlab="log(weight)",ylab="log(brain)")
text(AnimalsLog$body,AnimalsLog$brain-0.15,labels=rownames(AnimalsLog))


#c)
#draw
plot(c(),c(),xlim = c(-1,1),ylim = c(-1,1))
#17 circlepoints
pointsx= c()
pointsy= c()
for (i in 1:17){
  pointsx = c(pointsx,cos(2*pi*i/17))
  pointsy = c(pointsy,sin(2*pi*i/17))
}
#points(pointsx,pointsy)
#segments
for(i in 1:17){
  for (j in 1:17){
    segments(pointsx[i],pointsy[i],pointsx[j],pointsy[j])
  }
}
