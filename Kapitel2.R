library(MASS)

#Aufgabe 1

#sort by time
hills[order(hills$time, decreasing= TRUE), ]

#sort by dist, tiebreaker climb
hillstemp=hills
hillstemp=hillstemp[order(hillstemp$climb, decreasing= FALSE), ]
hillstemp=hillstemp[order(hillstemp$dist, decreasing= FALSE), ]
hillstemp

#sort by speed = dist/time
hills[order(hills$dist/hills$time, decreasing = TRUE), ]

#sort by name
hills[order(row.names(hills),decreasing = FALSE),]
hills[order(row.names(hills),decreasing = TRUE),]

#Aufgabe 2


roboSim= function(n, pos=c(0,0)){
  pathx=c(pos[1])
  pathy=c(pos[2])
  start= pos
  notSteppedOutFlag=TRUE
  for (x in 1:n){
    velx = sample(c(1,-1),1)
    vely = sample(c(1,-1),1)
    if (sample(c(0,1),1)==0){
      pos = pos + c(velx,0)
    }
    else{
      pos = pos+ c(0,vely)
    }
    pathx=append(pathx,pos[1])
    pathy=append(pathy,pos[2])
    
    if(notSteppedOutFlag&(abs(pos[1]-start[1])>10|abs(pos[2]-start[2])>10)){
      print(sprintf("Stepped out of 20x20 after %s Iterations",x))
      notSteppedOutFlag=FALSE
    }
  }
  pos
  #plot
  plot(pathx,pathy,type="l")
}

roboSim(1000)


#Aufgabe 3

#a)
fib.it=function(n){
  if(n<1)
    return(-1)
  a=-1
  b=1
  c=0
  for (x in 1:n){
    a=b
    b=c
    c=a+b
  }
  c
}
fib.it(30)
fib.it(40)
fib.it(50)
fib.it(51)/fib.it(50)
#ja, konvergiert gegen 1,61...


#b)

fib.lower=function(n){
  fibList = c()
  if(n<1)
    return(-1)
  a=-1
  b=1
  c=0
  while(TRUE){
    a=b
    b=c
    c=a+b
    if (c<n){
      fibList=append(fibList,c)
    } else {
      return(fibList)
    }
  }
  c
}
fib.lower(10)

#c)
fib.rek = function(n){
  if (n<1){
    return(-1)
  }
  if (n<3){
    return(1)
  }
  else {
    return(fib.rek(n-1)+fib.rek(n-2))
  }
}
fib.rek(30)


#Aufgabe 4
#a)

constructNumbers = function(n,base){
  string = rep("0",n)
  for (i in 1:length(base)^n){
    for (j in 1:n){
      string[j]=floor(i/(length(base)^(j-1)))%%length(base)
    }
    print(paste(rev(string), collapse = ""))
  }
}
constructNumbers(2,c(0,1,2,3,4))




revert = function(str, base){
  sum=0
  for (i in 1:nchar(str)){
    sum= sum+(match(substring(str,i,i),base)-1)*length(base)^(nchar(str)-i)
  }
  sum
}
revert("2100",0:9)


translate= function(str,frombase,tobase){
  val = revert(str,frombase)
  string=c()
  j=1
  while (!floor(val/(length(tobase)^(j-1)))==0) {
    string[j]=floor(val/(length(tobase)^(j-1)))%%length(tobase)
    j=j+1
  }
  return (paste(rev(string), collapse = ""))
}

translate("1111",0:1,0:9)




#Aufgabe 5
primesTo = function(n){
  
  primes = 2:n
  for (p in 2:max(2,floor(sqrt(n)))){
    if (primes[p-1]==0){
      next
    }
    if (2*p>=n){
      next
    }
    for (r in seq(from=2*p, to=n, by=p)){
      primes[r-1]=0
    }
  }
  primes= primes[which(!primes==0)]
  primes
}
primesTo(23)


pi= function(n){
  length(primesTo(n))
}



#data
actualVsApprox = function(BiggestN){
  primes = primesTo(BiggestN)
  noPrimes= c()
  j=1
  for (n in 1:BiggestN){
    while(j<=length(primes) && primes[j]<n){
      j=j+1
    }
    noPrimes= append(noPrimes,j-1)
  }
  
  plot(1:BiggestN,noPrimes,type="l")
  
  #gaussapprox
  g1= c()
  for (n in 1:BiggestN){
    g1= append(g1,n/log(n))
  }
  
  g2=c()
  for (n in 1:BiggestN){
    g2= append(g2,integrate(function(x){1/log(x)},2,max(2,n))$value)
  }
  
  lines(1:BiggestN, g1,col="red")
  lines(1:BiggestN, g2,col="blue")
}

actualVsApprox(1000)

actualVsApprox(100000)
