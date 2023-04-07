set.seed(123)
simulateAnpassungstest <- function(){
    n <- 1000
    p <- c(1/8, 1/4, 1/2, 1/8)
    a <- rmultinom(1, n, p)
    sum((a - n*p)^2/(n*p))
  }
  results <- c()
  for (i in 1:300){
    results =c(results ,simulateAnpassungstest())
  }
#plot relative freq histogram
hist(results, breaks=20, freq=FALSE, main="Histogramm der Anpassungstest-Statistik", xlab="Anpassungstest-Statistik", ylab="relative Häufigkeit")
