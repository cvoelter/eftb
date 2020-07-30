number.switches <- c()
n.cups <- 2

for(i in 1:100000){
  trial.count=1
  xx <- c()
  res <- c()
  while(trial.count <= 36){
    res <-c(res,sample(n.cups, size=1, replace=T))
    trial.count <- trial.count+1
  }
  xx<- rle(res)$lengths[rle(res)$values==1]#sequences of "1" (correct) values
  number.switches[i] <- sum(trunc(xx[xx>=3]/3))#sum of the sequences >=3 
}

range(number.switches)
ms<-mean(number.switches)

#new scale
ms/11
