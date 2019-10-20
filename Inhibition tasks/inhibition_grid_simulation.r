res<-c()
for(i in 1:1000000){ #iterations
x<-sample(1:13, size=6,replace=FALSE)<7 #are correct doors among the 6 chosen doors?
res[i]<-sum(x) #sum of correct doors chosen within 6 trials
}
mean(res)
quantile(res, c(0.025, 0.975)) #95% CI
hist(res)

