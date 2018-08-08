no_compare<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for (case in (1:20)*10) {
  
  a<- array(runif(case, min=0, max = 1000))
  print(a)
  
  
  for (times in 1:20) {
    sample(a)
    compare<-0
    for (i in 1:length(a)) {
      j<- i
      key<-a[i]
      while((j>1) && (a[j-1]>key)){
        a[j]<-a[j-1]
        j<-j-1
        compare<-compare+1
      }
      
      a[j]<-key
      compare<-compare+1
    }
    
    print(a)
    no_compare[(case)/10]<-no_compare[(case)/10]+compare
  }
  no_compare[(case)/10]<-no_compare[(case)/10]/20
  
}
print(no_compare)
size<-array((1:20)*10)
plot(size,no_compare,xlab = "size ",ylab = "no. of comparisons",col="red",pch=20)
lines(size,no_compare)
