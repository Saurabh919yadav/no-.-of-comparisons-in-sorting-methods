make.some_function<-function(){
  count<-0
  f<-function(){
    count<<-count+1
    
  }
  return(f)
}

some_function<-make.some_function()

mmerge<-function(a,b) {
  r<-numeric(length(a)+length(b))
  ai<-1; bi<-1; j<-1;
  for(j in 1:length(r)) {
    if((ai<=length(a) && a[ai]<b[bi]) || bi>length(b)) {
      r[j] <- a[ai]
      ai <- ai+1
      some_function()
    } else {
      r[j] <- b[bi]
      bi <- bi+1    
      some_function()
      
    }
  }
  r
}
mmergesort<-function(A) {
  if(length(A)>1) {
    q <- ceiling(length(A)/2)
    a <- mmergesort(A[1:q])
    b <- mmergesort(A[(q+1):length(A)])
    mmerge(a,b)
  } else {
    A
  }
}



no_compare<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for (case in (1:20)*10) {
  
  a<- array(runif(case, min=0, max = 1000))
  print(a)
  
  for (times in 1:20) {
    sample(a)
    some_function<-make.some_function()
    x1<-mmergesort(a)
    print(x1)
    
    no_compare[(case)/10]<-no_compare[(case)/10]+some_function()-1
    
  }
  no_compare[(case)/10]<-no_compare[(case)/10]/20
  
}
print(no_compare)
size<-array((1:20)*10)
plot(size,no_compare,xlab = "size ",ylab = "no. of comparisons",col="red",pch=20)
lines(size,no_compare)
