make.some_function<-function(){
  count<-0
  f<-function(){
    count<<-count+1
    
  }
  return(f)
}

some_function<-make.some_function()

quicksort<-function(arr){
  
  mid<-sample(arr,1)
  left<-c()
  right<-c()
  
  lapply(arr[arr != mid],function(d){
    if(d<mid){
      left<<-c(left,d)
      some_function()
    }
    else{
      right<<-c(right,d)
      some_function()
    }
  })
  
  if(length(left) > 1 ){
    
    left<- quicksort(left)
  }
  
  if(length(right) > 1){
    
    right<-quicksort(right)
  }
  
  c(left,mid,right)
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
