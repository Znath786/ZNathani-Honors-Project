num <-1
seq<- c(1:100)
for(i in seq) {
  if (num%%3==0 && num%%5==0){
      seq[num]<-"FizzBuzz"
    }
  else if (num%%3==0 & num%%5!=0){
      seq[num]<-"Fizz"
    }
  else if (num%%3!=0 & num%%5==0){
      seq[num]<-"Buzz"
    }
  else {
      seq[num]<-num
    }
  num <- num+1
}
paste(as.character(seq),"",collapse=", ",sep="")
##print(seq)





    
