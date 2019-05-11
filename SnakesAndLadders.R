sum<-0
for(i in 1:1000) {
    CurrentSpace<-1
    diceRolls<-0
    while(CurrentSpace<26){
        diceNum<-sample(c(1,2,3,4,5,6),1)
        ## print(diceNum) 
        diceRolls <- diceRolls +1
        CurrentSpace <- CurrentSpace + diceNum
        if(CurrentSpace==3) {CurrentSpace<- 11}
        if(CurrentSpace==6) {CurrentSpace<- 17}
        if(CurrentSpace==9) {CurrentSpace<- 18}
        if(CurrentSpace==10) {CurrentSpace<-12}
        if(CurrentSpace==19) {CurrentSpace<-8}
        if(CurrentSpace==22) {CurrentSpace<-20}
        if(CurrentSpace==24) {CurrentSpace<-16}
        ## print(CurrentSpace)
    }
    ## print(diceRolls)
    sum <- sum +diceRolls
}
AveDiceRolls <- sum/1000
print(capture.output(cat('Average Dice Rolls for 1000 trials: ', AveDiceRolls)))



   