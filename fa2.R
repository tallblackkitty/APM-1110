#1a
sample(0:1,10,rep=T)
Flipcoin = function(n) sample(0:1,n,rep=T)
d1=Flipcoin(10)
d1
sum(d1==1)/10
sum(d1==0)/10
#1b
sample(1:52,52,rep=T)
cards = function(n) sample(1:52,n,rep=T)
d1=cards(52)
d1
sum(d1>=26)/52
sum(d1<26)/52
#1c
sample(1:6,6,rep=T)
die = function(n) sample(1:6,n,rep=T)
d1=die(6)
d1
sum1<- sum(d1==2)/6
sum2<- sum(d1==4)/6
sum3<- sum(d1==6)/6
sum5<- sum(d1==1)/6
sum6<- sum(d1==3)/6
sum7<- sum(d1==5)/6
sum4<- sum1 + sum2 + sum3
sum8<- sum5 + sum6 + sum7
sum4
sum8
#2
sample(0:1,100,rep=T)
Flipcoin = function(n) sample(0:1,n,rep=T)
d1=Flipcoin(100)
d1
sum(d1==1)/100
sum(d1==0)/100





