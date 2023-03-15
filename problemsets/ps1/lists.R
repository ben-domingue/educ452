##lists are magic

##they can take all different kinds of values
l<-list(1,TRUE,'pony',1:5,function(x) x^2,list(1,2,3))

##they have pre-optimized functions that you can use to work with them
lapply(l,class)

##A common workflow for me is to use a list to store 'stuff' produced in simulations
l<-list()
for (i in 1:10) l[[i]]<-runif(10)
##note the double brackets and why they matter!
l[1]
l[[1]]

##I can then write a second function to process that 'stuff'
f<-function(x) c(mean(x),sd(x))
l2<-lapply(l,f)

##i then often use the following to get everything in a table
do.call("rbind",l2)
