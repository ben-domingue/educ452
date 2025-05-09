x<-read.csv("Dynarski2003.csv")

##see eqn 7': https://d1wqtxts1xzle7.cloudfront.net/48651415/w7422-libre.pdf?1473278215=&response-content-disposition=inline%3B+filename%3DDoes_Aid_Matter_Measuring_the_Effect_of.pdf&Expires=1746627068&Signature=OFt7cT2KfVNxynJccufgCRB962v5QZogK55Z0ex6aTq6VL8jEym8V6~m2W5i~keXR9Nm2qEYvklHerSPaMqyiD8XxxjW5r7BUD~uW8VjblzHcTj0oUMzlhDz0xdbpKqHqYB3bItTqQPhAXMFck6lmL~JMek6GxUxfCG4YY4fZs~B3Wk-v12wO46kYudAjsyDRCsqkPOBVdvJT3VDKE03YqL~xmv9hm5dBuyRWm0uxnoRUKCzCzVj2mo5qRo1LdaXWwWKNqxDTYO6bqRKHxQmuvwUcx-aM5fBQE3AF~1EYICrJlhAvkQAoWon~BEOKrWgKROv~WgOsO2YVw~m8kvNvA__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA

x$before<-ifelse(x$yearsr<=81,1,0)
L<-split(x,paste(x$before,x$fatherdec))
f<-function(x) weighted.mean(x$coll,x$wt88)
lapply(L,f) #compare to table 2

##maybe look at ols error
m<-lm(coll~fatherdec*before,x,weights=x$wt88)
summary(m) ##interaction estimate of 0.256 in table 3; i get 0.18
