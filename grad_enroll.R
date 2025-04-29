load("grad_enroll.Rdata") ##x; see /home/bdomingu/Dropbox/stanford/classes/edu400a/src/grad

##Los Angeles Unified
nboot<-100
par(mfrow=c(1,2),mgp=c(2,1,0))
plot(x[,-1])

lm(x[,3]~x[,2])

#############################################################################
##bootstrap [and why intercept=0?]
la<-est<-numeric()
for (i in 1:nboot) {
    index<-sample(1:nrow(x),nrow(x),replace=TRUE)
    x0<-x[index,]
    la[i]<-"Los Angeles Unified" %in% x0[,1]
    est[i]<-lm(x0[,3]~0+x0[,2])$coef[]
}

plot(density(est[la==1]),xlim=c(0,max(est)))
lines(density(est[la==0]),col='red')

##what do we think about lausd?
