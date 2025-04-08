load("grad_enroll.Rdata") ##x; see /home/bdomingu/Dropbox/stanford/classes/edu400a/src/grad

##Los Angeles Unified
nboot<-100
plot(x[,-1])

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

#############################################################################
##randomization inference
est.ri<-numeric()
for (i in 1:nboot) {
    x0<-x
    x0[,2]<-sample(x0[,2],nrow(x0),replace=FALSE)
    est.ri[i]<-lm(x0[,3]~0+x0[,2])$coef[1]
}
lines(density(est.ri),col='blue')

##does this make any sense?

#############################################################################
##what do we make of measurement error in x[,3]? x[,3] is likely almost always an undercount. 
coef(lm(x[,3]~0+x[,2]))
##Let's suppose that 60% of students who graduate enroll but that we are only able to identify tau % of them
plot(x[,-1])
abline(lm(x[,3]~0+x[,2]))
xv<-seq(min(x[,2]),max(x[,2]),length.out=25)
for (tau in c(.7,.9,.95)) lines(xv,xv*.6*tau,lty=2)

##if we had resources and could go to a few districts, what could we do to estimate tau? 

##perhaps have them make a conjuecture about a true model wherein there is both the undercounting of x[,3] and then measurement error?
#############################################################################
##measurement error in x[,3] is definitely heteroscedastic
err<-abs(lm(x[,3]~0+x[,2])$resid)
plot(x[,2],err)
##
sx<-sqrt(x[,2])
m<-lm(err~sx)
xv<-seq(min(x[,2]),max(x[,2]),length.out=500)
lines(xv,coef(m)[1]+coef(m)[2]*sqrt(xv))
##
lx<-log(x[,2])
m<-lm(err~lx)
xv<-seq(min(x[,2]),max(x[,2]),length.out=500)
lines(xv,coef(m)[1]+coef(m)[2]*log(xv),col='blue')
      




