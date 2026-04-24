load("LA_nice_sub.Rdata") 
 
##Let's first look at the empirical data
library(lme4)
ma<-df[df$subject=="MATHEMATICS",]
m<-by(ma$scale_score_std_lag_1,ma$teacher_id,mean,na.rm=TRUE)
m<-data.frame(teacher_id=names(m),lm=as.numeric(m))
ma<-merge(ma,m)
mod<-lmer(scale_score_std~scale_score_std_lag_1+lm+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
te<-ranef(mod)$teacher_id
te<-data.frame(teacher_id=rownames(te),te=te[,1])
s<-by(ma$scale_score_std_lag_1,ma$teacher_id,sd,na.rm=TRUE)
s<-data.frame(teacher_id=names(s),sd=as.numeric(s))
x<-merge(te,s)

plot(x$sd,x$te,pch=19,cex=.5)
abline(lm(te~sd,x))
cc<-cor(x$sd,x$te,use='p')
cc

##meh, not much going on, but let's turn to a different question: would we be able to detect it if true?
load("LA_nice_sub.Rdata") 
ma<-df[df$subject=="MATHEMATICS",]
m<-by(ma$scale_score_std_lag_1,ma$teacher_id,mean,na.rm=TRUE)
m<-data.frame(teacher_id=names(m),lm=as.numeric(m))
ma<-merge(ma,m)
sim<-function(ma,scale) { #note we are going to use `scale' to control whether teachers are more/less effective due to variation in SD of prior abilities
    ma$scale_score_std<-NA
    sd.error<-.55
    sd.teacherfx<-.29
    ##
    s<-by(ma$scale_score_std_lag_1,ma$teacher_id,sd,na.rm=TRUE)
    s<-data.frame(teacher_id=names(s),sd=as.numeric(s))
    s$sd<-(s$sd-mean(s$sd,na.rm=TRUE))/sd(s$sd,na.rm=TRUE)
    s<-s[!is.na(s$sd),]
    ##here is where we build in the 'structure' that we are going to then investigaet
    te<-rnorm(nrow(s),
              mean=scale*s$sd, #note scale
              sd=sd.teacherfx)
    te<-data.frame(teacher_id=s$teacher_id,te=te)
    ma<-merge(ma,te)
    ma$scale_score_std<-.7*ma$scale_score_std_lag_1+ma$te+rnorm(nrow(ma),mean=0,sd=sd.error)
    ##
    ma<-ma[!is.na(ma$scale_score_std_lag_1),]
    mod<-lmer(scale_score_std~scale_score_std_lag_1+lm+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
    te<-ranef(mod)$teacher_id
    te<-data.frame(teacher_id=rownames(te),te=te[,1])
    x<-merge(te,s)
    cor(x$te,x$sd) #so i'm returning correlations of estimated teacher effects and class SD of prior abilities
}
for (scale in seq(-.25,.25,length.out=5)) print(c(scale,sim(ma=ma,scale=scale)))
print("reminder of empirical results")
print(cc)
