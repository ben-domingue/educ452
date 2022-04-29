load("LA_nice_sub.Rdata") 
 
##Let's first look at the empirical data
library(lme4)
ma<-df[df$subject=="MATHEMATICS",]
mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
te<-ranef(mod)$teacher_id
te<-data.frame(teacher_id=rownames(te),te=te[,1])
s<-by(ma$scale_score_std_lag_1,ma$teacher_id,sd,na.rm=TRUE)
s<-data.frame(teacher_id=names(s),sd=as.numeric(s))
x<-merge(te,s)

plot(x$sd,x$te,pch=19,cex=.5)
abline(lm(te~sd,x))

##meh, not much going on (and, well, the opposite of my intuition!), but let's turn to a different question: would we be able to detect it if true?

sim<-function(ma,scale) {
    ma$scale_score_std<-NA
    sd.error<-.55
    sd.teacherfx<-.29
    ##
    s<-by(ma$scale_score_std_lag_1,ma$teacher_id,sd,na.rm=TRUE)
    s<-data.frame(teacher_id=names(s),sd=as.numeric(s))
    s$sd<-(s$sd-mean(s$sd,na.rm=TRUE))/sd(s$sd,na.rm=TRUE)
    s<-s[!is.na(s$sd),]
    ##
    te<-rnorm(length(unique(ma$teacher_id)),
              mean=scale*s$sd, #note scale
              sd=sd.teacherfx)
    te<-data.frame(teacher_id=unique(ma$teacher_id),te=te)
    ma<-merge(ma,te)
    ma$scale_score_std<-.7*ma$scale_score_std_lag_1+ma$te+rnorm(nrow(ma),mean=0,sd=sd.error)
    ##
    ma<-ma[!is.na(ma$scale_score_std_lag_1),]
    mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
    te<-ranef(mod)$teacher_id
    te<-data.frame(teacher_id=rownames(te),te=te[,1])
    x<-merge(te,s)
    cor(x$te,x$sd)
}
for (scale in seq(-1,1,by=.5)) print(sim(ma=ma,scale=scale))
