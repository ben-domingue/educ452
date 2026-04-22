load("LA_nice_sub.Rdata") 


####################################################################
ma<-df[df$subject=="MATHEMATICS",]




library(lme4)
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
##let's simulate new lagged scores where the obs score is truth but we see it with error
for (sd.me in seq(0,1,by=.25)) {
    g4<-ma[ma$grade==4,]
    g4<-g4[!is.na(g4$scale_score_std_lag_1),]
    g4$lagscore<-rnorm(nrow(g4),mean=g4$scale_score_std_lag_1,sd=sd.me)
    g4$lagscore<-std(g4$lagscore)
    ##
    g4$scale_score_std<-NA
    sd.error<-.55
    sd.teacherfx<-.29
    te<-rnorm(length(unique(g4$teacher_id)),mean=0,sd=sd.teacherfx)
    te<-data.frame(teacher_id=unique(g4$teacher_id),te=te)
    g4<-merge(g4,te)
    g4$scale_score_std<-.7*g4$scale_score_std_lag_1+g4$te+rnorm(nrow(g4),mean=0,sd=sd.error) ##note that this year's score is based off the **TRUE** lagged score
    mod<-lmer(scale_score_std~lagscore+in.title1+ell+join.after.k+factor(year)+(1|teacher_id),g4) ##but we control based on the noisy proxy
    tmp<-ranef(mod)$teacher_id
    z<-merge(te,tmp,by.x=1,by.y=0)
    names(z)[3]<-'est'
    ##
    tmp<-by(g4$lagscore,g4$teacher_id,mean,na.rm=TRUE)
    tmp<-data.frame(teacher_id=names(tmp),lm=as.numeric(tmp))
    z<-merge(z,tmp)
    ##
    err<-(z$est-z$te)
    bias<-mean(err)
    rmse<-sqrt(mean(err^2))
    corr<-cor(z$est,z$lm)
    print(c(sd.me,bias,rmse,corr)) #the sd of measurement error, bias in VA estimates, variance of VA estimates, correlation with prior year means
}
