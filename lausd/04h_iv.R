load("LA_nice_sub.Rdata") 


####################################################################
##we're going to need the lagged cross-subject score
x<-df[df$subject=="READING",]
x<-x[,c("grade","year","student_id","teacher_id","scale_score_std_lag_1")]
names(x)[5]<-"reading_lag"

ma<-df[df$subject=="MATHEMATICS",]
dim(ma)
ma<-merge(ma,x)
dim(ma)

##let's assume that the true lagged ability for the student was the average of the math and reading scores plus some error (i.e., they're both noisy proxies of true ability with different errors)
z<-ma$scale_score_std_lag_1+ma$reading_lag
ma<-ma[!is.na(z),]
z<-ma$scale_score_std_lag_1+ma$reading_lag
ma$true_lag<-rnorm(nrow(ma),mean=z,sd=.1)


####################################################################
ivfun<-function(x,fm=formula("lagscore~reading_lag")) {
    tmp<-x[,all.vars(fm)]
    x<-x[rowSums(is.na(tmp))==0,]
    mm<-lm(fm,x)
    x[[all.vars(fm)[1]]]<-mm$fitted #replacing the original IV with the instrumented IV
    x
}

####################################################################
ma$scale_score_std_lag_1<-NA #we're going to throw this away to make sure we don't use it

library(lme4)
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
##let's simulate new lagged scores where the obs score is reading score plus error
for (sd.me in seq(0,1,by=.25)) {
    g4<-ma[ma$grade==4,]
    g4$lagscore<-rnorm(nrow(g4),mean=g4$true_lag,sd=sd.me) ###this is important. last year's lagged maths score is a noisy version of the truth. note we won't use this again; we'll use reading_lag to IV lagscore
    g4$lagscore<-std(g4$lagscore)
    ##
    g4$scale_score_std<-NA
    sd.error<-.55
    sd.teacherfx<-.29
    te<-rnorm(length(unique(g4$teacher_id)),mean=0,sd=sd.teacherfx)
    te<-data.frame(teacher_id=unique(g4$teacher_id),te=te)
    g4<-merge(g4,te)
    g4$scale_score_std<-.7*g4$reading_lag+g4$te+rnorm(nrow(g4),mean=0,sd=sd.error) ##note that this year's score is based off the lagged reading score
    ##unadjusted way
    mod<-lmer(scale_score_std~lagscore+in.title1+ell+join.after.k+factor(year)+(1|teacher_id),g4)
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
    print("unadjusted")
    print(c(sd.me,bias,rmse,corr))
    ##adjusted
    g4<-ivfun(g4)
    mod<-lmer(scale_score_std~lagscore+in.title1+ell+join.after.k+factor(year)+(1|teacher_id),g4)
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
    print("adjusted")
    print(c(sd.me,bias,rmse,corr))
}
