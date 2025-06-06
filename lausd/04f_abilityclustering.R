load("LA_nice_sub.Rdata") 
library(lme4)
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)

####################################################################
ma<-df[df$subject=="MATHEMATICS",]
ma<-ma[!is.na(ma$scale_score_std_lag_1),]
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
ma$scale_score_std_lag_1<-std(ma$scale_score_std_lag_1)

mod0<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
sd(ranef(mod0)$teacher_id[,1])
## 0.27. we are going to induce variation around this quantity

##We are going to induce variation in 'teacher effects' via manipulation of sig.class and peer.effect
assign.class<-function(x,sig.class=1) {
    nn<-length(unique(x$teacher_id))
    M<-mean(x$scale_score_std_lag_1,na.rm=TRUE)
    ce<-rnorm(nn,mean=M,sd=sig.class) #these are the 'teacher means'
    p<-outer(x$scale_score_std_lag_1,ce,"-")
    getclass<-function(y) {
        S<-ifelse(nrow(x)>1,sd(x$scale_score_std_lag_1,na.rm=TRUE),1)
        d<-dnorm(y,sd=S)
        d<-d/sum(d)
        cl<-rmultinom(1,1,d)
        which(cl[,1]>0)
    }
    cl<-apply(p,1,getclass)
    x$class<-paste(x$school_id,cl,sep="__")
    x
}

peers<-function(x,peer.effect=.1) {
    mm<-by(x$scale_score_std_lag_1,x$class,mean)
    tmp<-data.frame(class=names(mm),mm=as.numeric(mm))
    x<-merge(x,tmp)
    x$ss<-rnorm(nrow(x),mean=x$scale_score_std_lag_1+peer.effect*x$mm,sd=1) #we are assigning test scores here. note that teachers play no role!! it's purely your score last year plus some offset (moderated by peer.effect) of your peers
    x
}

out<-list()
##we're going to vary two things below. sig.class will control the strength of sorting (stronger sorting when this number is smaller). peer.effect will control the strength of peer effects
for (peer.effect in c(.1,1)) for (sig.class in c(0.01,.1,1,2))  {
                                 L<-split(ma,paste(ma$school_id,ma$grade))
                                 L<-lapply(L,assign.class,sig.class=sig.class)
                                 ##checking role of sig.class
                                 f<-function(x) sd(by(x$scale_score_std_lag_1,x$class,mean,na.rm=TRUE))
                                 s<-sapply(L,f)
                                 per.class.var<-mean(s[!is.na(s)]) #this is the SD in class means of grades within a school
                                 ##
                                 L<-lapply(L,peers,peer.effect=peer.effect)
                                 z<-data.frame(do.call("rbind",L))
                                 ##
                                 mod<-lmer(ss~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|class),z)
                                 out[[paste(sig.class,peer.effect)]]<-c(sig.class=sig.class,per.class.var=per.class.var,peer.effect=peer.effect,teacher.sd=sd(ranef(mod)$class[,1]))
                             }
tab<-do.call("rbind",out)
tab

##What we can see here is that "teacher effects" are minimal when peer effects are small (which is what we'd expect given that there are no teacher effects here, it's all peers!). we also see a diminishment of variation in "teacher effects" when sig.class is large. this is all as we'd expect
