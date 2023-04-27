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

##We are going to induce variation in teacher effects via manipulation of sig.class and peer.effect
assign.class<-function(x,sig.class=1) {
    nn<-length(unique(x$teacher_id))
    ran<-range(ma$scale_score_std_lag_1)
    ce<-seq(ran[1],ran[2],length.out=nn+2)
    ce<-ce[-c(1,length(ce))]
    p<-outer(x$scale_score_std_lag_1,ce,"-")
    getclass<-function(y,sig) {
        d<-dnorm(y,sd=sig)
        d<-d/sum(d)
        cl<-rmultinom(1,1,d)
        which(cl[,1]>0)
    }
    cl<-apply(p,1,getclass,sig=sig.class)
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
for (sig.class in c(1,10)) for (peer.effect in c(.1,.5)) {
                                 L<-split(ma,ma$school_id)
                                 L<-lapply(L,assign.class,sig.class=sig.class)
                                 L<-lapply(L,peers,peer.effect=peer.effect)
                                 z<-data.frame(do.call("rbind",L))
                                 ##
                                 mod<-lmer(ss~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|class),z)
                                 out[[paste(sig.class,peer.effect)]]<-c(sig.class=sig.class,peer.effect=peer.effect,teacher.sd=sd(ranef(mod)$class[,1]))
                             }
tab<-do.call("rbind",out)
tab

##What we can see here is that "teacher effects" are minimal when peer effects are small (which is what we'd expect given that there are no teacher effects here, it's all peers!). we also see a diminishment of variation in "teacher effects" when sig.class is large. this is all as we'd expect
