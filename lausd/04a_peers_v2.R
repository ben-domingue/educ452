load("LA_nice_sub.Rdata") 
library(lme4)
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)

####################################################################
ma<-df[df$subject=="READING",]
ma<-ma[!is.na(ma$scale_score_std_lag_1),]
##
ma0<-ma #ma0 is original data
##mod0<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma0)
##

####################################################################
##Let's first look at this empirically
mm<-by(ma0$scale_score_std_lag_1,ma0$teacher_id,mean,na.rm=TRUE)
lm<-data.frame(teacher_id=names(mm),lm=as.numeric(mm))
ma0<-merge(ma0,lm)
ma0<-ma0[!is.na(ma0$lm),]
mod0a<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma0)
mod0b<-lmer(scale_score_std~scale_score_std_lag_1+lm+ ##note this last term
                in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma0)
z1<-ranef(mod0a)$teacher_id
z2<-ranef(mod0b)$teacher_id
tmp<-data.frame(teacher_id=rownames(z1),re1=z1[,1])
tmp2<-data.frame(teacher_id=rownames(z2),re2=z2[,1])
tmp<-merge(tmp,tmp2)
te<-merge(lm,tmp)
cor(te[,-1]) 
##there is an important structural diff between mod0a and mod0b

####################################################################
##let's probe this
simfun<-function(wt,te,ma0,
                 sd.error=.55,
                 sd.teacherfx=.29,
                 wt2 ##hi! i'm new here :)
                 ) {
    mm<-by(ma0$scale_score_std_lag_1,ma0$teacher_id,mean,na.rm=TRUE)
    te<-data.frame(teacher_id=names(mm),lm=as.numeric(mm))
    te$true<-rnorm(nrow(te),mean=wt2*te$lm,sd=1)
    te$te<-wt*te$lm+te$true #note that the teacher effect is part teacher ('true') and part class composition (te$lm)
    s1<-sd(te$te)
    te$te<-te$te*sd.teacherfx/s1
    sd(te$te)
    te$lm<-NULL
    ma<-merge(ma,te)
    ##
    zz<-by(ma$scale_score_std_lag_1,ma$teacher_id,mean,na.rm=TRUE)
    zz<-data.frame(teacher_id=names(zz),lm=as.numeric(zz))
    ma<-merge(ma,zz,all.x=TRUE)
    ##
    ma$scale_score_std_lag_1<-ifelse(is.na(ma$scale_score_std_lag_1),0,ma$scale_score_std_lag_1)
    ma$scale_score_std1<-.7*ma$scale_score_std_lag_1+ma$te+ma$lm+rnorm(nrow(ma),mean=0,sd=sd.error)
    mod1<-lmer(scale_score_std1~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
    mod2<-lmer(scale_score_std1~scale_score_std_lag_1+lm+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
    ##
    mm<-by(ma$scale_score_std_lag_1,ma$teacher_id,mean,na.rm=TRUE)
    tmp<-data.frame(teacher_id=names(mm),lag.mean=as.numeric(mm))
    te2<-merge(te,tmp)
    z<-ranef(mod1)$teacher_id
    z2<-ranef(mod2)$teacher_id
    tmp<-data.frame(teacher_id=rownames(z),re1=z[,1],re2=z2[,1])
    te2<-merge(te2,tmp)
    te2
}

out<-list()
for (wt in c(0,.5,1)) for (wt2 in c(-.5,0,.5)) out[[paste(wt,wt2)]]<-simfun(wt=wt,te=te,ma0=ma0,wt2=wt2)

z<-lapply(out,function(z) cor(z[,-1]))
true.re1<-sapply(z,function(x) x[1,4])
true.re2<-sapply(z,function(x) x[1,5])
total.re1<-sapply(z,function(x) x[2,4])
total.re2<-sapply(z,function(x) x[2,5])
wt<-names(z)
wt<-strsplit(wt," ")
wt1<-sapply(wt,"[",1)
wt2<-sapply(wt,"[",2)

df<-data.frame(wt1=wt1,wt2=wt2,true.re1,true.re2,total.re1,total.re2)
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,10))
L<-split(df,df$wt2)
nms<-as.numeric(names(L))
L<-L[order(nms)]
for (ii in 1:length(L)) {
    df<-L[[ii]]
    plot(NULL,xlim=0:1,ylim=0:1,xlab='wt',ylab='')
    for (i in 3:6) {
        lines(df[,1],df[,i])
        mtext(side=4,at=df[nrow(df),i],names(df)[i],las=2,line=.25)
    }
    legend("bottomleft",bty='n',legend=as.character(names(L)[ii]))
}





