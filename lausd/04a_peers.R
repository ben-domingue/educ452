##here we will let teacher effects depend on prior year's score

load("LA_nice_sub.Rdata") 
library(lme4)
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)

####################################################################
ma<-df[df$subject=="MATHEMATICS",]
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
mod0b<-lmer(scale_score_std~scale_score_std_lag_1+lm+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma0)
z1<-ranef(mod0a)$teacher_id
z2<-ranef(mod0b)$teacher_id
tmp<-data.frame(teacher_id=rownames(z),re1=z1[,1],re2=z2[,1])
te<-merge(lm,tmp)
cor(te[,-1]) 
##this leads me to worry about scenarios in which teacher effects are correlated with last year means. 

####################################################################
##we're goign to simulate scores but in a sneaky way
sd.error<-.55
sd.teacherfx<-.29
#te<-rnorm(length(unique(ma$teacher_id)),mean=0,sd=sd.teacherfx)
mm<-by(ma0$scale_score_std_lag_1,ma0$teacher_id,mean,na.rm=TRUE)
te<-data.frame(teacher_id=names(mm),lm=as.numeric(mm))
te$te<-.5*te$lm+rnorm(nrow(te),mean=0,sd=1)
s1<-sd(te$te)
te$te<-te$te*sd.teacherfx/s1
sd(te$te)
te$lm<-NULL
##
ma<-merge(ma,te)

zz<-by(ma$scale_score_std_lag_1,ma$teacher_id,mean,na.rm=TRUE)
zz<-data.frame(teacher_id=names(zz),lm=as.numeric(zz))
ma<-merge(ma,zz,all.x=TRUE)

ma$scale_score_std_lag_1<-ifelse(is.na(ma$scale_score_std_lag_1),0,ma$scale_score_std_lag_1)
ma$scale_score_std1<-.7*ma$scale_score_std_lag_1+ma$te+ma$lm+rnorm(nrow(ma),mean=0,sd=sd.error)
mod1<-lmer(scale_score_std1~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
mod2<-lmer(scale_score_std1~scale_score_std_lag_1+lm+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)


mm<-by(ma$scale_score_std_lag_1,ma$teacher_id,mean,na.rm=TRUE)
tmp<-data.frame(teacher_id=names(mm),lag.mean=as.numeric(mm))
te2<-merge(te,tmp)
z<-ranef(mod1)$teacher_id
z2<-ranef(mod2)$teacher_id
tmp<-data.frame(teacher_id=rownames(z),re1=z[,1],re2=z2[,1])
te2<-merge(te2,tmp)
cor(te2[,-1]) #this is going to make it hard to know whether true teacher effects are correlated with lagged class achievement!!


