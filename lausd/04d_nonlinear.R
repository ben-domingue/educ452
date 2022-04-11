load("LA_nice_sub.Rdata") 
library(lme4)


####################################################################
ma<-df[df$subject=="MATHEMATICS",]

##we're goign to simulat scores but in a sneaky way
sd.error<-.55
sd.teacherfx<-.29
te<-rnorm(length(unique(ma$teacher_id)),mean=0,sd=sd.teacherfx)
te<-data.frame(teacher_id=unique(ma$teacher_id),te=te)
ma<-merge(ma,te)
ma<-ma[!is.na(ma$scale_score_std_lag_1),]
##
ma0<-ma
##mod0<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma0)
##

std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
transform<-function(x) {
    f<-ecdf(x)
    x<-f(x)
    x<-sqrt(20*(x+.05))
    x
}
xx<-std(transform(ma$scale_score_std_lag_1))
ma$scale_score_std1<-xx+ma$te+rnorm(nrow(ma),mean=0,sd=sd.error)
transform<-function(x) cos(2*x)
xx<-std(transform(ma$scale_score_std_lag_1))
ma$scale_score_std2<-xx+ma$te+rnorm(nrow(ma),mean=0,sd=sd.error)

mod1<-lmer(scale_score_std1~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
mod2<-lmer(scale_score_std2~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)

par(mfrow=c(1,2))
ii<-sample(1:nrow(ma),5000)
tmp<-ma[ii,]
plot(tmp$scale_score_std_lag_1,tmp$scale_score_std1)
plot(tmp$scale_score_std_lag_1,tmp$scale_score_std2)

summary(mod1)$coef
summary(mod2)$coef #we're starting to see some attenuation of this coefficient here.

mm<-by(ma$scale_score_std_lag_1,ma$teacher_id,mean,na.rm=TRUE)
tmp<-data.frame(teacher_id=names(mm),lag.mean=as.numeric(mm))
te2<-merge(te,tmp)
z<-ranef(mod1)$teacher_id
z2<-ranef(mod2)$teacher_id
tmp<-data.frame(teacher_id=rownames(z),re1=z[,1],re2=z2[,1])
te2<-merge(te2,tmp)
cor(te2[,-1]) #the re2 estimates start to be pretty noisy, but not a ton of bias.

##this is due to the way we simulate outcomes: new scores <-f(old scores) + teacher effects + error
##estimates of the teacher effects are going to be fairly disentangled from the behavior of f() (even when f() is a real mess)
