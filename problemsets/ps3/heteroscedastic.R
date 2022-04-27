load("LA_nice_sub.Rdata") 
####################################################################
ma<-df[df$subject=="MATHEMATICS",]

##going to use this to simulate data below
library(lme4)
mod0<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)

##We're going to simulate current-year scores based on a simple formula: they are last year's scores (or 0 if no score from last year) plus teacher effects plus erro
ma$scale_score_std<-NA
sd.error<-.55
sd.teacherfx<-.29
te<-rnorm(length(unique(ma$teacher_id)),mean=0,sd=sd.teacherfx)
te<-data.frame(teacher_id=unique(ma$teacher_id),te=te)
ma<-merge(ma,te)
ma$scale_score_std_lag_1<-ifelse(is.na(ma$scale_score_std_lag_1),0,ma$scale_score_std_lag_1)

ma$scale_score_std<-.7*ma$scale_score_std_lag_1+ma$te+rnorm(nrow(ma),mean=0,sd=sd.error)
mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
re<-ranef(mod)$teacher_id
te<-merge(te,re,by.x=1,by.y=0)
cor(te[,-1])

##now let's try with heteroscedastic measurement error
ma$scale_score_std_lag_1<-ifelse(is.na(ma$scale_score_std_lag_1),0,ma$scale_score_std_lag_1)
ma$scale_score_std<-.7*ma$scale_score_std_lag_1+ma$te+rnorm(nrow(ma),mean=0,sd=sd.error+.25*abs(ma$scale_score_std_lag_1))
mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
re<-ranef(mod)$teacher_id
te<-merge(te,re,by.x=1,by.y=0)
cor(te[,-1])


