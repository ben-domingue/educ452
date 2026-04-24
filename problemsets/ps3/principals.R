load("LA_nice_sub.Rdata") 

####################################################################
ma<-df[df$subject=="MATHEMATICS",]

##going to use this to simulate data below
library(lme4)
mod0<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)

##let's group teacher VA estimates by school
z<-ranef(mod0)$teacher_id
z<-data.frame(teacher_id=rownames(z),te=z[,1])
x<-ma[,c("teacher_id","school_id")]
x<-x[!duplicated(x),]
x<-merge(x,z)

##let's look at variance of school means divided by total variance
tv<-var(x$te)
sm<-by(x$te,x$school_id,mean)
var(sm)/tv #this should be a number with a fairly straightforward interpretation. it's the variation in school-mean teacher VA divided by overall VA variation. we'll compute something similar for simulated adta below

##now let's construct a mock dgm
mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|school_id),ma)
summary(mod)$coeff #compare to table a-1, http://files.eric.ed.gov/fulltext/ED516008.pdf
summary(mod)$varcor #can use this to compute icc
0.285^2/(0.285^2+0.548^2) #=0.22, this is smaller than the 0.297 reported in table a1 of course.


ma$scale_score_std<-NA
sd.error<-.60
sd.schoolfx<-.29
te<-rnorm(length(unique(ma$teacher_id)),mean=0,sd=sd.teacherfx)
te<-data.frame(teacher_id=unique(ma$teacher_id),te=te)
ma<-merge(ma,te)
ma$scale_score_std_lag_1<-ifelse(is.na(ma$scale_score_std_lag_1),0,ma$scale_score_std_lag_1)

ma$scale_score_std<-.7*ma$scale_score_std_lag_1+ma$te+rnorm(nrow(ma),mean=0,sd=sd.error)
mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
