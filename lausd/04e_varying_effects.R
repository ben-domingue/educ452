##here we will let teacher effects depend on prior year's score

load("LA_nice_sub.Rdata") 
library(lme4)
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)

####################################################################
ma<-df[df$subject=="MATHEMATICS",]

##we're goign to simulate scores but in a sneaky way
sd.error<-.55
sd.teacherfx<-.29
te<-rnorm(length(unique(ma$teacher_id)),mean=0,sd=sd.teacherfx)
te<-data.frame(teacher_id=unique(ma$teacher_id),te=te)
ma<-merge(ma,te)
ma<-ma[!is.na(ma$scale_score_std_lag_1),]
##
ma0<-ma #ma0 is original data
##mod0<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma0)
##

##first we'll just allow for a little variation such that teachers are slightly more effective with students who had somewhat lower scores last year.
xx<-std(ma$scale_score_std_lag_1)
dd<-1-pnorm(xx)
ran<-c(.9,1.1)
sl<-(dd*(ran[2]-ran[1])+ran[1])
ma$scale_score_std1<-xx+ma$te*sl+rnorm(nrow(ma),mean=0,sd=sd.error) #some degree of violation of the linear association between last year and this year
mod1<-lmer(scale_score_std1~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)

##second we'll make it such that teachers don't have effects! it's all a function of different gains for students based on prior year scores
xx<-std(ma$scale_score_std_lag_1)
dd<-1-pnorm(xx)
ran<-c(-1,1)
sl<-(dd*(ran[2]-ran[1])+ran[1])
ma$scale_score_std2<-xx+sl+rnorm(nrow(ma),mean=0,sd=sd.error) #some degree of violation of the linear association between last year and this year
mod2<-lmer(scale_score_std2~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)


mm<-by(ma$scale_score_std_lag_1,ma$teacher_id,mean,na.rm=TRUE)
tmp<-data.frame(teacher_id=names(mm),lag.mean=as.numeric(mm))
te2<-merge(te,tmp)
z<-ranef(mod1)$teacher_id
z2<-ranef(mod2)$teacher_id
tmp<-data.frame(teacher_id=rownames(z),re1=z[,1],re2=z2[,1])
te2<-merge(te2,tmp)
sd(te2$re1)
sd(te2$re2) #pretty much 0
cor(te2[,-1]) #the re2 estimates don't capture anything about teachers which, given the design, they shouldn't!

