load("LA_nice_sub.Rdata") 
 
library(lme4)
ma<-df[df$subject=="MATHEMATICS",]
mm<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
re<-df[df$subject=="READING",]
mr<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),re)

z<-ranef(mm)$teacher_id
z2<-ranef(mr)$teacher_id
te<-merge(z,z2,by=0)
cor(te[,-1])

