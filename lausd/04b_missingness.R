load("LA_nice.Rdata") #https://www.dropbox.com/s/vsydexy87z9ot3u/LA_nice.Rdata?dl=0

####################################################################
#add prior score
add_scores<-function(x,lag=1) {#feed a df & # lag through function
  names(x)->nms#get names of variables
  #first get prior scores
  x$stud_id_this_year<-paste(x$student_id,x$year,x$grade,x$subject,sep="__")
  #paste names of vars together to get id for this year
  x$stud_id_last_year<-paste(x$student_id,x$year-lag,x$grade-lag,x$subject,sep="__")
  #subtract lag to get lagged grade & year
  tmp<-data.frame(stud_id_last_year=x$stud_id_this_year,scale_score_std_prior=x$scale_score_std)
  #put id & prior together
  col.nm<-paste("scale_score_std_lag",lag,sep="_")
  #rename the prior to match the lag you pulled
  names(tmp)[2]<-col.nm
  x<-merge(x,tmp,all.x=TRUE)
  #
  x[,c(nms,col.nm)]
}
df<-add_scores(df)


####################################################################
ma<-df[df$subject=="MATHEMATICS",]

x1<-ma$scale_score_std[!is.na(ma$scale_score_std_lag_1)]
x2<-ma$scale_score_std[is.na(ma$scale_score_std_lag_1)]
plot(density(x1),col='black')
lines(density(x2),col='red')


library(lme4)
mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)

ma$scale_score_std<-NA
sd.error<-.55
sd.teacherfx<-.29
te<-rnorm(length(unique(ma$teacher_id)),mean=0,sd=sd.teacherfx)
te<-data.frame(teacher_id=unique(ma$teacher_id),te=te)
ma<-merge(ma,te)

sig<-sd(ma$scale_score_std_lag_1,na.rm=TRUE)
rmse<-function(x,y) sqrt(mean((x-y)^2))
for (mu in c(0,-.05,-.1)) {
    ma$scale_score_std<-NA
    ##
    sc<-ma$scale_score_std_lag_1
    z<-rnorm(nrow(ma),mean=mu,sd=sig)
    ma$scale_score_std_lag_1<-ifelse(is.na(sc),z,sc)
    ##
    ma$scale_score_std<-.7*ma$scale_score_std_lag_1+ma$te+rnorm(nrow(ma),mean=0,sd=sd.error)
    mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
    te.est<-ranef(mod)$teacher_id
    ##
    tmp<-merge(te,te.est,by.x=1,by.y=0)
    print(rmse(tmp[,2],tmp[,3]))
}

##even if this doesn't work, it could be that it's due to the fact that missingness is unassociated with teacher quality. could simulate that
