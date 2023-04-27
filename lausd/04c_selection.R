load("LA_nice_sub.Rdata")

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
ma$scale_score_std<-NA
sd.error<-.55
sd.teacherfx<-sd(ma$scale_score_std_lag_1,na.rm=TRUE) #.29
te<-rnorm(length(unique(ma$teacher_id)),mean=0,sd=sd.teacherfx)
te<-data.frame(teacher_id=unique(ma$teacher_id),te=te)
ma$id<-paste(ma$school_id,ma$year,ma$grade)
L<-split(ma,ma$id)

f<-function(x,sig,te) {
    tmp<-te[te$teacher_id %in% unique(x$teacher_id),]
    sc<-ifelse(is.na(x$scale_score_std_lag_1),0,x$scale_score_std_lag_1)
    p<-outer(sc,tmp$te,function(x,y) abs(x-y))
    p<-t(apply(p,1,dnorm,mean=0,sd=sig))
    p<-p/rowSums(p)
    class<-numeric()
    for (i in 1:nrow(p)) class[i]<-which.max(rmultinom(1,1,p[i,]))
    x$te<-tmp$te[class]
    x$teacher_id<-tmp$teacher_id[class]
    x
}

rmse<-function(x,y) sqrt(mean((x-y)^2))
library(lme4)
for (sig in c(.5,10)) {
    L2<-lapply(L,f,te=te,sig=sig)
    ma2<-data.frame(do.call("rbind",L2))
    ##
    id<-paste(ma2$teacher_id,ma$year)
    ly<-by(ma2$scale_score_std_lag_1,id,mean,na.rm=TRUE)
    print(sd(as.numeric(ly),na.rm=TRUE))
    ##
    ma2$te<-.29*ma2$te #rescaling
    ma2$scale_score_std_lag_1<-ifelse(is.na(ma2$scale_score_std_lag_1),0,ma2$scale_score_std_lag_1)
    ma2$scale_score_std<-.7*ma2$scale_score_std_lag_1+ma2$te+rnorm(nrow(ma2),mean=0,sd=sd.error)
    mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma2)
    #
    te.est<-ranef(mod)$teacher_id
    tmp<-merge(te,te.est,by.x=1,by.y=0)
    tmp[,2]<-.29*tmp[,2]
    print(rmse(tmp[,2],tmp[,3]))
    print(cor(tmp[,-1]))
}
#this doesn't really lead to bias. i don't think that selecction alone is sufficient but need to tie this into the rothstein idea more fully.
