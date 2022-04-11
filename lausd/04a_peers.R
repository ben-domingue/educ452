load("LA_nice_sub.Rdata") 

####################################################################
ma<-df[df$subject=="MATHEMATICS",]

ma$id<-paste(ma$teacher_id,ma$year)
ly<-by(ma$scale_score_std_lag_1,ma$id,mean,na.rm=TRUE)
summary(as.numeric(ly))
ly<-data.frame(id=names(ly),last.class=as.numeric(ly))
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
ly$last.class<-std(ly$last.class)
ma<-merge(ma,ly)

for (load in c(0,.5,1)) {
    ma$scale_score_std<-NA
    sd.error<-.55
    #sd.teacherfx<-0
    #te<-rnorm(length(unique(ma$teacher_id)),mean=0,sd=sd.teacherfx)
    #te<-data.frame(teacher_id=unique(ma$teacher_id),te=te)
    #ma<-merge(ma,te)
    ma$te<-0
    ma$scale_score_std_lag_1<-ifelse(is.na(ma$scale_score_std_lag_1),0,ma$scale_score_std_lag_1)
    ma$scale_score_std<-.7*ma$scale_score_std_lag_1+ma$te+load*ma$last.class+rnorm(nrow(ma),mean=0,sd=sd.error)
    mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
    print(summary(mod))
}
