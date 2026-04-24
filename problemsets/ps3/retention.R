load("LA_nice_sub.Rdata") 

####################################################################
ma<-df[df$subject=="MATHEMATICS",]
ma<-ma[!is.na(ma$scale_score_std_lag_1),]

library(lme4)
retention<-function(delta=-2,ma) {
    ma$scale_score_std<-NA
    sd.error<-.55
    sd.teacherfx<-.29
    te<-rnorm(length(unique(ma$teacher_id)),mean=0,sd=sd.teacherfx)
    te<-data.frame(teacher_id=unique(ma$teacher_id),te=te)
    ma<-merge(ma,te)
    ma$scale_score_std<-.7*ma$scale_score_std_lag_1+ma$te+rnorm(nrow(ma),mean=0,sd=sd.error)
    ##now let's remove students
    p<-1/(1+exp(-3*(ma$scale_score_std_lag_1-delta)))
    retain<-rbinom(nrow(ma),1,p)
    print(table(retain))
    ma<-ma[retain==1,]
    print(nrow(ma))
    ##
    mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
    z<-ranef(mod)$teacher_id
    z<-data.frame(teacher_id=rownames(z),est=z[,1])
    z<-merge(te,z)
    z$bias<-z$est-z$te
    y<-by(ma$scale_score_std_lag_1,ma$teacher_id,mean,na.rm=TRUE)
    y<-data.frame(teacher_id=names(y),ly=as.numeric(y))
    z<-merge(z,y)
    list(delta,table(retain),z)
}

out<-list()
for (delta in seq(-3,-1,length.out=5)) out[[as.character(delta)]]<-retention(delta=delta,ma)

par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:length(out)) {
    z<-out[[i]][[3]]
    plot(z$ly,z$bias,col='gray',pch=19,cex=.5)
    mm<-loess(bias~ly,z)
    ly<-seq(quantile(z$ly,.01),quantile(z$ly,.99),length.out=250)
    yv<-predict(mm,data.frame(ly=ly))
    lines(ly,yv,col='red')
}

