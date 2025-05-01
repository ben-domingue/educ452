##Now let's look at rothstein-like analyses


load("LA_nice_sub.Rdata") 
library(lme4)
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)

####################################################################
ma<-df[df$subject=="MATHEMATICS",]
ma<-ma[!is.na(ma$scale_score_std_lag_1),]
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
ma$scale_score_std_lag_1<-std(ma$scale_score_std_lag_1)

mod0<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
sd(ranef(mod0)$teacher_id[,1])
## 0.27. we are going to induce variation around this quantity

##assign.class() and peers() are both in 04f_abilityclustering.R

rothsteintest<-function(x) {
    x$stud_id_this_year<-paste(x$student_id,x$year,x$grade,x$subject,sep="__")
    x$stud_id_last_year<-paste(x$student_id,x$year-1,x$grade-1,x$subject,sep="__") #contrast with line 10 in 00_...
    tmp<-x[,c("stud_id_last_year","class")]
    names(tmp)<-c("stud_id_this_year","class_lead")
    x<-merge(x,tmp)
    mod<-lmer(ss~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|class_lead),x)
    sd(ranef(mod)$class_lead[,1]) #focus on what we've catpured here. what would larger or smaller numbers here make you think?
}

out<-list()
##we're going to vary two things below. sig.class will control the strength of sorting (stronger sorting when this number is smaller). peer.effect will control the strength of peer effects
for (peer.effect in c(.1,1)) for (sig.class in c(1,2))  {
                                 L<-split(ma,ma$school_id)
                                 L<-lapply(L,assign.class,sig.class=sig.class)
                                 L<-lapply(L,peers,peer.effect=peer.effect)
                                 z<-data.frame(do.call("rbind",L))
                                 ##
                                 mod<-lmer(ss~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|class),z)
                                 out[[paste(sig.class,peer.effect)]]<-c(sig.class=sig.class,peer.effect=peer.effect,teacher.sd=sd(ranef(mod)$class[,1]),rothstein=rothsteintest(z))
                             }
tab<-do.call("rbind",out)
tab

