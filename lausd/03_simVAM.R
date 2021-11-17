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

##going to use this to simulate data below
library(lme4)
mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
summary(mod)$coeff #compare to table a-1, http://files.eric.ed.gov/fulltext/ED516008.pdf
summary(mod)$varcor #can use this to compute icc
0.287^2/(0.287^2+0.549^2) #=0.21, this is smaller than the 0.297 reported in table a1 of course.
nrow(ranef(mod)[[1]]) #11341 teachers

##We're going to simulate current-year scores based on a simple formula: they are last year's scores (or 0 if no score from last year) plus teacher effects plus erro
ma$scale_score_std<-NA
sd.error<-.55
sd.teacherfx<-.29
te<-rnorm(length(unique(ma$teacher_id)),mean=0,sd=sd.teacherfx)
te<-data.frame(teacher_id=unique(ma$teacher_id),te=te)
ma<-merge(ma,te)
ma$scale_score_std_lag_1<-ifelse(is.na(ma$scale_score_std_lag_1),0,ma$scale_score_std_lag_1)

summary(mod) #old model
ma$scale_score_std<-.7*ma$scale_score_std_lag_1+ma$te+rnorm(nrow(ma),mean=0,sd=sd.error)
mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
summary(mod) #new model

## ideas:
## could peer effects actually account for stuff?
## or non-random selection
## what about measurement error in place of the 0.7

