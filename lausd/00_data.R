load("LA_nice.Rdata") #https://www.dropbox.com/s/vsydexy87z9ot3u/LA_nice.Rdata?dl=0

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

set.seed(101010101)
ids<-table(df$teacher_id)
ids<-ids[ids>50]
ids<-sample(names(ids),5000) ##note this!
df<-df[df$teacher_id %in% ids,]

save(df,file="LA_nice_sub.Rdata") #https://www.dropbox.com/s/xh3sektbrc81gos/LA_nice_sub.Rdata?dl=0

##how many teachers?
##how many students?
##how many students/teacher?
##how many students in a teacher's class for a given year?
