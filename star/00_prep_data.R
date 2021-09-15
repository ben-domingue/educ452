library(foreign)
df<-read.spss("STAR_Students.sav",to.data.frame=TRUE)
names(df)<-tolower(names(df))
nms<-c("stdntid", "gender", "race", "birthyear", "flagsg3", "g3classtype", 
"g3freelunch", "g3treadss", "g3tmathss","g3schid","g3surban")
df<-df[,nms]

df<-df[df$flagsg3=="YES",]
save(df,file="star_df.Rdata")

#sch<-read.spss("STAR_K-3_Schools.sav",to.data.frame=TRUE)
