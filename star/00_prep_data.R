library(foreign)
df<-read.spss("STAR_Students.sav",to.data.frame=TRUE)
names(df)<-tolower(names(df))

nms<-c("stdntid", "gender", "race", "birthyear", "flagsg1", "g1classtype", 
"g1freelunch", "g1treadss", "g1tmathss","g1schid","g1surban")
df<-df[,nms]

df<-df[df$flagsg1=="YES",]
save(df,file="star_df.Rdata")

#sch<-read.spss("STAR_K-3_Schools.sav",to.data.frame=TRUE)


