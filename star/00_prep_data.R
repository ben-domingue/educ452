library(foreign)
df<-read.spss("STAR_Students.sav",to.data.frame=TRUE) #https://dataverse.harvard.edu/file.xhtml?fileId=666720&version=1.0
names(df)<-tolower(names(df))

##get a subset of data for our purposes
nms<-c("stdntid", "gender", "race", "birthyear", "flagsg1", "g1classtype", 
"g1freelunch", "g1treadss", "g1tmathss","g1schid","g1surban","g1classsize")
df<-df[,nms]

df<-df[df$flagsg1=="YES",] ##only those students in star in grade 1

save(df,file="star_df.Rdata") #my version is: https://www.dropbox.com/s/pwmie785p1cljsw/star_df.Rdata?dl=0
##save this somewhere special as you'll be using it frequently over next several weeks!


#sch<-read.spss("STAR_K-3_Schools.sav",to.data.frame=TRUE) #school-level data. not currently being used


