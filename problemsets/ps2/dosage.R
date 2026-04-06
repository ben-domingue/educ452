library(foreign)
df<-read.spss("STAR_Students.sav",to.data.frame=TRUE)
names(df)<-tolower(names(df))

df<-df[df$yearsstar==4,] ##double check this

L<-list()
for (i in 1:3) {
    ##get a subset of data for our purposes
    nms0<-c("classtype","freelunch","treadss","tmathss","schid","surban","classsize")
    nms<-c("stdntid","yearsstar", "gender", "race", "birthyear", paste("g",i,nms0,sep=''))
    z<-df[,nms]
    names(z)<-gsub(paste("g",i,sep=''),'',names(z))
    z$grade<-i
    L[[i]]<-z
}

df<-data.frame(do.call("rbind",L))

L<-split(df,df$grade)
f<-function(df) {
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    df$treadss<-std(df$treadss)
    df$tmathss<-std(df$tmathss)
    mod<-lm(treadss~classtype,df[df$classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    summary(mod)$coef
}
lapply(L,f)


