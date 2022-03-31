##kind of left this for dead. so little seeming attrition that i'm not super worried about it.




## load("star_df.Rdata")

## std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
## df$g1treadss<-std(df$g1treadss)
## df$g1tmathss<-std(df$g1tmathss)

## miss<-df[is.na(df$g1treadss),]
## table(miss$g1classtype)/nrow(miss)
## nomiss<-df[!is.na(df$g1treadss),]
## table(nomiss$g1classtype)/nrow(nomiss)
## ##no differences!

## sim<-function(df,nmiss) {
##     ##params to use
##     N<-nrow(df)
##     pr.frl<-mean(df$g1freelunch=="FREE LUNCH",na.rm=TRUE)
##     pr.class<-table(df$g1classtype)/N
##     mean.class<-by(df$g1treadss,df$g1classtype,mean,na.rm=TRUE)
##     ##
##     df0<-df
##     df<-df[,c("stdntid","g1schid")]
##     #free lunch
##     frl<-rbinom(N,size=1,prob=pr.frl)
##     df$g1freelunch<-ifelse(frl==1,"FREE LUNCH","NON-FREE LUNCH")
##     #class type
##     class<-rmultinom(N,1,pr.class)
##     class<-apply(class,2,which.max)
##     df$g1classtype<-names(pr.class)[class]
##     #scores
##     df$g1treadss<-rnorm(N,mean=mean.class[class],sd=1)
##     df
## }

##     df.tmp<-sim(df)
##     df.tmp$small<-ifelse(df.tmp$g1classtype=="SMALL CLASS",1,0)
##     mod<-lm(g1treadss~small+factor(g1schid),df.tmp[df.tmp$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
##     est[i]<-coef(mod)[2]
