load("star_df.Rdata") #https://www.dropbox.com/s/pwmie785p1cljsw/star_df.Rdata?dl=0

std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
df$g1treadss<-std(df$g1treadss)
df$g1tmathss<-std(df$g1tmathss)


sim<-function(df) { #this is going to automate what we saw in 02
    ##params to use
    N<-nrow(df)
    pr.frl<-mean(df$g1freelunch=="FREE LUNCH",na.rm=TRUE)
    pr.class<-table(df$g1classtype)/N
    mean.class<-by(df$g1treadss,df$g1classtype,mean,na.rm=TRUE)
    ##
    df0<-df
    df<-df[,c("stdntid","g1schid")]
    #free lunch
    frl<-rbinom(N,size=1,prob=pr.frl)
    df$g1freelunch<-ifelse(frl==1,"FREE LUNCH","NON-FREE LUNCH")
    #class type
    class<-rmultinom(N,1,pr.class)
    class<-apply(class,2,which.max)
    df$g1classtype<-names(pr.class)[class]
    #scores
    df$g1treadss<-rnorm(N,mean=mean.class[class],sd=1)
    df$g1treadss<-std(df$g1treadss)
    df
}

df$small<-ifelse(df$g1classtype=="SMALL CLASS",1,0)
mod<-lm(g1treadss~small+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
co<-coef(mod)[2]
se<-summary(mod)$coef[2,2]
est<-numeric()
##We're going to show variation in the estimate as a function of sampling variation by simulating 100 datasets.
for (i in 1:100) {
    df.tmp<-sim(df)
    df.tmp$small<-ifelse(df.tmp$g1classtype=="SMALL CLASS",1,0)
    mod<-lm(g1treadss~small+factor(g1schid),df.tmp[df.tmp$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    est[i]<-coef(mod)[2]
}
plot(density(est),lwd=2,col='red',xlim=c(0,.5)) #black bits are empirical data, red bits are simulated
abline(v=co,lwd=2)
segments(est-1.96*se,0,est+1.96*se,0,lwd=2)
##How does the parametric estimate of the SE compare to the variation we observe over estimates from simulated data?

