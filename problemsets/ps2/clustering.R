load("star_df.Rdata") #https://www.dropbox.com/s/pwmie785p1cljsw/star_df.Rdata?dl=0

std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
df$g1treadss<-std(df$g1treadss)
df$g1tmathss<-std(df$g1tmathss)
df0<-df

ratio.sd<-function(df) {
    s0<-var(df$g1treadss,na.rm=TRUE)
    mu<-aggregate(df$g1treadss,list(df$g1schid),mean,na.rm=TRUE)
    s1<-var(mu[,2],na.rm=TRUE)
    s1/s0
}
sim2<-function(df,mu) { #this is going to automate what we saw in 02
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
    ##add school offset
    df<-merge(df,mu)
    df$g1treadss<-df$g1treadss+df$mu #perhaps note here that since mu is indep of anything we could use random effects models?
    ##
    mod0<-lm(g1treadss~g1classtype,df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    mod1<-lm(g1treadss~g1classtype+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    c(ratio.sd(df),summary(mod0)$coef[2,1:2],summary(mod1)$coef[2,1:2])
}
mu<-aggregate(df0$g1treadss,list(df0$g1schid),mean,na.rm=TRUE)
mu<-data.frame(mu)
names(mu)<-c("g1schid","mu")
mu$mu<-sample(mu$mu,replace=TRUE)
est<-sim2(df0,mu)

out<-list()
for (prop in seq(0,2,by=.5)) {
    l<-list()
    for (i in 1:2) {
        mu.tmp<-mu
        mu.tmp$mu.tmp<-sample(mu.tmp$mu.tmp,replace=TRUE)
        mu.tmp$mu<-mu.tmp$mu*prop
        l[[i]]<-sim2(df0,mu.tmp)
    }
    tab<-do.call("rbind",l)
    out[[as.character(prop)]]<-c(prop,colMeans(tab))
}
tab<-do.call("rbind",out)
