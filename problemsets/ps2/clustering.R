load("star_df.Rdata") #https://www.dropbox.com/s/pwmie785p1cljsw/star_df.Rdata?dl=0

std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
df$g1treadss<-std(df$g1treadss)
df$g1tmathss<-std(df$g1tmathss)
df0<-df

##here is a baseline estimate to which we will compare what comes later
est<-lm(g1treadss~g1classtype,df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
summary(est)$coef

ratio.sd<-function(df) { #this is what we used in class
    s0<-var(df$g1treadss,na.rm=TRUE)
    mu<-aggregate(df$g1treadss,list(df$g1schid),mean,na.rm=TRUE)
    s1<-var(mu[,2],na.rm=TRUE)
    s1/s0
}
sim2<-function(df,mu) { #we also used this in class
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
    df$g1treadss<-df$g1treadss+df$mu 
    ##
    mod0<-lm(g1treadss~g1classtype,df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    mod1<-lm(g1treadss~g1classtype+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    c(ratio.sd(df),summary(mod0)$coef[2,1:2],summary(mod1)$coef[2,1:2]) #this will be the ratio of SDs, the vanilla lm estimate+SE, and then the estimate+SE for when we add school fixed effects
}
mu<-aggregate(df0$g1treadss,list(df0$g1schid),mean,na.rm=TRUE)
mu<-data.frame(mu)
names(mu)<-c("g1schid","mu")
mu$mu<-sample(mu$mu,replace=TRUE)

out<-list()
for (prop in seq(0,3,length.out=6)) { #here is where we are innovating! note the role of prop
    l<-list()
    for (i in 1:5) {
        mu.tmp<-mu
        mu.tmp$mu<-sample(mu.tmp$mu,replace=TRUE) #note school effects are random noise. not connected to frl for eaxmple
        mu.tmp$mu<-mu.tmp$mu*prop
        l[[i]]<-sim2(df0,mu.tmp)
    }
    tab<-do.call("rbind",l)
    out[[as.character(prop)]]<-c(prop,colMeans(tab))
}
tab<-data.frame(do.call("rbind",out))
names(tab)<-c("prop","sd.ratio","est.vanilla","se.vanilla","est.fe","se.fe")
tab
summary(est)$coef
##What do you see? What changes as we increase the between-school variation in test scores?
ratio.sd(df)
