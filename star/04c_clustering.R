load("star_df.Rdata") #https://www.dropbox.com/s/pwmie785p1cljsw/star_df.Rdata?dl=0

std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
df$g1treadss<-std(df$g1treadss)
df$g1tmathss<-std(df$g1tmathss)
df0<-df


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
    df
}


df<-sim(df0)
##Let's see how clustered scores are within schools as compared to our simulated work
ratio.sd<-function(df) {
    s0<-var(df$g1treadss,na.rm=TRUE)
    mu<-aggregate(df$g1treadss,list(df$g1schid),mean,na.rm=TRUE)
    s1<-var(mu[,2],na.rm=TRUE)
    s1/s0 #the variance of school level means standardized by the overall outcome variance
}
ratio.sd(df0)
ratio.sd(df) ##What's going on here?

##How can we simulate data to have the right structure?
sim2<-function(df,mu) { #note the mu argument! this is going to contain information about school-specific effects
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
    df
}

mu<-aggregate(df0$g1treadss,list(df0$g1schid),mean,na.rm=TRUE)
mu<-data.frame(mu)
names(mu)<-c("g1schid","mu")
mu$mu<-sample(mu$mu,replace=TRUE)
df<-sim2(df0,mu)
ratio.sd(df0)
ratio.sd(df)

