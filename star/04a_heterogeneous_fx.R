##heterogeneous treatment fx
##There has been substantial interest in the idea of heterogeneous treatment fx, or the notion that different units may be differentially impacted by the treatment.
load("star_df.Rdata") #https://www.dropbox.com/s/pwmie785p1cljsw/star_df.Rdata?dl=0
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
df$g1treadss<-std(df$g1treadss)
df$g1tmathss<-std(df$g1tmathss)
##params to use
N<-nrow(df)
pr.frl<-mean(df$g1freelunch=="FREE LUNCH",na.rm=TRUE)
pr.class<-table(df$g1classtype)/N
mean.class<-by(df$g1treadss,df$g1classtype,mean,na.rm=TRUE)
##
df0<-df
df<-df[,c("stdntid","g1schid")]
##free lunch
frl<-rbinom(N,size=1,prob=pr.frl)
df$g1freelunch<-ifelse(frl==1,"FREE LUNCH","NON-FREE LUNCH")
##class type
class<-rmultinom(N,1,pr.class)
class<-apply(class,2,which.max)
df$g1classtype<-names(pr.class)[class]

##let's produce scores where we allow for heterogeneity
mu<-mean.class[class]
mu<-2*mu*ifelse(df$g1freelunch=="FREE LUNCH",1,0) #why the 2*
df$g1treadss<-rnorm(N,mean=mu,sd=1)


results<-function(df) {
    df$small<-ifelse(df$g1classtype=="SMALL CLASS",1,0)
    mod1<-lm(g1treadss~small+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    co1<-coef(mod1)[2]
    mod2<-lm(g1treadss~small*g1freelunch+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    s<-summary(mod2)$coef
    list(co1,s[!grepl("^factor",rownames(s)),])
}
results(df)
##ok, so we are well-powered to detect effects of that size. but that is pretty extreme. what if we went with a less extreme version

##let's do the less extreme thing a bunch of times so that we don't get confused by a single result
est<-numeric()
for (i in 1:25) {
    mu<-mean.class[class]
    mu<-2*mu*ifelse(df$g1freelunch=="FREE LUNCH",1,.85)
    df$g1treadss<-rnorm(N,mean=mu,sd=1)
    est[i]<-results(df)[[2]][4,1]
}
summary(est) #we can't reliably distinguish this bit of heterogeneity from 0

##let's look at a more fully-fledged power analysis
pow<-numeric()
for (prop in seq(.5,1,by=.1)) {
    est<-numeric()
    for (i in 1:100) {
        mu<-mean.class[class]
        mu<-2*mu*ifelse(df$g1freelunch=="FREE LUNCH",1,prop)
        df$g1treadss<-rnorm(N,mean=mu,sd=1)
        est[i]<-results(df)[[2]][4,4]
    }
    pow[as.character(prop)]<-mean(est<.05)
}
pow
##so we are reasonably well-powered if prop is less than about 0.7. what do you make of results for prop=1?


results(df0) #not much evidence for heterogenity in our little example, although obviously we can't rule out small effects (as we just saw)


##one question: how sensitive is power to the relative proportion of FRL students in the sample?



