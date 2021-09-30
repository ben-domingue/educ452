##heterogeneous treatment fx
##There has been substantial interest in the idea of heterogeneous treatment fx, or the notion that different units may be differentially impacted by the treatment.
##Indeed, work with STAR has explored this idea: 
load("star_df.Rdata")
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
##scores
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
results(df0)



