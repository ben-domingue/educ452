##heterogeneous treatment fx
##There has been substantial interest in the idea of heterogeneous treatment fx, or the notion that different units may be differentially impacted by the treatment.
##Indeed, work with STAR has explored this idea: 
load("star_df.Rdata") #on github, see data folder
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
                                        #free lunch
frl<-rbinom(N,size=1,prob=pr.frl)
df$g1freelunch<-ifelse(frl==1,"FREE LUNCH","NON-FREE LUNCH")
                                        #class type
class<-rmultinom(N,1,pr.class)
class<-apply(class,2,which.max)
df$g1classtype<-names(pr.class)[class]

##random class size
df$g1classsize<-ifelse(df$g1classtype=="SMALL CLASS",sample(13:17,nrow(df),replace=TRUE),sample(22:25,nrow(df),replace=TRUE))


results<-function(df,f=NULL) {
    rho<-cor(df$g1classsize,df$g1treadss,use='p')
    df$small<-ifelse(df$g1classtype=="SMALL CLASS",1,0)
    mod1<-lm(g1treadss~small+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    mod2<-lm(g1treadss~g1classsize+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    mod3<-lm(g1treadss~factor(g1classsize)+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    co3<-summary(mod3)$coef[grepl("^factor.g1class",rownames(summary(mod3)$coef)),][,1]
    cs<-sapply(strsplit(names(co3),")"),"[",2)
    co3<-c(0,co3)
    cs<-c(13,cs)
    co3<-co3-co3[3] ##why
    ##
    plot(as.numeric(cs),co3,ylim=c(-.5,.5),pch=19,cex=2,col='gray',xlab="class size",ylab="offsets from class size=15",xlim=c(10,30))
    segments(13,0,17,0,col='red',lwd=2)
    segments(22,-1*coef(mod1)[2],25,-1*coef(mod1)[2],col='red',lwd=2)
    m<-coef(mod2)[2]
    abline(-15*m,m,col='blue')
    ##
    if (!is.null(f)) {
        x<-13:25
        lines(x,f(x)-f(15),lty=2,lwd=2)
        title<-''
    } else title<-"empirical"
    legend("topright",bty="n",legend=c("true","estimated","linear","small v big"),fill=c("black","gray","blue","red"),title=title)
    ##
    rho
}

par(mgp=c(2,1,0),mar=c(3,3,1,1),mfrow=c(2,2))
#################################
##vanilla
results(df0)
#################################
##linear functional form
f<-function(x) -.02*(x-19)
##question: given f is linear, how bad would the regular/small approximation be?
x<-13:25
#plot(x,f(x),type='l')
df$g1treadss<-rnorm(nrow(df),mean=f(df$g1classsize)-f(19),sd=1) ##note sd
results(df,f=f)
#################################
##logarithmic
f<-function(x) -.1*log(x-12.5)
#plot(x,f(x),type='l')
df$g1treadss<-rnorm(nrow(df),mean=f(df$g1classsize)-f(19),sd=.5) ##note sd
results(df,f=f)
#################################
f<-function(x) (-.08*(x-19))^2
#plot(x,f(x),type='l')
df$g1treadss<-rnorm(nrow(df),mean=f(df$g1classsize)-f(19),sd=.5) ##note sd
results(df,f=f)
