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
                                        #free lunch
frl<-rbinom(N,size=1,prob=pr.frl)
df$g1freelunch<-ifelse(frl==1,"FREE LUNCH","NON-FREE LUNCH")
                                        #class type
class<-rmultinom(N,1,pr.class)
class<-apply(class,2,which.max)
df$g1classtype<-names(pr.class)[class]

##random class size
df$class.size<-ifelse(df$g1classtype=="SMALL CLASS",sample(13:17,nrow(df),replace=TRUE),sample(22:25,nrow(df),replace=TRUE))

f<-function(x) -.1*log(x-12.5)
##question: if f() was linear, how bad would the regular/small approximation be?
x<-13:25
plot(x,f(x),type='l')
df$g1treadss<-rnorm(nrow(df),mean=f(df$class.size)-f(19),sd=.5) ##note sd

df$small<-ifelse(df$g1classtype=="SMALL CLASS",1,0)
mod1<-lm(g1treadss~small+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
mod2<-lm(g1treadss~class.size+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
mod3<-lm(g1treadss~factor(class.size)+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
co3<-summary(mod3)$coef[grepl("^factor.class",rownames(summary(mod3)$coef)),][,1]
cs<-sapply(strsplit(names(co3),")"),"[",2)
co3<-c(0,co3)
cs<-c(13,cs)
co3<-co3-co3[3] ##why
##

plot(as.numeric(cs),co3,ylim=c(-.2,.2),pch=19,cex=2,col='gray',xlab="class size",ylab="offsets from class size=15")
segments(13,0,17,0,col='red',lwd=2)
segments(22,-1*coef(mod1)[2],25,-1*coef(mod1)[2],col='red',lwd=2)
m<-coef(mod2)[2]
abline(-15*m,m,col='blue')
##
x<-13:25
lines(x,f(x)-f(15),lty=2,lwd=2)
legend("topright",bty="n",legend=c("true","estimated","linear","small v big"),fill=c("black","gray","blue","red"))
