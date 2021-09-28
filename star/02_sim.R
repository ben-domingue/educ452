load("star_df.Rdata")

std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
df$g1treadss<-std(df$g1treadss)
df$g1tmathss<-std(df$g1tmathss)

##We're going to steal the hierarchical structure of the data (although we'll look at it first) but then capture other key features
dim(df)
length(unique(df$stdntid))
length(unique(df$g1schid))
tab<-table(df$g1schid)
summary(as.numeric(tab))

plot(density(df$g1treadss,na.rm=TRUE))
##params to use
N<-nrow(df)
pr.frl<-mean(df$g1freelunch=="FREE LUNCH",na.rm=TRUE)
pr.class<-table(df$g1classtype)/N
mean.class<-by(df$g1treadss,df$g1classtype,mean,na.rm=TRUE)

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

mod<-lm(g1treadss~g1classtype+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
summary(mod)$coef[1:2,]
mod<-lm(g1treadss~g1classtype+factor(g1schid),df0[df0$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
summary(mod)$coef[1:2,]

