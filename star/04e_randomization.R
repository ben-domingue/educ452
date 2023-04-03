##our bootstrap analysis in 03 allowed us to see variation in point estimate given the [latent] DGM [that we replicate via resampling]
##now! we're going to look at variation in point estimates given a different DGM. in particular, we're going to ensure that there is no effect of class size by randomizing outcomes and make sure that estimates from empirical data look different

load("star_df.Rdata") #https://www.dropbox.com/s/pwmie785p1cljsw/star_df.Rdata?dl=0

std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
df$g1treadss<-std(df$g1treadss)
df$g1tmathss<-std(df$g1tmathss)

randomize<-function(df) {
    df$g1treadss<-sample(df$g1treadss,size=nrow(df))
    df
}

randomize.school<-function(df) { ##why this?
    L<-split(df,df$g1schid)
    L<-lapply(L,randomize)
    data.frame(do.call("rbind",L))
}

randomize.frl<-function(df) {
    df$id<-paste(df$g1schid,df$g1freelunch)
    L<-split(df,df$id)
    L<-lapply(L,randomize)
    df<-data.frame(do.call("rbind",L))
    df$id<-NULL
    df
}

df$small<-ifelse(df$g1classtype=="SMALL CLASS",1,0)
mod<-lm(g1treadss~small+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
co<-coef(mod)[2]
se<-summary(mod)$coef[2,2]
est.randomize<-est.randomizeschool<-est.frl<-numeric()
##We're going to show variation in the estimate as a function of sampling variation by simulating 100 datasets.
for (i in 1:100) {
    df.tmp<-df 
    df.tmp$small<-ifelse(df.tmp$g1classtype=="SMALL CLASS",1,0)
    df.tmp<-randomize(df.tmp)
    mod<-lm(g1treadss~small,df.tmp[df.tmp$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    est.randomize[i]<-coef(mod)[2]
    ##
    df.tmp<-df 
    df.tmp$small<-ifelse(df.tmp$g1classtype=="SMALL CLASS",1,0)
    df.tmp<-randomize.school(df.tmp)
    df.tmp$small<-ifelse(df.tmp$g1classtype=="SMALL CLASS",1,0)
    mod<-lm(g1treadss~small+factor(g1schid),df.tmp[df.tmp$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    est.randomizeschool[i]<-coef(mod)[2]
    ##
    df.tmp<-df 
    df.tmp$small<-ifelse(df.tmp$g1classtype=="SMALL CLASS",1,0)
    df.tmp<-randomize.frl(df.tmp)
    df.tmp$small<-ifelse(df.tmp$g1classtype=="SMALL CLASS",1,0)
    mod<-lm(g1treadss~small+factor(g1schid),df.tmp[df.tmp$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
    est.frl[i]<-coef(mod)[2]
}

plot(density(est.randomize),lwd=3,col='red',xlim=c(-.5,.5))
#lines(density(est.noeffect),lwd=2,col='gray')
lines(density(est.randomizeschool),lwd=3,col='green',lty=2)
lines(density(est.frl),lwd=3,col='blue',lty=3)
abline(v=co,lwd=2,col='black')
segments(co-1.96*se,0,co+1.96*se,0,lwd=2,col='black')
