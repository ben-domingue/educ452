load("star_df.Rdata") #https://www.dropbox.com/s/pwmie785p1cljsw/star_df.Rdata?dl=0

std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
df$g1treadss<-std(df$g1treadss)
df$g1tmathss<-std(df$g1tmathss)

################################################################
##raw effect size
by(df$g1treadss,df$g1classtype,mean,na.rm=TRUE)
##(treatment-control)/sd(control)
es0<-mean(df$g1treadss[df$g1classtype=="SMALL CLASS"],na.rm=TRUE)-mean(df$g1treadss[df$g1classtype=="REGULAR CLASS"],na.rm=TRUE)/sd(df$g1treadss[df$g1classtype=="REGULAR CLASS"],na.rm=TRUE)

##stop
################################################################
mod<-lm(g1treadss~g1classtype+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
summary(mod)$coef[1:5,]


##stop
################################################################
##gender & race
##first looking at whether there are any differences in treatment assignment as a function of group status
L<-split(df,df$gender)
lapply(L,function(x) table(x$g1classtype)/nrow(x))
L<-split(df,df$race)
lapply(L,function(x) table(x$g1classtype)/nrow(x))


mod<-lm(g1treadss~g1classtype+gender+race+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
summary(mod)$coef[1:5,]
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)                -0.396      0.137   -2.89  3.8e-03
## g1classtypeREGULAR CLASS   -0.278      0.028   -9.97  3.7e-23
## genderFEMALE                0.199      0.027    7.31  3.2e-13
## raceBLACK                  -0.314      0.052   -6.07  1.4e-09
## raceASIAN                   0.078      0.232    0.33  7.4e-01
##compare to 0.23 in table 5 of finn & achilles

################################################################
mod<-lm(g1tmathss~g1classtype+gender+race+factor(g1schid),df[df$g1classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
summary(mod)$coef[1:5,]
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)               -0.3135      0.135   -2.32  2.0e-02
## g1classtypeREGULAR CLASS  -0.2942      0.027  -10.83  5.4e-27
## genderFEMALE              -0.0086      0.026   -0.33  7.4e-01
## raceBLACK                 -0.5572      0.050  -11.05  5.0e-28
## raceASIAN                 -0.1272      0.228   -0.56  5.8e-01
##compare to 0.27 in table 5 of finn & achilles


##What are the differences betwen how we're choosing to analyze things versus the approach in F&A?
