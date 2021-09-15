load("star_df.Rdata")

std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
df$g3treadss<-std(df$g3treadss)
df$g3tmathss<-std(df$g3tmathss)

mod<-lm(g3treadss~g3classtype+gender+race+factor(g3schid),df[df$g3classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
summary(mod)$coef[1:5,]
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)                -0.094      0.155   -0.61  5.4e-01
## g3classtypeREGULAR CLASS   -0.209      0.032   -6.61  4.3e-11
## genderFEMALE                0.187      0.030    6.16  8.2e-10
## raceBLACK                  -0.397      0.061   -6.50  9.4e-11
## raceASIAN                   0.352      0.269    1.31  1.9e-01

mod<-lm(g3tmathss~g3classtype+gender+race+factor(g3schid),df[df$g3classtype %in% c("SMALL CLASS","REGULAR CLASS"),])
summary(mod)$coef[1:5,]
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)               -0.1971      0.150   -1.31  1.9e-01
## g3classtypeREGULAR CLASS  -0.1610      0.031   -5.27  1.4e-07
## genderFEMALE              -0.0062      0.029   -0.21  8.3e-01
## raceBLACK                 -0.4283      0.059   -7.21  6.8e-13
## raceASIAN                  0.4549      0.261    1.74  8.1e-02
