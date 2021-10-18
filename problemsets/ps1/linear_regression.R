set.seed(8675309)
library(MASS)
xz<-mvrnorm(10000,mu=rep(0,2),Sigma=diag(1,2))
x<-xz[,1]
z<-xz[,2]
y<-.5*x+.7*z+rnorm(length(x))
df<-data.frame(x=x,z=z,y=y)

lm(y~x+z,df)
