set.seed(8675309)
library(MASS)
xz<-mvrnorm(10000,mu=rep(0,2),Sigma=diag(1,2))
x<-xz[,1]
z<-xz[,2]
y<-.5*x+.7*z+rnorm(length(x))
df<-data.frame(x=x,z=z,y=y)

m<-lm(y~x+z,df) #note that here we are observing z!! different than what we previously considered here. https://github.com/ben-domingue/educ452/blob/main/regression_example.R
