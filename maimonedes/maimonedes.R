x<-read.csv("final4.csv") #https://github.com/laurenhanlon/Maimonides-Rule-and-Class-Sizes/blob/master/report/final_report.pdf  https://ditraglia.com/econ224/lab08.pdf
x1<-x[x$c_size %in% 36:45,]
x2<-x[x$c_size %in% 76:85,]
x<-data.frame(rbind(x1,x2))

m<-by(x$avgmath,x$classize,mean)
plot(m)


expected_classize <- function(enrollment) {
    y <- c()
    for (i in 1:length(enrollment)) {
        denominator <- as.integer((enrollment[i] - 1) / 40) + 1
        y[i] <- enrollment[i] / denominator
    }
    y
}

#Create a vector of average class size per enrollment category

enrollment5 <- (min(x$c_size):max(x$c_size))
expected_enrollment5 <- expected_classize(enrollment5)
z<-data.frame(c_size=enrollment5,mr=expected_enrollment5)
x<-merge(x,z)

m1<-lm(c_size~tipuach+mr,x)
x$fit<-predict(m1)

m2<-lm(avgmath~fit+classize+tipuach,x)
summary(m2)
