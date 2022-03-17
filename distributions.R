##bernoulli
##the bernoulli is a special case of the binomial distribution.
##bernoulli distributions are weighted coins. simple! only parameter is the proportion of heads/1s
rbinom(1,1,.5) #note that we are calling "rbinom". the second parameter being fixed at 1 is what makes it the special case of the bernoulli distribution
rbinom(10,1,.5) #the first parameter is the number of coin tosses we want
rbinom(100,1,.95) #the last parameter is the proportion of observations that should be 1s


##multinomial

##normal
