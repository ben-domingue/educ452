////////////////////////////////////////////////////////////////////////////////
// Bernoulli
// Bernoulli distributions are of interest when we want to simulate 
// dichotomous/binary random variables
// Bernoulli distributions are weighted coins. Simple! They only have one 
// parameter: the proportion of heads/1s
// The bernoulli is a special case of the binomial distribution

set obs 1
gen bern1 = rbinomial(1, 0.5)  // Single coin toss with p=0.5

clear
set obs 10
gen bern2 = rbinomial(1, 0.5)  // 10 coin tosses with p=0.5
list

clear
set obs 100
gen bern3 = rbinomial(1, 0.95)  // 100 coin tosses with p=0.95
list

// Binary outcomes can be both predictors and outcomes. When the latter, 
// they typically get modeled using logistic regression.

////////////////////////////////////////////////////////////////////////////////
// Multinomial
// The multinomial distribution is more complicated. It will generate outcomes 
// in different categories.

// Stata doesn't have a direct rmultinom equivalent, so we'll simulate it
clear
set obs 1
// For 1 observation in one of 4 groups with equal weights
gen rand = runiform()
gen category = 1 if rand < 0.25
replace category = 2 if rand >= 0.25 & rand < 0.5
replace category = 3 if rand >= 0.5 & rand < 0.75
replace category = 4 if rand >= 0.75 & missing(category)

// For 10 observations
clear
set obs 10
gen rand = runiform()
gen category = 1 if rand < 0.25
replace category = 2 if rand >= 0.25 & rand < 0.5
replace category = 3 if rand >= 0.5 & rand < 0.75
replace category = 4 if rand >= 0.75 & missing(category)
list

// Uneven weights: c(3/4, .25/3, .25/3, .25/3) = c(0.75, 0.0833, 0.0833, 0.0833)
clear
set obs 10
gen rand = runiform()
gen category = 1 if rand < 0.75
replace category = 2 if rand >= 0.75 & rand < (0.75 + 0.0833)
replace category = 3 if rand >= (0.75 + 0.0833) & rand < (0.75 + 2*0.0833)
replace category = 4 if rand >= (0.75 + 2*0.0833) & missing(category)
list

////////////////////////////////////////////////////////////////////////////////
// Normal distribution

clear
set obs 1
gen x1 = rnormal(0, 1)  // mean=0, sd=1
list

// Creating histograms with different parameters
clear
set obs 1000
gen x = rnormal(0, 1)
histogram x, bin(30) width(0.3) xlabel(-4(1)4)
graph export "hist1.png", replace

clear
set obs 1000
gen x = rnormal(1, 1)  // mean=1, sd=1
histogram x, bin(30) width(0.3) xlabel(-4(1)4)
graph export "hist2.png", replace

clear
set obs 1000
gen x = rnormal(0, 1.5)  // mean=0, sd=1.5
histogram x, bin(30) width(0.3) xlabel(-4(1)4)
graph export "hist3.png", replace

clear
set obs 1000
gen x = rnormal(-1, 0.1)  // mean=-1, sd=0.1
histogram x, bin(30) width(0.3) xlabel(-4(1)4)
graph export "hist4.png", replace

////////////////////////////////////////////////////////////////////////////////
// Multivariate normal
// Stata uses "drawnorm" for multivariate normal

clear
set obs 10000

// Create correlation matrix and draw from bivariate normal
// Variables with correlation = 0.5
matrix C = (1, 0.5 \ 0.5, 1)
drawnorm x1 x2, means(0, 0) corr(C)

correlate x1 x2

// Create histograms
histogram x1, bin(30) width(0.3) xlabel(-4(1)4)
graph export "mvn_hist1.png", replace

histogram x2, bin(30) width(0.3) xlabel(-4(1)4)
graph export "mvn_hist2.png", replace

// Histogram of x1 where x2 > 2
histogram x1 if x2 > 2, bin(30) width(0.3) xlabel(-4(1)4)
graph export "mvn_hist3.png", replace

// Histogram of x1 where x2 < 0
histogram x1 if x2 < 0, bin(30) width(0.3) xlabel(-4(1)4)
graph export "mvn_hist4.png", replace

////////////////////////////////////////////////////////////////////////////////
// Basic regression examples

clear
set obs 10000
gen x = rnormal(0, 1)  // This will be the IV

// Generate continuous outcome using standard linear model
local b = 1
local err_var = 1
gen y = `b' * x + rnormal(0, `err_var')

regress y x

////////////////////////////////////////////////////////////////////////////////
// QQ1: Binary variable x to produce continuous outcome y

clear
set obs 10000
gen x = rbinomial(1, 0.5)  // Binary variable
gen y = 1 * x + rnormal(0, 1)
regress y x
correlate x y

// To control correlation, adjust the coefficient and/or error variance
gen y2 = 2 * x + rnormal(0, 0.5)  // Stronger relationship
correlate x y2

////////////////////////////////////////////////////////////////////////////////
// QQ2: Categorical variable x to produce continuous outcome

clear
set obs 10000
gen rand = runiform()
gen x = 1 if rand < 0.25
replace x = 2 if rand >= 0.25 & rand < 0.5
replace x = 3 if rand >= 0.5 & rand < 0.75
replace x = 4 if rand >= 0.75 & missing(x)

// Create outcome based on category
gen y = 0 + 1*(x==2) + 2*(x==3) + 3*(x==4) + rnormal(0, 1)

// Regression with categorical variable (need to use i. prefix or create dummies)
regress y i.x

// Alternatively with explicit dummies:
tabulate x, generate(x_cat)
regress y x_cat2 x_cat3 x_cat4  // x_cat1 is reference category
