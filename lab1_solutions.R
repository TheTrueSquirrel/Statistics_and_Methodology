### Title:    Stats & Methods Lab 1 Suggested Solutions
### Authors:  Kyle M. Lang, L.V.D.E. Vogelsmeier, Edo
### Created:  2018-04-10
### Modified: 2022-02-03


###          ###
### Overview ###
###          ###

## You will practice inferential testing, some basic EDA techniques, missing
## data descriptives, and outlier analysis.

## You will need four datasets to complete the following tasks: "bfi_clean.rds",
## "tests.rds", "bfiOE.rds", and "airQual.rds". These datasets are saved in the
## "data" directory for this set of lab materials.


###                   ###
### Tasks / Questions ###
###                   ###

rm(list = ls(all = TRUE))

##--Preliminaries-------------------------------------------------------------##

## 1) If you have not already done so, use the "install.packages" function to
##    install the "mice" package.

install.packages("mice", repos = "http://cloud.r-project.org")

## 2) Use the "library" function to load the "mice" and "MASS" packages.

library(mice)
library(MASS)

## 3) Use the "paste0" function and the "readRDS" function to load the four
##    datasets into memory.

dataDir <- "../data/"

bfi1    <- readRDS(paste0(dataDir, "bfi_clean.rds"))
bfi2    <- readRDS(paste0(dataDir, "bfiOE.rds"))
tests   <- readRDS(paste0(dataDir, "tests.rds"))
airQual <- readRDS(paste0(dataDir, "airQual.rds"))


##--Testing-------------------------------------------------------------------##

### Use the "bfi_clean" data to complete the following analyses/tasks:

## 4a) Conduct a t-test to check for gender differences in mean levels of
##     "agree" (i.e., agreeableness). Do not assume equal variances.

out1.1 <- t.test(agree ~ gender, data = bfi1)
out1.1

## 4b) What is the value of the estimated mean difference in "agree"?

diff(out1.1$estimate)

### OR ###

diff(rev(out1.1$estimate))

## 4c) What is the value of the estimated t-statistic?

out1.1$statistic

## 4d) Is the estimated mean difference significant at the alpha = 0.05 level?

ifelse(out1.1$p.value < 0.05, "YES", "NO")

## 5a) Conduct a t-test to check for gender differences in mean levels of
##     "agree" (i.e., agreeableness). Assume equal variances.

out1.2 <- t.test(agree ~ gender, data = bfi1, var.equal = TRUE)
out1.2

## 5b) What is the value of the estimated mean "agree" for males?

out1.2$estimate[1]

## 5c) What is the value of the estimated t-statistic?

out1.2$statistic

## 5d) What is the 95% CI for the estimated mean difference?

out1.2$conf.int

## 5e) Is the t-statistic you found here different from the one you computed in
##     Q1? If so, why?

### YES. Although the estimated mean difference hasn't changed, the SE in Q1 was
### corrected to control for the unequal group variances, so the estimated
### t-statistic in Q1 was smaller.

### EXTRA: Compute everything manually
###        You can compute all of the values obtained with the t.test() 
###        function manually.
###        This is extra content that might help you understand important
###        statistical comcepts. However, you do not need to know how to do
###        this for the exam / quiz.
# First get all the information you need on the two samples (men and women)
mean_1 <- mean(bfi1$agree[bfi1$gender == "male"])   # mean of agree for men
s2_1   <- var(bfi1$agree[bfi1$gender == "male"])    # variance of agree for men
n_1    <- sum(bfi1$gender == "male")                # sample size of men

mean_2 <- mean(bfi1$agree[bfi1$gender == "female"]) # mean of agree for women
s2_2   <- var(bfi1$agree[bfi1$gender == "female"])  # variance of agree for women
n_2    <- sum(bfi1$gender == "female")              # sample size of women

est_h0 <- 0 # value of the estimate according to the null hypothesis

# Second, compute the estiamte of interest (difference between group means)
est <- mean_1 - mean_2

# Third, compute the pooled standard deviation
pool_sd <- sqrt( ( (n_1-1) * s2_1 + (n_2-1) * s2_2)/(n_1 + n_2 - 2))

# Fourth, compute the variability measure (standard error of the estimate)
variability <- pool_sd * sqrt(1/n_1 + 1/n_2)

# Fifth, compute the t-statistic
t_statistic <- (est - est_h0)/variability

# Sixth, compute the degrees of freedom
df <- n_1 + n_2 - 2

# Seventh, obtain the pvalue by checking the probability of observing the
# t_statistic you obsereved or something more extreme on a t-distribution 
# with degrees of freedom df
pvalue <- pt(q = t_statistic, df = df) * 2 # * 2 beacuse it's a two sided test!

# Check the value is the same as the one you computed with the t.test() function
c(fast = out1.2$p.value, manual = pvalue)

## 6a) Test for a significant Pearson correlation between "agree" and "neuro"
##    (i.e., neuroticism).

out1.3 <- with(bfi1, cor.test(agree, neuro))
out1.3

## 6b) What is the value of the correlation coefficient?

out1.3$estimate

## 6c) Is this correlation significantly different from zero at the alpha = 0.05
##     level?

ifelse(out1.3$p.value < 0.05, "YES", "NO")

## EXTRA: Compute everything manually
r <- cor(bfi1$agree, bfi1$neuro)            # correlation coefficient
n <- nrow(bfi1)                             # sample size
t_statistic <- r * sqrt((n-2)/(1-r^2))      # t-statistic
df <- n - 2                                 # degrees of freedom
pvalue <- pt(q = t_statistic, df = df) * 2  # * 2 beacuse it's a two sided test!
c(fast = out1.3$p.value, manual = pvalue)   # check the results are the same

## 7a) Test the hypothesis that the correlation between "consc"
##     (i.e., conscientiousness) and "neuro" is less than zero.

out1.4 <- with(bfi1, cor.test(consc, neuro, alternative = "less"))
out1.4

## 7b) What is the value of the estimated correlation coefficient?

out1.4$estimate

## 7c) What is the 95% CI for the estimated correlation coefficient?

out1.4$conf.int

## 7d) Is this correlation significantly less than zero at the alpha = 0.05
##     level?

ifelse(out1.4$p.value < 0.05, "YES", "NO")

## EXTRA: Compute everything manually
r <- cor(bfi1$consc, bfi1$neuro)            # correlation coefficient
n <- nrow(bfi1)                             # sample size
t_statistic <- r * sqrt((n-2)/(1-r^2))      # t statistic
df <- n - 2                                 # degrees of freedom
pvalue <- pt(q = t_statistic, df = df)      # no * 2 this time! It's one sided.
c(fast = out1.4$p.value, manual = pvalue)   # check the results are the same

##--EDA-----------------------------------------------------------------------##

### Use the "tests" data to answer the following questions:

## 8) What are the dimensions of these data?

dim(tests)

## 9) What is the mean "SATQ" score?

mean(tests$SATQ)

## 10) What is the variance of the "SATQ" scores?

var(tests$SATQ)

## 11) What is the median "SATV" score?

median(tests$SATQ)

## 12) Create a histogram of the "ACT" variable.

hist(tests$ACT)

## 13) Create a kernel density plot of the "ACT" variable.

plot(density(tests$ACT))

## 14) Overlay a normal density on top of the "ACT" histogram.

m <- mean(tests$ACT)
s <- sd(tests$ACT)
x <- with(tests, seq(min(ACT), max(ACT), length.out = 1000))
d <- dnorm(x = x, mean = m, sd = s)

hist(tests$ACT, probability = TRUE, ylim = range(d))
lines(x = x, y = d)

## 15) Create a grouped boxplot that plots "ACT" by "education".

with(tests, boxplot(ACT ~ education))

## 16) Create a frequency table of "education".

table(tests$education)

## 17) Create a contingency table that cross-classifies "gender" and
##     "education".

with(tests, table(gender, education))

## 18) Suppose a certain university admits any student with an ACT score of, at
##     least, 25. How many of the women in the "tests" data would be admitted?

tab <- with(tests, table(gender, admit = ACT >= 25))
tab
tab["female", "TRUE"]

### OR ###

sum(with(tests, gender == "female" & ACT >= 25))


##--Missing Data Descriptives-------------------------------------------------##

### Use the "bfiOE" data to answer the following questions:

## 19a) Compute the proportion of missing values for each variable.

pm <- colMeans(is.na(bfi2))
pm

## 19b) What is the percentage of missing data for "O1"?

100 * pm["O1"]

## 20a) Compute the number of observed values for each variable:

nObs <- colSums(!is.na(bfi2))
nObs

## 20b) What is the number of observed values for "E1"?

nObs["E1"]

## 21a) Compute the covariance coverage matrix.

cc <- md.pairs(bfi2)$rr / nrow(bfi2)
cc

## 21b) What is the range of covariance coverage values?

### Extract unique coverage values:
cc2 <- cc[lower.tri(cc)]
range(cc2)

## 21c) What is the covariance coverage between "E2" and "O4"?

cc["E2", "O4"]

## 21d) How many unique covariance coverages are less than 0.75?

sum(cc2 < 0.75)

## 22a) Compute the missing data patterns for these data.

pats <- md.pattern(bfi2, plot = FALSE)
pats

## 22b) How many distinct missing data patterns exist in these data?

nrow(pats) - 1

## 22c) How many missing data patterns have only one missing value?

### Find the entries in the last column equal to 1:
flag <- pats[-nrow(pats), ncol(pats)] == 1

sum(flag)

## 22d) How many observations are affected by patterns that involve only one
##      missing value?

### Convert the rownames into a numeric vector giving the number of cases
### affected by each pattern:
counts <- as.numeric(rownames(pats))[-nrow(pats)]

### Count the number of cases affected by the patterns flagged in (4d):
sum(counts[flag])


##--Outliers------------------------------------------------------------------##

### NOTE: You can use the functions provided in the demonstrations script to
###       complete the following tasks.

### Initialize the functions we'll need to answer the questions below:

## Function to implement the boxplot method:
bpOutliers <- function(x) {
    ## Compute inner and outer fences:
    iFen <- boxplot.stats(x, coef = 1.5)$stats[c(1, 5)]
    oFen <- boxplot.stats(x, coef = 3.0)$stats[c(1, 5)]
    
    ## Return the row indices of flagged 'possible' and 'probable' outliers:
    list(possible = which(x < iFen[1] | x > iFen[2]),
         probable = which(x < oFen[1] | x > oFen[2])
         )
}

## Function to implement the MAD method:
madOutliers <- function(x, cut = 2.5, na.rm = TRUE) {
    ## Compute the median and MAD of x:
    mX   <- median(x, na.rm = na.rm)
    madX <- mad(x, na.rm = na.rm)
    
    ## Return row indices of observations for which |T_MAD| > cut:
    which(abs(x - mX) / madX > cut)
}

## Function to implement a robust version of Mahalanobis distance using MCD
## estimation:
mcdMahalanobis <- function(data, prob, ratio = 0.75, seed = NULL) {
    ## Set a seed, if one is provided:
    if(!is.null(seed)) set.seed(seed)
    
    ## Compute the MCD estimates of the mean and covariance matrix:
    stats <- cov.mcd(data, quantile.used = floor(ratio * nrow(data)))
    
    ## Compute robust squared Mahalanobis distances
    md <- mahalanobis(x = data, center = stats$center, cov = stats$cov)
    
    ## Find the cutoff value:
    crit <- qchisq(prob, df = ncol(data))
    
    ## Return row indices of flagged observations:
    which(md > crit)
}

### Use the "airQual" data to complete the following:

## 23a) Use Tukey's boxplot method to find possible and probable outliers in the
##      "Ozone", "Solar.R", "Wind", and "Temp" variables.

### Define a vector of variable names:
vars <- c("Ozone", "Solar.R", "Wind", "Temp")

### Check for outliers in each of the variables named in 'vars':
bpOuts <- lapply(airQual[vars], bpOutliers)
bpOuts

## 23b) Did you find any possible outliers?

### Extract possible outliers:
posOl <- lapply(bpOuts, "[[", x = "possible")

### Check for the presence of possible outliers:
check <- any(sapply(posOl, length) > 0)
ifelse(check, "YES", "NO")

## 23c) Did you find any probable outliers?

### Extract probable outliers:
probOl <- lapply(bpOuts, "[[", x = "probable")

### Check for the presence of probable outliers:
check <- any(sapply(probOl, length) > 0)
ifelse(check, "YES", "NO")

## 23d) Which, if any, observations were possible outliers on "Ozone"?

bpOuts$Ozone$possible

## 23e) Which, if any, observations were probable outliers on "Wind"?

bpOuts$Wind$probable

## 24a) Use the MAD method (with a cutoff of 3) to find potential outliers in
##      the "Ozone", "Solar.R", "Wind", and "Temp" variables.

madOuts <- lapply(airQual[vars], madOutliers, cut = 3.0)
madOuts

## 24b) Did you find any potential outliers?

check <- length(unlist(madOuts)) > 0
ifelse(check, "YES", "NO")

## 24c) Which, if any, observations are potential outliers on "Wind"?

madOuts$Wind

### For Question 3, you will use different parameterizations of robust
### Mahalanobis distance (with MCD estimation) to check for multivariate
### outliers on the "Ozone", "Solar.R", "Wind", and "Temp" variables.

### NOTE: When running the mcdMahaonobis() function, set the seed to "235711".

## 25a) Which, if any, observations are flagged as multivariate outliers when
##      using 75% of the sample for the MCD estimation and using a probability of
##      0.99 for the cutoff value?

mcdMahalanobis(airQual[vars], prob = 0.99, ratio = 0.75, seed = 235711)

## 25b) Which, if any, observations are flagged as multivariate outliers when
##      using 75% of the sample for the MCD estimation and using a probability of
##      0.999 for the cutoff value?

mcdMahalanobis(airQual[vars], prob = 0.999, ratio = 0.75, seed = 235711)

## 25c) Which, if any, observations are flagged as multivariate outliers when
##      using 50% of the sample for the MCD estimation and using a probability of
##      0.99 for the cutoff value

mcdMahalanobis(airQual[vars], prob = 0.99, ratio = 0.5, seed = 235711)

## 25d) Which, if any, observations are flagged as multivariate outliers when
##      using 50% of the sample for the MCD estimation and using a probability of
##      0.999 for the cutoff value?

mcdMahalanobis(airQual[vars], prob = 0.999, ratio = 0.5, seed = 235711)

## 25e) Based on the above, what consequences do you observe when changing the
##      fraction of the sample used for MCD estimation and when changing the
##      cutoff probability?

### Increasing the cutoff probability or the sample ratio decreases the number
### of identified outliers.
