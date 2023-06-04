### Title:    Stats & Methods Lab 2 Suggested Solutions
### Author:   Kyle M. Lang, L.V.D.E. Vogelsmeier, Edo
### Created:  2018-04-10
### Modified: 2022-02-15


###          ###
### Overview ###
###          ###

## You will practice basic regression modeling and model comparison.

## You will need the "longley.rds" dataset. This dataset is available in the
## "data" directory for this lab.

## IMPORTANT NOTE:
## You have to load the data from the folder as indicated here. R has a "longley" 
## dataset as well but, if you use this, it will lead to different results because 
## it differs a bit from the one in the folder. As it is a build-in dataset, 
## the function lm(GNP ~ Year, data = longley) would even work without loading the 
## build-in data. If you use the wrong dataset and obtain wrong results, you will 
## not get back lost points for corresponding questions in Quiz 3. Thus, please 
## make sure to load the data from the folder and use it for all analyses.

###                   ###
### Tasks / Questions ###
###                   ###


##--Preliminaries-------------------------------------------------------------##

## 1) Use the "install.packages" function to install the "MLmetrics" package.

install.packages("MLmetrics", repos = "http://cloud.r-project.org")

## 2) Use the "library" function to load the "MLmetrics" packages.

library(MLmetrics)

## 3) Use the "paste0" function and the "readRDS" function to load the
##    "longley.rds" data into your workspace.


dataDir <- "../data/"
fn1     <- "longley.rds"
longley <- readRDS(paste0(dataDir, fn1))


##--Linear Regression---------------------------------------------------------##

### Use the "longley" data for the following:

## 4a) Regress "GNP" onto "Year".

out1 <- lm(GNP ~ Year, data = longley)

## Check the summary to make sure nothing broke:
summary(out1)

## 4b) What is the effect of "Year" on "GNP"?

coef(out1)["Year"]

## 4c) Is the effect of "Year" on "GNP" statistically significant at the
##     alpha = 0.05 level?

## Define a function to extract p-values from a fitted lm object:
getP <- function(obj, what) summary(obj)$coef[what, "Pr(>|t|)"]

## Define function to answer yes/no significance questions:
isSig <- function(obj, what, alpha = 0.05)
    ifelse(getP(obj, what) < alpha, "YES", "NO")

isSig(out1, "Year")

## 4d) Is the effect of "Year" on "GNP" statistically significant at the
##     alpha = 0.01 level?

isSig(out1, "Year", 0.01)


## 5a) Regress "GNP" onto "Year" and "Population".

out2 <- lm(GNP ~ Year + Population, data = longley)

## OR ##

out2 <- update(out1, ". ~ . + Population")

summary(out2)

## 5b) Is there a significant effect (at the alpha = 0.05 level) of "Year" on
##     "GNP", after controlling for "Population"?

isSig(out2, "Year")

## 5c) Is there a significant effect (at the alpha = 0.01 level) of "Year" on
##     "GNP", after controlling for "Population"?

isSig(out2, "Year", 0.01)

## 5d) What is the 95% confidence interval (CI) for the partial effect of
##     "Population" on "GNP"?

confint(out2)["Population", ]

## OR ##

confint(out2, "Population")


## 6a) Regress "GNP" onto "Year" and "Employed".

out3 <- lm(GNP ~ Year + Employed, data = longley)

## OR ##

out3 <- update(out1, ". ~ . + Employed")

summary(out3)

## 6b) What is the partial effect of "Employed" on "GNP", after controlling for
##     "Year"?

coef(out3)["Employed"]

## 6c) Is the partial effect of "Employed" on "GNP" statistically significant at
##     the alpha = 0.05 level?

isSig(out3, "Employed")

## 6d) What is the 99% confidence interval (CI) for the partial effect of
##     "Employed" on "GNP"?

confint(out3, parm = "Employed", level = 0.99)


## 7a) Regress "GNP" onto "Year" and "Unemployed".

out4 <- lm(GNP ~ Year + Unemployed, data = longley)

## OR ##

out4 <- update(out1, ". ~ . + Unemployed")

## OR ##

out4 <- update(out3, ". ~ . + Unemployed - Employed")

summary(out4)

## 7b) What is the partial effect of "Unemployed" on "GNP", after controlling
##     for "Year"?

coef(out4)["Unemployed"]

## 7c) Is the partial effect of "Unemployed" on "GNP" statistically significant
##     at the alpha = 0.05 level?

isSig(out4, "Unemployed")


##--Model Comparison----------------------------------------------------------##

### Use the "longley" data for the following:

## 8a) What is the difference in R-squared between the simple linear regression
##     of "GNP" onto "Year" and the multiple linear regression of "GNP" onto
##     "Year" and "Population"?

summary(out2)$r.squared - summary(out1)$r.squared

## 8b) Is this increase in R-squared significantly different from zero at the
##     alpha = 0.05 level?

av1 <- anova(out1, out2)
av1

ifelse(av1[2, "Pr(>F)"] < 0.05, "YES", "NO")

## 8c) What is the value of the test statistic that you used to answer (8b)?

av1[2, "F"]


## 9a) What is the MSE for the model that regresses "GNP" onto "Year" and
##     "Employed"?

mse <- c()

mse["employed"] <- MSE(y_pred = predict(out3), y_true = longley$GNP)
mse["employed"]

## 9b) What is the MSE for the model that regresses "GNP" onto "Year" and
##     "Unemployed"?

mse["unemployed"] <- MSE(y_pred = predict(out4), y_true = longley$GNP)
mse["unemployed"]

## 9c) According to the MSE values calculated above, is "Employed" or
##     "Unemployed" the better predictor of "GNP"?

names(which.min(mse))

## 9d) Could we do the comparison in (9c) using an F-test for the difference in
##     R-squared values? Why or why not?

### NO. THE COMPARED MODELS ARE NOT NESTED, SO WE CANNOT COMPARE THEM WITH A
### CHANGE IN R^2 TEST.
