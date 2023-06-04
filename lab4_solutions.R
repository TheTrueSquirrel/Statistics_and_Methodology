### Title:    Stats & Methods Lab 4 Practice Script
### Author:   Kyle M. Lang, L.V.D.E. Vogelsmeier, Edo
### Created:  2018-09-24
### Modified: 2022-02-23


###          ###
### Overview ###
###          ###

## You will practice fitting MLR models with categorical predictor variables.

## You will need the built-in R datasets "bfi" (from the psych package) and
## "BMI" (from the wec) package.


###                   ###
### Tasks / Questions ###
###                   ###


##--Preliminaries-------------------------------------------------------------##

## 1) Use the "install.packages" function to install the "wec" and "psych"
##    packages.

install.packages(c("wec", "psych"), repos = "http://cloud.r-project.org")

## 2) Use the "library" function to load the "psych" and "wec" packages.

library(psych)
library(wec)

## 3) Use the "data" function to load the "bfi" and "BMI" datasets into your
##    workspace.

data(bfi)
data(BMI)

## 4) Source the "studentFunctions.R" file to initialize the summary.cellMeans()
##    function.

source("studentFunctions.R")

##--Factors-------------------------------------------------------------------##

### Use the "bfi" data to complete the following:
### -- You may ignore any missing data, for the purposes of these exercises
###    (although you should never do so in a real data analysis).

## 5) Refer to the help file of the "bfi" dataset to find the correct levels for
##    the "gender" and "education" variables.

?bfi

## 6) Create factors for the "gender" and "education" variables with sensible
##    sets of labels for the levels.

gender    <- factor(bfi$gender, levels = c(1, 2), labels = c("male", "female"))
education <- factor(bfi$education,
                    levels = 1 : 5,
                    labels = c("some_hs",
                               "hs_grad",
                               "some_college",
                               "college_grad",
                               "graduate_degree")
                    )

## 7) How many women in this sample have graduate degrees?

table(gender, education)["female", "graduate_degree"]


##--Dummy Codes---------------------------------------------------------------##

### Use the "BMI" data to complete the following:

## 8) How many levels does the "education" factor have?

nlevels(BMI$education)

## 9a) What is the reference level of the "sex" factor?

levels(BMI$sex)[1]

## 9b) What is the reference level of the "education" factor?

levels(BMI$education)[1]

## 10a) Run a linear regression model wherein "BMI" is predicted by dummy-coded
##     "sex" and "education".
##     -- Set the reference group to "male" for the "sex" factor
##     -- Set the reference group to "highest" for the "education" factor

BMI$education <- relevel(BMI$education, ref = "highest")

out1 <- lm(BMI ~ sex + education, data = BMI)
summary(out1)

## 10b) Is there a significant effect (at alpha = 0.05) of "sex" on "BMI" after
##     controlling for "education"?

### Define a function to extract p-values from a fitted lm object:
getP <- function(obj, what) summary(obj)$coef[what, "Pr(>|t|)"]

### Define function to answer yes/no significance questions:
isSig <- function(obj, what, alpha = 0.05)
    ifelse(getP(obj, what) < alpha, "YES", "NO")

isSig(out1, "sexfemale")

## 10c) Based on the parameter estimates from (10a): What is the expected BMI for 
##     males in the highest education group?

coef(out1)["(Intercept)"]


##--Cell-Means Codes----------------------------------------------------------##

### Use the "BMI" data to complete the following:

## 11) Create a new variable by centering "BMI" on 25.

BMI$bmi25 <- BMI$BMI - 25

## 12a) Regress the centered BMI from (11) onto the set of cell-means codes for
##     "education".

out2 <- lm(bmi25 ~ education - 1, data = BMI)
s2   <- summary.cellMeans(out2)
s2

## 12b) Is there a significant effect of education on BMI, at the alpha = 0.05
##     level?

f <- s2$fstatistic
p <- pf(f[1], f[2], f[3], lower.tail = FALSE)

ifelse(p < 0.05, "YES", "NO")

## 12c) What is the value of the test statistic that you used to answer (12b)?

f[1]

## 12d) Is the mean BMI level in the "lowest" education group significantly
##     different from 25, at an alpha = 0.05 level?

isSig(out2, "educationlowest")

## 12e) Is the mean BMI level in the "middle" education group significantly
##     different from 25, at an alpha = 0.05 level?

isSig(out2, "educationmiddle")

## 12f) Is the mean BMI level in the "highest" education group significantly
##     different from 25, at an alpha = 0.05 level?

isSig(out2, "educationhighest")


##--Unweighted Effects Codes--------------------------------------------------##

### Use the "BMI" data to complete the following:

## 13) Regress "BMI" onto an unweighted effects-coded representation of
##    "education" and a dummy-coded representation of "childless".
##    -- Adjust the contrasts attribute of the "education" factor to implement
##       the unweighted effects coding.

contrasts(BMI$education) <- contr.sum(levels(BMI$education))
contrasts(BMI$education)
colnames(contrasts(BMI$education)) <- c("highest", "lowest")
contrasts(BMI$education)

out3 <- lm(BMI ~ education + childless, data = BMI)
s3   <- summary(out3)
s3

## 14) Change the reference group (i.e., the omitted group) for the unweighted
##    effects codes that you implemented in (13) and rerun the model regressing
##    "BMI" onto "education" and "childless".

BMI$education            <- changeOmitted(BMI$education)
contrasts(BMI$education) <- contr.sum(levels(BMI$education))

contrasts(BMI$education)
colnames(contrasts(BMI$education)) <- c("middle","highest")
contrasts(BMI$education)

out4 <- lm(BMI ~ education + childless, data = BMI)
s4   <- summary(out4)
s4

## 15a) Based on the parameter estimates from (14): What is the expected BMI 
##     (averaged across education groups) for people with children?

coef(out4)["(Intercept)"]

## 15b) Based on the parameter estimates from (14): What is the expected 
##     difference in BMI between the most highly educated group and the average 
##     BMI across education groups, after controlling for childlessness?

coef(out4)["educationhighest"]

## 15c) Is the difference you reported in (15b) significantly different from zero,
##     at the alpha = 0.05 level?

isSig(out4, "educationhighest")

## 15d) Based on the parameter estimates from (14): What is the expected 
##     difference in BMI between the middle education group and the average BMI 
##     across education groups, after controlling for childlessness?

coef(out4)["educationmiddle"]

## 15e) Is the difference you reported in (15d) significantly different from zero,
##     at the alpha = 0.05 level?

isSig(out4, "educationmiddle")


##--Weighted Effects Codes----------------------------------------------------##

### Use the "BMI" data to complete the following:

## 16) Regress "BMI" onto a weighted effects-coded representation of "education"
##    and a dummy-coded representation of "sex".
##    -- Adjust the contrasts attribute of the "education" factor to implement
##       the weighted effects coding.

contrasts(BMI$education) <- contr.wec(BMI$education, omitted = "lowest")

out5 <- lm(BMI ~ sex + education, data = BMI)
s5   <- summary(out5)
s5

## 17) Change the reference group (i.e., the omitted group) for the weighted
##    effects codes that you implemented in (16) and rerun the model regressing
##    "BMI" onto "education" and "sex".

contrasts(BMI$education) <- contr.wec(BMI$education, omitted = "highest")

out6 <- lm(BMI ~ sex + education, data = BMI)
s6   <- summary(out6)
s6

## 18a) What is the expected difference in BMI between the least educated group
##     and the average BMI, after controlling for sex?

coef(out6)["educationlowest"]

## 18b) Is the difference you reported in (18a) significantly different from zero,
##     at the alpha = 0.01 level?

isSig(out6, "educationlowest")

## 18c) What is the expected difference in BMI between the most highly educated
##     group and the average BMI, after controlling for sex?

coef(out5)["educationhighest"]

## 18d) Is the difference you reported in (18c) significantly different from zero,
##     at the alpha = 0.01 level?

isSig(out5, "educationhighest")

## 19a) Does education level explain a significant proportion of variance in BMI,
##     above and beyond sex?

out7 <- update(out6, ". ~ . - education")
summary(out7)

av7  <- anova(out7, out6)
av7

sig <- av7[2, "Pr(>F)"] < 0.05
ifelse(sig, "YES", "NO")

## 19b) What is the value of the test statistic that you used to answer (19a)?

av7[2, "F"]

##----------------------------------------------------------------------------##
