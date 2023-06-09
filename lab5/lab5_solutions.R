### Title:    Stats & Methods Lab 5 Suggested Solutions
### Author:   Kyle M. Lang, L.V.D.E. Vogelsmeier, Edo
### Created:  2018-09-24
### Modified: 2022-09-16


###          ###
### Overview ###
###          ###

## You will practice using MLR models for moderation analysis.

## You will need the "msq2.rds" data and the built-in R datasets "cps3" and
## "leafshape" (from the DAAG package). The "msq2.rds" dataset is available in
## the "data" directory for this lab.


###                   ###
### Tasks / Questions ###
###                   ###


##--Preliminaries-------------------------------------------------------------##

## 1) Use the "install.packages" function to install the "rockchalk" and "DAAG"
##    packages.

install.packages(c("rockchalk", "DAAG"), repos = "http://cloud.r-project.org")

## 2) Use the "library" function to load the "rockchalk" and "DAAG" packages.

library(rockchalk)
library(DAAG)

## 3) Use the "readRDS" function to load the "msq2.rds" dataset into your
##    workspace.

dataDir <- "../data/"
msq2    <- readRDS(paste0(dataDir, "msq2.rds"))

## 4) Use the "data" function to load the "cps3" and "leafshape" datasets into
##    your workspace.

data(cps3)
data(leafshape)


##--Continuous Variable Moderation--------------------------------------------##

### Use the "msq2" data to complete the following:

## 5a) Estimate a model that tests if the effect of Energetic Arousal (EA) on
##     Tense Arousal (TA) varies as a function of Negative Affect (NegAff),
##     after controlling for Positive Affect (PA).

out1 <- lm(TA ~ PA + EA * NegAff, data = msq2)
summary(out1)

## 5b) What is the value of the parameter estimate that quantifies the effect of
##     Negative Affect on the Energetic Arousal -> Tense Arousal effect, after
##     controlling for Positive Affect?

coef(out1)["EA:NegAff"]

## 5c) Does Negative Affect significantly moderate (at the alpha = 0.05 level)
##     the relationship between Energetic Arousal and Tense Arousal, after
##     controlling for Positive Affect?

### Define a function to extract p-values from a fitted lm object:
getP <- function(obj, what) summary(obj)$coef[what, "Pr(>|t|)"]

### Define function to answer yes/no significance questions:
isSig <- function(obj, what, alpha = 0.05)
    ifelse(getP(obj, what) < alpha, "YES", "NO")

isSig(out1, "EA:NegAff")

## 5d) After controlling for Positive Affect, how does Negative Affect impact
##     the relationship between Energetic Arousal and Tense Arousal? Provide a
##     sentence interpreting the appropriate effect.

### For a unit increase in Negative Affect, the effect of Energetic Arousal on
### Tense Arousal is expected to increase by 0.0196, after controlling for
### Positive Affect.

## 6a) Use the centering method to test the simple slopes of the model you
##     estimated in (5a) at Negative Affect values of 0, 10, and 20.

msq2$na10 <- msq2$NegAff - 10
msq2$na20 <- msq2$NegAff - 20

out1.10 <- lm(TA ~ PA + EA*na10, data = msq2)
out1.20 <- lm(TA ~ PA + EA*na20, data = msq2)
summary(out1.10)
summary(out1.20)

## 6b) After controlling for Positive Affect, what is the simple slope of
##     Energetic Arousal on Tense Arousal when Negative Affect is 0.

coef(out1)["EA"]

## 6c) Is the simple slope you estimated in (6b) statistically significant at
##     the alpha = 0.05 level?

isSig(out1, "EA")

## 6d) After controlling for Positive Affect, what is the simple slope of
##     Energetic Arousal on Tense Arousal when Negative Affect is 10.

coef(out1.10)["EA"]

## 6e) Is the simple slope you estimated in (6d) statistically significant at
##     the alpha = 0.05 level?

isSig(out1.10, "EA")

## 6f) After controlling for Positive Affect, what is the simple slope of
##     Energetic Arousal on Tense Arousal when Negative Affect is 20.

coef(out1.20)["EA"]

## 6g) Is the simple slope you estimated in (6f) statistically significant at
##     the alpha = 0.05 level?

isSig(out1.20, "EA")

## 7a) Use the 'rockchalk' package to test the same simple slopes you estimated
##     in (6a).

psOut1 <-
    plotSlopes(out1, plotx = "EA", modx = "NegAff", modxVals = c(0, 10, 20))
tsOut1 <- testSlopes(psOut1)
tsOut1$hypotests

## 7b) Do the results of the centering approach agree with the results from
##     'rockchalk'?

### Calculate the differences between the two approaches:
dif <- tsOut1$hypotests[ , -1] - rbind(summary(out1)$coefficients["EA", ],
                                       summary(out1.10)$coefficients["EA", ],
                                       summary(out1.20)$coefficients["EA", ]
                                       )
### Are all differences lower than machine precision?
check <- all(dif < .Machine$double.eps)

ifelse(check, "YES", "NO")

## 8a) Use the 'rockchalk' package to implement a Johnson-Neyman analysis of the
##     interaction you estimated in (5a).

### We already did this to answer (7a).

## 8b) What are the boundaries of the Johnson-Neyman region of significance?

tsOut1$jn$roots

## 8c) Where in the distribution of Negative Affect is the effect of Energetic
##     Arousal on Tense Arousal (controlling for Positive Affect) statistically
##     significant?

summary(msq2$NegAff)

### The simple slope of Energetic Arousal on Tense Arousal is significant at all
### levels of Negative Affect.


##--Binary Categorical Moderators---------------------------------------------##

### Use the "cps3" data to complete the following:

## 9a) Estimate a model that tests if the effect of Years of Education on Real
##     Earnings in 1975 is significantly moderated by being Hispanic, after
##     controlling for Real Earnings in 1974.
##     HINT: The Hispanic variable is not a factor. You may want to change that.

?cps3
cps3$hF <-
    factor(cps3$hisp, levels = c(0, 1), labels = c("non-hispanic", "hispanic"))

out2 <- lm(re75 ~ re74 + educ * hF, data = cps3)
summary(out2)

## 9b) After controlling for 1974 Earnings, does being Hispanic significantly
##     affect the relationship between Years of Education and 1975 Earnings at
##     the alpha = 0.05 level

isSig(out2, "educ:hFhispanic")

## 9c) After controlling for 1974 Earnings, does being Hispanic significantly
##     affect the relationship between Years of Education and 1975 Earnings at
##     the alpha = 0.01 level?

isSig(out2, "educ:hFhispanic", 0.01)

## 10a) What is the simple slope of Years of Education on 1975 Earnings
##      (controlling for 1974 Earnings) for Non-Hispanic people?

coef(out2)["educ"]

## 10b) Is the simple slope from (10a) statistically significant at the
##      alpha = 0.05 level?

isSig(out2, "educ")

## 10c) What is the simple slope of Years of Education on 1975 Earnings
##      (controlling for 1974 Earnings) for Hispanic people?
##      Note: To obtain the result with more decimals you can always use
##            the coef() function.

cps3$hF2 <- relevel(cps3$hF, ref = "hispanic") 
out2.2   <- lm(re75 ~ re74 + educ * hF2, data = cps3)

coef(out2.2)["educ"]

## 10d) Is the simple slope from (10c) statistically significant at the
##      alpha = 0.05 level?

isSig(out2.2, "educ")

## 10e) Visualize the simple slopes computed above in an appropriate way.

plotSlopes(out2, plotx = "educ", modx = "hF")


##--Nominal Categorical Moderators--------------------------------------------##

### Use the "leafshape" data to complete the following:

## 11a) What are the levels of the "location" factor?

levels(leafshape$location)

## 11b) What are the group sizes for the "location" factor?

table(leafshape$location)

## 12a) Estimate a model that tests if the effect of Leaf Width on Leaf Length
##      differs significantly between Locations.

?leafshape

out3.0 <- lm(bladelen ~ bladewid + location, data = leafshape)
out3.1 <- lm(bladelen ~ bladewid * location, data = leafshape)

summary(out3.0)
summary(out3.1)

## 12b) Does the effect of Leaf Width on Leaf Length differ significantly
##      (alpha = 0.05) between Locations?

av3 <- anova(out3.0, out3.1)
sig <- av3[2, "Pr(>F)"] < 0.05

ifelse(sig, "YES", "NO")

## 12c) What is the value of the test statistic that you used to answer (12b)?

av3[2, "F"]

## 13a) What is the simple slope of Leaf Width on Leaf Length in Sabah?

levels(leafshape$location)

coef(out3.1)["bladewid"]

## 13b) Is the simple slope you reported in (13a) significant at the alpha = 0.05
##      level?

isSig(out3.1, "bladewid")

## 13c) What is the simple slope of Leaf Width on Leaf Length in Panama?

leafshape$l2 <- relevel(leafshape$location, ref = "Panama")
out3.2       <- lm(bladelen ~ bladewid * l2, data = leafshape)

coef(out3.2)["bladewid"]

## 13d) Is the simple slope you reported in (13c) significant at the alpha = 0.05
##     level?

isSig(out3.2, "bladewid")

## 13e) What is the simple slope of Leaf Width on Leaf Length in South
##      Queensland?

leafshape$l3 <- relevel(leafshape$location, ref = "S Queensland")
out3.3       <- lm(bladelen ~ bladewid*l3, data = leafshape)

coef(out3.3)["bladewid"]

## 13f) Is the simple slope you reported in (13e) significant at the alpha = 0.05
##      level?

isSig(out3.3, "bladewid")

## 14a) In which Location is the effect of Leaf Width on Leaf Length strongest?

cf  <- coef(out3.1)
tmp <- cf[grep("bladewid", names(cf))]

best <- levels(leafshape$location)[which.max(c(tmp[1], tmp[1] + tmp[-1]))]
best

## 14b) What caveat might you want to place on the conclusion reported in (14a)?
##      HINT: Look at the answers to Question 11.

table(leafshape$location)[best]

### We only have 9 observations from Tasmania. The estimated simple slope for
### Tasmania is going to be very sensitive to those particular 9 observations.

##----------------------------------------------------------------------------##
