### Title:    Stats & Methods Lab 3 Practice Script
### Author:   Kyle M. Lang, L.V.D.E. Vogelsmeier, Edo
### Created:  2018-04-10
### Modified: 2022-03-02


###          ###
### Overview ###
###          ###

## You will practice prediction, cross-validation, and multiple imputation.

## You will need the "yps.rds" and "bfiANC2.rds" datasets to answer the
## following questions. These datasets are available in the "data" directory for
## this lab.


###                   ###
### Tasks / Questions ###
###                   ###


##--Preliminaries-------------------------------------------------------------##

## 1) Use the "library" function to load the "MLmetrics" and "mice" packages.

library(MLmetrics)
library(mice)

## 2) Use the "source" function to source the "studentFunctions.R" script.

source("studentFunctions.R")

## 3) Use the "paste0" function and the "readRDS" function to load the "yps.rds"
##    and "bfiANC2.rds" datasets into your workspace.

dataDir <- "../data/"
bfiANC2     <- readRDS(paste0(dataDir, "bfiANC2.rds"))
yps     <- readRDS(paste0(dataDir, "yps.rds"))

##--Prediction/Split-Sample Cross-Validation----------------------------------##

### Use the "yps" data to complete the following:

## 4) Randomly split the sample into disjoint training and testing sets with
##    sample sizes of N = 800 and N = 210, respectively.

set.seed(235711) # Set the random number seed

ind   <- sample(c(rep("train", 800), rep("test", 210)))
tmp   <- split(yps, ind)
train <- tmp$train
test  <- tmp$test

## 5a) Use the training data to estimate a baseline model that regresses
##     "Number.of.friends" onto "Age" and "Gender".

out1 <- lm(Number.of.friends ~ Age + Gender, data = train)
summary(out1)

## 5b) Update the baseline model (from 5a) by adding "Keeping.promises",
##     "Empathy", "Friends.versus.money", and "Charity" as additional
##     predictors.

out2 <- update(out1, ". ~ . + Keeping.promises + Empathy + Friends.versus.money + Charity")
summary(out2)

## 5c) Update the baseline model (from 5a) by adding "Branded.clothing",
##     "Entertainment.spending", "Spending.on.looks", and "Spending.on.gadgets"
##     as additional predictors.

out3 <- update(out1, ". ~ . + Branded.clothing + Entertainment.spending + Spending.on.looks + Spending.on.gadgets")
summary(out3)

## 5d) Update the baseline model (from 5a) by adding "Workaholism",
##     "Reliability", "Responding.to.a.serious.letter", and "Assertiveness" as
##     additional predictors.

out4 <- update(out1, ". ~ . + Workaholism + Reliability + Responding.to.a.serious.letter + Assertiveness")
summary(out4)

## 6a) Compute training-set predictions from the three models you estimated in
##     (5b), (5c), and (5d).

p2 <- predict(out2)
p3 <- predict(out3)
p4 <- predict(out4)

## 6b) Compute training-set MSEs for the three models you estimated in (5b),
##     (5c), and (5d).

mse2 <- MSE(y_pred = p2, y_true = train$Number.of.friends)
mse3 <- MSE(y_pred = p3, y_true = train$Number.of.friends)
mse4 <- MSE(y_pred = p4, y_true = train$Number.of.friends)

## 6c) Compute test-set predictions from the three models you estimated in (5b),
##     (5c), and (5d).

p2.2 <- predict(out2, newdata = test)
p3.2 <- predict(out3, newdata = test)
p4.2 <- predict(out4, newdata = test)

## 6d) Compute test-set MSEs for the three models you estimated in (5b), (5c),
##     and (5d).

mse2.2 <- MSE(y_pred = p2.2, y_true = test$Number.of.friend)
mse3.2 <- MSE(y_pred = p3.2, y_true = test$Number.of.friend)
mse4.2 <- MSE(y_pred = p4.2, y_true = test$Number.of.friend)

## 6e) When comparing the models you estimated in (5b), (5c), and (5d) based on
##     their relative training-set prediction errors, which model should be
##     preferred?

mse <- c(mse2, mse3, mse4)
mse
min(mse)
which.min(mse)

## 6f) When comparing the models you estimated in (5b), (5c), and (5d) based on
##     their relative test-set prediction errors, which model should be
##     preferred?

mse.2 <- c(mse2.2, mse3.2, mse4.2)
mse.2
min(mse.2)
which.min(mse.2)

## 7) Randomly split the sample into disjoint training, validation, and testing
##    sets with sample sizes of N = 700, N = 155, and N = 155, respectively.

set.seed(235711) # Set the random number seed

ind   <- sample(c(rep("train", 700), rep("valid", 155), rep("test", 155)))
yps2  <- split(yps, ind)
train <- yps2$train
valid <- yps2$valid
test  <- yps2$test

## 8a) Use the training data from (7) to re-estimate the model from (5b).

out2.2 <- update(out2, data = train)
summary(out2.2)

## 8b) Use the training data from (7) to re-estimate the model from (5c).

out3.2 <- update(out3, data = train)
summary(out3.2)

## 8c) Use the training data from (7) to re-estimate the model from (5d).

out4.2 <- update(out4, data = train)
summary(out4.2)

## 9a) Compute the validation-set predictions from the three models you
##     estimated in (8a), (8b), and (8c).

p2.3 <- predict(out2.2, newdata = valid)
p3.3 <- predict(out3.2, newdata = valid)
p4.3 <- predict(out4.2, newdata = valid)

## 9b) Compute the validation-set MSEs for the three models you estimated in
##     (8a), (8b), and (8c).

mse2.3 <- MSE(y_pred = p2.3, y_true = valid$Number.of.friend)
mse3.3 <- MSE(y_pred = p3.3, y_true = valid$Number.of.friend)
mse4.3 <- MSE(y_pred = p4.3, y_true = valid$Number.of.friend)

mse.3 <- c(mse2.3, mse3.3, mse4.3)

## 9c) When comparing the models you estimated in (8a), (8b), and (8c) based on
##     their relative prediction errors, which model should be preferred?

mse.3
min(mse.3)
which.min(mse.3)

## 10a) Re-estimate the chosen model in (9c) using the combined training and
##     validation data.

out3.3 <- update(out3.2, data = rbind(train, valid))
summary(out3.3)

## 10b) Use the testing data that you set aside in (7) to estimate the prediction
##     error (i.e., test set MSE) of the updated model chosen in (10a).

MSE(y_pred = predict(out3.3, newdata = test), y_true = test$Number.of.friend)


##--Prediction/K-Fold Cross-Validation----------------------------------------##

### Use the "yps" data to complete the following:

## 11) Randomly split the sample into disjoint training and testing sets with
##    sample sizes of N = 800 and N = 210, respectively.

set.seed(235711) # Set the random number seed

index <- sample(c(rep("train", 800), rep("test", 210)))
yps2  <- split(yps, index)
train <- yps2$train
test  <- yps2$test

## 12) Use the training data from (11) to run 5-fold cross-validation comparing
##    the following three models:
##    -- A model regressing "Number.of.friends" onto "Age" and "Gender",
##       "Keeping.promises", "Empathy", "Friends.versus.money", and "Charity".
##    -- A model regressing "Number.of.friends" onto "Age" and "Gender",
##       "Branded.clothing", "Entertainment.spending", "Spending.on.looks", and
##       "Spending.on.gadgets".
##    -- A model regressing "Number.of.friends" onto "Age" and "Gender",
##       "Workaholism", "Reliability", "Responding.to.a.serious.letter", and
##       "Assertiveness".

models <- c("Number.of.friends ~ Age + Gender + Keeping.promises + Empathy + Friends.versus.money + Charity",
            "Number.of.friends ~ Age + Gender + Branded.clothing + Entertainment.spending + Spending.on.looks + Spending.on.gadgets",
            "Number.of.friends ~ Age + Gender + Workaholism + Reliability + Responding.to.a.serious.letter + Assertiveness")

### Check the formulas by training the models:
fits <- lapply(models, lm, data = yps2$train)
lapply(fits, summary)

### Perform the cross-validation with the cv.lm() function using "seed = 235711" inside the function:
cve <- cv.lm(data   = train,
             models = models,
             K      = 5,
             seed   = 235711)

## 13a) When comparing the models you tested in (12) based on their relative
##     cross-validation errors, which model should be preferred?

cve
cve[which.min(cve)]

## 13b) Use the testing data that you set aside in (11) to estimate the prediction
##     error (i.e., test set MSE) of the model chosen in (13a).

MSE(y_pred = predict(fits[[which.min(cve)]], newdata = test),
    y_true = test$Number.of.friends)

##--Multiple Imputation-------------------------------------------------------##

### Use the "bfiANC2" data for the following:

## 14) Use the "mice" package, with the following setup, to create multiple
##    imputations of the "bfiANC2" data.
##    -- 25 imputations
##    -- 15 iterations
##    -- A random number seed of "314159"
##    -- All "A" variables imputed with predictive mean matching
##    -- All "C" variables imputed with linear regression using bootstrapping
##    -- All "N" variables imputed with Bayesian linear regression
##    -- The "education" variable imputed with polytomous logistic regression
##    -- A predictor matrix generated with the "quickpred" function using the
##       following setup:
##    ---- The minimum correlation set to 0.25
##    ---- The "age" and "gender" variables included in all elementary
##         imputation models
##    ---- The "id" variable excluded from all elementary imputation models
##
##    Tip 1: starts with defining your own method vector (e.g., "meth") and the 
##    predictor matrix (e.g., "predMat").
##    Tip 2: use the helpfunctions ?mice and ?quickpred to read more about the
##    arguments (e.g., the built-in imputation methods).

## Chack the column names:
meth        <- rep("", ncol(bfiANC2))
names(meth) <- colnames(bfiANC2)

## Define our own method vector:
meth[2:6] <- rep("pmm", 5)
meth[7:11] <- rep("norm.boot",5)
meth[12:16] <- rep("norm",5)
meth["education"] <- "polyreg"

## A more fancy/eficient way would be the following:
# meth        <- rep("", ncol(bfiANC2))
# names(meth) <- colnames(bfiANC2)
# 
# meth[grep("^A\\d", names(meth))] <- "pmm"
# meth[grep("^N\\d", names(meth))] <- "norm"
# meth[grep("^C\\d", names(meth))] <- "norm.boot"
# meth["education"]                <- "polyreg"

## Check the method vector:
meth

## Use mice::quickpred to generate a predictor matrix:
predMat <- quickpred(data   = bfiANC2, 
                     mincor  = 0.25, 
                     include = c("age", "gender"), 
                     exclude = "id")

## Check the predictor matrix:
predMat

## Impute missing using the method vector from above:
miceOut1 <- mice(data = bfiANC2, 
                 m = 25, 
                 maxit = 15, 
                 predictorMatrix = predMat,
                 method = meth, 
                 seed = 314159)

## 15a) Create traceplots of the imputed values' means and SDs to check that the
##     imputation models converged.

plot(miceOut1)

## 15b) Create overlaid density plots of the imputed vs. observed values to
##     sanity-check the imputaitons.

densityplot(miceOut1)

## 15c) Based on the plots you created in (15a) and (15b), would you say that the
##     imputations are valid?

### YES. THE IMPUTATION MODELS LOOK TO HAVE CONVERGED AND THE IMPUTED VALUES
### SEEM REASONABLE.

## 16a) Use the "lm.mids" function to regress "A1" onto "C1", "N1", and
##     "education" using the multiply imputed data from (14)

fits1 <- lm.mids(A1 ~ C1 + N1 + education, data = miceOut1)
summary(fits1)

# You will get a warning saying that you should use the with() function instead
# of lm.mids(). If you check out the lm.mids helpfile you will see that in the
# description it is mentioned the function is superseded by with.mids().
# Both currently work, you can check the results are the same:
fits1_with <- with(miceOut1, lm(A1 ~ C1 + N1 + education))
summary(fits1_with)

## 16b) Use the "lm.mids" function to regress "A1" onto "C1", "N1", "education",
##     "age", and "gender" using the multiply imputed data from (14)

fits2 <- lm.mids(A1 ~ C1 + N1 + education + age + gender, data = miceOut1)
summary(fits2)


## 17a) What is the MI estimate of the slope of "age" on "A1" from (16b)?

poolFit2 <- pool(fits2)
summary(poolFit2)

tmp     <- poolFit2$pooled[poolFit2$pooled == "age", ]
tmp["estimate"]

## 17b) Is the effect in (17a) significant at the alpha = 0.05 level?

### Compute the t-statistic and p-value (or read it from the summary):
tStat <- with(tmp, estimate / sqrt(t))
pVal  <- 2 * pt(q = abs(tStat), df = tmp$df, lower.tail = FALSE)

ifelse(pVal < 0.05, "YES", "NO")

## 18a) What is the MI estimate of the R^2 from the model in (16a)?

r2.1 <- pool.r.squared(fits1)[ , "est"]
r2.1

## 18b) What is the MI estimate of the R^2 from the model in (16b)?

r2.2 <- pool.r.squared(fits2)[ , "est"]
r2.2

## 19a) What is the MI estimate of the increase in R^2 when going from the model
##     in (16a) to the model in (16b)?

r2.2 - r2.1

## 19b) Is the increase in R^2 from (19a) statistically significant at the
##     alpha = 0.05 level?

## Do an F-test for the increase in R^2:
out6b <- D1(fits2, fits1)

check <- out6b$result[ , "P(>F)"] < 0.05
ifelse(check, "YES", "NO")

## 19c) What is the value of the test statistic that you used to answer (19b)?
out6b$result[ , "F.value"]
