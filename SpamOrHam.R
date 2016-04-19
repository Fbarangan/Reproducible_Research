# Spam or Ham
library(kernlab)
data(spam)

# Perform random sampling
set.seed(3435)
trainIndicator <- rbinom(4601, size = 1, prob = 0.5)

# check table to see the above random result
table(trainIndicator)

# divide the data set into train and test
trainSpam <- spam[trainIndicator == 1,]
testSpam <- spam[trainIndicator == 0,]


# Training set
# Change the type into a numeric , the add as another variable
# # This will change nonspam = 0 and Spam into 1
trainSpam$numType <- as.numeric(trainSpam$type) - 1

# this is a logical expression. What its function is to always convert
# TRUE = 1, and FALSE = 0
costFunction <- function(x,y) sum(x != (y > 0.5))

#Just replication NA 55 times...? Ok this i just a placeholder after the
# for i ...
cvError <- rep(NA, 55)
library(boot)


for (i in 1:55) {
        lmFormula <- reformulate(names(trainSpam)[i], response = "numType" )
        glmFit <- glm(lmFormula, family = "binomial", data = trainSpam)
        cvError[i] <- cv.glm(trainSpam, glmFit, costFunction, 2 )$delta[2]
}

# which predictor has the minimum cross-validated error ?
names(trainSpam[which.min(cvError)])

# Use the best model based on the min cross-validated error
predictionModel <- glm(numType ~ charDollar, family = "binomial", data = trainSpam)

# Get prediction on the test set
predictionTest <- predict(predictionModel, testSpam)

#This is just a placeholder, again... it labelled all the observation as "nonspam"
#then based on the logic below it will override those  prob > 0.5 with the word "spam"
predictedSpam <- rep("nonspam", dim(testSpam)[1])

## Classify as "Spam for those with prob >0.5
predictedSpam[predictionModel$fitted.values >0.5] = "spam"

#Classification Table
table(predictedSpam, testSpam$type)

# Then calculate for the error. actually this is like NPV negative predictive value
# calculate diagonal
# in other words, the predictedSpam was 61 was classified as spam but should
# be nonspam, 458 was clasiffied as spam but should be nonspam
(61 + 458 ) / (1346 + 458 + 61 + 449 )
