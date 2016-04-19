# Spam or Ham
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
