##################################################
############# Sayeed Bin Hossain #################
################## c3340471 ######################
##################################################

##################################################
## SETUP #########################################
##################################################

# Load some packages
library(boot)   
library(pROC)
library(tree)


#Set working directory and load dataset
setwd("C:/Users/88017/Desktop/Trimester 4/INFT6201 BIG DATA/Assessment 1 - Lab Report/Item 5 - Lab Projects")
moviedata = read.csv("moviedata.csv",header=TRUE,sep=",",dec=".",na.strings="NA")

# Create new variables as per the specification with no NA values
moviedata <- subset(moviedata, !is.na(budget) & !is.na(screens) & !is.na(aggregate_followers))
moviedata$profit <- moviedata$gross - moviedata$budget
moviedata$netlikes = moviedata$likes - moviedata$dislikes


##################################################
## Exercise 01 ###################################
##################################################

moviedata$posrating<-ifelse(moviedata$ratings>=6.8,1,0)

# Fit a logistic model
glm.fit <- glm(posrating ~ aggregate_followers + dummy_sequel + netlikes + sentiment, data = moviedata, family=binomial)

# Get Summary
summary(glm.fit)

# Get the probability for each observation
moviedata$glm.probs <- predict (glm.fit, type="response")

# Prediction of movie based on probability: YES/NO
moviedata$glm.pred[moviedata$glm.probs<=.5]="No"
moviedata$glm.pred[moviedata$glm.probs>.5]="Yes"
moviedata$glm.pred <- as.factor(moviedata$glm.pred)

# Get Coefficient
coef(glm.fit)

#Call:
#glm(formula = posrating ~ aggregate_followers + dummy_sequel + 
#      netlikes + sentiment, family = binomial, data = moviedata)
#
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-1.5586  -0.9005  -0.8241   1.2825   2.0336  

#Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         -9.130e-01  2.304e-01  -3.963 7.39e-05 ***
#aggregate_followers  2.070e-08  3.232e-08   0.641  0.52184    
#dummy_sequel        -7.787e-02  3.949e-01  -0.197  0.84368    
#netlikes             6.541e-06  7.535e-06   0.868  0.38534    
#sentiment            7.000e-02  2.459e-02   2.847  0.00442 ** 
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 245.15  on 186  degrees of freedom
#Residual deviance: 233.93  on 182  degrees of freedom
#AIC: 243.93
#
#Number of Fisher Scoring iterations: 4

##################################################
## Exercise 02 ###################################
##################################################

# Create a confusion matrix
table(moviedata$glm.pred,moviedata$posrating)

#      0   1
#No  110  55
#Yes   9  13

# Calculate sensitivity
sensitivity = nrow(subset(moviedata, posrating=="1" & glm.pred=="Yes")) / 
  nrow(subset(moviedata, posrating=="1"))

sensitivity
# sensitivity = 0.1911765 a= 19.1% which means 19.1% positive cases were correctly identified by the classifier as positive.

# Calculate specificity
specificity = nrow(subset(moviedata, posrating=="0" & glm.pred=="No")) / 
  nrow(subset(moviedata, posrating=="0"))

specificity
# specificity = 0.9243697 = 92.4% which means the classifier identified 92.4% negative cases as negative.

Accuracy = ((nrow(subset(moviedata, posrating=="1" & glm.pred=="Yes"))) + (nrow(subset(moviedata, posrating=="0" & glm.pred=="No")))) / nrow(subset(moviedata))
Accuracy

#Accuracy = 0.657754 which means 65.8% of total number of predictions were correct.


##################################################
## Exercise 03 ###################################
##################################################

# Create 2 logistic regression models

#1st model - posrating on sentiment.
glm.fit1 <- glm(posrating ~ sentiment, data = moviedata, family=binomial)
moviedata$glm.probs1<-predict (glm.fit1, type="response")

#2nd model - posrating on budget, dummy_sequel, and sentiment. 
glm.fit2 <- glm(posrating ~ budget + dummy_sequel + sentiment, data = moviedata, family=binomial)
moviedata$glm.probs2<-predict (glm.fit2, type="response")


# Create a ROC Curve for model-1
roc1 <- roc(posrating ~ glm.probs1, data=moviedata)

# Call:
# roc.formula(formula = posrating ~ glm.probs1, data = moviedata)
#
# Data: glm.probs1 in 119 controls (posrating 0) < 68 cases (posrating 1).
# Area under the curve: 0.6681


# Create a ROC Curve for model-2
roc2 <- roc(posrating ~ glm.probs2, data=moviedata)

# Call:
# roc.formula(formula = posrating ~ glm.probs2, data = moviedata)
#
# Data: glm.probs2 in 119 controls (posrating 0) < 68 cases (posrating 1).
# Area under the curve: 0.7338

#plot the ROCs
plot(roc1)
plot(roc2)

#ROC curve shows the relationship between sensitivity and specificity.
#The Area Under the ROC (AUC) measures the overall performance of a classifier over all possible thresholds.
#Here, Model-1 has AUC of 0.6681 and model-2 has AUC of 0.7338 which is sightly higher. 
#So, we can say that model-2 is better than model-1. 


##################################################
## Exercise 04 ###################################
##################################################

# Use the winsor function discussed in Week 3 to create a variable "sentiment_winsor" with a multiplier of 2.2
winsor <- function(x, multiplier) {
  if(length(multiplier) != 1 || multiplier <= 0) {
    stop("bad value for 'multiplier'")}
  
  quartile1 = summary(x)[2] # Calculate lower quartile
  quartile3 = summary(x)[5] # Calculate upper quartile
  iqrange = IQR(x) # Calculate interquartile range
  
  y <- x
  boundary1 = quartile1 - (iqrange * multiplier)
  boundary2 = quartile3 + (iqrange * multiplier)
  
  y[ y < boundary1 ] <- boundary1
  y[ y > boundary2 ] <- boundary2
  
  y
}

sentiment_winsor <- winsor(moviedata$sentiment, 2.2)

# Create a regression tree that uses budget and sentiment to predict ratings.
tree.ratings <- tree(ratings~budget+sentiment_winsor, data=na.omit(moviedata))

# Create a scatterplot of budget and sentiment, and add the partitions of the regression tree.
plot(moviedata$budget, sentiment_winsor, pch=20, col="blue", xlab = "budget", ylab = "sentiment_winsor")
partition.tree(tree.ratings, ordvars=c("budget","sentiment_winsor"), add=TRUE)

summary(tree.ratings)

# Regression tree:
# tree(formula = ratings ~ budget + sentiment_winsor, data = na.omit(moviedata))
# Number of terminal nodes:  9 
# Residual mean deviance:  0.7009 = 124.8 / 178 
# Distribution of residuals:
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -2.991000 -0.419000  0.009302  0.000000  0.502300  1.795000 

##################################################
## Exercise 05 ###################################
##################################################

#Perform Cross Validation
cv.ratings = cv.tree(tree.ratings)
plot(cv.ratings)

#Get the best size
best.size <- cv.ratings$size [which(cv.ratings$dev == min(cv.ratings$dev))]

#Prune the tree. 
pruned.ratings <- prune.tree(tree.ratings, best=best.size)

#Plot the tree
plot(pruned.ratings)
text(pruned.ratings,col="blue",label=c("yval"),cex=.8)

##################################################
## Exercise 06 ###################################
##################################################

#As predictors, take into account the variables aggregate_followers, comments, likes, dislikes, and sentiment. 
tree.posrating = tree(posrating ~ aggregate_followers + comments + likes + dislikes + sentiment, data = moviedata, na.action=na.pass)
summary(tree.posrating)

# Perform cross-validation to determine the optimal tree size and prune the tree.
cv.posrating <- cv.tree(tree.posrating, FUN=prune.misclass)

# Get the best tree size
best.size <- cv.posrating$size[which(cv.posrating$dev==min(cv.posrating$dev))]

# Prune the tree
prune.posrating <- prune.misclass(tree.posrating, best=best.size)

# Plot the pruned classification tree
plot(prune.posrating)
text(prune.posrating, pretty=0)
