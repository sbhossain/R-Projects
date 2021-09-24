##################################################
############# Sayeed Bin Hossain #################
################## c3340471 ######################
##################################################

##################################################
## SETUP #########################################
##################################################

install.packages("glmnet")
install.packages("boot")

library(ggplot2)
library (glmnet)
library(boot)

setwd("C:/Users/88017/Desktop/Trimester 4/INFT6201 BIG DATA/Assessment 1 - Lab Report/Item 4 - Lab Projects/")

housing = read.csv("Housing.csv",header=TRUE,sep=",",dec=".",na.strings="?")


##################################################
## Exercise 01 ###################################
##################################################

#Perform multiple linear regression
fit.lm <- lm(MEDV ~ CRIM + RM + NOX + DIS + AGE, data = housing)
summary(fit.lm)
summary(fit.lm)$fstatistic

#Call:
#lm(formula = MEDV ~ CRIM + RM + NOX + DIS + AGE, data = housing)
#
#  Min      1Q  Median      3Q     Max 
#-18.313  -2.917  -0.785   1.979  38.442 
#
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    -6.22734    4.01469  -1.551    0.122    
#CRIM           -0.20808    0.03404  -6.113 1.97e-09 ***
#  RM            7.73531    0.39542  19.562  < 2e-16 ***
#  NOX         -18.05089    3.94709  -4.573 6.06e-06 ***
#  DIS          -1.19104    0.21675  -5.495 6.23e-08 ***
#  AGE          -0.06662    0.01514  -4.400 1.33e-05 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 5.901 on 500 degrees of freedom
#Multiple R-squared:  0.5924,	Adjusted R-squared:  0.5883 
#F-statistic: 145.3 on 5 and 500 DF,  p-value: < 2.2e-16

#Looking at the result, we see that 
#Holding all other predictors fixed, increase in RM will increase MEDV at a rate of 7.73531.
#All other predictors has negative coefficient which indicates that increase in any of them will decrease MEDV.
#We can see our model has a y intercept of -6.22734.

#"A multiple linear regression was calculated to predict MEDV based on CRIM + RM + NOX + DIS + AGE. A significant
#regression equation was found (F(5, 500) = 145.3, p < .001), with an R² of 0.592."
#Our model explains roughly 59.2% of the variance. 

##################################################
## Exercise 02 ###################################
##################################################

#Create NOXCAT in housing dataset and factoring them into low, medium and high category.
housing$NOXCAT <- factor(ifelse(housing$NOX <= quantile(housing$NOX,0.3) , "LOW",
                                    ifelse(housing$NOX <= quantile(housing$NOX,0.7) , "MEDIUM", "HIGH")),
                             ordered = T, levels = c("LOW","MEDIUM","HIGH"))

#Create a boxplot which shows MEDV for different values of NOXCAT (LOW, MEDIUM,HIGH).
ggplot(data=housing, aes(x = NOXCAT, y = MEDV))+
  stat_boxplot(geom="errorbar")+
  geom_boxplot()+
  xlab("NOX category")+
  ylab ("MEDV")+
  ggtitle("MEDV by NOX category")


##################################################
## Exercise 03 ###################################
##################################################

#Create dummy variable for NOXCAT with low and high
housing$dummy.low[housing$NOXCAT=="LOW"] <- 1
housing$dummy.low[housing$NOXCAT!="LOW"] <- 0
housing$dummy.high[housing$NOXCAT=="HIGH"] <- 1
housing$dummy.high[housing$NOXCAT!="HIGH"] <- 0

#Regression 
lm.fitdummy = lm(MEDV ~ dummy.low + dummy.high, data = housing)
summary(lm.fitdummy)

#(Intercept)  23.2065     0.5956  38.964  < 2e-16 ***
#dummy.low     3.5664     0.9014   3.957 8.69e-05 ***
#  dummy.high   -5.9184     0.9081  -6.518 1.74e-10 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 8.423 on 503 degrees of freedom
#Multiple R-squared:  0.1646,	Adjusted R-squared:  0.1613 
#F-statistic: 49.55 on 2 and 503 DF,  p-value: < 2.2e-16

#We can see that the y intercept is 23.2065 which is the average for our default case "medium".
# The dummy low is 3.5664 and the dummy high is -5.9184.Both of this are significant. 
# This means that both of them has significant impact on MEDV. If dummy low increase MEDV incease and if dummy high increase MEDV decrease. 

##################################################
## Exercise 04 ###################################
##################################################

#Create scatterplot of MEDV by LSTAT.
ggplot(housing, aes(LSTAT, MEDV))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "red" ,formula= y ~ x )+
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x,2), colour = "green")+
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x,3), colour = "blue")+
  labs(title = "MEDV of suburb by LSTAT") +
  xlab("LSTAT") + 
  ylab("MEDV") +
  ggtitle("MEDV of suburb by LSTAT")

##################################################
## Exercise 05 ###################################
##################################################

#Linear, quadratic, cubic model and quartic model
glm.fit1 =glm(MEDV ~ LSTAT, data = housing)
glm.fit2 =glm(MEDV ~ poly(LSTAT,2), data = housing)
glm.fit3 =glm(MEDV ~ poly(LSTAT,3), data = housing)
glm.fit4 =glm(MEDV ~ poly(LSTAT,4), data = housing)

#MSE model
cv.err1=cv.glm(housing, glm.fit1)$delta 
cv.err2=cv.glm(housing, glm.fit2)$delta 
cv.err3=cv.glm(housing, glm.fit3)$delta
cv.err4=cv.glm(housing, glm.fit4)$delta

#calculated MSEs
cv.err1[1]
cv.err2[1]
cv.err3[1]
cv.err4[1]


#[1] 38.8901
#[1] 30.73622
#[1] 29.42262
#[1] 28.25187

#From the result We see a sharp drop in the estimate test MSE between the linear and quadratic fits. 
#From the quadratic model to the cubic model to the quartic model, the estimate test MSE drops slightly. 
#So the MSE drop significantly from linear to quartic model. 

##################################################
## Exercise 06 ###################################
##################################################

#11-fold validation using loop
system.time({
  cv.error.11 <- rep(0,8)
  for (i in 1:8){
    glm.fit <- glm(MEDV~poly(LSTAT,i), data=housing)
    cv.error.11[i] <- cv.glm(housing,glm.fit,K=11)$delta[1]
    }
  cv.error.11})

# Create a plot for the different MSE estimates
polyorder <- c(1:8)
estimates <- data.frame(polyorder, cv.error.11)
ggplot(data = estimates, aes(y=cv.error.11, x=polyorder)) +
  geom_line(linetype = 5, colour = "blue", size = 1) +
  geom_point(colour = "blue", size = 3) +
  theme_bw() +
  scale_x_continuous(breaks=1:11) +
  xlab("Degrees of Polynomial") + ylab("Mean Squared Error")

# 11-fold cross validation in this particular case is advantageous compared to 10-fold cross-validation
# because in this particular dataset we have 506 data and if we use 10 fold we can only cover 500 data. 
# But when we use 11 we can cover the whole dataset. Hence giving better result and not ignoring any data. 
# To conclude, no data is being ignored while we are doing the regression.