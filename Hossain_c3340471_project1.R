##################################################
############# Sayeed Bin Hossain #################
################## c3340471 ######################
##################################################


##################################################
## Exercise 01 ###################################
##################################################

setwd("C:/Users/88017/INFT6201/")
getwd()

pimadata = read.csv("pimadata.csv", header=TRUE, sep=",", dec=".", na.strings = "?")

numberoflines<- dim(pimadata) [1]
numberofcolumns<- dim(pimadata) [2]

cat("The total number of lines:", numberoflines, "and the total number of columns:", numberofcolumns)


##################################################
## Exercise 02 ###################################
##################################################

pimadata = read.csv("pimadata.csv", header=TRUE, sep=",", dec=".", na.strings = "?")

mean_BMI_Diabetes <- mean(pimadata$BMI [pimadata$diabetes==1])
mean_BMI_noDiabetes <- mean(pimadata$BMI [pimadata$diabetes==0])

meanDiff <- mean_BMI_Diabetes - mean_BMI_noDiabetes

cat( "The difference of two groups is: ", meanDiff)


##################################################
## Exercise 03 ###################################
##################################################

pimadata = read.csv("pimadata.csv", header=TRUE, sep=",", dec=".", na.strings = "?")

pimadata = subset(pimadata, pimadata$TSFT > 0, select = c(4,9))


sd_TSFT_diabetes <- sd(pimadata$TSFT [pimadata$diabetes==1])
var_TSFT_diabetes <- var(pimadata$TSFT [pimadata$diabetes==1])

cat("The standard deviation is:", sd_TSFT_diabetes, "\nThe variance is:", var_TSFT_diabetes)

#Person with TSFT=0 has Insulin=0.

##################################################
## Exercise 04 ###################################
##################################################

pimadata = read.csv("pimadata.csv", header=TRUE, sep=",", dec=".", na.strings = "?")

sdBMI <- sd(pimadata$BMI)
n <- sqrt(dim(pimadata) [1])

sdBMI
n

#Standard Deviation shows how far the individual values are from the sample mean
#where as Standard Error shows how far the is sample mean from the population mean.

##################################################
## Exercise 05 ###################################
##################################################

pimadata = read.csv("pimadata.csv", header=TRUE, sep=",", dec=".", na.strings = "?")

pimadata <- subset(pimadata, pimadata$BMI > 0, select = c(6,8))

pimadata$agecat <- ifelse(pimadata$age %in% 21:35, "21 to 35", ifelse(pimadata$age %in% 36:55, "36 to 55", "56 to 85"))

pimadata$agecat = as.factor(pimadata$agecat)

median1 <- median(pimadata$BMI [pimadata$agecat=="21 to 35"])
median2 <- median(pimadata$BMI [pimadata$agecat=="36 to 55"])
median3 <- median(pimadata$BMI [pimadata$agecat=="56 to 85"])

medianBMIs <- c(median1, median2, median3)

cat("The minimum median BMI:", min(medianBMIs), "\nThe maximum median BMI:", max(medianBMIs))

##################################################
## Exercise 06 ###################################
##################################################

if(median1 > median2){
  cat("Age bracket of 21 to 35 is higher BMI median compare to 36 to 55")
} else {
  cat("Age bracket of 36 to 55 is higher BMI median compare to 21 to 35")
}

##################################################
## Exercise 07 ###################################
##################################################

calc90CI <- function(){
  pimadata = read.csv("pimadata.csv", header=TRUE, sep=",", dec=".", na.strings = "?")
  
  pimadata$agecat <- ifelse(pimadata$age %in% 21:35, "21 to 35", ifelse(pimadata$age %in% 36:55, "36 to 55", "56 to 85"))
  
  pimadata$agecat = as.factor(pimadata$agecat)
  
  pimadata <- subset(pimadata, pimadata$BMI>0 & pimadata$agecat=="56 to 85", select = c(6,10))
  
  meanBMI <- mean(pimadata$BMI)
  n <- sqrt(dim(pimadata) [1])
  sdBMI <- sd(pimadata$BMI)
  multiplier <- 1.64
  
  se = sdBMI/n
  
  CI_upper =  meanBMI + (se * multiplier)
  CI_lower =  meanBMI - (se * multiplier)
  
  cat(c(CI_upper,CI_lower))
}

calc90CI()