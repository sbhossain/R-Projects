##################################################
############# Sayeed Bin Hossain #################
################## c3340471 ######################
##################################################

##################################################
## SETUP #########################################
##################################################

# install.packages("rtf")
# install.packages("moments")

library(ggplot2)
library(plyr)
library(rtf)
library(corrplot)
library(moments)

#setting working directory
setwd("C:/Users/88017/Desktop/Trimester 4/INFT6201 BIG DATA/Assessment 1 - Lab Report/Item 3 - Lab Projects/")

#Loading the csv file
forestfires=read.csv("forestfires.csv",header=TRUE,sep=",",dec=".",na.strings="?")

##################################################
## Exercise 01 ###################################
##################################################

#Separating months based on the quarter
forestfires$quarter[forestfires$month=="jan"|forestfires$month=="feb"|forestfires$month=="mar"]<-"Q1"
forestfires$quarter[forestfires$month=="apr"|forestfires$month=="may"|forestfires$month=="jun"]<-"Q2"
forestfires$quarter[forestfires$month=="jul"|forestfires$month=="aug"|forestfires$month=="sep"]<-"Q3"
forestfires$quarter[forestfires$month=="oct"|forestfires$month=="nov"|forestfires$month=="dec"]<-"Q4"

#Creating plot and directly exporting the plot into a png file
png("rh_quarters.png", width=900,height=900)
print(ggplot(forestfires, aes(x=RH,  fill=quarter)) +
        geom_density(alpha = .8) +
        scale_fill_brewer(palette="Set2", name="Quarter") +
        labs(title = "Relative Humidity for Q1-4") +
        theme_bw() +
        ylab("Density") +
        xlab("Relative Humidity"))
dev.off()


##################################################
## Exercise 02 ###################################
##################################################

#Creating dataframe "summarystat" using ddply()
summarystat<-ddply(forestfires, c("quarter"),
                   summarise,
                   N=length(quarter),
                   ffmc_avg = format(round(mean(FFMC),2), nsmall = 2),
                   dmc_avg = format(round(mean(DMC),2), nsmall = 2),
                   dc_avg = format(round(mean(DC),2), nsmall = 2),
                   isi_avg = format(round(mean(ISI),2), nsmall = 2),
                   temp_avg = format(round(mean(temp),2), nsmall = 2),
                   RH_avg = format(round(mean(RH),2), nsmall = 2),
                   wind_avg = format(round(mean(wind),2), nsmall = 2),
                   rain_avg = format(round(mean(rain),2), nsmall = 2),
                   area_avg = format(round(mean(area),2), nsmall = 2))

#Creating RTF Document
rtf<-RTF("output.rtf", width=9, height=12, font.size=10, omi=c(1,1,1,1))

#Adding the summarystat to the newly created RTF Document
colnames(summarystat)<- c( "quarter", "N", "ffmc_avg","dmc_avg","dc_avg","isi_avg", "temp_avg", "RH_avg", "wind_avg", "rain_avg", "area_avg")
addTable(rtf, summarystat, font.size=8, row.names=TRUE, NA.string="-", col.justify="C", header.col.justify="C")

#Closing the RTF document
done(rtf)

##################################################
## Exercise 03 ###################################
##################################################

#Calculating correlation matrices separately for different groups
corr1 = tapply(rownames(forestfires), forestfires$quarter,
               function(x) cor(forestfires[x,c(5,6,7,8,9,10,11,13)],
                               method="pearson", use="pairwise.complete.obs"))

#Creating pdf file
pdf(file="corrplots.pdf", width=9, heigh=9)

#Displaying all four plots into one bigger plot by
#plotting region into a 2x2 grid of panel
par(mfrow=c(2,2))

#Plotting correlation plots for each quarter
corrplot.mixed(corr1$Q1, lower="number", upper="circle", title="Q1", mar=c(0,0,1,0))
corrplot.mixed(corr1$Q2, lower="number", upper="circle", title="Q2", mar=c(0,0,1,0))
corrplot.mixed(corr1$Q3, lower="number", upper="circle", title="Q3", mar=c(0,0,1,0))
corrplot.mixed(corr1$Q4, lower="number", upper="circle", title="Q4", mar=c(0,0,1,0))
dev.off()

#Diff1: In Q4, the correlation coefficient of ISI and Wind 
#       is -0.46 which is a moderate downhill (negative) relationship.
#       Whereas in Q1, Q2 and Q3 the correlation coefficient is positive 
#       and is a weak uphill (positive) linear relationship. So we say that
#       only in Q4 when the wind speed increases the initial spread rate increases.

#Diff2: In Q1, the correlation coefficient of temp-wind is positive 0.1.
#       It gradually decreases and in Q4 reaches -0.73 which is 
#       a strong downhill (negative) linear relationship. 

#Diff3: In Q1 and Q2 the correlation coefficient of DC-ISI is 0.12 and 0.2
#       which is a weak uphill (positive) linear relationship. In Q3 the coefficient is -0.2. 
#       But in Q4 the coefficient reaches .72 which is a
#       a strong uphill (positive) linear relationship. It indicates that as
#       DC increases ISI increases strongly. 


##################################################
## Exercise 04 ###################################
##################################################

#Calculating a linear model 
lm.fit=lm(RH~temp, data=forestfires)

#Creating a scatterplot of RH over temp
with(forestfires, plot(temp, RH))
abline(lm.fit, lwd=3, col="blue")

##################################################
## Exercise 05 ###################################
##################################################

#Calculating a linear model based on the dataset "forestfires" that regresses RH on temp
lm.fit=lm(RH~temp, data=forestfires)

#Listed the coefficients of the model
coef(lm.fit)

#Here, the y-intercept is 72.282782 which is B0 and the gradient or slope is -1.482044 which is the B1.
#We can say that as the temperature increases the Relative Humidity decreases at a rate of -1.482044. 

#Get the r-squared
summary(lm.fit)$r.squared

#R-squared measures the strength of the relationship between 
#the model and the dependent variable
#Here the r-squared value is 0.2781406 which means the model explains 27.81% of the total variance. 


##################################################
## Exercise 06 ###################################
##################################################

skewness(forestfires$area)

#skewness is a measure of symmetry
#Here, skewness is 12.80963 which is a
#positive skewness which indicates that the mean of the data values is 
#larger than the median, and the data distribution is right-skewed.
