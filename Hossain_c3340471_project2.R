##################################################
############# Sayeed Bin Hossain #################
################## c3340471 ######################
##################################################

##################################################
## SETUP #########################################
##################################################

# Install some packages (only needed when used for first time)
# install.packages("ggplot2")
install.packages("Hmisc")
install.packages("RColorBrewer")
install.packages("wesanderson")
install.packages("plyr")


# Load some packages
library(ggplot2)
library(Hmisc)
library(RColorBrewer) # A color scale package
library(wesanderson)  # Another color scale package
library(plyr)

# Set Working Directory and Load CSV datafile
setwd("C:/Users/88017/Desktop/Trimester 4/INFT6201 BIG DATA/Assessment 1/Item 2 - Lab Projects/")
moviedata = read.csv("moviedata.csv", header=TRUE, sep=",", dec = ".", na.strings ="NA")

##################################################
## Exercise 01 ###################################
##################################################

# Convert the integers indicating the origin to a factor
moviedata$year <- factor(moviedata$year, levels = c(2014, 2015), labels = c("Y2014", "Y2015"))

# Create a box plot for movies initially launched
ggplot(moviedata[!is.na(moviedata$screens),], aes(x = year, y = screens, fill=year)) +
  stat_boxplot(geom = "errorbar") + 
  geom_boxplot() +                  
  theme_bw() +                       
  labs(title = "Number of screens for each movies in 2014/2015") +
  xlab("Year of Movie Release") +
  ylab("Number of Screens") +
  guides(fill=FALSE)

##################################################
## Exercise 02 ###################################
##################################################


#Calculating profit and adding the results as a new variable
moviedata$profit <- as.numeric(moviedata$gross) - as.numeric(moviedata$budget)

#Separating Original and Sequel Movies
moviedata$sequelcat <- factor(moviedata$dummy_sequel, levels = c(0, 1),labels = c("ORIGINAL", "SEQUEL"))

#plotting
ggplot(moviedata[!is.na(moviedata$profit),], aes(x = sequelcat, y = profit,  fill = sequelcat)) + 
  geom_violin() +                   
  theme_bw() +                      
  labs(title = "Profit of ORIGINAL and SEQUEL Movies") +
  #ylim(-200000000, 600000000) +
  xlab("Movie Category") +                               
  ylab("Profit(Gross-Budget) in USD") +
  scale_y_continuous(labels = scales::dollar) +
  guides(fill=FALSE) +
  scale_fill_brewer(palette = "YlOrRd") +
  geom_boxplot(width=0.2)+
  stat_summary(fun.y = mean, geom = "point", size = 4, color = "red") #adding further summary statistics 

##################################################
## Exercise 03 ###################################
##################################################

# Creating subset without NA values
moviedatasub <- subset(moviedata[!is.na(moviedata$budget & moviedata$screens & moviedata$aggregate_followers),])
str(moviedatasub)

# Create a Function to Winsorize Data
winsor<- function(x,multiplier) {
  
  if(length(multiplier) != 1 || multiplier <= 0)
  {
    stop("bad value for 'multiplier'")
  }
  
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

#adding likes_winsor to moviedatasub
moviedatasub$likes_winsor <- winsor(moviedatasub$likes, 1.5)

#Checking by box plotting likes and likes_winsor
with(moviedatasub, boxplot(likes))
with(moviedatasub, boxplot(likes_winsor)) 

##################################################
## Exercise 04 ###################################
##################################################

#cutting the set into 3 buckets
moviedatasub$ratingscat <- cut(moviedatasub$ratings, breaks = c(-Inf, 5.9, 6.7, Inf), labels = c("negative", "neutral", "positive"), include.lowest = TRUE)

#creating box plot of gross over likes_winsor
ggplot(moviedatasub, aes(x=likes_winsor, y = gross, color=ratingscat)) + 
  geom_point(size=2.5) +                  # Make it a scatterplot
  labs(title = "Gross income by likes") + 
  theme_bw() +                            # Change Background Color
  ylab("Gross Income in USD") +         
  xlab("Number of Likes (Winsorized)") +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(values=wes_palette("FantasticFox1"),  
                     name="Ratings Category")


##################################################
## Exercise 05 ###################################
##################################################

#using ddply package to create data frame
ddply(moviedatasub, c("ratingscat", "sequelcat"), 
      summarise,N=length(profit),
      profit_avg= mean(profit),
      profit_sd= sd(profit),
      gross_avg= mean(gross),
      gross_sd= sd(gross),
      budget_avg= mean(budget),
      budget_sd= sd(budget) )


##################################################
## Exercise 06 ###################################
##################################################

#bartlett Test
bartlett.test(profit ~ ratingscat, data=moviedatasub)

# Answer -  The p-value is less than 0.05, we reject the null hypothesis. Hence, variances are not homogeneous.

#ANOVA Test
aovResult = aov(profit ~ ratingscat, data=moviedatasub)
summary(aovResult)

# Answer - When F-statistic has a value of greater than 1, there is a systematic influence of
# the group. Here f-value=7.154 which indicates that There is a significant relationship
# between the profit and ratings category of the movie (F(2, 184) = 7.154, p<.001)."

#PostHoc Analysis

#Pairwise t-Tests with Bonferroni Adjustment
with(moviedatasub, pairwise.t.test(profit, ratingscat, p.adjust="bonferroni", pool.sd=FALSE))
#                  negative neutral
#         neutral  0.585    -      
#         positive 0.002    0.036 

#Tukey HSD Test
TukeyHSD(aovResult, conf.level = 0.95)
#                      diff       lwr      upr     p adj
#neutral-negative  11402489 -17114502 39919480 0.6126943
#positive-negative 42793666  14276675 71310657 0.0014355
#positive-neutral  31391177   4989574 57792781 0.0151261

#Answer - the different PostHoc analysis procedures come to different
#         results with respect to the profit difference between rating category of movie. 
#         This is most likely due to the non-homogeneous variances.

##################################################
## Exercise 07 ###################################
##################################################

#ANOVA Test
aovResult = aov(profit ~ sequelcat, data=moviedatasub)
summary(aovResult)

#Answer - We should use ANOVA test to whether there is significant difference 
#in profit in between ORIGINAL and SEQUEL movie.The ANOVA only determines whether there is any significant difference (i.e., at least one) in the K
#population means. However, it does not provide us with information which of the groups differ from
#each other significantly .

#Here f-value=10.66 which indicates that there is a significant relationship
#between the profit and sequel category of the movie (F(1, 185) = 10.66, p>.001).

