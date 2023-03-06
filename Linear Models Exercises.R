setwd("/Users/gabriellefrieder/Downloads/BUAD466 IS w: Dee")
getwd()

hotel.df <- read.csv(file="hotelsat-data.csv",stringsAsFactors=TRUE)

str(hotel.df)
summary(hotel.df)

hotel.df$eliteStatus <- factor(hotel.df$eliteStatus,
								levels=c("NoStatus","Platinum","Silver","Gold"))

## 1 ##
library(car)
scatterplotMatrix(hotel.df[,c(1:9)]) # satisfaction variables part 1 (normal distributions)
scatterplotMatrix(hotel.df[,c(10:18)]) # satisfaction variables part 2
scatterplotMatrix(hotel.df[,c(19:25)]) # all other variables

# Distance traveled, nights stayed, and average food spend per night should all be
# transformed because their distributions all appear to be right-skewed

hotel.df$logdistTraveled <- log(hotel.df$distanceTraveled)
hotel.df$lognightsStayed <- log(hotel.df$nightsStayed)
hotel.df$logavgFoodSpend <- log(hotel.df$avgFoodSpendPerNight+1)

hist(hotel.df$logdistTraveled)
hist(hotel.df$lognightsStayed)
hist(hotel.df$logavgFoodSpend)

## 2 ##
install.packages(x=corrplot)
library(corrplot)
corrplot.mixed(cor(hotel.df[,c(1:18,22,24,26:28)]),upper="ellipse")

# None of the correlations in the plot appear to be too high of a concern to address 
# multicollinearity, therefore, we can proceed with linear modeling. One interesting observation
# to point out is how most of the highest correlations include the satisfaction with the front 
# staff. It appears that front staff of a hotel make a significant impact on other elements of the
# overall hotel experience. One interesting correlation to point out is the -0.36 between 
# satisfaction of Valet Staff and satisfaction of the Parking Price, which means that the 
# lower the satisfaction for valet staff is associated with a higher satisfaction of parking price,
# and more satisfaction for valet staff is associated with a lower satisfaction of the parking price.

## 3 ##
cor(hotel.df$satCleanRoom,hotel.df$satCleanBath) # correlation = 0.46
cor(hotel.df$satCleanRoom,hotel.df$satCleanCommon) # correlation = 0.22
cor(hotel.df$satCleanBath,hotel.df$satCleanCommon) # correlation = 0.26

# There do not seem to be strong correlations between the satisfaction of the cleanliness factors .
# The correlations of satisfaction of a clean room with satisfaction of a clean common as well as 
# clean bath and clean common are weak, however, the correlation of satisfaction of clean room with
# the satisfaction of a clean bath is moderate. A different measure of the coefficents would not 
# matter in this dataset.

## 4 ##
lm1 <- lm(satOverall~satPerks,data=hotel.df)
summary(lm1)

# Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.19760    0.10739   20.46   <2e-16 ***
# satPerks     0.41166    0.02416   17.04   <2e-16 ***

# The coefficent for satPerks is 0.41166. This indicates that for every 1 point increase in 
# satisfaction of perks is associated with a 0.41 point increase in satisfaction overall. 

## 5 ##
lm2 <- lm(satOverall~satPerks+satFrontStaff+satCity,data=hotel.df)
summary(lm2)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    1.33814    0.11718  11.419  < 2e-16 ***
# satPerks       0.17652    0.02689   6.565 7.15e-11 ***
# satFrontStaff  0.34743    0.02961  11.732  < 2e-16 ***
# satCity        0.12847    0.02563   5.012 6.02e-07 ***

# The addition of satFrontStaff and satCity to the model with satPerks decreases the coefficient for
# perks in relation to its impact on overall satisfaction. The coefficient for satPerks now is around
# 0.18, which means that for every 1 point increase in satisfaction of perks is associated with a 0.18
# increase in overall satisfaction. This impact on overall satisfaction is way smaller compared to the
# previous model where satPerks was the only IV in the model. The coefficient for satFrontStaff is 
# 0.35, which indicates that for every 1 point increase in satisfaction of front staff is associated 
# with a 0.35 point increase in overall satisfaction. Finally, the coefficient for satCity is 0.13
# which means that for every 1 point increase in the satisfaction of the city the hotel is located is
# associated with a 0.13 point increase in overall satisfaction. It is also important to mention that 
# all coefficients are statistically significant (different from 0).

# When more predictor variables are added to a model, each individual variable carrys weight/
# contributes to the effect on overall satisfaction. 

# Q: Are all the coefficients meaningfully significant?
# sd(hotel.df$satOverall)

## 6 ##
lm3 <-lm(satRecognition~satFrontStaff+satCleanRoom+satPoints+satPerks,
data=hotel.df[(hotel.df$eliteStatus=="Gold" | hotel.df$eliteStatus=="Platinum"),])
summary(lm3)

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    1.54096    0.29092   5.297 1.85e-07 ***
# satFrontStaff  0.10001    0.04682   2.136 0.033205 *  
# satCleanRoom   0.03991    0.03779   1.056 0.291484    
# satPoints      0.26002    0.05264   4.939 1.11e-06 ***
# satPerks       0.22069    0.05630   3.920 0.000102 ***

# According to this model, the strategy that should be considered first is investing more in the 
# points awarded to elite members. This is because the t-value is the highest following the incercept. 
# Plus, the coefficient for satPoints is statistically significant (different than 0).

## 7 ##
# There was a lot of unexplained variation in the data as given by the adjusted R-squared of 19.45%. 
# More than 80% of the variation in the variable is unaccounted for in the model. The present problem 
# with the data is that it does not give you all the important information that targets specific areas 
# that the hotel should improve to enhance customers' overall satisfaction, especially from the highest 
# status members. Focusing on Gold and Platinum members, the hotel needs to figure out what those 
# customers are not satisfied with specifically. Asking specific comments regarding improvements for 
# different categories to enhance their experience overall could help target areas that should be 
# prioritized. Being able to prioritize customers' opinions should result in improved satisfaction in 
# the respective categories as well as overall satisfaction.

## 8 ##
# Based on the created model, the hotel should not invest in room cleanliness, because the coefficient
# is not significant. Aside from the model indicating that cleanliness is not statistically 
# significant, when people stay in a hotel, room cleanliness is not at top of mind when they want
# satisfaction from their experience. 

## 9 ##
lm4 <- lm(logavgFoodSpend~eliteStatus+satDiningPrice,data=hotel.df)
summary(lm4)

# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          2.35745    0.08688  27.135  < 2e-16 ***
# eliteStatusPlatinum -0.02995    0.08276  -0.362    0.718    
# eliteStatusSilver   -0.02216    0.05466  -0.405    0.685    
# eliteStatusGold     -0.10308    0.06321  -1.631    0.103    
# satDiningPrice       0.14789    0.02214   6.681 3.33e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.8806 on 1495 degrees of freedom
# Multiple R-squared:  0.03077,	Adjusted R-squared:  0.02817 
# F-statistic: 11.86 on 4 and 1495 DF,  p-value: 1.719e-09

# Based on the model, there does not seem to much of a relationship of average food spend with
# elite status and satisfaction with food price. None of the coefficients are significant for elite 
# status in comparison to the base level, which is No Status. The only significant coefficient is for 
# satisfaction with food price. The coefficient of 0.148 means that for every 1 point increase in 
# satisfaction with food price, average food spend per night increases by $0.15. Another important 
# piece of the model is the multiple and adjusted R-squared values. The adjusted R-squared of 0.02817 
# means that about 3% of the variation in average food spend per night can be attributed to elite 
# status and satisfaction with food price.

## 10 ##
cor(hotel.df$satDiningPrice,hotel.df$logavgFoodSpend)
# [1] 0.1702417
cor(hotel.df$satOverall,hotel.df$logavgFoodSpend)
# [1] 0.02666003

lm5 <- lm(logavgFoodSpend~satDiningPrice,data=hotel.df)
summary(lm5)

lm6 <- lm(logavgFoodSpend~eliteStatus,data=hotel.df)
summary(lm6)

# If any of the relationships were to be supported by the data, it would be the relationship between 
# satisfaction in dining price and the (log) average spend on food per night. Based on the correlations, 
# people will spend more money on average for food each night when they they are more satisfied with the 
# dining prices. Also, higher overall satisfaction is associated (very weakly) with higher average food 
# spend per night. People being satisfied with food prices can be looked at from two lenses: higher prices 
# indicate higher quality, thus people will be willing to spend more money OR people will be more satisfied 
# with lower food prices because they will have to pay less for their food. 

## 11 ##
predfoodspend <- lm(logavgFoodSpend~lognightsStayed,data=hotel.df)
summary(predfoodspend)

# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      2.64987    0.04522  58.606   <2e-16 ***
# lognightsStayed  0.20152    0.03497   5.763    1e-08 ***  

# predfoodspend = 2.64987 + 0.20152*lognightsStayed
2.64987 + 0.20152*log(40)
# [1] 3.393253
exp(3.393253)
# [1] 29.76261

# If someone stayed 40 nights, their estimated average food spend per night would be around $29.76.

## 12 ##
predfoodspendline <- lm(lognightsStayed~logavgFoodSpend,data=hotel.df[(hotel.df$eliteStatus=="Platinum"),])
summary(predfoodspendline)

plot(lognightsStayed~logavgFoodSpend,data=hotel.df[(hotel.df$eliteStatus=="Platinum"),],
					xlab="Nights Stayed",ylab="Avg Food Spend Per Night")
					
# Based on the plot, the average food spend per night among Platinum members does not seem to differ 
# regardless of how many nights are spent at the hotel. The points are scattered mostly in the lower half 
# with a few outliers at zero and closer to the higher end of (log) average food spend per night. In terms 
# of a restaurant strategy, food prices should not be targeted for a specific elite status group. The 
# restaurant's food prices should satisfy all customers that stay at that hotel. This is somewhat 
# consistent with the previous models because it clearly shows that average food spend and satisfaction 
# with food prices do not have a relationship with a particular status that a hotel customer has. In the 
# previous models, elite status was an insignficant factor, meaning that being a specific status will not 
# change how customers feel about spending money on food nor how many nights they stay. In this plot, it is 
# clear that the customers may not be the most satisfied with either the food itself or the food prices, 
# thus, regardless of how many nights are being spent, customers are finding other places to spend food 
# rather than at the specific restaurant. 

