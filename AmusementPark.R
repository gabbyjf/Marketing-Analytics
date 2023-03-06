setwd("/Users/gabriellefrieder/Downloads/BUAD466 IS w: Dee/Linear Models")
getwd()

sat.df <- read.csv("amusementpark.csv",stringsAsFactors=TRUE)

str(sat.df)
# 'data.frame':	500 obs. of  8 variables:
# $ weekend  : Factor w/ 2 levels "no","yes": 2 2 1 2 1 1 2 1 1 2 ...
# $ num.child: int  0 2 1 0 4 5 1 0 0 3 ...
# $ distance : num  114.6 27 63.3 25.9 54.7 ...
# $ rides    : int  87 87 85 88 84 81 77 82 90 88 ...
# $ games    : int  73 78 80 72 87 79 73 70 88 86 ...
# $ wait     : int  60 76 70 66 74 48 58 70 79 55 ...
# $ clean    : int  89 87 88 89 87 79 85 83 95 88 ...
# $ overall  : int  47 65 61 37 68 27 40 30 58 36 ...

summary(sat.df) # data inspection

library(car)
scatterplotMatrix(sat.df)

# distance variable is left-skewed, thus it will be log-transformed
sat.df$logdist <- log(sat.df$distance)

str(sat.df)

library(corrplot)
corrplot.mixed(cor(sat.df[ , c(2, 4:9)]), upper="ellipse")

# relationship between overall satisfaction and satisfaction with rides
plot(overall~rides, data=sat.df, xlabs="Satisfaction with Rides", ylab="Overall Satisfaction")

lm(overall~rides,data=sat.df)
# Coefficients:
# (Intercept)        rides  
#     -94.962        1.703  

# Someone who gives a rating of 95 for satisfaction with rides will give an overall rating of:
-94.962 + 1.703*95 
# [1] 66.823

m1 <- lm(overall~rides,data=sat.df)

plot(overall~rides, data=sat.df, xlabs="Satisfaction with Rides", ylab="Overall Satisfaction")
abline(m1, col='blue')

str(m1)
m1$coefficients
summary(m1)
# Call:
# lm(formula = overall ~ rides, data = sat.df)

# Residuals:
#    Min      1Q  Median      3Q     Max 
# -33.597 -10.048   0.425   8.694  34.699 

# Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -94.9622     9.0790  -10.46   <2e-16 ***
# rides         1.7033     0.1055   16.14   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 12.88 on 498 degrees of freedom
# Multiple R-squared:  0.3434,	Adjusted R-squared:  0.3421 
# F-statistic: 260.4 on 1 and 498 DF,  p-value: < 2.2e-16

# As shown by the summary of the linear model, both the intercept and rides coefficient are 
# significant as shown by the ***. We can interpret the rides coefficient as the following:
# for every 1 point increase in satisfaction with rides, overall satisfaction increases by 1.70 
# points. The adjusted R-squared value of 0.3421 indicates that 34.21% of the variability in overall 
# satisfaction can be explained by satisfaction with rides.

confint(m1) # confidence intervals for the coefficients
#                   2.5 %     97.5 %
# (Intercept) -112.800120 -77.124371
# rides          1.495915   1.910656

cor(sat.df$overall,sat.df$rides)^2 
# when there is only 1 predictor, the R-squared can be calculated by squaring the correlation 
# coefficient between the predictor and response variables.

## MODEL FIT ASSUMPTIONS 

# 1
# the relationship between the predictors and response variable is linear
# code below plots the linear model of y is a function of x-squared, then line is drawn through the 
# curvature
x <- rnorm(500)
y <- x^2 + rnorm(500)
toy.model <- lm(y~x)
summary(toy.model)
plot(y~x)
abline(toy.model)

# 2
# prediction errors are normally distributed and look random with no pattern (homoskedastic)
plot(toy.model$fitted.values, toy.model$residuals)

par(mfrow=c(2,2))
plot(m1)
# Top left plot in 2x2 matrix is the fitted values vs residuals for m1
# Lower left plot is similar to the top left but instead plots the square root of standardized
# residual
# Upper right is a QQ-plot that detects whether the residuals follow a normal distribution, which
# is the 3rd assumption
# Lower right plot helps to identify any potential outliers

# inspecting the rows of which were detected as potential outliers
sat.df[c(57,129,295),]
#     weekend num.child distance rides games wait clean overall  logdist
# 57      yes         2 63.29248    98    87   89   100     100 4.147767
# 129     yes         0 11.89550    76    77   51    77       6 2.476161
# 295      no         0 11.74474    98    83   63    92      45 2.463406

# what may be obscure about these 3 observations lies within row 129 in which their overall 
# satisfaction rating is a 6, which may be possible even if seems unlikely

m2 <- lm(overall~rides+games+wait+clean,data=sat.df)
summary(m2)

# Call:
# lm(formula = overall ~ rides + games + wait + clean, data = sat.df)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -29.944  -6.841   1.072   7.167  28.618 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -131.40919    8.33377 -15.768  < 2e-16 ***
# rides          0.52908    0.14207   3.724 0.000219 ***
# games          0.15334    0.06908   2.220 0.026903 *  
# wait           0.55333    0.04781  11.573  < 2e-16 ***
# clean          0.98421    0.15987   6.156 1.54e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 10.59 on 495 degrees of freedom
# Multiple R-squared:  0.5586,	Adjusted R-squared:  0.5551 
# F-statistic: 156.6 on 4 and 495 DF,  p-value: < 2.2e-16

install.packages("coefplot")
library(coefplot)
coefplot(m2,intercept=FALSE,outerCI=1.96,lwdOuter=1.5, ylab="Rating of Feature", 
		 xlab="Association with Overall Satisfaction") 

summary(m1)$r.squared
# [1] 0.3433799
summary(m2)$r.squared
# [1] 0.558621

summary(m1)$adj.r.squared
# [1] 0.3420614
summary(m2)$adj.r.squared
# [1] 0.5550543

plot(sat.df$overall,fitted(m1),col='red',xlim=c(0,100),ylim=c(0,100),xlab="Actual Overall Satisfaction",
	 ylab="Fitted Overall Satisfaction")
points(sat.df$overall,fitted(m2),col='blue')
legend("topleft",legend=c("model 1","model 2"),col=c("red","blue"),pch=1)

anova(m1,m2)
# Analysis of Variance Table

# Model 1: overall ~ rides
# Model 2: overall ~ rides + games + wait + clean
#   Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
# 1    498 82612                                  
# 2    495 55532  3     27080 80.463 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coef(m2)["(Intercept)"]+coef(m2)["rides"]*100+coef(m2)["games"]*100+coef(m2)["wait"]*100+
	coef(m2)["clean"]*100
# (Intercept) 
#   90.58612

# more efficient way to compute model predictions by using matrix operations to multiply coefficients
# by a vector of predictor values
coef(m2)%*%c(1,100,100,100,100)
#          [,1]
# [1,] 90.58612

# using the predict function also can find predictions for given rows of customers in the data
predict(m2,sat.df[1:10,])
#        1        2        3        4        5        6        7        8        9       10 
# 46.60864 54.26012 51.17289 50.30434 52.94625 27.87214 36.27435 43.13123 66.91439 45.38024 
fitted(m2)[1:10] # gives same result as above because the predictions for observations are stored in the 
# model object

# standardizing the data by converting to zero-centered units of standard deviation
(sat.df$rides-mean(sat.df$rides))/sd(sat.df$rides)
scale(sat.df$rides)

sat.std <- sat.df[,-3] # sat but remove distance
sat.std[,3:8] <- scale(sat.std[,3:8])
head(sat.std)
#   weekend num.child      rides       games         wait       clean    overall   logdist
# 1     yes         0  0.2112477 -0.69750817 -0.918784090  0.21544189 -0.2681587 1.7886823
# 2     yes         2  0.2112477 -0.08198737  0.566719693 -0.17555973  0.8654385 0.3226360
# 3      no         1 -0.1548662  0.16422095  0.009655775  0.01994108  0.6135280 1.1862757
# 4     yes         0  0.3943047 -0.82061233 -0.361720171  0.21544189 -0.8979350 0.2803106
# 5      no         4 -0.3379232  1.02595006  0.381031720 -0.17555973  1.0543714 1.0385034
# 6      no         5 -0.8870941  0.04111679 -2.032911927 -1.73956621 -1.5277112 0.1452467

summary(sat.std)

m3 <- lm(overall~rides+games+wait+clean+weekend+logdist+num.child,data=sat.std)
summary(m3)
# Call:
# lm(formula = overall ~ rides + games + wait + clean + weekend + 
#     logdist + num.child, data = sat.std)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.51427 -0.40271  0.01142  0.41613  1.69000 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.37271    0.04653  -8.009 8.41e-15 ***
# rides        0.21288    0.04197   5.073 5.57e-07 ***
# games        0.07066    0.03026   2.335   0.0199 *  
# wait         0.38138    0.02777  13.734  < 2e-16 ***
# clean        0.29690    0.04415   6.725 4.89e-11 ***
# weekendyes  -0.04589    0.05141  -0.893   0.3725    
# logdist      0.06470    0.02572   2.516   0.0122 *  
# num.child    0.22717    0.01711  13.274  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.5709 on 492 degrees of freedom
# Multiple R-squared:  0.6786,	Adjusted R-squared:  0.674 
# F-statistic: 148.4 on 7 and 492 DF,  p-value: < 2.2e-16

sat.std$num.child.factor <- factor(sat.std$num.child)

m4 <- lm(overall~rides+games+wait+clean+weekend+logdist+num.child.factor,data=sat.std)
summary(m4)
# Call:
# lm(formula = overall ~ rides + games + wait + clean + weekend + 
#     logdist + num.child.factor, data = sat.std)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.25923 -0.35048 -0.00154  0.31400  1.52690 

# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -0.69100    0.04488 -15.396  < 2e-16 ***
# rides              0.22313    0.03541   6.301 6.61e-10 ***
# games              0.04258    0.02551   1.669   0.0958 .  
# wait               0.38472    0.02338  16.453  < 2e-16 ***
# clean              0.30917    0.03722   8.308 9.72e-16 ***
# weekendyes        -0.02227    0.04322  -0.515   0.6065    
# logdist            0.03187    0.02172   1.467   0.1429    
# num.child.factor1  1.01610    0.07130  14.250  < 2e-16 ***
# num.child.factor2  1.03732    0.05640  18.393  < 2e-16 ***
# num.child.factor3  0.98000    0.07022  13.955  < 2e-16 ***
# num.child.factor4  0.93154    0.08032  11.598  < 2e-16 ***
# num.child.factor5  1.00193    0.10369   9.663  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.4795 on 488 degrees of freedom
# Multiple R-squared:  0.7751,	Adjusted R-squared:   0.77 
# F-statistic: 152.9 on 11 and 488 DF,  p-value: < 2.2e-16

sat.std$has.child <- factor(sat.std$num.child>0)

m5 <- lm(overall~rides+games+wait+clean+weekend+logdist+has.child,data=sat.std)
summary(m5)
# Call:
# lm(formula = overall ~ rides + games + wait + clean + weekend + 
#     logdist + has.child, data = sat.std)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.22366 -0.35107 -0.01747  0.31852  1.45703 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -0.69039    0.04478 -15.418  < 2e-16 ***
# rides          0.22193    0.03517   6.310 6.24e-10 ***
# games          0.04409    0.02541   1.735   0.0833 .  
# wait           0.38603    0.02328  16.583  < 2e-16 ***
# clean          0.30904    0.03699   8.354 6.75e-16 ***
# weekendyes    -0.02280    0.04311  -0.529   0.5971    
# logdist        0.03404    0.02159   1.576   0.1156    
# has.childTRUE  1.00485    0.04689  21.428  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.4785 on 492 degrees of freedom
# Multiple R-squared:  0.7742,	Adjusted R-squared:  0.771 
# F-statistic:   241 on 7 and 492 DF,  p-value: < 2.2e-16

m6 <- lm(overall~rides+games+wait+clean+weekend+logdist+has.child+rides:has.child+games:has.child+
	   wait:has.child+clean:has.child+rides:weekend+games:weekend+wait:weekend+clean:weekend,data=sat.std)
summary(m6)
# Call:
# lm(formula = overall ~ rides + games + wait + clean + weekend + 
#     logdist + has.child + rides:has.child + games:has.child + 
#     wait:has.child + clean:has.child + rides:weekend + games:weekend + 
#     wait:weekend + clean:weekend, data = sat.std)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.15097 -0.31487 -0.01245  0.30277  1.45388 

# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         -0.677443   0.043034 -15.742  < 2e-16 ***
# rides                0.146980   0.067982   2.162  0.03110 *  
# games                0.079569   0.049365   1.612  0.10765    
# wait                 0.129718   0.044266   2.930  0.00355 ** 
# clean                0.312757   0.079685   3.925 9.93e-05 ***
# weekendyes          -0.020461   0.041261  -0.496  0.62021    
# logdist              0.025801   0.020671   1.248  0.21258    
# has.childTRUE        0.995076   0.044869  22.177  < 2e-16 ***
# rides:has.childTRUE  0.057837   0.073070   0.792  0.42902    
# games:has.childTRUE -0.064043   0.052797  -1.213  0.22572    
# wait:has.childTRUE   0.350649   0.047241   7.423 5.21e-13 ***
# clean:has.childTRUE -0.001854   0.079710  -0.023  0.98146    
# rides:weekendyes     0.061784   0.067750   0.912  0.36225    
# games:weekendyes     0.018511   0.049036   0.377  0.70597    
# wait:weekendyes      0.035168   0.044463   0.791  0.42936    
# clean:weekendyes    -0.027305   0.071005  -0.385  0.70074    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.4521 on 484 degrees of freedom
# Multiple R-squared:  0.8018,	Adjusted R-squared:  0.7956 
# F-statistic: 130.5 on 15 and 484 DF,  p-value: < 2.2e-16

m7 <- lm(overall~rides+games+wait+clean+logdist+has.child+wait:has.child,data=sat.std)
summary(m7)
# Call:
# lm(formula = overall ~ rides + games + wait + clean + logdist + 
#     has.child + wait:has.child, data = sat.std)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.12939 -0.32350 -0.00167  0.31705  1.43253 

# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.69316    0.03684 -18.814  < 2e-16 ***
# rides               0.21264    0.03313   6.419 3.24e-10 ***
# games               0.04870    0.02394   2.034   0.0425 *  
# wait                0.15095    0.03688   4.093 4.98e-05 ***
# clean               0.30244    0.03485   8.678  < 2e-16 ***
# logdist             0.02919    0.02027   1.440   0.1504    
# has.childTRUE       0.99830    0.04416  22.606  < 2e-16 ***
# wait:has.childTRUE  0.34688    0.04380   7.920 1.59e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.4508 on 492 degrees of freedom
# Multiple R-squared:  0.7996,	Adjusted R-squared:  0.7968 
# F-statistic: 280.5 on 7 and 492 DF,  p-value: < 2.2e-16

coefplot(m7,intercept=FALSE,outerCI=1.96,lwdOuter=1.5,ylab="Rating of Feature",
		 xlab="Association with Overall Satisfaction")
