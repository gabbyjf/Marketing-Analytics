setwd("/Users/gabriellefrieder/Downloads/BUAD466 IS w: Dee")
getwd()

conjoint.df <- read.csv(file="chapter9-bag.csv",stringsAsFactors=TRUE)
summary(conjoint.df)
str(conjoint.df)

conjoint.df$price <- factor(conjoint.df$price)

## 11 ##
lm1 <- lm(rating~price+color+zipper+finish,data=conjoint.df)
summary(lm1)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  10.09489    0.09419  107.18   <2e-16 ***
# price17      -1.45539    0.10043  -14.49   <2e-16 ***
# price19      -3.25022    0.10380  -31.31   <2e-16 ***
# price20      -0.03290    0.07835   -0.42    0.675    
# colorgray    -1.59112    0.09336  -17.04   <2e-16 ***
# colornavy    -2.27853    0.09791  -23.27   <2e-16 ***
# zippersilver -0.70815    0.06970  -10.16   <2e-16 ***
# finishpatent -0.95940    0.07484  -12.82   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1.82 on 4492 degrees of freedom
# Multiple R-squared:  0.5498,	Adjusted R-squared:  0.5491 
# F-statistic: 783.7 on 7 and 4492 DF,  p-value: < 2.2e-16

## 12 ##
hlm1 <- lmer(rating~price+color+zipper+finish+
				(price+color+zipper+finish | resp.id),data=conjoint.df,
				control=lmerControl(optCtrl=list(method="Nelder-Mead"),
                                   optimizer="optimx"))
summary(hlm1)

# Linear mixed model fit by REML ['lmerMod']
# Formula: rating ~ price + color + zipper + finish + (price + color + zipper +      finish | resp.id)
#    Data: conjoint.df
# Control: lmerControl(optCtrl = list(method = "Nelder-Mead"), optimizer = "optimx")
#
# REML criterion at convergence: 17182
#
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.9737 -0.5900  0.0047  0.5666  3.6362 
#
# Random effects:
#  Groups   Name         Variance Std.Dev. Corr                                     
#  resp.id  (Intercept)  1.0908   1.0444                                            
#           price17      0.3761   0.6133   -0.02                                    
#           price19      0.5641   0.7511    0.24  0.32                              
#           price20      1.3351   1.1555   -0.02  0.25  0.32                        
#           colorgray    0.4552   0.6747    0.50 -0.30 -0.17 -0.17                  
#           colornavy    1.0490   1.0242    0.17 -0.09 -0.75 -0.14 -0.05            
#           zippersilver 1.0703   1.0346   -0.75  0.04 -0.09  0.05 -0.07 -0.21      
#           finishpatent 1.2307   1.1094   -0.61 -0.11 -0.37 -0.23 -0.16 -0.20  0.10
#  Residual              1.7704   1.3306                                            
# Number of obs: 4500, groups:  resp.id, 300
#
# Fixed effects:
#              Estimate Std. Error t value
# (Intercept)  10.09489    0.09154 110.277
# price17      -1.45539    0.08153 -17.851
# price19      -3.25022    0.08742 -37.179
# price20      -0.03290    0.08794  -0.374
# colorgray    -1.59112    0.07860 -20.243
# colornavy    -2.27853    0.09286 -24.537
# zippersilver -0.70815    0.07852  -9.019
# finishpatent -0.95940    0.08424 -11.388
#
# Correlation of Fixed Effects:
#             (Intr) pric17 pric19 pric20 clrgry clrnvy zpprsl
# price17      0.083                                          
# price19      0.142  0.543                                   
# price20     -0.196  0.300  0.314                            
# colorgray   -0.079 -0.462 -0.261 -0.192                     
# colornavy   -0.282 -0.401 -0.642 -0.120  0.422              
# zippersilvr -0.587  0.079 -0.049  0.056 -0.200 -0.079       
# finishpatnt -0.552 -0.355 -0.331 -0.069  0.116  0.101 -0.004
# optimizer (optimx) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# optimx: No match to available methods
# Default method when bounds specified is L-BFGS-B to match optim()

  bagPrice20 <- fixef(hlm1)["price20"]+ranef(hlm1)$resp.id[,"price20"]
  hist(bagPrice20)

## 13 ##
# estimated rating for a black bag with matte finish and a gold zipper
rating = 10.09489 

# each of the above handbag attributes are the base levels, which are built into the intercept

## 14 ##

  coef.colornavy <- fixef(hlm1)["colornavy"]+ranef(hlm1)$resp.id[,"colornavy"]
  which(coef.colornavy==min(coef.colornavy))
  which(coef.colornavy==max(coef.colornavy))

