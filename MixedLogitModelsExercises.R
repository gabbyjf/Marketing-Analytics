setwd("/Users/gabriellefrieder/Downloads/BUAD466 IS w: Dee/Choice-Based Conjoint")
getwd()

sportscar <- read.csv(file="sportscar_choice_long.csv", stringsAsFactors=TRUE)

str(sportscar)

## 1 ##
summary(sportscar)

## Atrribute levels
# segment: basic, fun, racer
# seat: 2, 3, 4, 5
# trans: auto, manual
# convert: yes, no
# price: 30, 25, 40

tail(sportscar)
#     resp_id ques alt segment seat  trans convert price choice
# 6025     200    9   1     fun    5 manual     yes    30      0
# 6026     200    9   2     fun    2 manual      no    35      0
# 6027     200    9   3     fun    4   auto     yes    35      1
# 6028     200   10   1     fun    5   auto     yes    35      1
# 6029     200   10   2     fun    5   auto      no    35      0
# 6030     200   10   3     fun    5   auto     yes    40      0

# Based on the output, the price of the chosen alternative in the last question is $35k

## 2 ##
library(mlogit)
library(dfidx)

sportscar$seat <- factor(sportscar$seat,levels=c("2","4","5"))
sportscar$price <- factor(sportscar$price,levels=c("30","35","40"))
sportscar$choice <- factor(sportscar$choice,levels=c(0,1),labels=c("no","yes"))

str(sportscar)
head(sportscar)

sportscar$chid <- rep(x=1:(nrow(sportscar)/3),each=3)
sportscar.mlogit.df <- dfidx(data=sportscar,choice="choice", 
                      idx=list(c("chid","resp_id"),"alt"))

mlogit.model1 <- mlogit(choice~0+seat+convert+trans+price,
                        data=sportscar.mlogit.df)
                      
summary(mlogit.model1)

# Coefficients :
#             Estimate Std. Error  z-value  Pr(>|z|)    
# seat4       -0.016989   0.075740  -0.2243 0.8225193    
# seat5        0.422579   0.075159   5.6224 1.883e-08 ***
# convertyes   0.215872   0.061979   3.4830 0.0004958 ***
# transmanual -1.226761   0.066485 -18.4518 < 2.2e-16 ***
# price35     -0.812610   0.071657 -11.3403 < 2.2e-16 ***
# price40     -1.939368   0.088407 -21.9368 < 2.2e-16 ***

# Based on model 1, the ideal sportscar for the respondents has 5 seats, is a convertible, has an 
# automatic transmission, and is $30,000.

# The coefficient that is most precisely estimated is price40 because the absolute value of the 
# z-value is the largest. 

## Is it reasonable to charge $5000 for a convertible top?
# calculate WTP

## WTP(attribute.level) = coef(attribute.level)/abs(coef[SINGLE.price])

as.numeric(as.character(sportscar.mlogit.df$price))
mlogit.model2 <- mlogit(choice~0+seat+convert+trans+as.numeric(as.character(price)),
                          data=sportscar.mlogit.df)
summary(mlogit.model2)

wtp.convertyes <- coef(mlogit.model2)["convertyes"]/
				(-coef(mlogit.model2)["as.numeric(as.character(price))"]/1e3)
wtp.convertyes
# $1,108.87
# Based on this calculation, a customer will not pay $5,000 for a convertible top! If determining a
# price point for a convertible top, then it must be somewhere near the $1,100 mark in order for
# customers to be willing to pay for that feature.

## PREDICT SHARES

  predict.shares <- function(model,data) {
# function for predicting shares from a multinomial logit model 
# model: mlogit object returned by mlogit()
# data:  data frame containing set of designs for which you want to 
#        predict shares--same format at the data used to estimate model
  data.model <- model.matrix(object=update(model$formula,0~.),data=data)[,-1]
  utility <- data.model%*%model$coef
  share <- exp(utility)/sum(exp(utility))
  cbind(share,data)
  }

newcars <- data.frame(seat=factor(c("2","4","5")),
				trans=factor(c("manual","automatic","automatic")),
				convert=factor(c("no","yes","no")),
				price=c("40","37","35"))

predict.shares(model=mlogit.model1,data=newcars)

#       share seat     trans convert price
# 1 0.01998991    2    manual      no    40
# 2 0.25662879    4 automatic     yes    37
# 3 0.72338129    5 automatic      no    35

## SENSITIVITY ANALYSIS

  sensitivity.shares <- function(model,attrib,base.data,competitor.data) {
# function for creating data for a share-sensitivity chart
# model:           mlogit object returned by mlogit() function
# attrib:          list of vectors with attribute levels to be used
# base.data:       data frame containing baseline design of target product
# competitor.data: data frame contining product designs of competitive set
  data <- rbind(base.data,competitor.data)
  base.share <- predict.shares(model,data)[1,1]
  share <- NULL
  for(a in seq_along(attrib)) {
    for(i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share,predict.shares(model,data)[1,1])
      }
    }
  data.frame(level=unlist(attrib),share=share,increase=share-base.share)
  }
 
 attrib <- list (seat    = c("2","4","5"),
  				 trans   = c("manual","automatic"),
  				 convert = c("yes","no"),
  				 price   = c("35","37","40"))
 
  expand.grid(attrib)
  base.data <- expand.grid(attrib)[28,]
  competitor.data <- expand.grid(attrib)[c(29,30,25,34,16,4),]
  
  (tradeoff <- sensitivity.shares(model=mlogit.model1,attrib=attrib,
                                  base.data=base.data,
                                  competitor.data=competitor.data))
  barplot(height=tradeoff$increase,horiz=FALSE,names.arg=tradeoff$level,
          ylim=c(-.01,.25),ylab="Change in Share from First Sports Car")

# Suggestions to make (which features to change): 2 --> 5 seats, lower the price either to 37k or 35k
# For the highest share, drop price from 40k to 35k.

## 3 ##

mlogit.model3 <- mlogit(choice~0+seat+convert+trans+price,
                        data=sportscar.mlogit.df[sportscar$segment=="racer",])
                      
summary(mlogit.model3)

# Coefficients :
#            Estimate Std. Error z-value  Pr(>|z|)    
# seat4       -0.11142    0.25665 -0.4342   0.66418    
# seat5        0.48869    0.26574  1.8390   0.06591 .  
# convertyes  -1.75632    0.24919 -7.0481 1.814e-12 ***
# transmanual  2.00417    0.25967  7.7182 1.177e-14 ***
# price35     -0.47820    0.25114 -1.9041   0.05690 .  
# price40     -1.74542    0.28805 -6.0594 1.367e-09 ***

# Yes, some of the predictions are different than in model 1. In model 1, all coefficients
# were significant except seat4, however, in this model, seat4 and seat5 are both insignificant,
# indicating it is not statistically different from 0, and same applies to price35, where the 
# coefficient is also insignificant, indicating it is not statistically different from 0. 

predict.shares(model=mlogit.model3,data=newcars)

#       share seat     trans convert price
# 1 0.42873404    2    manual      no    40
# 2 0.03169454    4 automatic     yes    37
# 3 0.53957142    5 automatic      no    35

# The shares for the newcars in model3 change significantly. While in model 1, the 4 seater convertible
# with automatic transmission and priced at 37k held the 2nd highest share (middle of the pack), in model
# 3, that type of vehicle holds basically no market share (about 3%). In model 1, the share for a 5 seat
# non-convertible with automatic transmission and priced at 35k held almost 3/4 of the market's shares,
# but in model 3, this type of sports car holds less market share and only holds approximately 11% more
# market share than a 2 seater, manual transmission non-convertible priced at 40k. In model 1, this 
# most recently mentioned sports car only held approximately 2% market share compared to 42% share in 
# model 3. 

## 4 ##

(mlogit.model1.rpar1 <- rep(x="n",length=length(mlogit.model1$coef)))
names(mlogit.model1.rpar1) <- names(mlogit.model1$coef)

mlogit.model1.hier1 <- mlogit(choice~0+seat+convert+trans+price,
                		data= sportscar.mlogit.df,panel=TRUE,correlation=FALSE,
                               rpar=mlogit.model1.rpar1)
summary(mlogit.model1.hier1)

# Frequencies of alternatives:choice
#      1       2       3 
# 0.32935 0.32537 0.34527 

# Coefficients :
#                Estimate Std. Error  z-value  Pr(>|z|)    
# seat4          -0.025335   0.087147  -0.2907 0.7712722    
# seat5           0.516425   0.088341   5.8458 5.042e-09 ***
# convertyes      0.268027   0.074023   3.6208 0.0002937 ***
# transmanual    -1.468569   0.088452 -16.6029 < 2.2e-16 ***
# price35        -1.017021   0.087911 -11.5688 < 2.2e-16 ***
# price40        -2.348006   0.118515 -19.8119 < 2.2e-16 ***
# sd.seat4       -0.106566   0.188960  -0.5640 0.5727832    
# sd.seat5        0.392857   0.161516   2.4323 0.0150032 *  
# sd.convertyes   1.104474   0.116162   9.5080 < 2.2e-16 ***
# sd.transmanual  1.385664   0.124773  11.1055 < 2.2e-16 ***
# sd.price35      0.492799   0.152952   3.2219 0.0012734 ** 
# sd.price40      0.439834   0.187790   2.3422 0.0191724 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Log-Likelihood: -1628.6

# random coefficients
#            Min.     1st Qu.      Median        Mean     3rd Qu. Max.
# seat4       -Inf -0.09721203 -0.02533464 -0.02533464  0.04654275  Inf
# seat5       -Inf  0.25144690  0.51642462  0.51642462  0.78140233  Inf
# convertyes  -Inf -0.47692944  0.26802676  0.26802676  1.01298295  Inf
# transmanual -Inf -2.40318456 -1.46856855 -1.46856855 -0.53395255  Inf
# price35     -Inf -1.34940857 -1.01702074 -1.01702074 -0.68463292  Inf
# price40     -Inf -2.64466941 -2.34800602 -2.34800602 -2.05134263  Inf

# Transmission manual has the largest variation across respondents because it has the largest
# coefficient for the standard deviation.
