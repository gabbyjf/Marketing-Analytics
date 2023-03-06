# CHOICE MODELING -- CHAPTER 13

  setwd("/Users/gabriellefrieder/Downloads/BUAD466 IS w: Dee/Choice-Based Conjoint")
  getwd()

# load data

  cbc.df <- read.csv(file="choice_based_conjoint.csv",stringsAsFactors=TRUE)
  str(cbc.df)

# clean data

  cbc.df$seat <- factor(x=cbc.df$seat,levels=c("6","7","8"))
  cbc.df$price <- factor(x=cbc.df$price,levels=c("30","35","40"))
  cbc.df$eng <- factor(x=cbc.df$eng,levels=c("gas","hyb","elec"))
  cbc.df$carpool <- factor(x=cbc.df$carpool,levels=c("yes","no"))
  cbc.df$choice <- factor(x=cbc.df$choice,levels=c(0,1),labels=c("no","yes"))
  
  str(cbc.df)
  summary(cbc.df)
  head(cbc.df)

  head(cbc.df,n=45) # 45 represents the  total number of product profiles one respondent will see
  str(cbc.df)
  summary(cbc.df)
  
# set up attribute list

  attrib <- list (seat  = c("6","7","8"),
  				  cargo = c("2ft","3ft"),
  				  eng   = c("gas","hyb","elec"),
  				  price = c("30","35","40"))

# fitting choice models with mlogit

 install.packages("mlogit")
 install.packages("dfidx")
 library(mlogit)
 library(dfidx)
 
# add column with unique question numbers
  
 cbc.df$chid <- rep(x=1:(nrow(cbc.df)/3),each=3)
 cbc.df[cbc.df$chid==1870,]
  
# shape data for mlogit model

  head(cbc.df,n=15)
  cbc.mlogit.df <- dfidx(data=cbc.df,choice="choice", 
                      idx=list(c("chid","resp.id"),"alt"))
  head(cbc.mlogit.df)

## CHOICE MODEL #1  --> classic logit model
## > if you have a dummy/binary response variable, e.g.,
##   purchase = (0,1) --> ("no","yes") ("not buy","buy")

## CHOICE MODEL #2 --> multinomial logit model
## > if you have more than two choices possible for the response variable
## 	 but you do not allow for individual random effects for each customer;
## > we do allow for population-level fixed effects like a logit model

## fit multinomial logit model

# if you include intercepts, you obtain an intercept for each alternative
# but these intercepts SHOULD be insignificant in the model if the 
# alternatives presented to subjects were truly randomized

  mlogit.model1 <- mlogit(formula=choice~0+seat+cargo+eng+price,
                          data=cbc.mlogit.df)
  summary(mlogit.model1)
  
# each coefficient represents the utility of attribute preference

# Coefficients :
#          Estimate Std. Error  z-value  Pr(>|z|)    
# seat7    -0.535280   0.062360  -8.5837 < 2.2e-16 ***
# seat8    -0.305840   0.061129  -5.0032 5.638e-07 ***
# cargo3ft  0.477449   0.050888   9.3824 < 2.2e-16 ***
# enghyb   -0.811282   0.060130 -13.4921 < 2.2e-16 ***
# engelec  -1.530762   0.067456 -22.6926 < 2.2e-16 ***
# price35  -0.913656   0.060601 -15.0765 < 2.2e-16 ***
# price40  -1.725851   0.069631 -24.7856 < 2.2e-16 ***

# we can assess the average preferences by comparing each level
#   to the baseline level, BUT...
# how do we interpret the coefficients?

  mlogit.model2 <- mlogit(formula=choice~seat+cargo+eng+price,
                          data=cbc.mlogit.df)
  summary(mlogit.model2)
  
# Coefficients :
#               Estimate Std. Error  z-value  Pr(>|z|)    
# (Intercept):2  0.028980   0.051277   0.5652    0.5720 <<<< INSIGNIFICANT   
# (Intercept):3  0.041271   0.051384   0.8032    0.4219 <<<< INSIGNIFICANT      
# seat7         -0.535369   0.062369  -8.5840 < 2.2e-16 ***
# seat8         -0.304369   0.061164  -4.9763 6.481e-07 ***
# cargo3ft       0.477705   0.050899   9.3854 < 2.2e-16 ***
# enghyb        -0.811494   0.060130 -13.4956 < 2.2e-16 ***
# engelec       -1.529423   0.067471 -22.6677 < 2.2e-16 ***
# price35       -0.913777   0.060608 -15.0769 < 2.2e-16 ***
# price40       -1.726878   0.069654 -24.7922 < 2.2e-16 ***


## SANITY CHECK: Are the intercepts all insignificant? If yes, then we 
##				 are confident the alternatives truly were randomized
##			     when presented to subjects

# Because this a complex logistic regression model, we cannot even
# compute a pseudo-R^2 to know how good of a fit our model is, and so
# what we can do instead is COMPARE MODELS to find when one model is 
# "better" than another model, but that's it!

# Since we do NOT, in general, include the intercepts in multinomial 
# logit models, we must have an alternative goodness-of-fit test and
# that is called the LIKELIHOOD RATIO TEST --> It takes the log-likelihood
# of your FIRST choice model and divides it by the log-likelihood of your
# SECOND choice model, etc.

# Model 1: LL = -2581.6
# Model 2: LL = -2581.3 (more parameters=intercepts)
# Test Statistics LR = -2(LL(model1)-LL(model2))

  lrtest(mlogit.model1,mlogit.model2)
  
# Likelihood ratio test

# Model 1: choice ~ 0 + seat + cargo + eng + price
# Model 2: choice ~ seat + cargo + eng + price
#  #Df  LogLik Df  Chisq Pr(>Chisq)
# 1   7 -2581.6                     
# 2   9 -2581.3  2 0.6789     0.7122 > 0.5

# The p-value is 0.7122 > 0.05 --> CANNOT REJECT NULL ("ACCEPTING" NULL)
# What is the null hypothesis of the LR test?
# > The more complex model is NOT better than the simpler model.
# What is the alternative hypothesis of the LR test?
# > The more complex model is better than the simpler model.

#  as.numeric(as.character(cbc.mlogit.df$price))
  
# FACTOR (MLOGIT FORM) --> CHARACTER --> NUMERIC (REGULAR DF FORM)

  mlogit.model3 <- mlogit(formula=choice~0+seat+cargo+eng+
                          as.numeric(as.character(price)),data=cbc.mlogit.df)
  summary(mlogit.model3)
  
#  as.numeric(as.character(price)) -0.1733053  0.0069398 -24.9726 < 2.2e-16 ***
  
  lrtest(mlogit.model1,mlogit.model3)
  
# Likelihood ratio test

# Model 1: choice ~ 0 + seat + cargo + eng + price
# Model 3: choice ~ 0 + seat + cargo + eng + as.numeric(as.character(price))
# Df  LogLik Df  Chisq Pr(>Chisq)
# 1   7 -2581.6                     
# 2   6 -2582.1 -1 0.9054     0.3413 > .05 --> cannot reject null 

# calculate the LR test statistic:

  -2*(mlogit.model1$logLik-mlogit.model3$logLik)

# Model 3 is not better than Model 1. SANITY CHECK!

##### APPLICATIONS OF MULTINOMIAL LOGIT MODEL #####

## 1 ##

# WILLINGNESS-TO-PAY --> this is how we interpret the coefficients
# WTP is calculated by dividing the coefficient on a given level of an
# attribute by the SINGLE coefficient on price

## WTP(attribute.level) = coef(attribute.level)/abs(coef[SINGLE.price])
## >> coefficients here have to be from the same model!


## MANUAL CALC: WTP(cargo3ft) = 0.4766936/(-(-0.1733053)/1000) = $2,750.60
## MANUAL CALC: WTR(engelec) = -1.5291247/(-(-0.1733053)/1000) = $8,823.30

  (wtp.cargo <- coef(mlogit.model3)["cargo3ft"]/
               (-coef(mlogit.model3)["as.numeric(as.character(price))"]/1e3))

## 2 ##

## PREDICTING/SIMULATING MARKET SHARES

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

  (all.data <- expand.grid(attrib))
  predict.shares(model=mlogit.model1,data=all.data)

  (new.data <- expand.grid(attrib)[c(4,9,11,18),])
  predict.shares(model=mlogit.model1,data=new.data)

  (new.data <- expand.grid(attrib)[c(8,1,3,41,49,26),])
  rownames(new.data) <- NULL

  predict.shares(model=mlogit.model3,data=new.data)
  predict.shares(model=mlogit.model1,data=new.data)

# SHARE SENSITIVITY ANALYSIS

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
  
  expand.grid(attrib)
  base.data <- expand.grid(attrib)[4,]
  competitor.data <- expand.grid(attrib)[c(5,6,1,10,16,22,40),]
  
  (tradeoff <- sensitivity.shares(model=mlogit.model1,attrib=attrib,
                                  base.data=base.data,
                                  competitor.data=competitor.data))
  barplot(height=tradeoff$increase,horiz=FALSE,names.arg=tradeoff$level,
          ylim=c(-0.2,0),ylab="Change in Share for Baseline Product")

# share predictions for two identical vehicles ** DO NOT DO THIS! **

  new.data2 <- expand.grid(attrib)[c(8,8,1,3,41,49,26),]
  predict.shares(model=mlogit.model1,data=new.data2)  

## CHOICE MODEL #3
## fit (hierarchical) multinomial logit model (a.k.a. mixed logit model)
##											  (a.k.a random coefficients logit model)
##											  (a.k.a. BLP model)
##											  (Berry, Levinson, and Pakes)

# How many coefficients are we estimating here? 200*7+7 = 1,407
# There are 7 fixed effects and 1,400 random effects across 200 respondents. 

# Coefficients :
#          Estimate Std. Error  z-value  Pr(>|z|)    
# seat7    -0.535280   0.062360  -8.5837 < 2.2e-16 *** --> 200
# seat8    -0.305840   0.061129  -5.0032 5.638e-07 *** --> 200
# cargo3ft  0.477449   0.050888   9.3824 < 2.2e-16 *** --> 200
# enghyb   -0.811282   0.060130 -13.4921 < 2.2e-16 *** --> 200
# engelec  -1.530762   0.067456 -22.6926 < 2.2e-16 *** --> 200
# price35  -0.913656   0.060601 -15.0765 < 2.2e-16 *** --> 200
# price40  -1.725851   0.069631 -24.7856 < 2.2e-16 *** --> 200

## QUICK STAT REVIEW:
## 95% confidence intervals <--> p-value = 0.05
## 90% confidence intervals <--> p-value = 0.1
## 99% confidence intervals <--> p-value = 0.01


# setting up our assumption about how these attributes are distributed
# in the population of respondents
# "rpar" stands for "random parameters"

  (mlogit.model1.rpar1 <- rep(x="n",length=length(mlogit.model1$coef)))
  
  (mlogit.model1.rpar2 <- c(rep(x="n",length=length(mlogit.model1$coef)-2),
  							rep(x="ln",length=length(mlogit.model1$coef)-5)))
  
  names(mlogit.model1.rpar1) <- names(mlogit.model1$coef)
  names(mlogit.model1.rpar2) <- names(mlogit.model1$coef)
  mlogit.model1.rpar1
  mlogit.model1.rpar2

# estimate mixed logit model using rpar1 (assume ALL variables are normal)

  mlogit.model1.hier <- mlogit(formula=choice~0+seat+eng+cargo+price, 
                               data=cbc.mlogit.df,panel=TRUE,correlation=FALSE,
                               rpar=mlogit.model1.rpar1)
  summary(mlogit.model1.hier)
# Frequencies of alternatives:choice
#      1       2       3 
# 0.32700 0.33467 0.33833 

# Coefficients :
#             Estimate Std. Error  z-value  Pr(>|z|)    
# seat7       -0.642241   0.070893  -9.0593 < 2.2e-16 ***
# seat8       -0.390021   0.070460  -5.5353 3.106e-08 ***
# enghyb      -0.926145   0.067456 -13.7296 < 2.2e-16 ***
# engelec     -1.831864   0.083439 -21.9544 < 2.2e-16 ***
# cargo3ft     0.550838   0.058459   9.4226 < 2.2e-16 ***
# price35     -1.081310   0.070874 -15.2567 < 2.2e-16 ***
# price40     -1.991787   0.085312 -23.3471 < 2.2e-16 ***
# NOTE: These fixed effects represent the average effect/coefficients
#		of the 200 respondents.
# sd.seat7     0.651807   0.101906   6.3961 1.594e-10 ***
# sd.seat8     0.995007   0.093397  10.6535 < 2.2e-16 ***
# sd.enghyb    0.159495   0.137950   1.1562  0.247607    
# sd.engelec   0.973303   0.099850   9.7476 < 2.2e-16 ***
# sd.cargo3ft  0.307194   0.131109   2.3430  0.019127 *  
# sd.price35   0.260907   0.121369   2.1497  0.031579 *  
# sd.price40   0.418148   0.128104   3.2641  0.001098 ** 
# NOTE: Standard deviations are the SDs of the normally distributed
#       estimates of the 200 respondents' random effects/coefficients
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Log-Likelihood: -2498.5

# random coefficients
#         Min.    1st Qu.     Median       Mean    3rd Qu. Max.
# seat7    -Inf -1.0818780 -0.6422410 -0.6422410 -0.2026039  Inf
# seat8    -Inf -1.0611428 -0.3900209 -0.3900209  0.2811010  Inf
# cargo3ft -Inf  0.3436387  0.5508377  0.5508377  0.7580366  Inf
# enghyb   -Inf -1.0337226 -0.9261449 -0.9261449 -0.8185673  Inf
# engelec  -Inf -2.4883466 -1.8318636 -1.8318636 -1.1753805  Inf
# price35  -Inf -1.2572890 -1.0813097 -1.0813097 -0.9053304  Inf
# price40  -Inf -2.2738236 -1.9917870 -1.9917870 -1.7097505  Inf

# [1] The mean and median are the same for a normal distribution.
# [2] The min and max are -Inf and Inf, respectively, because technically
#	  that is the case for a normal distribution.
  
  stdev(mlogit.model1.hier)


  mlogit.model2.hier <- update(mlogit.model1.hier,correlation=TRUE)
  summary(mlogit.model2.hier)
  cov2cor(cov.mlogit(mlogit.model2.hier))
  
  mlogit.model2.hier$indpar
  summary(mlogit.model2.hier$indpar[-1])
  
  cor(mlogit.model2.hier$indpar[2],mlogit.model2.hier$indpar[3])

# simulating predicted market shares

  library(MASS)
  predict.hier.shares <- function(model,data,nresp=1000) {
# function to predict shares from a hierarchical multinomial logit model 
# model: mlogit object returned by mlogit()
# data:  data frame containing set of designs for which you want to 
#        predict shares--same format as data used to estimate model
# this code assumes all model parameters are random
  data.model <- model.matrix(object=update(model$formula,0~.),data=data)[,-1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- model$coef[1:dim(coef.Sigma)[1]]
  draws <- mvrnorm(n=nresp,mu=coef.mu,Sigma=coef.Sigma)
  shares <- matrix(NA,nrow=nresp,ncol=nrow(data))
  for(i in 1:nresp) {
    utility <- data.model%*%draws[i,]
    share <- exp(utility)/sum(exp(utility))
    shares[i,] <- share
    }
  cbind(market.share=colMeans(shares),data)
  }
  predict.hier.shares(model=mlogit.model2.hier,data=new.data)
  
# market share predictions from multinomial logit model
#    market.share seat cargo  eng price
# 4    0.61633797    6   3ft  gas    30
# 9    0.12511384    8   2ft  hyb    30
# 11   0.16032978    7   3ft  hyb    30
# 18   0.09821841    8   3ft elec    30  
  
# market share predictions from random coefficients/mixed logit model  
#    market.share seat cargo  eng price
# 4     0.6190162    6   3ft  gas    30
# 9     0.1420742    8   2ft  hyb    30
# 11    0.1227220    7   3ft  hyb    30
# 18    0.1161876    8   3ft elec    30