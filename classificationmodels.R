###########################
## CLASSIFICATION MODELS ##
###########################

  setwd("C:/Users/.../BUAD476")

# install and then call R packages for use in this study

  install.packages("gam")
  install.packages("rpart")
  install.packages("e1071")
  install.packages("randomForest")
  install.packages("nnet")
  install.packages("NeuralNetTools")
  install.packages("rpart.plot")
  install.packages("psych")
  
  library(lattice)        # lattice plot
  library(vcd)            # mosaic plots
  library(ROCR)           # ROC curve objects for binary classification

  library(gam)            # generalized additive models for probability smoothing
  library(rpart)          # tree-structured modeling
  library(e1071)          # support vector machines
  library(randomForest)   # random forests
  library(nnet)           # neural networks
  library(NeuralNetTools) # neural network plotting
  library(rpart.plot)     # plot tree-structured model information
  library(psych)          # Cohen's kappa
  
  install.packages("devtools")
  library(devtools)
  devtools::install_github('skinner927/reprtree')
  library(reprtree)       # random forest tree classification objects

## user-defined function for plotting ROC curve

  plot.roc <- function(train.roc,train.auc,test.roc,test.auc) {
              plot(train.roc,col="blue",lty="solid",main="",lwd=2,
                   xlab="False Positive Rate",ylab="True Positive Rate")
              plot(test.roc,col="red",lty="dashed",lwd=2,add=TRUE)
              abline(c(0,1))
              train.legend <- paste("Training AUC = ",round(train.auc,digits=3))
              test.legend <- paste("Test AUC = ",round(test.auc,digits=3))
              legend("bottomright",legend=c(train.legend,test.legend),
                     lty=c("solid","dashed"),lwd=2,col=c("blue","red"))
              }

# read in comma-delimited text file and create data frame
# there are blank character fields for missing data

  att.df <- read.csv("att.csv")

  class(att.df)
  dim(att.df)
  str(att.df)
  summary(att.df)
  head(att.df)

#####################################################################
##                   OPTIONAL TO KNOW; MUST RUN!                   ##
#####################################################################

# convert character variables to factor variables and reassign 
# levels for certain factors

# for some variables, you simply have to factorize the variable using factor()

# for other variables, you have to use "ifelse" statements to generate
#   categorical variables, then factorize the variable using factor(), and
#   then define the levels of the factor variable

  att.df$pick <- factor(ifelse(att.df$pick=="OCC","OCC","ATT"),
                        levels=c("ATT","OCC"))

  att.df$age <- factor(att.df$age)

  att.df$employment <- factor(ifelse(att.df$employment=="D","Disabled",
                                ifelse(att.df$employment=="F","Full-Time",
                                  ifelse(att.df$employment=="H","Homemaker",
                                    ifelse(att.df$employment=="P","Part-Time",
                                      ifelse(att.df$employment=="R","Retired",
                                        ifelse(att.df$employment=="S","Student","Unemployed")))))),
                              levels=c("Disabled","Full-Time","Homemaker",
                                       "Part-Time","Retired","Student","Unemployed"))

  att.df$nonpub <- factor(att.df$nonpub)
  att.df$reachout <- factor(att.df$reachout)
  att.df$card <- factor(att.df$card)

  att.df$income <- factor(ifelse(att.df$income=="<7.5","<7.5",
                            ifelse(att.df$income=="7.5-15","7.5-15",
                              ifelse(att.df$income=="15-25","15-25",
                                ifelse(att.df$income=="25-35","25-35",
                                  ifelse(att.df$income=="35-45","35-45",
                                    ifelse(att.df$income=="45-75","45-75",">75")))))),
                          levels=c("<7.5","7.5-15","15-25","25-35","35-45","45-75",">75"))

  att.df$education <- factor(ifelse(att.df$education=="<HS","<HS",
                               ifelse(att.df$education=="HS","HS",
                                 ifelse(att.df$education=="Voc","Voc",
                                   ifelse(att.df$education=="Coll","Coll",
                                     ifelse(att.df$education=="BA","BA",">BA"))))),
                             levels=c("<HS","HS","Voc","Coll","BA",">BA"))

# check revised structure of data frame

  str(att.df)
  dim(att.df)
  head(att.df)
  summary(att.df,maxsum=10)

#####################################################################
##                           OPTIONAL END                          ##
#####################################################################

## EXPLORATORY DATA ANALYSIS

# user-defined program to create correlation heat map

  load("correlation_heat_map.RData")

# create data frame of just NUMERIC predictor variables

  X.df <- na.omit(att.df[,c(3,7)])

# create correlation heat map for explanatory variables

  cor.mat <- cor(X.df) # correlation matrix of predictors
  correlation_heat_map(cor.mat)
  
# examine relationship between moves and choice of carrier

  histogram(~moves|pick,data=att.df,type="density",layout=c(1,2),
            xlab="Number of Recent Moves")
  histogram(~log(moves+1)|pick,data=att.df,type="density",layout=c(1,2),
            xlab="(Log) Number of Recent Moves")

# examine relationship between usage and choice of carrier

  histogram(~usage|pick,data=att.df,type="density",layout=c(1,2),
            xlab="Telephone Usage (in minutes per month)")
  histogram(~log(usage+1)|pick,data=att.df,type="density",layout=c(1,2),
            xlab="(Log) Telephone Usage (in log-minutes per month)")

  att.df$log.usage = log(att.df$usage+1)

# probability smoothing for usage and switching using GAMS model

  att.gam.model <- gam(pick=="OCC"~s(usage),family=binomial,
                       data=att.df) 
  plot(att.df$usage,att.df$pick=="OCC",type="n", 
       ylim=c(-0.1,1.1),yaxt="n",ylab="Estimated Probability of Switching", 
       xlab="Telephone Usage (in minutes per month)") 
  axis(side=2,at=c(0,0.4854,1)) 
  points(jitter(att.df$usage), 
         att.df$pick=="OCC",pch="|") 
  ord.usage <- order(att.df$usage) 
  lines(att.df$usage[ord.usage],fitted(att.gam.model)[ord.usage])

  att.gam.model <- gam(pick=="OCC"~s(log.usage),family=binomial,
                       data=att.df) 
  plot(att.df$log.usage,att.df$pick=="OCC",type="n", 
       ylim=c(-0.1,1.1),yaxt="n",ylab="Estimated Probability of Switching", 
       xlab="(Log) Telephone Usage (in log-minutes per month)") 
  axis(side=2,at=c(0,0.4854,1)) 
  points(jitter(att.df$log.usage), 
         att.df$pick=="OCC",pch="|") 
  ord.usage <- order(att.df$log.usage) 
  lines(att.df$log.usage[ord.usage],fitted(att.gam.model)[ord.usage])

# examine relationship between income and choice

  mosaic(~pick+income,data=att.df,
         labeling_args=list(set_varnames=c(pick="Service Provider Choice", 
                                           income="Customer's Income Level")),
         highlighting="income",
         highlighting_fill=c("red","orange","yellow","green","blue","violet","brown",
                             "red","orange","yellow","green","blue","violet","brown"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

# examine relationship between age and choice

  mosaic(~pick+age,data=att.df,
         labeling_args=list(set_varnames=c(pick="Service Provider Choice", 
                                           age="Customer's Age Level")),
         highlighting="age",
         highlighting_fill=c("red","orange","yellow","green","blue","violet",
                             "red","orange","yellow","green","blue","violet"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

# examine relationship between education and choice

  mosaic(~pick+education,data=att.df,
         labeling_args=list(set_varnames=c(pick="Service Provider Choice", 
                                           education="Customer's Education Level")),
         highlighting="education",
         highlighting_fill=c("red","orange","yellow","green","blue","violet",
                             "red","orange","yellow","green","blue","violet"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

# examine relationship between employment and choice

  mosaic(~pick+employment,data=att.df,
         labeling_args=list(set_varnames=c(pick="Service Provider Choice", 
                                           employment="Customer's Employment")),
         highlighting="employment",
         highlighting_fill=c("red","orange","yellow","green","blue","violet","brown",
                             "red","orange","yellow","green","blue","violet","brown"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

# examine relationship between nonpub and choice

  mosaic(~pick+nonpub,data=att.df,
         labeling_args=list(set_varnames=c(pick="Service Provider Choice", 
                                           nonpub="Customer Has Unlisted Number")),
         highlighting="nonpub",
         highlighting_fill=c("yellow","blue",
                             "yellow","blue"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

# examine relationship between reachout and choice

  mosaic(~pick+reachout,data=att.df,
         labeling_args=list(set_varnames=c(pick="Service Provider Choice", 
                                           reachout="AT&T Reach Out America Plan")),
         highlighting="reachout",
         highlighting_fill=c("yellow","blue",
                             "yellow","blue"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

# examine relationship between card and choice

  mosaic(~pick+card,data=att.df,
         labeling_args=list(set_varnames=c(pick="Service Provider Choice",
                                           card="AT&T Credit Card")),
         highlighting="card",
         highlighting_fill=c("yellow","blue",
                             "yellow","blue"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

## DETERMINING PREDICTORS FOR CLASSIFICATION MODELS

# select predictor variables for use in classification models

  att.model <- {pick~income+moves+age+education+employment+log.usage+nonpub+
                reachout+card}

  att.model.fit <- glm(att.model,family=binomial,data=na.omit(att.df))
  summary(att.model.fit)

  anova(att.model.fit,test="Chisq")

  (McFaddenR2.model.fit <- 1-(att.model.fit$deviance/
                              att.model.fit$null.deviance))

# stepwise logistic regression helps determine predictors to use
# in classification models

  step(att.model.fit,direction="both")

# subset data to only include variables from stepwise regression

  att.df <- subset(att.df,select=c("pick","moves","log.usage","nonpub",
                                      "reachout","card"))

# subset data to eliminate any observations with missing data
  
  att.df <- na.omit(att.df)

# provide overview of data

  class(att.df)
  dim(att.df)
  str(att.df)
  summary(att.df)
  head(att.df)

# calibrate logistic regrssion model with abbreviated sample

  att.model.new <- {pick~moves+log.usage+nonpub+reachout+card}

  att.model.fit.new <- glm(att.model.new,family=binomial,data=att.df)
  summary(att.model.fit.new)

  anova(att.model.fit.new,test="Chisq")

  (McFaddenR2.model.fit.new <- 1-(att.model.fit.new$deviance/
                                  att.model.fit.new$null.deviance))

# compute predicted probability of switching service providers and
# then plot density lattice of predicted probabilities conditional
# on observed probabilities

  att.df$pred.prob <- predict.glm(att.model.fit.new,type="response") 

  densityplot(~pred.prob|pick,data=att.df, 
              layout=c(1,2),aspect=1,col="darkblue", 
              plot.points="rug",xlim=c(0,1),
              strip=function(...) strip.default(...,style=1),
              xlab="Predicted Probability of Switching")
  
  summary(as.numeric(att.df$pick)-1) # suggested probabilistic cutoff value

# use 0.4854 cutoff value for this problem

  att.df$pred.pick <- factor(ifelse((att.df$pred.prob>0.4854),"OCC","ATT"),
                             levels=c("ATT","OCC"),labels=c("ATT","OCC")) 
  
# generate confusion matrix

  (confusion.mat <- table(actual.pick=att.df$pick,
                          predicted.pick=att.df$pred.pick))

# compute predictive accuracy

  tp<-confusion.mat[2,2]
  tn<-confusion.mat[1,1]
  sum.confmat <- sum(confusion.mat)
  (predictive.accuracy <- (tp+tn)/sum.confmat)                                             

# mosaic rendering of classifier with 0.4854 cutoff value

  mosaic(~pick+pred.pick,data=att.df,
         labeling_args=list(set_varnames=c(pick="Actual Service Provider",
                                           pred.pick="Predicted Service Provider (48.54% cut-off)")),
         highlighting=c("pick","pred.pick"),
         highlighting_fill=c("green","red","red","green"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

# decision-tree-structured classification 

  att.tree.fit <- rpart(att.model.new,data=att.df[,1:6],
                        control=rpart.control(cp=.0025))

# plot classification tree result to learn most important predictors

  prp(att.tree.fit,main="",
      digits=3,                    # digits to display in terminal nodes
      nn=FALSE,                    # display node numbers
      branch=0.4854,                  # change angle of branch lines
      branch.lwd=2,                # width of branch lines
      faclen=0,                    # do not abbreviate factor levels
      trace=0,                     # print automatically calculated cex
      shadow.col=0,                # no shadows under leaves
      branch.lty=1,                # draw branches using dotted lines
      split.cex=1.2,               # make split text larger than node text
      split.prefix="Is ",          # put "Is" before split text
      split.suffix="?",            # put "?" after split text
      split.box.col="blue",        # blue split boxes (default is white)
      split.col="white",           # color of text in split box 
      split.border.col="darkgray", # darkgray border on split boxes
      split.round=.25)             # round split box corners a tad

# random forest model for importance of predictor variables

  set.seed(476) # for reproducibility
  att.rf.fit <- randomForest(att.model.new,data=att.df[,1:6],mtry=3,
                             ntree=10000,importance=TRUE)

# check importance of individual predictor variables 

  varImpPlot(att.rf.fit,color="blue",pch=20,cex=2,main="")

## TRAINING-AND-TEST REGIMEN FOR EVALUATING ALTERNATIVE METHODS

  set.seed(476)
  att.sample <- sort(sample(dim(att.df)[1],
                     size=(2/3)*dim(att.df)[1],
                     replace=FALSE)) # permuted list of row index numbers
  att.train.samp <- att.df[att.sample,1:6]
  rownames(att.train.samp) <- NULL
  att.test.samp <- att.df[-att.sample,1:6]
  rownames(att.test.samp) <- NULL

  dim(att.df)
  dim(att.train.samp)
  dim(att.test.samp)

## LOGISTIC REGRESSION CLASSIFICATION

# fit logistic regression model to training sample

  att.train.lr.fit <- glm(att.model.new,family=binomial,data=att.train.samp)
  summary(att.train.lr.fit)
  att.train.samp$lr.pred.prob <- predict(att.train.lr.fit,type="response")
  att.train.lr.pred <- prediction(att.train.samp$lr.pred.prob,att.train.samp$pick)
  att.train.lr.auc <- as.numeric(performance(att.train.lr.pred,"auc")@y.values)

# use model fit to training sample to evaluate on test sample

  att.test.samp$lr.pred.prob <- as.numeric(predict(att.train.lr.fit,
                                           type="response",newdata=att.test.samp))
  densityplot(~lr.pred.prob|pick,data=att.test.samp, 
              layout=c(1,2),aspect=1,col="darkblue", 
              plot.points="rug",xlim=c(0,1),
              strip=function(...) strip.default(...,style=1),
              xlab="Predicted Probability of Switching") 
  summary(as.numeric(att.test.samp$pick)-1)
  att.test.samp$lr.pred.pick <- factor(ifelse((att.test.samp$lr.pred.prob>0.475),
                                       "OCC","ATT"),levels=c("ATT","OCC"),
                                       labels=c("ATT","OCC"))
  att.test.lr.confmat <- table(att.test.samp$pick,att.test.samp$lr.pred.pick)
  att.test.lr.predacc <- (att.test.lr.confmat[1,1]+att.test.lr.confmat[2,2])/
                         sum(att.test.lr.confmat)
  att.test.lr.cohenkappa <- cohen.kappa(att.test.lr.confmat)
  att.test.lr.pred <- prediction(att.test.samp$lr.pred.prob,att.test.samp$pick)
  att.test.lr.auc <- as.numeric(performance(att.test.lr.pred,"auc")@y.values)

# ROC for logistic regression

  att.train.lr.roc <- performance(att.train.lr.pred,"tpr","fpr")
  att.test.lr.roc <- performance(att.test.lr.pred,"tpr","fpr")
  plot.roc(train.roc=att.train.lr.roc,train.auc=att.train.lr.auc, 
           test.roc=att.test.lr.roc,test.auc=att.test.lr.auc)     

  att.train.samp <- att.train.samp[,1:6]
  att.test.samp <- att.test.samp[,1:6]

 
## SUPPORT VECTOR MACHINES CLASSIFICATION


# determine tuning parameters prior to fitting model to training sample

  set.seed(476) # for reproducibility
  att.train.svm.tune <- tune(svm,att.model.new,data=att.train.samp,
                             ranges=list(gamma=2^(-8:1),cost=2^(0:4)),
                             tunecontrol=tune.control(sampling="fix"))

# fit support vector machine to training sample using tuning parameters

  att.train.svm.fit <- svm(att.model.new,data=att.train.samp, 
                           cost=att.train.svm.tune$best.parameters$cost, 
                           gamma=att.train.svm.tune$best.parameters$gamma, 
                           probability=TRUE)
  att.train.svm.predict <- predict(att.train.svm.fit,att.train.samp,probability=TRUE)
  att.train.samp$svm.pred.prob <- attr(att.train.svm.predict,"probabilities")[,2]
  att.train.svm.prediction <- prediction(att.train.samp$svm.pred.prob,att.train.samp$pick)
  att.train.svm.auc <- as.numeric(performance(att.train.svm.prediction,"auc")@y.values)

# plot support vectors for response variable by log.usage

  att.train.samp$pick.no <- as.numeric(att.train.samp$pick)-1
  plot(att.train.svm.fit,att.train.samp,pick.no~log.usage)

# use model fit to training sample to evaluate on test sample

  att.test.svm.predict <- predict(att.train.svm.fit,att.test.samp,probability=TRUE)
  att.test.samp$svm.pred.prob <- attr(att.test.svm.predict,"probabilities")[,2]
  densityplot(~svm.pred.prob|pick,data=att.test.samp, 
              layout=c(1,2),aspect=1,col="darkblue", 
              plot.points="rug",xlim=c(0,1),
              strip=function(...) strip.default(...,style=1),
              xlab="Predicted Probability of Switching")
  att.test.samp$svm.pred.pick <- factor(ifelse((att.test.samp$svm.pred.prob>0.475),
                                        "OCC","ATT"),levels=c("ATT","OCC"),
                                        labels=c("ATT","OCC"))
  att.test.svm.confmat <- table(att.test.samp$pick,att.test.samp$svm.pred.pick)
  att.test.svm.predacc <- (att.test.svm.confmat[1,1]+att.test.svm.confmat[2,2])/
                          sum(att.test.svm.confmat)
  att.test.svm.cohenkappa <- cohen.kappa(att.test.svm.confmat)
  att.test.svm.prediction <- prediction(att.test.samp$svm.pred.prob,att.test.samp$pick)
  att.test.svm.auc <- as.numeric(performance(att.test.svm.prediction,"auc")@y.values)

# ROC for support vector machines classification

  att.train.svm.roc <- performance(att.train.svm.prediction,"tpr","fpr")
  att.test.svm.roc <- performance(att.test.svm.prediction,"tpr","fpr")
  plot.roc(train.roc=att.train.svm.roc,train.auc=att.train.svm.auc, 
           test.roc=att.test.svm.roc,test.auc=att.test.svm.auc)   

 att.train.samp <- att.train.samp[,1:6]
 att.test.samp <- att.test.samp[,1:6]

 
## ARTIFICIAL NEURAL NETWORKS CLASSIFICATION


# fit neural network model to training sample

  set.seed(476) # for reproducibility
  att.train.nnet.fit <- nnet(att.model.new,data=att.train.samp,size=3,decay=0.5,
                             probability=TRUE,trace=FALSE) 
  att.train.samp$nnet.pred.prob <- as.numeric(predict(att.train.nnet.fit,
                                                      newdata=att.train.samp))
  att.train.nnet.prediction <- prediction(att.train.samp$nnet.pred.prob,att.train.samp$pick)
  att.train.nnet.auc <- as.numeric(performance(att.train.nnet.prediction,"auc")@y.values)

# plot artificial neural network

  plotnet(att.train.nnet.fit,bias=FALSE)

# use model fit to training sample to evaluate on test sample

  att.test.samp$nnet.pred.prob <- as.numeric(predict(att.train.nnet.fit,
                                                     newdata=att.test.samp))
  densityplot(~nnet.pred.prob|pick,data=att.test.samp, 
              layout=c(1,2),aspect=1,col="darkblue", 
              plot.points="rug",xlim=c(0,1),
              strip=function(...) strip.default(...,style=1),
              xlab="Predicted Probability of Switching") 
  att.test.samp$nnet.pred.pick <- factor(ifelse((att.test.samp$nnet.pred.prob>0.475),
                                         "OCC","ATT"),levels=c("ATT","OCC"),
                                         labels=c("ATT","OCC"))
  att.test.nnet.confmat <- table(att.test.samp$pick,att.test.samp$nnet.pred.pick)
  att.test.nnet.predacc <- (att.test.nnet.confmat[1,1]+att.test.nnet.confmat[2,2])/
                           sum(att.test.nnet.confmat)
  att.test.nnet.cohenkappa <- cohen.kappa(att.test.nnet.confmat)
  att.test.nnet.prediction <- prediction(att.test.samp$nnet.pred.prob,att.test.samp$pick)
  att.test.nnet.auc <- as.numeric(performance(att.test.nnet.prediction,"auc")@y.values)

# ROC for neural network classification

  att.train.nnet.roc <- performance(att.train.nnet.prediction,"tpr","fpr")
  att.test.nnet.roc <- performance(att.test.nnet.prediction,"tpr","fpr")
  plot.roc(train.roc=att.train.nnet.roc,train.auc=att.train.nnet.auc, 
           test.roc=att.test.nnet.roc,test.auc=att.test.nnet.auc)

  att.train.samp <- att.train.samp[,1:6]
  att.test.samp <- att.test.samp[,1:6]

  
## NAIVE BAYES CLASSIFICATION


# fit naive Bayes model to training sample

  set.seed(476) # for reproducibility
  att.train.nb.fit <- naiveBayes(att.model.new,data=att.train.samp)
  att.train.nb.fit 
  att.train.samp$nb.pred.prob <- as.numeric(predict(att.train.nb.fit,
                                                    type="raw",
                                                    newdata=att.train.samp)[,2])
  att.train.nb.prediction <- prediction(att.train.samp$nb.pred.prob,att.train.samp$pick)
  att.train.nb.auc <- as.numeric(performance(att.train.nb.prediction,"auc")@y.values)

# use model fit to training sample to evaluate on test sample

  att.test.samp$nb.pred.prob <- as.numeric(predict(att.train.nb.fit,
                                                   type="raw",
                                                   newdata=att.test.samp)[,2])
  densityplot(~nb.pred.prob|pick,data=att.test.samp, 
              layout=c(1,2),aspect=1,col="darkblue", 
              plot.points="rug",xlim=c(0,1),
              strip=function(...) strip.default(...,style=1),
              xlab="Predicted Probability of Switching") 
  att.test.samp$nb.pred.pick <- factor(ifelse((att.test.samp$nb.pred.prob>0.475),
                                               "OCC","ATT"),levels=c("ATT","OCC"),
                                               labels=c("ATT","OCC"))
  att.test.nb.confmat <- table(att.test.samp$pick,att.test.samp$nb.pred.pick)
  att.test.nb.predacc <- (att.test.nb.confmat[1,1]+att.test.nb.confmat[2,2])/
                         sum(att.test.nb.confmat)
  att.test.nb.cohenkappa <- cohen.kappa(att.test.nb.confmat)
  att.test.nb.prediction <- prediction(att.test.samp$nb.pred.prob,att.test.samp$pick)
  att.test.nb.auc <- as.numeric(performance(att.test.nb.prediction,"auc")@y.values)

# ROC for naive Bayes classification

  att.train.nb.roc <- performance(att.train.nb.prediction,"tpr","fpr")
  att.test.nb.roc <- performance(att.test.nb.prediction,"tpr","fpr")
  plot.roc(train.roc=att.train.nb.roc,train.auc=att.train.nb.auc, 
           test.roc=att.test.nb.roc,test.auc=att.test.nb.auc)

  att.train.samp <- att.train.samp[,1:6]
  att.test.samp <- att.test.samp[,1:6]

  
## RANDOM FORESTS CLASSIFICATION


# fit random forests model to training sample

  set.seed(476) # for reproducibility
  att.train.rf.fit <- randomForest(att.model.new,data=att.train.samp,mtry=3,
                                   importance=FALSE,ntree=6500)
  att.train.samp$rf.pred.prob <- as.numeric(predict(att.train.rf.fit,type="prob")[,2])
  att.train.rf.prediction <- prediction(att.train.samp$rf.pred.prob,att.train.samp$pick)
  att.train.rf.auc <- as.numeric(performance(att.train.rf.prediction,"auc")@y.values)

# plot forest

  getTree(att.train.rf.fit,k=1,labelVar=TRUE)
  reprtree:::plot.getTree(att.train.rf.fit)

# use model fit to training sample to evaluate on test sample

  att.test.samp$rf.pred.prob <- as.numeric(predict(att.train.rf.fit,type="prob",
                                                   newdata=att.test.samp)[,2])
  densityplot(~rf.pred.prob|pick,data=att.test.samp, 
              layout=c(1,2),aspect=1,col="darkblue", 
              plot.points="rug",xlim=c(0,1),
              strip=function(...) strip.default(...,style=1),
              xlab="Predicted Probability of Switching") 
  att.test.samp$rf.pred.pick <- factor(ifelse((att.test.samp$rf.pred.prob>0.475),
                                       "OCC","ATT"),levels=c("ATT","OCC"),
                                       labels=c("ATT","OCC"))
  att.test.rf.confmat <- table(att.test.samp$pick,att.test.samp$rf.pred.pick)
  att.test.rf.predacc <- (att.test.rf.confmat[1,1]+att.test.rf.confmat[2,2])/
                         sum(att.test.rf.confmat)
  att.test.rf.cohenkappa <- cohen.kappa(att.test.rf.confmat)
  att.test.rf.prediction <- prediction(att.test.samp$rf.pred.prob,att.test.samp$pick)
  att.test.rf.auc <- as.numeric(performance(att.test.rf.prediction,"auc")@y.values)

# ROC for random forest classification

  att.train.rf.roc <- performance(att.train.rf.prediction,"tpr","fpr")
  att.test.rf.roc <- performance(att.test.rf.prediction,"tpr","fpr")
  plot.roc(train.roc=att.train.rf.roc,train.auc=att.train.rf.auc, 
           test.roc=att.test.rf.roc,test.auc=att.test.rf.auc)

# variable importance for predictor variables using training sample

  varImpPlot(att.train.rf.fit,main="",pch=20,cex=1.25,color="blue")

  att.train.samp <- att.train.samp[,1:6]
  att.test.samp <- att.test.samp[,1:6]

  
## SUMMARIES OF CLASSIFICATION MODELS


  pred.acc <- rbind(att.test.lr.predacc,
                    att.test.svm.predacc,
                    att.test.nnet.predacc,
                    att.test.nb.predacc,
                    att.test.rf.predacc)
  rownames(pred.acc) <- c("Logistic Regression","Support Vector Machines",
                          "Neural Networks","Naive Bayes","Random Forests")
  colnames(pred.acc) <- "Predictive Accuracy"
  print(pred.acc)

  coh.kappa <- rbind(att.test.lr.cohenkappa$kappa,
                     att.test.svm.cohenkappa$kappa,
                     att.test.nnet.cohenkappa$kappa,
                     att.test.nb.cohenkappa$kappa,
                     att.test.rf.cohenkappa$kappa)
  rownames(coh.kappa) <- c("Logistic Regression","Support Vector Machines",
                           "Neural Networks","Naive Bayes","Random Forests")
  colnames(coh.kappa) <- "Cohen's Kappa"
  print(coh.kappa)

  auc.roc.curve <- rbind(att.test.lr.auc,
                         att.test.svm.auc,
                         att.test.nnet.auc,
                         att.test.nb.auc,
                         att.test.rf.auc)
  rownames(auc.roc.curve) <- c("Logistic Regression","Support Vector Machines",
                               "Neural Networks","Naive Bayes","Random Forests")
  colnames(auc.roc.curve) <- "Area Under the ROC Curve (Test Sample)"
  print(auc.roc.curve)

  colnames(auc.roc.curve) <- "AUC-Test"
  print(cbind(pred.acc,coh.kappa,auc.roc.curve))

  
## PREDICTION FROM NEW CUSTOMER DATA


## logistic regression

  att.new.df <- data.frame(moves=0,log.usage=3.218876,nonpub="YES",reachout="NO",card="NO")
  (att.new.df$lr.pred.prob1 <- as.numeric(predict(att.train.lr.fit,
                                          type="response",newdata=att.new.df)))

  att.new.df <- data.frame(moves=0,log.usage=3.218876,nonpub="YES",reachout="YES",card="NO")
  (att.new.df$lr.pred.prob2 <- as.numeric(predict(att.train.lr.fit,
                                          type="response",newdata=att.new.df)))

## support vector machines

  att.new.df <- data.frame(moves=c(0,0),log.usage=c(3.218876,3.218876),nonpub=c("YES","NO"),
                           reachout=c("NO","YES"),card=c("NO","YES"))
  att.new.df.predict <- predict(att.train.svm.fit,att.new.df,probability=TRUE)
 (att.new.df$svm.pred.prob1 <- attr(att.new.df.predict,"probabilities")[1,2])

  att.new.df <- data.frame(moves=c(0,0),log.usage=c(3.218876,3.218876),nonpub=c("YES","NO"),
                           reachout=c("YES","NO"),card=c("NO","YES"))
  att.new.df.predict <- predict(att.train.svm.fit,att.new.df,probability=TRUE)
 (att.new.df$svm.pred.prob2 <- attr(att.new.df.predict,"probabilities")[1,2])

## artifical neural networks

  att.new.df <- data.frame(moves=0,log.usage=3.218876,nonpub="YES",reachout="NO",card="NO")
  (att.new.df$nnet.pred.prob1 <- as.numeric(predict(att.train.nnet.fit,
                                                    newdata=att.new.df)))

  att.new.df <- data.frame(moves=0,log.usage=3.218876,nonpub="YES",reachout="YES",card="NO")
  (att.new.df$nnet.pred.prob2 <- as.numeric(predict(att.train.nnet.fit,
                                                    newdata=att.new.df)))

## naive Bayes

  att.new.df <- data.frame(moves=0,log.usage=3.218876,nonpub="YES",reachout="NO",card="NO")
  (att.new.df$nb.pred.prob1 <- as.numeric(predict(att.train.nb.fit,
                                                  type="raw",
                                                  newdata=att.new.df)[,2]))

  att.new.df <- data.frame(moves=0,log.usage=3.218876,nonpub="YES",reachout="YES",card="NO")
  (att.new.df$nb.pred.prob2 <- as.numeric(predict(att.train.nb.fit,
                                                  type="raw",
                                                  newdata=att.new.df)[,2]))

## random forests

  att.new.df <- data.frame(moves=0,log.usage=3.218876,nonpub="YES",reachout="NO",card="NO")
  att.new.df <- rbind(att.train.samp[1,2:6],att.new.df)
  att.new.df <- att.new.df[-1,]
 (att.new.df$rf.pred.prob1 <- predict(att.train.rf.fit,type="prob",
                                      newdata=att.new.df)[,2])

  att.new.df <- data.frame(moves=0,log.usage=3.218876,nonpub="YES",reachout="YES",card="NO")
  att.new.df <- rbind(att.train.samp[1,2:6],att.new.df)
  att.new.df <- att.new.df[-1,]  
 (att.new.df$rf.pred.prob2 <- predict(att.train.rf.fit,type="prob",
                                      newdata=att.new.df)[,2])