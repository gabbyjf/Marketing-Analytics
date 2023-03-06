####################################
## CHAPTER 6. Retaining Customers ##
####################################

  options(scipen=4)

### Classification is classifying customers into categories --> segmentation
### BINARY CLASSIFIERS: Two categories --> "yes" or "no"
###                                        "buy" or "not buy"
###                                        "churn" or "retain"

  setwd("/Users/gabriellefrieder/Downloads/BUAD466 IS w: Dee/Classification Models")
  getwd()

  library(lattice)          # lattice plot
  install.packages("vcd")   # install mosaic plots package
  library(vcd)      		    # mosaic plots
  install.packages("ROCR")  # install ROC curve objects package
  library(ROCR)             # ROC curve objects for binary classification

  install.packages("gam")
  library(gam)              # generalized additive models for probability smoothing
  library(rpart)            # tree-structured modeling
  install.packages("e1071") # install support vector machines package
  library(e1071)            # support vector machines
  install.packages("randomForest")
  library(randomForest)     # random forests
  library(nnet)             # neural networks
  install.packages("NeuralNetTools")
  library(NeuralNetTools)   # neural network plotting
  install.packages("rpart.plot")
  library(rpart.plot)       # plot tree-structured model information
  library(psych)            # Cohen's kappa
# library(reprtree)         # random forest tree classification objects
							# not able to install package

# pre-defined function that plots ROC curve

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

  churn.df <- read.csv("churn.csv",stringsAsFactors=TRUE)
  str(churn.df)
  churn.df <- churn.df[,-1]
  str(churn.df)

  churn.df$senior.citizen <- factor(ifelse(churn.df$senior.citizen==0,"No","Yes"),
                                    levels=c("No","Yes"),labels=c("No","Yes"))
  str(churn.df)

## STEP I. EXPLORATORY DATA ANALYSIS

  load("correlation_heat_map.RData")
  
  str(churn.df)
  X.df <- na.omit(churn.df[,c(6,11:12)])

  X.df <- na.omit(churn.df[,c(6,11:12)])
  cor.mat <- cor(X.df) # correlation matrix of predictors
  correlation_heat_map(cor.mat)
  summary(churn.df)

  histogram(~tenure|churn,data=churn.df,type="density",layout=c(1,2),
            xlab="Tenure with Company (months)")
  histogram(~log(tenure+1)|churn,data=churn.df,type="density",layout=c(1,2),
            xlab="(Log) Tenure with Company") # overcorrects

  histogram(~monthly.charge|churn,data=churn.df,type="density",layout=c(1,2),
            xlab="Monthly Charge ($)")

  histogram(~total.charges|churn,data=churn.df,type="density",layout=c(1,2),
            xlab="Total Charges over Tenure ($)")
  histogram(~log(total.charges)|churn,data=churn.df,type="density",layout=c(1,2),
            xlab="(Log) Total Charges over Tenure") # overcorrects

  churn.gam.model <- gam(churn=="Yes"~s(tenure),family=binomial,
                         data=churn.df) 
  plot(x=churn.df$tenure,y=churn.df$churn=="Yes",type="n", 
       ylim=c(-0.1,1.1),yaxt="n",ylab="Estimated Probability of Churning", 
       xlab="Tenure with Company (months)") 
  axis(side=2,at=c(0,0.5,1)) 
  points(jitter(churn.df$tenure), 
         churn.df$churn=="Yes",pch="|") 
  ord.tenure <- order(churn.df$tenure) 
  lines(churn.df$tenure[ord.tenure],fitted(churn.gam.model)[ord.tenure])

  mosaic(~churn+contract,data=churn.df,
         labeling_args=list(set_varnames=c(churn="Does the customer churn?", 
                                           contract="How long is customer contract?")),
         highlighting="contract",
         highlighting_fill=c("red","yellow","blue",
                             "red","yellow","blue"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

  mosaic(~churn+internet.service,data=churn.df,
         labeling_args=list(set_varnames=c(churn="Does the customer churn?", 
                                           internet.service="What kind of ISP does the customer have?")),
         highlighting="internet.service",
         highlighting_fill=c("red","yellow","blue",
                             "red","yellow","blue"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

  mosaic(~churn+tech.support,data=churn.df,
         labeling_args=list(set_varnames=c(churn="Does the customer churn?", 
                                           tech.support="Has the customer interacted with tech support?")),
         highlighting="tech.support",
         highlighting_fill=c("yellow","blue",
                             "yellow","blue"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

  mosaic(~churn+streaming.tv,data=churn.df,
         labeling_args=list(set_varnames=c(churn="Does the customer churn?", 
                                           streaming.tv="Does the customer have streaming service (TV)?")),
         highlighting="streaming.tv",
         highlighting_fill=c("yellow","blue",
                             "yellow","blue"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

  mosaic(~churn+streaming.movies,data=churn.df,
         labeling_args=list(set_varnames=c(churn="Does the customer churn?", 
                                           streaming.movies="Does the customer have streaming movies?")),
         highlighting="streaming.movies",
         highlighting_fill=c("yellow","blue",
                             "yellow","blue"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0,top=0.6))

## STEP II. DETERMINING PREDICTORS FOR CLASSIFICATION MODELS

## LOGIT CLASSIFICATION ##

# select predictor variables for use in classification models

  churn.df$total.charges <- churn.df$total.charges/1e2

  churn.model <- {churn~.}
  churn.model.fit <- glm(formula=churn.model,family=binomial,
                         data=na.omit(churn.df))
  summary(churn.model.fit)

  anova(churn.model.fit,test="Chisq")

  (McFaddenR2.model.fit <- 1-(churn.model.fit$deviance/
                              churn.model.fit$null.deviance))

# stepwise logistic regression helps determine predictors to use
# in classification models

  step(object=churn.model.fit,direction="both")

# subset data to only include variables from stepwise regression

  churn.df <- subset(churn.df,select=c("churn","senior.citizen","dependents",
                     "tenure","contract","paperless.billing","payment.method",
                     "monthly.charge","total.charges","multiple.lines",
                     "internet.service","online.security","tech.support",
                     "streaming.tv","streaming.movies"))

# subset data to eliminate any observations with missing data
  
  churn.df <- na.omit(churn.df)

# provide overview of data

  class(churn.df)
  dim(churn.df)
  str(churn.df)
  summary(churn.df)
  head(churn.df)

# calibrate logistic regrssion model with abbreviated sample

  churn.model.new <- {churn~.}

  churn.model.fit.new <- glm(churn.model.new,family=binomial,data=churn.df)
  summary(churn.model.fit.new)

# from alt model	      
# w/o total charges		  # from final model including both collinear vars
# tenure = -0.033867 <=>  [ tenure = -0.060492 + total.charges = 0.033114 ]
#                                         = -0.027378
# from alt model
# w/o tenure              # from final model including both collinear vars
# total.charges = -0.031795 <=>  [ total.charges = 0.033114 + tenure = -0.060492]
#                                         = -0.027378

# REMOVE: total.charges, multiple.lines

  anova(churn.model.fit.new,test="Chisq")

  (McFaddenR2.model.fit.new <- 1-(churn.model.fit.new$deviance/
                                  churn.model.fit.new$null.deviance))
# FINAL PARSIMONIOUS MODEL!

  churn.model.FINAL <- {churn~. -total.charges -multiple.lines} 
  churn.model.fit.FINAL <- glm(churn.model.FINAL,family=binomial,
  							   data=churn.df)
  summary(churn.model.fit.FINAL)
  anova(churn.model.fit.FINAL,test="Chisq")

  (McFaddenR2.model.fit.FINAL <- 1-(churn.model.fit.FINAL$deviance/
                                  churn.model.fit.FINAL$null.deviance))

######################## FOR TUESDAY! #####################################

## DECISION TREE CLASSIFICATION ##

# decision-tree-structured classification 

  churn.tree.fit <- rpart(churn.model.FINAL,data=churn.df,
                          control=rpart.control(cp=.0025))

# plot classification tree result to learn most important predictors

  prp(churn.tree.fit,main="",cex=.75,
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

# DECISION TREE CLASSIFICATION: contract, internet.service, tenure, payment.method,
#						        monthly.charge, paperless.billing, online.security,
#					            tech.support, senior.citizen

## RANDOM FOREST CLASSIFICATION ##

# random forest model for importance of predictor variables
# why it's random is that you are sampling three variables of the 12 variables
# for each tree you are creating, and you are sampling a new three variables
# for all 35,000 trees

  set.seed(11) # for reproducibility
  churn.rf.fit <- randomForest(churn.model.FINAL,data=churn.df[,1:15],mtry=3,
                               ntree=35000,importance=TRUE)

# check importance of individual predictor variables 

  varImpPlot(churn.rf.fit,color="blue",pch=20,cex=2,main="")
  
# RANDOM FOREST CLASSIFICATION: tenure, contract, monthly.charge, internet service, ...  

# Root Mean Squared Error (RMSE) --> PLOT 1
> Measure of the average residual/error
# Residual Sum of Squares (SSE)  --> PLOT 2
> Total/sum of all squared errors

## STEP III. SPLITTING DATA INTO TRAINING AND TEST SAMPLES

## TRAINING-AND-TEST REGIMEN FOR EVALUATION OF ALTERNATIVE METHODS

  set.seed(11)
  churn.sample <- sort(sample(dim(churn.df)[1],
                       size=(2/3)*dim(churn.df)[1],
                       replace=FALSE)) # permuted list of row index numbers
  churn.train.samp <- churn.df[churn.sample,]
  rownames(churn.train.samp) <- NULL
  churn.test.samp <- churn.df[-churn.sample,]
  rownames(churn.test.samp) <- NULL

  dim(churn.df)
  dim(churn.train.samp)
  dim(churn.test.samp)

## STEP IV. ESTIMATING CLASSIFICATION MODELS (5)

## LOGISTIC REGRESSION CLASSIFICATION

# STEP 1: FIT CLASSIFICATION MODEL USING TRAINING SAMPLE
# fit logistic regression model to training sample
# and then made predictions using training sample --> fitting the model

  churn.train.lr.fit <- glm(churn.model.FINAL,family=binomial,data=churn.train.samp)
  churn.train.samp$lr.pred.prob <- predict(churn.train.lr.fit,type="response")
  churn.train.lr.pred <- prediction(churn.train.samp$lr.pred.prob,churn.train.samp$churn)
  churn.train.lr.auc <- as.numeric(performance(churn.train.lr.pred,"auc")@y.values)

# STEP 2: USING FITTED CLASSIFICATION MODEL ON TEST SAMPLE
# use model fit to training sample to evaluate on test sample

  churn.test.samp$lr.pred.prob <- as.numeric(predict(churn.train.lr.fit,
                                 			 type="response",newdata=churn.test.samp))
  densityplot(~lr.pred.prob|churn,data=churn.test.samp, 
              layout=c(1,2),aspect=1,col="darkblue", 
              plot.points="rug",xlim=c(0,1),
              strip=function(...) strip.default(...,style=1),
              xlab="Predicted Probability of Churning") 
  probcutoff <- mean(as.numeric(churn.test.samp$churn)-1)
  churn.test.samp$lr.pred.churn <- factor(ifelse((churn.test.samp$lr.pred.prob>probcutoff),
                                          "Yes","No"),levels=c("No","Yes"),
                                          labels=c("No","Yes"))
 # confusion matrix                                         
  (churn.test.lr.confmat <- table(actual=churn.test.samp$churn,predicted=churn.test.samp$lr.pred.churn))
 # predictive accuracy 
  (churn.test.lr.predacc <- (churn.test.lr.confmat[1,1]+churn.test.lr.confmat[2,2])/
                            sum(churn.test.lr.confmat))
# Cohen's kappa statistic                            
  (churn.test.lr.cohenkappa <- cohen.kappa(churn.test.lr.confmat)$kappa)
# One possible interpretation of Cohen's kappa is that there is very weak agreement for
# k < 0.2, weak agreement for 0.2 < k < 0.4, moderate agreement for 0.4 < k < 0.6, 
# strong agreement for 0.6 < k < 0.8, and very strong agreement for k > 0.8
  
# area under ROC curve  
  churn.test.lr.pred <- prediction(churn.test.samp$lr.pred.prob,churn.test.samp$churn)
  churn.test.lr.auc <- as.numeric(performance(churn.test.lr.pred,"auc")@y.values)

# ROC for logistic regression

  churn.train.lr.roc <- performance(churn.train.lr.pred,"tpr","fpr")
  churn.test.lr.roc <- performance(churn.test.lr.pred,"tpr","fpr")
  plot.roc(train.roc=churn.train.lr.roc,train.auc=churn.train.lr.auc, 
           test.roc=churn.test.lr.roc,test.auc=churn.test.lr.auc)     

  churn.train.samp <- churn.train.samp[,1:15]
  churn.test.samp <- churn.test.samp[,1:15]

####################################################################
## !! support vector machine classification !!                    ##
####################################################################

# determine tuning parameters prior to fitting model to training sample
# tuning support vector machine parameters is unique to SVMs

  set.seed(476) # for reproducibility
  churn.train.svm.tune <- tune(svm,churn.model.new,data=churn.train.samp,
                               ranges=list(gamma=2^(-8:8),cost=2^(0:8)),
                               tunecontrol=tune.control(sampling="fix"))

# fit support vector machine to training sample using tuning parameters

  churn.train.svm.fit <- svm(churn.model.new,data=churn.train.samp, 
                             cost=churn.train.svm.tune$best.parameters$cost, 
                             gamma=churn.train.svm.tune$best.parameters$gamma, 
                             probability=TRUE)
  churn.train.svm.predict <- predict(churn.train.svm.fit,churn.train.samp,probability=TRUE)
  churn.train.samp$svm.pred.prob <- attr(churn.train.svm.predict,"probabilities")[,2]
  churn.train.svm.prediction <- prediction(churn.train.samp$svm.pred.prob,churn.train.samp$churn)
  churn.train.svm.auc <- as.numeric(performance(churn.train.svm.prediction,"auc")@y.values)

# use model fit to training sample to evaluate on test sample

  churn.test.svm.predict <- predict(churn.train.svm.fit,churn.test.samp,probability=TRUE)
  churn.test.samp$svm.pred.prob <- attr(churn.test.svm.predict,"probabilities")[,2]
  densityplot(~svm.pred.prob|churn,data=churn.test.samp, 
              layout=c(1,2),aspect=1,col="darkblue", 
              plot.points="rug",xlim=c(0,1),
              strip=function(...) strip.default(...,style=1),
              xlab="Predicted Probability of Churning")
  churn.test.samp$svm.pred.churn <- factor(ifelse((churn.test.samp$svm.pred.prob>0.2718),
                                           "Yes","No"),levels=c("No","Yes"),
                                           labels=c("No","Yes"))
  churn.test.svm.confmat <- table(churn.test.samp$churn,churn.test.samp$svm.pred.churn)
  churn.test.svm.predacc <- (churn.test.svm.confmat[1,1]+churn.test.svm.confmat[2,2])/
                             sum(churn.test.svm.confmat)
  churn.test.svm.cohenkappa <- cohen.kappa(churn.test.svm.confmat)
  churn.test.svm.prediction <- prediction(churn.test.samp$svm.pred.prob,churn.test.samp$churn)
  churn.test.svm.auc <- as.numeric(performance(churn.test.svm.prediction,"auc")@y.values)

# ROC for support vector machines classification

  churn.train.svm.roc <- performance(churn.train.svm.prediction,"tpr","fpr")
  churn.test.svm.roc <- performance(churn.test.svm.prediction,"tpr","fpr")
  plot.roc(train.roc=churn.train.svm.roc,train.auc=churn.train.svm.auc, 
           test.roc=churn.test.svm.roc,test.auc=churn.test.svm.auc)   

  churn.train.samp <- churn.train.samp[,1:15]
  churn.test.samp <- churn.test.samp[,1:15]

####################################################################
## !! artificial neural network classification !!                 ##
####################################################################

# fit neural network model to training sample

  set.seed(476) # for reproducibility
  churn.train.nnet.fit <- nnet(churn.model.new,data=churn.train.samp,size=3,decay=0.5,
                               probability=TRUE,trace=FALSE) 
  churn.train.samp$nnet.pred.prob <- as.numeric(predict(churn.train.nnet.fit,
                                                        newdata=churn.train.samp))
  churn.train.nnet.prediction <- prediction(churn.train.samp$nnet.pred.prob,churn.train.samp$churn)
  churn.train.nnet.auc <- as.numeric(performance(churn.train.nnet.prediction,"auc")@y.values)

# use model fit to training sample to evaluate on test sample

  churn.test.samp$nnet.pred.prob <- as.numeric(predict(churn.train.nnet.fit,
                                                       newdata=churn.test.samp))
  densityplot(~nnet.pred.prob|churn,data=churn.test.samp, 
              layout=c(1,2),aspect=1,col="darkblue", 
              plot.points="rug",xlim=c(0,1),
              strip=function(...) strip.default(...,style=1),
              xlab="Predicted Probability of Churning") 
  churn.test.samp$nnet.pred.churn <- factor(ifelse((churn.test.samp$nnet.pred.prob>0.2718),
                                            "Yes","No"),levels=c("No","Yes"),
                                            labels=c("No","Yes"))
  churn.test.nnet.confmat <- table(churn.test.samp$churn,churn.test.samp$nnet.pred.churn)
  churn.test.nnet.predacc <- (churn.test.nnet.confmat[1,1]+churn.test.nnet.confmat[2,2])/
                              sum(churn.test.nnet.confmat)
  churn.test.nnet.cohenkappa <- cohen.kappa(churn.test.nnet.confmat)
  churn.test.nnet.prediction <- prediction(churn.test.samp$nnet.pred.prob,churn.test.samp$churn)
  churn.test.nnet.auc <- as.numeric(performance(churn.test.nnet.prediction,"auc")@y.values)

# ROC for neural network classification

  churn.train.nnet.roc <- performance(churn.train.nnet.prediction,"tpr","fpr")
  churn.test.nnet.roc <- performance(churn.test.nnet.prediction,"tpr","fpr")
  plot.roc(train.roc=churn.train.nnet.roc,train.auc=churn.train.nnet.auc, 
           test.roc=churn.test.nnet.roc,test.auc=churn.test.nnet.auc)

  churn.train.samp <- churn.train.samp[,1:15]
  churn.test.samp <- churn.test.samp[,1:15]

####################################################################
## !! naive Bayes classification !!                               ##
####################################################################

# fit naive Bayes model to training sample

  set.seed(476) # for reproducibility
  churn.train.nb.fit <- naiveBayes(churn.model.new,data=churn.train.samp)
  churn.train.samp$nb.pred.prob <- as.numeric(predict(churn.train.nb.fit,
                                                      type="raw",
                                                      newdata=churn.train.samp)[,2])
  churn.train.nb.prediction <- prediction(churn.train.samp$nb.pred.prob,churn.train.samp$churn)
  churn.train.nb.auc <- as.numeric(performance(churn.train.nb.prediction,"auc")@y.values)

# use model fit to training sample to evaluate on test sample

  churn.test.samp$nb.pred.prob <- as.numeric(predict(churn.train.nb.fit,
                                                     type="raw",
                                                     newdata=churn.test.samp)[,2])
  densityplot(~nb.pred.prob|churn,data=churn.test.samp, 
              layout=c(1,2),aspect=1,col="darkblue", 
              plot.points="rug",xlim=c(0,1),
              strip=function(...) strip.default(...,style=1),
              xlab="Predicted Probability of Churning") 
  churn.test.samp$nb.pred.churn <- factor(ifelse((churn.test.samp$nb.pred.prob>0.2718),
                                                  "Yes","No"),levels=c("No","Yes"),
                                                  labels=c("No","Yes"))
  churn.test.nb.confmat <- table(churn.test.samp$churn,churn.test.samp$nb.pred.churn)
  churn.test.nb.predacc <- (churn.test.nb.confmat[1,1]+churn.test.nb.confmat[2,2])/
                            sum(churn.test.nb.confmat)
  churn.test.nb.cohenkappa <- cohen.kappa(churn.test.nb.confmat)
  churn.test.nb.prediction <- prediction(churn.test.samp$nb.pred.prob,churn.test.samp$churn)
  churn.test.nb.auc <- as.numeric(performance(churn.test.nb.prediction,"auc")@y.values)

# ROC for naive Bayes classification

  churn.train.nb.roc <- performance(churn.train.nb.prediction,"tpr","fpr")
  churn.test.nb.roc <- performance(churn.test.nb.prediction,"tpr","fpr")
  plot.roc(train.roc=churn.train.nb.roc,train.auc=churn.train.nb.auc, 
           test.roc=churn.test.nb.roc,test.auc=churn.test.nb.auc)

  churn.train.samp <- churn.train.samp[,1:15]
  churn.test.samp <- churn.test.samp[,1:15]

####################################################################
## !! random forests classification !!                            ##
####################################################################

# fit random forests model to training sample

  set.seed(476) # for reproducibility
  churn.train.rf.fit <- randomForest(churn.model.new,data=churn.train.samp,mtry=3,
                                     importance=TRUE,ntree=35000)
  churn.train.samp$rf.pred.prob <- as.numeric(predict(churn.train.rf.fit,type="prob")[,2])
  churn.train.rf.prediction <- prediction(churn.train.samp$rf.pred.prob,churn.train.samp$churn)
  churn.train.rf.auc <- as.numeric(performance(churn.train.rf.prediction,"auc")@y.values)

# use model fit to training sample to evaluate on test sample

  churn.test.samp$rf.pred.prob <- as.numeric(predict(churn.train.rf.fit,type="prob",
                                                     newdata=churn.test.samp)[,2])
  densityplot(~rf.pred.prob|churn,data=churn.test.samp, 
              layout=c(1,2),aspect=1,col="darkblue", 
              plot.points="rug",xlim=c(0,1),
              strip=function(...) strip.default(...,style=1),
              xlab="Predicted Probability of Churning") 
  churn.test.samp$rf.pred.churn <- factor(ifelse((churn.test.samp$rf.pred.prob>0.4854),
                                          "Yes","No"),levels=c("No","Yes"),
                                          labels=c("No","Yes"))
  churn.test.rf.confmat <- table(churn.test.samp$churn,churn.test.samp$rf.pred.churn)
  churn.test.rf.predacc <- (churn.test.rf.confmat[1,1]+churn.test.rf.confmat[2,2])/
                            sum(churn.test.rf.confmat)
  churn.test.rf.cohenkappa <- cohen.kappa(churn.test.rf.confmat)
  churn.test.rf.prediction <- prediction(churn.test.samp$rf.pred.prob,churn.test.samp$churn)
  churn.test.rf.auc <- as.numeric(performance(churn.test.rf.prediction,"auc")@y.values)

# ROC for random forest classification

  churn.train.rf.roc <- performance(churn.train.rf.prediction,"tpr","fpr")
  churn.test.rf.roc <- performance(churn.test.rf.prediction,"tpr","fpr")
  plot.roc(train.roc=churn.train.rf.roc,train.auc=churn.train.rf.auc, 
           test.roc=churn.test.rf.roc,test.auc=churn.test.rf.auc)

# variable importance for predictor variables using training sample

  varImpPlot(churn.train.rf.fit,main="",pch=20,cex=1.25,color="blue")

  churn.train.samp <- churn.train.samp[,1:15]
  churn.test.samp <- churn.test.samp[,1:15]

  churn.df <- churn.df[,1:15]

####################################################################
## !! summaries of classification methods !!                      ##
####################################################################

  pred.acc <- rbind(churn.test.lr.predacc,
                    churn.test.svm.predacc,
                    churn.test.nnet.predacc,
                    churn.test.nb.predacc,
                    churn.test.rf.predacc)
  rownames(pred.acc) <- c("Logistic Regression","Support Vector Machines",
                          "Neural Networks","Naive Bayes","Random Forests")
  colnames(pred.acc) <- "Predictive Accuracy"
  print(pred.acc)

  coh.kappa <- rbind(churn.test.lr.cohenkappa$kappa,
                     churn.test.svm.cohenkappa$kappa,
                     churn.test.nnet.cohenkappa$kappa,
                     churn.test.nb.cohenkappa$kappa,
                     churn.test.rf.cohenkappa$kappa)
  rownames(coh.kappa) <- c("Logistic Regression","Support Vector Machines",
                           "Neural Networks","Naive Bayes","Random Forests")
  colnames(coh.kappa) <- "Cohen's Kappa"
  print(coh.kappa)

  auc.roc.curve <- rbind(churn.test.lr.auc,
                         churn.test.svm.auc,
                         churn.test.nnet.auc,
                         churn.test.nb.auc,
                         churn.test.rf.auc)
  rownames(auc.roc.curve) <- c("Logistic Regression","Support Vector Machines",
                               "Neural Networks","Naive Bayes","Random Forests")
  colnames(auc.roc.curve) <- "AUC (Test)"
  print(auc.roc.curve)

  print(cbind(pred.acc,coh.kappa,auc.roc.curve))

####################################################################
## !! prediction from new customer data !!                        ##
####################################################################

## artifical neural networks

  churn.new.df <- data.frame(senior.citizen="No",
                             dependents="No",
                             tenure=1,
                             contract="Two year",
                             paperless.billing="Yes",
                             payment.method="Bank transfer (automatic)",
                             monthly.charge=79.99,
                             total.charges=79.99,
                             multiple.lines="No",
                             internet.service="Fiber optic",
                             online.security="Yes",
                             tech.support="No",
                             streaming.tv="Yes",
                             streaming.movies="Yes")

  (churn.new.df$nnet.pred.prob1 <- as.numeric(predict(churn.train.nnet.fit,
                                                      newdata=churn.new.df)))

  churn.new.df <- data.frame(senior.citizen="No",
                             dependents="Yes",
                             tenure=1,
                             contract="Two year",
                             paperless.billing="Yes",
                             payment.method="Bank transfer (automatic)",
                             monthly.charge=129.99,
                             total.charges=129.99,
                             multiple.lines="Yes",
                             internet.service="Fiber optic",
                             online.security="No",
                             tech.support="Yes",
                             streaming.tv="No",
                             streaming.movies="No")

  (churn.new.df$nnet.pred.prob2 <- as.numeric(predict(churn.train.nnet.fit,
                                                      newdata=churn.new.df)))
