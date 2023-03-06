#######################################
## Cluster Analysis for Segmentation ##
#######################################

  setwd("/Users/gabriellefrieder/Downloads/BUAD466 IS w: Dee/Cluster Analysis")
  getwd()

# install and then call R packages for use in this study

  install.packages("cluster")
  install.packages("mclust")
  install.packages("poLCA")

  library(lattice)   # multivariate data visualization
  library(vcd)       # data visualization for categorical variables
  library(cluster)   # cluster analysis methods
  library(mclust)    # comparison of model solutions
  library(poLCA)     # polytomous latent class analysis

# read bank segmentation data from .csv file and create data frame

  bank.df <- read.csv("bank_seg.csv",stringsAsFactors=TRUE)
  str(bank.df)
  summary(bank.df)
  
# [1] geographic variables
# [2] behavioral variables
# [3] demographic variables
# [4] psychographic variables

# DO NOT EVER USE SALES RESPONSE VARIABLES because they are historical and we
# are trying to identify new segments of customers  

  bank.df$job <- factor(bank.df$job,levels=c("Not In Workforce","Blue Collar",
  											 "White Collar"))
  bank.df$marital <- factor(bank.df$marital,levels=c("Single","Married",
                                                     "Divorced"))
  bank.df$age.cat <- factor(bank.df$age.cat,levels=c("Young","Middle Aged",
                                                     "Old"))
                                                     
  str(bank.df)
  summary(bank.df)

# +6500 pesos = deposits + interest + payments on loans --> assets (to the bank)
# -9800 pesos = money dispensed on loans --> liabilities (to the bank)
# -----------
# -3300 pesos (bank liability that the customer owes to the bank)

## EXPLORATORY ANALYSIS OF PREDICTOR VARIABLES

# As a preliminary step BEFORE clustering, you should always explore data,
# look for MISSING data, and look for strange values of variables, etc.;
# use histograms for numeric data; mosaic plots for categorical data

# examine relationship between age and response to promotion
  
  histogram(~age|response,data=bank.df,type="density",
            layout=c(1,2),xlab="Age of Bank Client")

# examine relationship between education and response to promotion
  
  mosaic(~response+education,data=bank.df,
         labeling_args=list(set_varnames=c(response="Response to Offer",
                                           education="Education Level")),
         highlighting="education",
         highlighting_fill=c("red","yellow","blue",
                             "red","yellow","blue"),
         rot_labels=c(left=90,top=0),
         pos_labels=c("center","center"),
         offset_labels=c(0.0,0.6))

# examine relationship between job type and response to promotion
  
  mosaic(~response+job,data=bank.df,
         labeling_args=list(set_varnames=c(response="Response to Offer",
                                           job="Job Type")),
         highlighting="job",
         highlighting_fill=c("red","yellow","blue",
                             "red","yellow","blue"),
         rot_labels=c(left=0,top=0),
         pos_labels=c("center","center"),
         offset_labels=c(0.0,0.6))

# examine relationship between marital status and response to promotion
# NOTE: "Divorced" means divorced or widowed
  
  mosaic(~response+marital,data=bank.df,
         labeling_args=list(set_varnames=c(response="Response to Offer",
                                           marital="Marital Status")),
         highlighting="marital",
         highlighting_fill=c("red","yellow","blue",
                             "red","yellow","blue"),
         rot_labels=c(left=0,top=0),
         pos_labels=c("center","center"),
         offset_labels=c(0.0,0.6))

# What cluster analysis does is take the four kinds of customer data and
# looks for ways to divide up customers into segments by that data, and
# then classify each customer in the data as belonging to one and only one
# of those segments of customers --> the raw output from cluster analysis
# is simply a number indicating segment membership (groups)

## user-defined function to summarize segment data via mean and median

## !! SEGMENT INSPECTION FUNCTION !!
## RUN THE CODE BELOW BEFORE ATTEMPTING CLUSTER ANALYSES

  segment.summary <- function(data,groups) {
                     mean.table <-   aggregate(data,by=list(groups),
                                       FUN=function(x) mean(as.numeric(x)))
                     median.table <- aggregate(data,by=list(groups),
                                       FUN=function(x) median(as.numeric(x)))
                     cat("\nMean Demographic Level by Segment:\n")
                     print(cbind(table(groups),mean.table[,-1]))
                     cat("\nMedian Demographic Level by Segment:\n")
                     print(cbind(table(groups),median.table[,-1]))
                     }
##
## CLUSTER ANALYSIS TECHNIQUES
##

####################################
## 1. HIERARCHICAL CLUSTERING     ##
####################################

# Steps to Hierarchical Clustering:
# i.   Prepare the data and determine the appropriate columns;
# ii.  Compute a distance matrix;
# iii. Apply the hierarchical clustering technique;
# iv.  Parse the clustering object by the number of desired segments;
# v.   Generate the segment inspection function and seek observable and 
#	   actionable business outcomes. If necessary, export the output to Excel.

  str(bank.df)

#  [1]/[2] generate dissimilarity (distance) matrix

  bank.dissmat <- daisy(x=bank.df[,2:5]) # compute distances to every person

  as.matrix(bank.dissmat)[1:5,1:5]       # looks at sample of dissimilarity matrix

# [3] conduct hierarchical clustering analysis using complete linkage method

  bank.hier.comp <- hclust(d=bank.dissmat,method="complete")
  plot(x=bank.hier.comp,sub="",xlab="Distance",labels=FALSE)
  
# examine similarity and dissimilarity between observations in data

  plot(x=cut(as.dendrogram(bank.hier.comp),h=0.2)$lower[[2]])
  bank.df[c(1256,1407),2:5]
  bank.df[c(572,2557),2:5]
  bank.df[c(127,1407),2:5]

# compute cophenetic correlation coefficient for each technique

  cor(x=cophenetic(bank.hier.comp),y=bank.dissmat) # goodness-of-fit test

# plot dendrogram for four-cluster solution

  plot(x=bank.hier.comp,sub="",xlab="Distance",labels=FALSE)
  rect.hclust(tree=bank.hier.comp,k=4,border="red")

# [4] create segment membership for four-cluster solution

  bank.hier.segments <- cutree(tree=bank.hier.comp,k=4)
  table(bank.hier.segments)

# [5] summarize segment membership data using segment inspection function
# first argument is demographic data; second argument is segment membership

  segment.summary(data=bank.df[,2:5],groups=bank.hier.segments)
  
# Median Demographic Level by Segment:
#  groups Freq age job marital education
# 1      1  263  55   1       2         1 --> 55, Not in workforce, Married, Primary
# 2      2 1412  38   3       2         3 --> 38, White Collar, Married, Tertiary
# 3      3 1605  38   2       2         2 --> 38, Blue Collar, Married, Secondary
# 4      4  256  45   2       3         2 --> 45, Blue Collar, Divorced, Secondary

########################################################
## 2. K-MEANS CLUSTERING (MEANS-BASED CLUSTERING)     ##              
########################################################

# conduct k-means clustering analysis

# data types: NUMERIC ONLY

  str(bank.df)

  set.seed(476)
  bank.kmeans <- kmeans(x=bank.df[,c(2,7:9)],centers=4)

# summarize segment membership data using segment inspection function

  segment.summary(data=bank.df[,c(2,7:9)],groups=bank.kmeans$cluster)

# K-means clustering with 4 clusters of sizes 629, 998, 883, 1026
# Cluster means:
#        age   job.no marital.no education.no
# 1 57.42925 1.953895   2.125596     1.942766
# 2 29.71844 1.718437   1.517034     2.246493
# 3 46.19592 1.628539   2.056625     2.050963
# 4 37.10331 1.570175   1.864522     2.242690

# Mean Demographic Level by Segment:
#   groups Freq      age   job.no marital.no education.no
# 1      1  629 57.42925 1.953895   2.125596     1.942766
# 2      2  998 29.71844 1.718437   1.517034     2.246493
# 3      3  883 46.19592 1.628539   2.056625     2.050963
# 4      4 1026 37.10331 1.570175   1.864522     2.242690

# Median Demographic Level by Segment:
#   groups Freq age job.no marital.no education.no
# 1      1  629  56      2          2            2 --> 56, Blue collar, Married, Secondary
# 2      2  998  30      2          1            2 --> 30, Blue collar, Single, Secondary
# 3      3  883  46      2          2            2 --> 46, Blue collar, Married, Secondary
# 4      4 1026  37      2          2            2 --> 37, Blue collar, Married, Secondary

# CONCLUSION: Not as interesting a solution as hierarchical clustering

###################################################
## 3. PARTITIONING-AROUND-MEDOIDS CLUSTERING     ##
###################################################

# conduct partitioning-around-medoids clustering analysis

  set.seed(476)
  bank.pam <- pam(x=bank.dissmat,k=4)

# summarize segment membership data using segment inspection function

  segment.summary(data=bank.df[,2:5],group=bank.pam$clustering)

# Median Demographic Level by Segment:
#   groups Freq age job marital education
# 1      1  548  49   2       2         1 --> 49, Blue collar, Married, Primary
# 2      2 1078  37   3       2         3 --> 37, White collar, Married, Tertiary****
# 3      3 1187  44   2       2         2 --> 44, Blue collar, Married, Secondary
# 4      4  723  32   2       1         2 --> 32, Blue collar, Single, Secondary

#####################################
## 4. MODEL-BASED CLUSTERING       ##
#####################################

## WARNING: MODEL-BASED CLUSTERING REQUIRES SPEED AND MEMORY!
##          MODEL-BASED CLUSTERING IS UNSTABLE FOR SMALL DATA SETS!

# DATA TYPE: NUMERIC ONLY

# Steps to Model-Based Clustering:
# i.   Prepare the data and determine the appropriate columns
# ii.  Set the seed and apply the model-based clustering technique
# iii. Generate the segment inspection function and seek observable and
#	   actionable business outcomes

# conduct model-based clustering analysis with G=4 segments
# Note: maximum BIC for the four-cluster model is -38,557.13

  str(bank.df)
  
  set.seed(476)
  bank.mbc.Mc4 <- Mclust(data=bank.df[,c(2,7:9)],G=4)
  summary.Mclust(object=bank.mbc.Mc4,parameters=TRUE)
  bank.mbc.Mc4$bic
  
# Use set.seed(3) to find the true maximum BIC  

  set.seed(3)
  bank.mbc.Mc4 <- Mclust(data=bank.df[,c(2,7:9)],G=4)
  summary.Mclust(object=bank.mbc.Mc4,parameters=TRUE)
  bank.mbc.Mc4$bic  
  
# With the Akaike Information Criterion [AIC], the point is always to
# minimize the AIC to find the best fitting model
# With the Schwarz Bayesian Information Criterion [BIC], the point is
# always to maximize the BIC to find the best fitting model

# AIC() --> minimize value
# BIC() --> take opposite of real BIC (always negative) so that you still are 
#           minimizing when in reality you are maximizing BIC 

# BIC() --> minimize the value
# bic from within the command itself --> maximize value

# conduct model-based clustering analysis with G=3 segments
# Note: maximum BIC for the three-cluster model is -40,626.45

  bank.mbc.Mc3 <- Mclust(data=bank.df[,c(2,7:9)],G=3)
  summary.Mclust(object=bank.mbc.Mc3,parameters=TRUE)
  bank.mbc.Mc3$bic

# conduct model-based clustering analysis with G=5 segments
# Note: maximum BIC for the five-cluster model is -39,868.64

  bank.mbc.Mc5 <- Mclust(data=bank.df[,c(2,7:9)],G=5)
  summary.Mclust(object=bank.mbc.Mc5,parameters=TRUE)
  bank.mbc.Mc5$bic

# compute Bayesian information criterion for each solution

  bank.mbc.Mc$bic
  bank.mbc.Mc4$bic
  bank.mbc.Mc3$bic
  bank.mbc.Mc5$bic

# summarize segment membership data using segment inspection function

  segment.summary(data=bank.df[,c(2,7:9)],groups=bank.mbc.Mc4$class)

# Median Demographic Level by Segment:
#  groups Freq age job.no marital.no education.no
# 1      1  780  47      2          2            2 --> 47, Blue collar, Married, Secondary
# 2      2 1367  39      1          2            3 --> 39, Not in workforce, Married, Tertiary
# 3      3  980  34      2          2            2 --> 34, Blue collar, Married, Secondary
# 4      4  409  44      2          1            2 --> 44, Blue collar, Single, Secondary

#############################################
## 5. LATENT CLASS ANALYSIS                ##
#############################################

# DATA TYPE: CATEGORICAL/FACTOR ONLY

  str(bank.df)
  
# Steps to Polytomous Latent Class Analysis:
# i.   Prepare the data and determine the appropriate columns
# ii.  Set the seed and apply the latent class analysis technique, after
#	   creating a formula for the latent class analysis model
# iii. Generate the segment inspection function and seek observable and
#      actionable business outcomes 
# generate function that specifies predictor variables and informs
# latent class analysis technique to only generate intercepts

  bank.lca.function <- with(bank.df[,3:6],
                            cbind(job,marital,education,age.cat)~1)

# conduct latent class analysis clustering technique for four classes

  set.seed(476)
  bank.lca4 <- poLCA(formula=bank.lca.function,data=bank.df[,3:6],
                     nclass=4,maxiter=1e6)
                     
# set.seed(93) is the one that produces the lowest BIC

  set.seed(93)
  bank.lca4 <- poLCA(formula=bank.lca.function,data=bank.df[,3:6],
                     nclass=4,maxiter=1e6)
                     
# conduct latent class analysis clustering technique for three classes

  set.seed(476)
  bank.lca3 <- poLCA(formula=bank.lca.function,data=bank.df[,3:6],
                     nclass=3,maxiter=1e6)

# conduct latent class analysis clustering technique for five classes

  set.seed(476)
  bank.lca5 <- poLCA(formula=bank.lca.function,data=bank.df[,3:6],
                     nclass=5,maxiter=1e6)

# compute Bayesian information criterion for each solution

  bank.lca4$bic
  bank.lca3$bic
  bank.lca5$bic

# summarize segment membership data using segment inspection function

  segment.summary(data=bank.df[,3:6],groups=bank.lca4$predclass)

# Median Demographic Level by Segment:
#  groups Freq job marital education age.cat
# 1      1  678   2       1         2       1 --> Blue collar, Single, Secondary, Young
# 2      2  932   2       2         2       2 --> Blue collar, Married, Secondary, Middle-Aged
# 3      3  901   2       2         2       3 --> Blue collar, Married, Seondary, Old
# 4      4 1025   3       2         3       2 --> White collar, Married, Tertiary, Middle-Aged

## MORAL: Since this new product is a CD (certificate of deposit), the most
## lucrative and actionable segment is the white collar, married, middle-aged 
## segment with tertiary education

## SELECTING OPTIMAL NUMBER OF SEGMENTS

## [1] PARTITIONING-AROUND-MEDOIDS CLUSTERING METHOD
## --> yields silhouette coefficient

# plot silhouette coefficient and segment membership from two to 10 clusters

  minclust <- 2
  maxclust <- 10
  data.clust <- bank.df[,2:5]
# evaluate alternative numbers of clusters/segments by using average
#   silhouette width as statistical criterion
# selected algorithm is partitioning-around-medoids
  evaluation.vector <- NULL # initialize placeholder vector 
  par(mfrow=c(3,3))
  for(k in minclust:maxclust) {
      diss.mat <- daisy(data.clust)
      pam.clust <- pam(x=diss.mat,k=k)
      evaluation.vector <- rbind(evaluation.vector,
                                 data.frame(k, 
                                 mean.silhouette.width= 
                                 pam.clust$silinfo$avg.width))
# show plot for this clustering solution
      plot(pam.clust) # add this clustering solution to results file         
      }
  par(mfrow=c(1,1))

# examine cluster solution results:
#  > look for average silhouette width > 0.5
#  > look for last large jump in average silhouette width

  print(evaluation.vector)
  
# according to the silhuette plot a segment size of 10 is optimal -->
# but that is highly suspect because the silhouette width just keeps
# getting larger with every single increase in segment size!

 # provide single summary plot for clustering solutions

  plot(x=evaluation.vector$k,y=evaluation.vector$mean.silhouette.width,
       xlab="Number of Clusters",ylab="Average Silhouette Width",
       main="Optimal Number of Clusters Using PAM",pch=19,col="blue")
  abline(h=0.5,col="red",lwd=1.5,lty="dashed")

# examine four-cluster solution from partitioning-around-medoids in more detail

  diss.mat <- daisy(x=data.clust)
  pam.clust <- pam(x=diss.mat,k=4)
  plot(x=pam.clust,main="Silhouette Plot of Partitioning-around-Medoids")

# MORAL: Don't trust the silhouette coefficient results!  

## [2] MODEL-BASED CLUSTERING METHOD
## --> BIC plot

# conduct model-based clustering analysis with no predefined segments
# with BETTER initialization of EM algorithm

  set.seed(476)
  bank.svd.BIC <- mclustBIC(data=svd(bank.df[,c(2,7:9)])$u)
  plot(bank.svd.BIC)
  summary(bank.svd.BIC)

# bank.mbc.Mc <- Mclust(data=svd(bank.df[,c(2,7:9)])$u,x=bank.svd.BIC)
# summary.Mclust(object=bank.mbc.Mc,parameters=TRUE)
# bank.mbc.Mc$G

# conduct model-based clustering analysis with no predefined segments
# with no initialization of EM algorithm

  set.seed(476)
  bank.BIC <- mclustBIC(data=bank.df[,c(2,7:9)])
  plot(bank.BIC)
  summary(bank.BIC)
###########################################################################################################

# from silhouette plot four clusters appear to be large and well-defined; 
# add cluster membership information and select first four clusters

  bank.df$cluster <- pam.clust$clustering
  segment.summary(data=bank.df[,2:5],groups=bank.df$cluster)

  bank.seg.df <- subset(bank.df,subset=(cluster<=4))
  bank.seg.df$cluster <- factor(bank.seg.df$cluster,labels=c("A","B","C","D"))

# response to term deposit offer among segments

  table(bank.seg.df$cluster,bank.seg.df$response)
  mosaic(~response+cluster,data=bank.seg.df,
         labeling_args=list(set_varnames=c(response="Response to Term Deposit Offer", 
                                           cluster="Segment Membership")),
         highlighting="response",
         highlighting_fill=c("gold","blue"),
         rot_labels=c(left=90,top=0),
         pos_labels=c(left="center",top="center"),
         offset_labels=c(left=0.0,top=0.6))

# compute percentage of "yes" responses to term deposit offer

  response.table <- table(bank.seg.df$cluster,bank.seg.df$response)
  cat("\nPercentage Responses\n")
  for(i in 1:4) 
      cat("\n",toupper(letters[i]),round(100*response.table[i,2]/ 
          sum(response.table[i,]),digits=4))

# note percentage of customers receiving offers for first time belonging to 
# each cluster/segment

  round(100*table(bank.seg.df$cluster)/nrow(bank.seg.df),digits=4)

# summarize segment membership data using segment inspection function

  segment.summary(data=bank.seg.df[,c(2:5,10:13)],groups=bank.seg.df$cluster)


## DISCRIMINANT ANALYSIS

  install.packages("klaR")
  library(klaR)

  bank.df$cluster <- pam.clust$clustering

# assess numeric variables for normality

  histogram(~balance,data=bank.df,xlab="Customer's Average Annual Balance")
  histogram(~log(balance+abs(min(balance))+1),data=bank.df,xlab="Customer's Average Annual Balance")
  
  bank.df$log.balance <- log(bank.df$balance+abs(min(bank.df$balance))+1)

# normalize numeric data

  bank.sc.df <- bank.df
  bank.sc.df[,2] <- scale(bank.df[,2])
  bank.sc.df[,15] <- scale(bank.df[,15])

# create training and test sample regimen

  set.seed(476)
  bank.seg.sample <- sort(sample(dim(bank.df)[1],size=(1/3)*dim(bank.df)[1],
                                 replace=FALSE))
  bank.seg.train <- bank.sc.df[bank.seg.sample,]
  rownames(bank.seg.train) <- NULL
  bank.seg.test <- bank.sc.df[-bank.seg.sample,]
  rownames(bank.seg.test) <- NULL

  bank.train.seg <- subset(bank.seg.train,subset=(cluster<=4))
  bank.train.seg$cluster <- factor(bank.train.seg$cluster,labels=c("A","B","C","D"))
  bank.test.seg <- subset(bank.seg.test,subset=(cluster<=4))
  bank.test.seg$cluster <- factor(bank.test.seg$cluster,labels=c("A","B","C","D"))

## LINEAR DISCRIMINANT ANALYSIS

# perform linear discriminant analysis

  (lda.fit <- lda(bank.train.seg$cluster~.,data=bank.seg.train[,c(2:5,10,12:13,15)]))
  plot(lda.fit,col=as.integer(bank.train.seg$cluster))
  plot(lda.fit,dimen=1,type="b")

# generate LDA predictions and assess validity of model

  lda.pred <- predict(lda.fit,newdata=bank.seg.test[,c(2:5,10,12:13,15)])
  lda.pred$class
  (lda.confmat <- table(actual.cluster=bank.test.seg$cluster,
                        predicted.cluster=lda.pred$class))
  (lda.predacc <- mean(bank.test.seg$cluster==lda.pred$class))

## QUADRATIC DISCRIMINANT ANALYSIS

  set.seed(476)
  bank.seg.sample <- sort(sample(dim(bank.df)[1],size=(2/3)*dim(bank.df)[1],
                                 replace=FALSE))
  bank.seg.train <- bank.sc.df[bank.seg.sample,]
  rownames(bank.seg.train) <- NULL
  bank.seg.test <- bank.sc.df[-bank.seg.sample,]
  rownames(bank.seg.test) <- NULL

  bank.train.seg <- subset(bank.seg.train,subset=(cluster<=4))
  bank.train.seg$cluster <- factor(bank.train.seg$cluster,labels=c("A","B","C","D"))
  bank.test.seg <- subset(bank.seg.test,subset=(cluster<=4))
  bank.test.seg$cluster <- factor(bank.test.seg$cluster,labels=c("A","B","C","D"))

# perform quadratic discriminant analysis

  (qda.fit <- qda(bank.train.seg$cluster~.,data=bank.seg.train[,c(2,7:10,12:13,15)]))

# generate QDA predictions and assess validity of model

  qda.pred <- predict(qda.fit,newdata=bank.seg.test[,c(2,7:10,12:13,15)])
  qda.pred$class
  (qda.confmat <- (table(actual.cluster=bank.test.seg$cluster,
                         predicted.cluster=qda.pred$class)))
  (qda.predacc <- mean(bank.test.seg$cluster==qda.pred$class))
