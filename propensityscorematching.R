setwd("/Users/gabriellefrieder/Downloads/BUAD466 IS w: Dee")
getwd()

## Install Packages
install.packages("MatchIt")
install.packages("dplyr")
install.packages("AER")
## Load Packages and Set Seed
library(MatchIt)
library(dplyr)
library(AER)
set.seed(1)

##Read in the Marketing Mix Data
psmatch <- read.csv(file="retail_psmatch.csv")
str(psmatch)

##Look at Pre-matched Data
psmatch %>%
group_by(email)%>%
summarise(n_customers=n(),
mean_purchase_amt=mean(purchase_amt),
.groups='keep')

##Difference of Covariates
psmatch_keep <- c('email','revenue','number_of_orders','number_of_orders2',
'recency_days')
psmatch_cov <- subset(psmatch,select=psmatch_keep)
psmatch_cov%>%
group_by(email)%>%
summarise_all(mean)

## Determine Optimal Caliper Size
ps_match <- glm(email~revenue+number_of_orders+number_of_orders2+recency_days,
family=binomial(),data=psmatch)
ps_match_df <- data.frame(pr_score=predict(ps_match,type="response"),
email=ps_match$model$email)
0.2*sd(ps_match_df$pr_score)

## Remove Rows with Missing Values
psmatch_nomiss <- psmatch%>%
select(customer,purchase_amt,one_of(psmatch_keep))%>%
na.omit()

## Executing a Matching Algorithm
psmatch_nomiss <- psmatch%>%
select(customer,purchase_amt,one_of(psmatch_keep))%>%
na.omit()
mod_match <- matchit(email~revenue+number_of_orders+number_of_orders2+recency_days,
method="nearest",ratio=1,caliper=0.023,replace=FALSE,data=psmatch_nomiss)
matched_data <- match.data(mod_match)
dim(matched_data)

## Looking at Post-matching Means
matched_data_cov <- subset(matched_data,select=psmatch_keep)
matched_data_cov%>%
group_by(email)%>%
summarise_all(mean)

## Tobit Model with PS Match Data
tobit_treat <- tobit(purchase_amt~email+revenue+number_of_orders+number_of_orders2+
recency_days,left=0,data=matched_data)
summary(tobit_treat)

## Predict Expected Value
mu <- fitted(tobit_treat)
sigma <- tobit_treat$scale
p0 <- pnorm(mu/sigma)
lambda <- function(x) dnorm(x)/pnorm(x)
ey0 <- mu+sigma *lambda(mu/sigma)
matched_data$pred_purchase_amt <- p0*ey0

##Compare the Expected Value for the Email and No Email groups (matched)
matched_data%>%
group_by(email)%>%
summarise(mean_pred_purchase_amt=mean(pred_purchase_amt),.groups='keep')

## Run the Tobit Model with the Unmatched Data
tobit_treat_nomatch <-tobit(purchase_amt~email+revenue+number_of_orders+number_of_orders2+
recency_days,left=0,data=psmatch)
summary(tobit_treat_nomatch)
## Predict the Expected Value for the Unmatched Data
mu <- fitted(tobit_treat_nomatch)
sigma <- tobit_treat_nomatch$scale
p0 <- pnorm(mu/sigma)
lambda <- function(x) dnorm(x)/pnorm(x)
ey0 <- mu+sigma *lambda(mu/sigma)
psmatch$pred_purchase_amt <- p0*ey0
## Compare the Expected Value for the Email and No Email groups (no match)
psmatch%>%
group_by(email)%>%
summarise(mean_pred_purchase_amt=mean(pred_purchase_amt),.groups='keep')

## Export the Data Set
write.csv(matched_data,file.choose(new=TRUE),row.names=FALSE) 
## Name the file psmatch_predicted.csv




