
setwd("/Users/gabriellefrieder/Downloads/BUAD466 IS w: Dee")
getwd()

# install.packages("dplyr") # YOU NEED ONLY INSTALL THIS LIBRARY ONCE!
  library(dplyr)

# read file and review data frame structure

  hesscust.df <- read.csv(file="hesss.csv",stringsAsFactors=TRUE)
  str(hesscust.df)

# compute profit margin, discount factor, and probability of being "alive"

  hesscust.df$margin <- ((hesscust.df$revenue/36)*.52)-1
  hesscust.df$discount <- ((1+.10)^(1/12))-1
  hesscust.df$prAlive <- ((1095-hesscust.df$recencyDays)/1095)^hesscust.df$numOrders

# compute CLV using standard form with either [a] probability of being "alive" or [b] predicted
# probabilities of purchase from logistic regression

  hesscust.df$clv_prAlive <- (hesscust.df$margin)*
                            ((1+hesscust.df$discount)/
                            (1+hesscust.df$discount-hesscust.df$prAlive))
							
  hesscust.df$clv_predict <- (hesscust.df$margin)*
                           ((1+hesscust.df$discount)/
                            (1+hesscust.df$discount-hesscust.df$predict))

# create bar plots for each CLV calculation

  hesscust.df$deciles_prAlive <- ntile(x=hesscust.df$clv_prAlive,n=10)
  clvXdecile_prAlive <- aggregate(formula=clv_prAlive~deciles_prAlive,
                                  data=hesscust.df,FUN=mean)
  bp_prAlive <- barplot(height=clvXdecile_prAlive$clv_prAlive,
                  names.arg=clvXdecile_prAlive$deciles_prAlive,
                  col="red",yaxp=c(-8,72,40),
                  xlab="Decile",ylab="CLV Using Pr(Alive)",
                  main="CLV Decile Chart Using Pr(Alive)")
  text(x=bp_prAlive,y=0,cex=.75,pos=3,offset=-1,
       labels=round(x=clvXdecile_prAlive$clv_prAlive,digits=2))
	   
  hesscust.df$deciles_predict <- ntile(x=hesscust.df$clv_predict,n=10)
  clvXdecile_predict <- aggregate(formula=clv_predict~deciles_predict,
                                  data=hesscust.df,FUN=mean)
  bp_predict <- barplot(height=clvXdecile_predict$clv_predict,
                  names.arg=clvXdecile_predict$deciles_predict,
                  col="red",yaxp=c(-8,72,40),
                  xlab="Decile",ylab="CLV Using Logit Prediction",
                  main="CLV Decile Chart Using Logit Prediction")
  text(x=bp_predict,y=0,cex=.75,pos=3,offset=.25,
       labels=round(x=clvXdecile_predict$clv_predict,digits=2))
  
# compute correlation between CLV calculation

  cor(x=clvXdecile_prAlive$clv_prAlive,y=clvXdecile_predict$clv_predict)