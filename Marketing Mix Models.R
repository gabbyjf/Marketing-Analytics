setwd("/Users/gabriellefrieder/Downloads/BUAD466 IS w: Dee")
getwd()

## Install Packages
install.packages("tseries")
## Load Packages and Set Seed
library(tseries)
set.seed(1)

## Read in the Marketing Mix Data
mmix <- read.csv(file="mmix_data.csv",stringsAsFactors=TRUE)

str(mmix)

## Look at Means of Variables
lapply(sapply(mmix,mean),round,2)

## Create Natural Log, Lag, and Weekday Variables
mmix$ln_quantity <- log(mmix$quantity)
mmix$ln_price <- log(mmix$price)
mmix$ln_digital_ad <- log(mmix$digital_ad)
mmix$ln_digital_search <- log(mmix$digital_search)
mmix$ln_print <- log(mmix$print+1) 	## some time periods have 0 spend
mmix$ln_tv <- log(mmix$tv)

## Lag of Quantity Sold
mmix$lln_quantity <- c(0,mmix$ln_quantity[1:length(mmix$ln_quantity)-1])

## Create Dummy Variable for Time Period (day of the week)
mmix$weekdays <- weekdays(as.Date(mmix$date))

## Check for Unit Root
adf.test(mmix$ln_quantity)

## Check for Multicollinearity
cor_vars <- c("ln_quantity","lln_quantity","ln_price","ln_digital_ad","ln_digital_search",
			  "ln_print","ln_tv")
cor_data <- mmix[cor_vars]
cor_table <- cor(cor_data)
round(cor_table,2)

## Combine ln_digital_ad and ln_digital_search
mmix$ln_digital <- log(mmix$digital_ad+mmix$digital_search)

## Run the Regression
mmix_reg <- lm(ln_quantity~lln_quantity+ln_price+ln_digital+ln_print+ln_tv+factor(weekdays), data=mmix)
summary(mmix_reg)

## Create predicted valies for actual quantity
mmix$pred_quantity <- exp(predict(mmix_reg))

## Export the Data Set
mmix_predicted.csv <- write.csv(mmix,file.choose(new=TRUE),row.names=FALSE)
