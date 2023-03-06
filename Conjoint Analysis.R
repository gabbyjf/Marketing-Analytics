setwd("/Users/gabriellefrieder/Downloads/BUAD466 IS w: Dee")
getwd()

## Install Packages
install.packages("conjoint")
## Load Packages and Set Seed
library(conjoint)
set.seed(1)

## Set up attributes and levels as a list
attrib.level <- list(brand=c("CR","Apple","Samsung","Fitbit"), 
ship=c("$0","$10","$20"),
restock=c("0%","5%","10%","15%"), 
retdays=c("7 days","14 days","21 days"), 
price=c("$150","$200","$250","$300"))

## Create the fractional factorial design
experiment <- expand.grid(attrib.level)
design <- caFactorialDesign(data=experiment,type="fractional",cards=30,seed=1)

## Check for correlation in fractional factorial design
print(cor(caEncodedDesign(design)))

## Export design for survey
conjoint_profiles.csv <- write.csv(design,file.choose(new=TRUE),row.names=FALSE)

## Read in the survey preference results
pref <- read.csv(file="conjoint_preferences.csv",stringsAsFactors=TRUE)
str(pref)

## Set up attributes and levels as a vector and Estimate the part-worths for each respondent
attrib.vector <- data.frame(unlist(attrib.level,use.names=FALSE))
colnames(attrib.vector) <- c("levels")
part.worths <- NULL
for(i in 1:ncol(pref)){
temp <- caPartUtilities(pref[,i],design,attrib.vector)

## Pick the baseline case
## Adjust coding as needed based on number of attributes and levels

## Base Case: Brand CR, Shipping $0, Restock 0%, Retdays 7 days, Price $150
Base_Brand <- temp[,"CR"];Base_Ship <- temp[,"$0"];Base_Restock <- temp[,"0%"]
Base_Retdays <- temp[,"7 days"];Base_Price <- temp[,"$150"]

## Adjust Intercept
temp[,"intercept"] <- temp[,"intercept"] - Base_Brand - Base_Ship - Base_Restock - 
Base_Retdays - Base_Price

## Adjust Coefficients

##Brand
L1 <- length(attrib.level$brand) + 1 ## Add 1 for the intercept
for(j in 2:L1){temp[,j] <- temp[,j] - Base_Brand}

## Shipping
L2 <- length(attrib.level$ship) + L1
for(k in (L1+1):L2){temp[,k] <- temp[,k] - Base_Ship}

## Restock
L3 <- length(attrib.level$restock) + L2 
for(l in (L2+1):L3){temp[,l] <- temp[,l] - Base_Restock}

## Retdays	
L4 <- length(attrib.level$retdays) + L3 
for(m in (L3+1):L4){temp[,m] <- temp[,m] - Base_Retdays}

## Price
L5 <- length(attrib.level$price) + L4 
for(n in (L4+1):L5){temp[,n] <- temp[,n] - Base_Price}
part.worths <- rbind(part.worths,temp)
}
rownames(part.worths) <- colnames(pref)

## Export part-worths from analysis
conjoint_partworths.csv <- write.csv(part.worths,file.choose(new=TRUE), row.names=FALSE)
