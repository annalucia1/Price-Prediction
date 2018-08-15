#Step2: Data Exploration

setwd("~/Projects/Zillow")
train <- read.csv("train data with filtered features.csv",stringsAsFactors = F)
library(corrplot)
library(tabplot)
library(GGally)
library(lattice)

#there are 29 predicting variables after excluding features with more than 20% of missing values 
#I divided them into 5 groups based on the information they provide.

##Group 1:describes the furnishment of the house. 
##There are 11 variables:9 numeric, which are 'bedroomcnt', 'bathroomcnt', 'calculatedbathnbr', 'fullbathcnt', 'roomcnt', 'calculatedfinishedsquarefeet', 'finishedsquarefeet12', 'lotsizesquarefeet', 'yearbuilt'; and 2 logical, which are 'hashottuborspa', 'fireplaceflag'.
#check the intra-group correlations first.
cor1_1 <- cor(train[,c('bedroomcnt','bathroomcnt','calculatedbathnbr','fullbathcnt','roomcnt','calculatedfinishedsquarefeet','finishedsquarefeet12','lotsizesquarefeet','yearbuilt')],use='pairwise.complete.obs')
corrplot(cor1_1, method = "number", tl.cex = 1, type = 'lower',tl.col="black", tl.srt=45,number.font = 1)

#check the correlation of the remianing 6 numeric variables with the response
cor1_2 <- cor(train[,c('bedroomcnt','bathroomcnt','roomcnt','calculatedfinishedsquarefeet','lotsizesquarefeet','yearbuilt','logerror')],use='pairwise.complete.obs')
corrplot(cor1_2, method = "number", tl.cex = 1, type = 'lower',tl.col="black", tl.srt=45,number.font = 1)

#check the relationship between the response with all 6 numeric and 2 logical variables. 
#The relationship can be beyond linear relationship.
tableplot(train, select = c('logerror','bedroomcnt','bathroomcnt','roomcnt','calculatedfinishedsquarefeet','lotsizesquarefeet','yearbuilt','hashottuborspa','fireplaceflag'))

#check the response variable on the 2 logical variables
bwplot(logerror ~ hashottuborspa, data = subset(train, abs(logerror) < 0.09),xlab="hashottuborspa")
bwplot(logerror ~ fireplaceflag, data = subset(train, abs(logerror) < 0.09),xlab="fireplaceflag")


##Group 2: 3 variables about the property land
##"propertycountylandusecode", "propertylandusetypeid",and "propertyzoningdesc"
train$propertylandusetypeid <- as.character(train$propertylandusetypeid)
bwplot(logerror ~ propertylandusetypeid, data = subset(train, abs(logerror) < 0.09),xlab="propertylandusetypeid")
bwplot(logerror ~ propertycountylandusecode, data = subset(train, abs(logerror) < 0.09),xlab="propertycountylandusecode")


##Group 3: the tax assessment information
##6 variables: "assessmentyear", "taxvaluedollarcnt","landtaxvaluedollarcnt", "taxamount", "structuretaxvaluedollarcnt", and "taxdelinquencyflag"
sub_train = train[, c("logerror", "taxvaluedollarcnt","landtaxvaluedollarcnt", "taxamount", "structuretaxvaluedollarcnt")]
sub_train.dropna = sub_train[complete.cases(sub_train),]
ggpairs(sub_train.dropna)
#"taxvaluedollarcnt","landtaxvaluedollarcnt", "taxamount" and "structuretaxvaluedollarcnt" are highly correlated. 

#check the relationship between logerror and the categorial variable "taxdelinquencyflag".
table(train$taxdelinquencyflag)
bwplot(logerror ~taxdelinquencyflag , data = subset(train, abs(logerror) < 0.09),xlab="taxdelinquencyflag")


##Group 4: the location of the parcel
##8 variables: "latitude", "longitude","fips","rawcensustractandblock","censustractandblock", "regionidcity", "regionidcounty", and "regionidzip". 

plot(train$fips, train$regionidcounty)
train$fips <- as.character(train$fips)
bwplot(logerror ~fips, data = subset(train, abs(logerror) < 0.09),xlab="fips")
#'fips' and 'regionidcounty' represent the same information, the county


cor(train[,c("rawcensustractandblock","censustractandblock")],use='pairwise.complete.obs')
summary(train[,c("rawcensustractandblock","censustractandblock")])
plot(train$rawcensustractandblock, train$logerror)
#"rawcensustractandblock","censustractandblock" are very similar


train$regionidzip <- as.character(train$regionidzip)
with(subset(train,regionidzip!='399675'), plot(regionidzip, logerror))
#There is an outlier in the 'regionidzip', which is '399675'. Is it mis-input?


train$regionidcity <- as.character(train$regionidcity)
bwplot(logerror ~regionidcity, data = subset(train, abs(logerror) < 0.09),xlab="regionidcity")


##Group 5: 1 variable "transactiondate"
bwplot(logerror ~transactiondate, data = subset(train, abs(logerror) < 0.09),xlab="transactiondate")
train$txnmonth <- sapply(strsplit(train$transactiondate, '-'), '[[', 2)
bwplot(logerror ~txnmonth, data = subset(train, abs(logerror) < 0.09),xlab="txnmonth")