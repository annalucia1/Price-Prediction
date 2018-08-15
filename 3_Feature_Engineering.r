#Step3: Feature Engineering

setwd("~/Projects/Zillow")
train <- read.csv("train data with filtered features.csv",stringsAsFactors = F)

##Initial feature removing and creating

#remove redundant features
redundant <- c('X','calculatedbathnbr','fullbathcnt','finishedsquarefeet12', 'assessmentyear', 'regionidcounty', 'censustractandblock')
train <- train[,!(colnames(train) %in% redundant)]

#create transaction date features
train$trans_year <- sapply(strsplit(train$transactiondate, '-'), '[', 1) #trans_year only has one value, '2016'. 
train$trans_month <- sapply(strsplit(train$transactiondate, '-'), '[', 2)
train$trans_day <- sapply(strsplit(train$transactiondate, '-'), '[', 3)
train$trans_date <- as.Date(train$transactiondate)
train$trans_weekday <- weekdays(train$trans_date)

#create abs.logerror
train$abs.logerror <- abs(train$logerror)

##Correct variable data types
variable_num <- c("logerror",
                  "taxvaluedollarcnt","landtaxvaluedollarcnt","structuretaxvaluedollarcnt","taxamount",
                  "calculatedfinishedsquarefeet","lotsizesquarefeet",
                  "latitude","longitude")
variable_int <- c("bathroomcnt","bedroomcnt","roomcnt")
variable_logi <- c("hashottuborspa","fireplaceflag","taxdelinquencyflag")
variable_char <- c("parcelid",
                   "fips","regionidcity","regionidzip","rawcensustractandblock",
                   "propertycountylandusecode","propertylandusetypeid","propertyzoningdesc")
variable_date <- c("yearbuilt","trans_date","trans_year","trans_month","trans_day","trans_weekday")

#convert logical to 0, 1
train[train$fireplaceflag == "", "fireplaceflag"] = 0
train[train$fireplaceflag == "true", "fireplaceflag"] = 1
train[train$hashottuborspa == "", "hashottuborspa"] = 0
train[train$hashottuborspa == "true", "hashottuborspa"] = 1
train[train$taxdelinquencyflag == "", "taxdelinquencyflag"] = 0
train[train$taxdelinquencyflag == "Y", "taxdelinquencyflag"] = 1

# convert numeric to double
train[,variable_num] = sapply(train[,variable_num], as.numeric)

# convert discrete to int
train[,variable_int] = sapply(train[,variable_int], as.integer)

# convert categorical to character
train[,variable_char] = sapply(train[,variable_char], as.character)


##Check missing similarity
MissSim <- function(X, feature){
  
  na.df <- as.data.frame(is.na(X))
  
  na.similarity <- as.data.frame(as.matrix(proxy::dist(t(na.df), method = "jaccard", diag = T, upper = T))) 
  # Jaccard dist: 0-1, 0 means similar, 1 means not similar
  # calculate similarity between columns
  
  na.mat = as.matrix(na.similarity[order(na.similarity[feature,]), order(na.similarity[feature,])]) 
  # order na.similarity, then convert to matrix
  
  library(plotly)
  plot_ly(x=colnames(na.mat),
          y=rownames(na.mat),
          z = na.mat,
          type = "heatmap")
}

MissSim(train, "calculatedfinishedsquarefeet")

# missing value of logical
MissSim(train[, variable_logi], "fireplaceflag")

# missing value of discrete
MissSim(train[, variable_int], "bathroomcnt")

# missing value of nominal
MissSim(train[, variable_char], "regionidcity")

# missing value of numeric
MissSim(train[, variable_num], "calculatedfinishedsquarefeet")

##Check the target variable distribution
library(ggplot2)
par(mfrow=c(1,2))

#plot the logerror histogram
ggplot(data = train, aes(x=logerror)) + 
  geom_histogram(aes(y=..count../sum(..count..)),
                 bins = 400,
                 fill="red",
                 color="black") +
  theme_bw()+
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(-0.5,0.5))

#plot the abs.logerror histogram
ggplot(data = train, aes(x=abs.logerror)) + 
  geom_histogram(aes(y=..count../sum(..count..)),
                 bins = 400,
                 fill="red",
                 color="black") +
  theme_bw()+
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(0,0.5))

#check if the distribution of logerror is Gaussian
ggplot(train, aes(sample = logerror)) +
  stat_qq()

#seems like a laplace distribution
logerror_ecdf = ecdf(train$logerror)

library(rmutil)
x = sort(train$logerror)
cdfl = plaplace(x, m = mean(train$logerror) ,s=sd(train$logerror)*0.4)
qq = data.frame(cdfl = cdfl, ecdf = logerror_ecdf(x))
ggplot(data = qq, aes(x=cdfl, y=ecdf)) + geom_line()


##Check the relationships between target variable and date variables

###With transaction date
abs.error.date = by(train, train$trans_date, function(x){return(mean(x$abs.logerror))})
error.date = by(train, train$trans_date, function(x){return(mean(x$logerror))})
par(mfrow=c(3,1))
plot(abs.error.date, type="b") + title("date v.s abs.logerror")
plot(error.date, type="b") + title("date v.s logerror")
barplot(table(train$trans_date)) + title("histogram of transaction date")


###With transaction month
abs.error.month = by(train, train$trans_month, function(x){return(mean(x$abs.logerror))})
error.month = by(train, train$trans_month, function(x){return(mean(x$logerror))})
plot(abs.error.month, type="b") + title("month v.s abs.logerror")
plot(error.month, type="b") + title("month v.s logerror")
barplot(table(train$trans_month)) + title("histogram of transaction month")


###With transaction day of month
abs.error.day = by(train, train$trans_day, function(x){return(mean(x$abs.logerror))})
error.day = by(train, train$trans_day, function(x){return(mean(x$logerror))})
plot(abs.error.day, type="b") + title("day of month v.s abs.logerror")
plot(error.day, type="b") + title("day of month v.s logerror")
barplot(table(train$trans_day)) + title("histogram of transaction day of month")


###With transaction weekday
abs.error.weekday = by(train, train$trans_weekday, function(x){return(mean(x$abs.logerror))})
error.weekday = by(train, train$trans_weekday, function(x){return(mean(x$logerror))})
plot(abs.error.weekday, type="b") + title("weekday v.s abs.logerror")+axis(1,at=1:7, labels=names(abs.error.weekday))
plot(error.weekday, type="b") + title("weekday v.s logerror")+axis(1,at=1:7, labels=names(error.weekday))
barplot(table(train$trans_weekday)) + title("histogram of transaction weekday")
```

###With yearbuilt
abs.error.yearbuilt = by(train, train$yearbuilt, function(x){return(mean(x$abs.logerror))})
error.yearbuilt = by(train, train$yearbuilt, function(x){return(mean(x$logerror))})
err.yearbuilt = data.frame(year = as.integer(names(abs.error.yearbuilt)),
                           abs_logerror = as.numeric(abs.error.yearbuilt),
                           logerror = as.numeric(error.yearbuilt))
library(ggplot2)
ggplot(data=err.yearbuilt, aes(x = year, y = abs_logerror)) + 
  geom_point() + geom_line() + geom_smooth() + geom_hline(yintercept = mean(train$abs.logerror), color="red")

ggplot(data=err.yearbuilt, aes(x = year, y = logerror)) + 
  geom_point() + geom_line() + geom_smooth() + coord_cartesian(ylim=c(-0.1,0.1))+geom_hline(yintercept = mean(train$logerror), color="red")


##Check the relationships with discrete variables

###With bathroomcnt + bedroomcnt & roomcnt
par(mfrow=c(1,1))
with(train, plot(bathroomcnt + bedroomcnt, roomcnt))
abline(0,1)

# Why there are so many houses with roomcnt smaller than bath + bed?
# Assumption: error in data
boxplot(subset(train, roomcnt < bathroomcnt + bedroomcnt)$logerror,
        subset(train, roomcnt >= bathroomcnt + bedroomcnt)$logerror)
quantile(train$abs.logerror, 0.9)
boxplot(subset(train, roomcnt < bathroomcnt + bedroomcnt & abs.logerror < 0.145)$logerror,
        subset(train, roomcnt >= bathroomcnt + bedroomcnt & abs.logerror < 0.145)$logerror)
boxplot(subset(train, roomcnt < bathroomcnt + bedroomcnt & abs.logerror < 0.145)$abs.logerror,
        subset(train, roomcnt >= bathroomcnt + bedroomcnt & abs.logerror < 0.145)$abs.logerror)
with(train, t.test(logerror ~ (roomcnt < bathroomcnt + bedroomcnt)))
with(train, t.test(abs.logerror ~ (roomcnt < bathroomcnt + bedroomcnt)))

#create a new logical feature: correct.roomcnt
train$correct.roomcnt <- as.numeric(with(train, roomcnt >= bathroomcnt + bedroomcnt))


##Check the relationships between target variable and categorical variables 

###With fips (a few levels)
####6037 Los Angeles; 6059 Orange County; 6111 Ventura County
table(train$fips)
str(train$fips)
error.fips = by(train, train$fip, function(x) {return(mean(x$logerror))})
with(train, t.test(logerror ~ (fips == '6111')))

with(train, anova(lm(logerror ~ fips)))

#Why 6111 have larger logerror, maybe we have too few data points
num.fips <- by(train, train$fips, function(x){
  return(length(unique(x$parcelid)))})
train$num.fips <- num.fips[train$fips]


###With other location variables
head(train[,c("regionidcity","regionidzip","rawcensustractandblock")])
head(unique(train[,c("regionidcity","regionidzip")]))
by(train,train$fips, function(x){return(length(unique(x$regionidcity)))})
length(unique(train$regionidcity))
length(unique(train$regionidzip))
length(unique(train$rawcensustractandblock))


###With regionidzip
table(train$regionidzip)
error.zip <- by(train, train$regionidzip, function(x) {return(mean(x$logerror))})
plot(density(error.zip))
train$error.zip <- error.zip[train$regionidzip]
summary(train$error.zip) # seeing NA due to regionidzip is NA. 

summary(lm(logerror ~ regionidzip, train))
summary(lm(logerror ~ error.zip, train))
#We see that the adjusted R-squared increased from 0.006164 to 0.01041.

train$regionidzip.new <- ifelse(train$error.zip < quantile(error.zip, 0.05), '1',
                                ifelse(train$error.zip < quantile(error.zip, 0.95), '2', '3'))
summary(lm(logerror ~ regionidzip.new, train))
# The adjusted R-squared decreased  to 0.003761.

train$regionidzip.new <- ifelse(train$error.zip < quantile(error.zip, 0.05), '1',
                                ifelse(train$error.zip < quantile(error.zip, 0.25), '2', 
                                       ifelse(train$error.zip < quantile(error.zip, 0.75), '3',
                                              ifelse(train$error.zip < quantile(error.zip, 0.95), '4', '5'))))
summary(lm(logerror ~ regionidzip.new, train))
# The adjusted R-squared increased to 0.00817.

# check these extreme cases and find out they also have relative sparse data.
error.zip[which(error.zip < -0.1)]
length(train$parcelid[which(train$regionidzip=="96329")])
error.zip[which(error.zip > 0.1)]
dim(subset(train, regionidzip == 96226))

# Assumption: few number of houses for certain region id zip caused logerror large.
# How to verify
num.zip <- by(train, train$regionidzip, function(x) {
  return(dim(x)[1])})
train$num.zip = num.zip[train$regionidzip]
with(train, cor.test(logerror, num.zip, use = 'pairwise.complete.obs'))
with(train, cor.test(abs.logerror, num.zip, use = 'pairwise.complete.obs'))


###With propertycountylandusecode
error.landusecode <- by(train, train$propertycountylandusecode, function(x){return(mean(x$logerror))})
plot(density(error.landusecode))
train$error.landusecode <- error.landusecode[train$propertycountylandusecode]
summary(lm(logerror ~ propertycountylandusecode, train))
summary(lm(logerror ~ error.landusecode, train))


###With rawcensustractandblock
error.censustract <- by(train, train$rawcensustractandblock, function(x){return(mean(x$logerror))})
plot(density(error.censustract))
train$error.censustract <- error.censustract[train$rawcensustractandblock]
summary(lm(logerror ~ rawcensustractandblock, train))
summary(lm(logerror ~ error.censustract, train))


## Check the relationships with Continuous variables 
###With tax-related variables
####taxvaluedollarcnt: value to be taxed
####structuretaxvaluedollarcnt: value to be taxed from structure
####landtaxvaluedollarcnt: value to be taxed from land
####taxamount: actual paid tax
with(train, plot(structuretaxvaluedollarcnt + landtaxvaluedollarcnt, taxvaluedollarcnt))
abline(0,1)
#We can just use 2 out of the 3 variables.
train$landtaxtototal <- as.numeric(train$landtaxvaluedollarcnt/train$taxvaluedollarcnt)

summary(with(train, taxamount/taxvaluedollarcnt))

with(train, plot(taxamount/taxvaluedollarcnt, logerror))
with(subset(train,taxamount/taxvaluedollarcnt <= 0.1),
     plot(taxamount/taxvaluedollarcnt, logerror))
with(subset(train,taxamount/taxvaluedollarcnt <= 0.1), 
     cor.test(logerror, taxamount, use = 'pairwise.complete.obs'))
with(subset(train,taxamount/taxvaluedollarcnt <= 0.1), 
     cor.test(logerror, taxvaluedollarcnt, use = 'pairwise.complete.obs')) 
with(subset(train,taxamount/taxvaluedollarcnt <= 0.1), 
     cor.test(logerror, taxamount/taxvaluedollarcnt,use = 'pairwise.complete.obs'))
#There is a significant correlation, so we create a new feature, taxamount/value.
with(train, cor.test(logerror, taxamount/taxvaluedollarcnt, use = 'pairwise.complete.obs'))
#The correlation is not significant with extreme outliers, so we need to remove these outliers.

train$taxamounttovalue <- as.numeric(train$taxamount/train$taxvaluedollarcnt)
summary(train[which(train$taxamount/train$taxvaluedollarcnt>0.1),c('taxamount','taxvaluedollarcnt','taxamounttovalue')])

with(subset(train,taxamount/taxvaluedollarcnt <= 1), 
     cor.test(logerror, taxamount/taxvaluedollarcnt,use = 'pairwise.complete.obs'))


### With living area variables
train$finishedtolots <- with(train, calculatedfinishedsquarefeet/lotsizesquarefeet)
with(train, cor.test(logerror, calculatedfinishedsquarefeet, use = 'pairwise.complete.obs'))
with(train, cor.test(logerror, lotsizesquarefeet, use = 'pairwise.complete.obs'))
with(train, cor.test(logerror, finishedtolots, use = 'pairwise.complete.obs'))


## Save the processed dataset 
names(train)
dim(train[c("parcelid", "trans_year", "transactiondate","structuretaxvaluedollarcnt")])<- list(NULL)
write.csv(train,file="train w feature engineering.csv")
