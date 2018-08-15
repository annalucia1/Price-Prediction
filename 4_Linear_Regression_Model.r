#Linear regression model

setwd("~/Projects/Zillow")
train <- read.csv("train w feature engineering.csv",stringsAsFactors = F)

train.sub <- train[ , c("logerror", 
                        "bathroomcnt", "bedroomcnt", "roomcnt", "correct.roomcnt",
                        "hashottuborspa", "fireplaceflag", 
                        "yearbuilt",
                        "latitude", "longitude",
                        "fips", "error.zip", "num.zip", "error.landusecode", "error.censustract",
                        "taxvaluedollarcnt", "landtaxvaluedollarcnt", "landtaxtototal",
                        "taxamount", "taxdelinquencyflag", "taxamounttovalue", 
                        "calculatedfinishedsquarefeet", "lotsizesquarefeet", "finishedtolots",
                        "trans_month", "trans_day", "trans_weekday")]
str(train.sub)

logical <- c("correct.roomcnt", "hashottuborspa", "fireplaceflag", "taxdelinquencyflag")
categorical <- c("fips", "trans_month", "trans_day", "trans_weekday")
train.sub[,categorical] <- sapply(train.sub[,categorical],as.character)
str(train.sub)

#remove rows with missing values
trainsub.cp <- train.sub[which(apply(train.sub, 1, function(x) length(which(is.na(x))) == 0)), ]

#scale predicting variables
trainsub.cp[, !names(trainsub.cp) %in% c(logical, categorical)] <- scale(trainsub.cp[, !names(trainsub.cp) %in% c(logical, categorical)])

summary(trainsub.cp)

#Trian-Test Split
set.seed(1)
index <- sample(1:dim(trainsub.cp)[1],dim(trainsub.cp)[1]*0.7)
trainset <- trainsub.cp[index,]
testset <- trainsub.cp[-index,]

#1. build robust linear regression model
lm1 <- lm(logerror ~ ., data = trainset)
summary(lm1)

#adjusted R-squared is 0.5419
#significant predictors are: hashottuborspa; error.landusecode; error.censustract             
#  landtaxtototal; taxamount; taxdelinquencyflag            
#  taxamounttovalue; calculatedfinishedsquarefeet; 
#  trans_month4; trans_month5; trans_weekdaySunday

plot(lm1)
#residual is unbiased but heteroscedastic
#residual is not normal distributed
#24623, 24400,30554 are high leverage, high residual point


library(car)
vif(lm1)
#taxvaluedollarcnt; landtaxvaluedollarcnt; taxamount; roomcnt; correct.roomcnt have high VIF

MAE1 <- mean(abs(predict(lm1, newdata=testset[,-1])-testset[,1]))
MAE1
#0.2735427

#2. remove high leverage/high residual points
trainset2 <- trainset[-c(24400,24623,30554),]
lm2 <- lm(logerror ~ ., data =trainset2)
summary(lm2)
#adjusted R-sqaured is 0.5419
plot(lm2)
MAE2 <- mean(abs(predict(lm2, newdata=testset[,-1])-testset[,1]))
MAE2
#0.2735429
#MAE2 is very close to MAE1, so we keep the high leverage points

library(glmnet)
train.x <- model.matrix( ~., trainset[, -1])
test.x <- model.matrix(~., testset[,-1])
train.y <- trainset$logerror
test.y <- testset$logerror
lm3 <- glmnet(x=train.x, y=train.y)
plot(lm3, label = T)
plot(lm3, xvar = "lambda", label = T)
print(lm3)

# use cross validation to get optimal value of lambda
cvlm3 <- cv.glmnet(train.x, train.y)
plot(cvlm3)
 
cvlm3$lambda.min
#0.001920282
cvlm3$lambda.1se
#0.152179, only keeps 1 feature

MAE3 <- mean(abs(predict(cvlm3, newx=test.x, s="lambda.min" )-test.y))
MAE3
#0.2708631


cv.min.coef <-  coef(cvlm3, s = "lambda.min")

feature.names <-  cv.min.coef@Dimnames[[1]]
feature.coefs <-  rep(0, length(cv.min.coef@Dimnames[[1]])) # initalize a container for feature coefs
feature.coefs[cv.min.coef@i+1] <-  abs(cv.min.coef@x) # store absolute coefs in feature.coefs

coef.df <-  data.frame(feature.names = feature.names, feature.coefs = feature.coefs) # convert to dataframe
coef.df <-  coef.df[order(coef.df$feature.coefs), ] # sorted by absolute coefs

# visualize result
barplot(abs(coef.df$feature.coefs),
        names.arg = coef.df$feature.names,
        cex.names=0.8, las=2)
