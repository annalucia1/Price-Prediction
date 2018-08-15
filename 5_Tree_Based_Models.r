#Tree-based Models

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
logical <- c("correct.roomcnt", "hashottuborspa", "fireplaceflag", "taxdelinquencyflag")
categorical <- c("fips", "trans_month", "trans_day", "trans_weekday")
train.sub[,categorical] <- sapply(train.sub[,categorical],as.character)

#remove rows with missing values
trainsub.cp <- train.sub[which(apply(train.sub, 1, function(x) length(which(is.na(x))) == 0)), ]

#convert categorical features to factors for RandomForest model
trainsub.cp$fips <- as.factor(trainsub.cp$fips)
trainsub.cp$trans_month <- as.factor(trainsub.cp$trans_month)
trainsub.cp$trans_day <- as.factor(trainsub.cp$trans_day)
trainsub.cp$trans_weekday <- as.factor(trainsub.cp$trans_weekday)

str(trainsub.cp)

set.seed(2)
index <- sample(1:dim(trainsub.cp)[1],dim(trainsub.cp)[1]*0.7)
trainset <- trainsub.cp[index,]
testset <- trainsub.cp[-index,]
test.x <- testset[,-1]
test.y <- testset[,1]

formula <- paste("logerror ~ ", paste(colnames(train.sub[,-1])[-length(colnames(train.sub))], collapse = " + "))


#1.Decision Tree
library(rpart)

tree0 <- rpart(formula, method = 'anova', data = trainset, 
               control=rpart.control(cp = 0.001))
tree0
MAE.t0 <- mean(abs(predict(tree0, newdata=test.x )-test.y))
MAE.t0

printcp(tree0)
plotcp(tree0) 

bestcp <- tree0$cptable[which.min(tree0$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree0, cp = bestcp)
tree.pruned
# plot tree
library(rpart.plot)
prp(tree.pruned, faclen = 0, cex = 0.8)

MAE.tp <- mean(abs(predict(tree.pruned, newdata=test.x)-test.y))
MAE.tp

MSE.tp <- sum((predict(tree.pruned, newdata=test.x) - test.y)^2)/dim(test.x)[1]
MSE.tp


#2.Random Forest
library(randomForest)

set.seed(3)
rf <- randomForest(as.formula(formula), data = trainset, importance = TRUE,
                   ntree = 50)
#output tree 1 for example with variable labeled
getTree(rf, k = 1, labelVar = TRUE)

par(mar=rep(2,4))
par(mfrow = c(1,1))

varImpPlot(rf)
 
importanceOrder <- order(rf$importance[, "%IncMSE"], decreasing = T)
names <- rownames(rf$importance)[importanceOrder]

partialPlot(rf, trainset, eval('landtaxvaluedollarcnt'), 
            xlab='landtaxvaluedollarcnt')

library(tabplot)
tableplot(trainset, c('logerror', 'landtaxvaluedollarcnt'), scales = 'lin')
# Verify the relationships between feature and response

#Check the overall OOB error
plot(rf)

MAE.rf <- mean(abs(predict(rf, test.x)-test.y))
MAE.rf

MSE.rf <- sum((predict(rf, test.x) - test.y)^2)/dim(testset)[1]
MSE.rf

#3.Gradient Boosting Tree
library(xgboost)

data.matrix <- model.matrix(~., data = trainset)
data.matrix <- data.matrix[,-1]

#grid search for parameters.
all_param = NULL
all_test_rmse = NULL
all_train_rmse = NULL

for (iter in 1:20) {
  param <- list(objective = "reg:linear",
                max_depth = sample(5:12, 1),
                subsample = runif(1, .6, .9),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
               )
  cv.nround = 50
  cv.nfold = 5
  
  set.seed(iter)
  mdcv <- xgb.cv(data=data.matrix[,-1], 
                 label = data.matrix[,1], 
                 params = param, 
                 nfold=cv.nfold, 
                 nrounds=cv.nround,
                 verbose = F, 
                 early_stop_round=8, 
                 maximize = FALSE)
  min_train_rmse = min(mdcv$evaluation_log$train_rmse_mean)
  min_test_rmse = min(mdcv$evaluation_log$test_rmse_mean)
  
  all_param <- rbind(all_param, unlist(param)[-1])
  all_train_rmse <- c(all_train_rmse, min_train_rmse)
  all_test_rmse <- c(all_test_rmse, min_test_rmse)
}
all_param_df <- as.data.frame(all_param)
par <- all_param_df[which(all_test_rmse==min(all_test_rmse)),]
par

#find the best nrounds
gbt.cv <- xgb.cv(params = par, 
                 verbose = F, 
                 early_stop_round=8, 
                 maximize = FALSE,
                 data = data.matrix[,-1], 
                 label = data.matrix[,1],
                 nfold = 5, 
                 nrounds = 100)

plot(gbt.cv$evaluation_log$train_rmse_mean, type = 'l')
lines(gbt.cv$evaluation_log$test_rmse_mean, col = 'red')
bestround <-  which(gbt.cv$evaluation_log$test_rmse_mean == min(gbt.cv$evaluation_log$test_rmse_mean))
bestround

#build the best GBT model
gbt <- xgboost(data =  data.matrix[,-1], 
               label = data.matrix[,1], 
               params=par,
               nround = bestround,
               nthread = 3,
               verbose = 2)

importance <- xgb.importance(feature_names = colnames(data.matrix[,-1]), model = gbt)
importance

xgb.plot.importance(importance)

#prediction
MAE.gb <- mean(abs(predict(gbt, model.matrix( ~ ., data = testset[, -1]))-testset[,1]))
MAE.gb

MSE.gb <- sum((predict(gbt, model.matrix( ~ ., data = testset[, -1])) - testset[,1])^2)/dim(testset)[1]
MSE.gb

