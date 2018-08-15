#Step1:Data Loading and Processing

setwd("~/Projects/Zillow")
library(Amelia)
library(ggplot2)


## Load in Source Data
properties <- read.csv("properties_2016.csv",stringsAsFactors = F)
train <- read.csv("train_2016_v2.csv",stringsAsFactors = F)

length(unique(train$parcelid))

properties <- subset(properties,parcelid %in% train$parcelid)
train <- merge(properties,train,by="parcelid",all.y=T)

#explore the data
summary(train)
str(train)
head(train)
colnames(train)


## Check Missing Values
missmap(train[-1],col=c('grey','steelblue'),y.cex=0.5,x.cex=0.5)

###visualize the NA's
num.NA <- sort(colSums(sapply(train, is.na)))
dfnum.NA <- data.frame(ind = c(1:length(num.NA)),
                       percentage = num.NA/nrow(train),
                       per80 = num.NA/nrow(train)>=0.2,
                       name = names(num.NA),
                       row.names = NULL)

ggplot(data = dfnum.NA, aes(x=ind, y=percentage)) + 
  geom_bar(aes(fill=per80), stat="identity") + 
  scale_x_discrete(name ="column names", 
                   limits=dfnum.NA$name)+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
        legend.position = "none") +
  geom_hline(yintercept = 0.2) + 
  annotate("text", 5, 0.21, label="20 percent", size = 5) + 
  annotate("text", 4, 0.35, label="drop", color="#00BFC4",size=5) + 
  annotate("text", 4, 0.05, label="keep", color="#F8766D",size=5) + 
  ggtitle("percentage of missing")

#calculate the percentage of data missing in train
sum(is.na(train))/(nrow(train)*ncol(train))

## Save the processed Train Data for Next Step
remain.col <- names(num.NA)[which(num.NA <= 0.2 * dim(train)[1])]
train <- train[, remain.col]
write.csv(train,"train data with filtered features.csv")


