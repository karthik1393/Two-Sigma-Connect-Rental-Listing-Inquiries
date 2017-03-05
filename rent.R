library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(missForest)
library(rpart.plot)
library(tm)
library(caTools)
library(mice)
library(gbm)
library(Boruta)
library(corrplot)
library(xgboost)
library(ROCR)
library(caTools)
library(party)
library(plyr)
library(dplyr)
library(dummies)
library(e1071)
library(caTools)
library(nnet)
library(adabag)
library(SuperLearner)
library(dplyr)
library(knncat)
library(jsonlite)
library(rjson)
library(purrr)
library(ggplot2)
library(RColorBrewer)
getwd()
json<-jsonlite::fromJSON("train.json")
View(json)
# unlist every variable except `photos` and `features` and convert to tibble
vars <- setdiff(names(json), c("photos", "features"))
rent<- map_at(json, vars, unlist) %>% tibble::as_tibble(.)
str(json)
View(head(rent,n=1))
##test data
rent_test<-jsonlite::fromJSON("test.json")
vars1 <- setdiff(names(rent_test), c("photos", "features"))
rent_test<- map_at(rent_test, vars1, unlist) %>% tibble::as_tibble(.)
# class(features)
# features<-do.call(rbind, lapply(features, data.frame, stringsAsFactors=FALSE))
# str(features)
##numerical features for training set
str(rent)
rent=rent2
names(rent)
rent$description_count<-sapply(strsplit(rent$description, " "), length)
head(rent$description_count)
num_vars<-select(rent,-c(7,12))
names(num_vars)
num_vars<-as.data.frame(num_vars)
# num_vars[num_vars==""]<-NA
# table(is.na(num_vars))
# sum(is.na(num_vars$bathrooms))
num_vars[complete.cases(num_vars),]
num_vars<-na.omit(num_vars)
#write.csv(num_vars,"num_vars.csv")
names(num_vars)
num_vars$interest_level<-as.character(num_vars$interest_level)
num_vars$interest_level[num_vars$interest_level=="low"]=0
num_vars$interest_level[num_vars$interest_level=="medium"]=1
num_vars$interest_level[num_vars$interest_level=="high"]=2
num_vars$interest_level<-as.factor(num_vars$interest_level)
head(num_vars$interest_level,n=12)

##numerical features for test set
rent_num<-rent_test
names(rent_num)
rent_num<-select(rent_num,-c(7,12))
rent_num$feature_count<-unlist(lapply(rent_test$features,length))
rent_num$photos_count<-unlist(lapply(rent_test$photos,length))
rent_num$created<-as.POSIXct(rent_test$created)
rent_num$year<-substr(rent_test$created,1,4)
rent_num$month<-substr(rent_test$created,6,7)
rent_num$days<-substr(rent_test$created,9,10)
rent_num$description_count<-sapply(strsplit(rent_test$description, " "), length)

####Split the data set
set.seed(144)
split<-sample.split(num_vars$interest_level,SplitRatio = 0.7)
num_train<-subset(num_vars,split==TRUE)
num_test<-subset(num_vars,split==FALSE)
View(head(num_train))
num_train$manager_id<-as.numeric(as.factor(num_train$manager_id))
rent_num$manager_id<-as.numeric(as.factor(rent_num$manager_id))

###RF
set.seed(1009)
num_rf<-randomForest(interest_level~bedrooms+bathrooms+price+latitude+longitude+feature_count+photos_count+year+month+days+description_count+manager_id,data=num_train,ntree=501)
varImpPlot(num_rf)
plot(num_rf)
print(num_rf)
num_rent<-predict(num_rf,newdata = rent_num,type="prob")
num_rent
write.csv(num_rent,"rf_num2.csv")
str(num_train)


##ADABOOST
ind1<-num_train[,c("bedrooms","bathrooms","price","latitude","longitude","feature_count","photos_count","month","days","description_count")]
set.seed(1001)
ada_control=trainControl(method = "cv",number=10)
ada_grid<-expand.grid(iter=100,maxdepth=3,nu=0.1)
ada<-train(ind1,num_train[,"interest_level"],method="ada",trControl=ada_control,tuneGrid=ada_grid)
warnings()

##nb
rent_nb<-naiveBayes(interest_level~bedrooms+bathrooms+price+latitude+longitude+feature_count+photos_count+year+month+days+description_count+manager_id,data=num_train)
nb_pre<-predict(rent_nb,newdata = rent_num,type="raw")
write.csv(num_train,"count.csv")
?naiveBayes
names(num_train)

##SVM
rent_svm<-svm(interest_level~bedrooms+bathrooms+price+latitude+longitude+feature_count+photos_count+description_count+manager_id,data=num_train,probability=TRUE)
svm_pre<-predict(rent_svm,newdata=rent_num,probability=TRUE)
pre<-attr(svm_pre,"probabilities")
write.csv(pre,"pred1.csv")         ###loww score



































#  ##XGBoost
# names(num_train)
# train_1<-select(num_train,-13)
# 
# names(train_X)
# xgb.DMatrix(train_X)
# train_Y<-select(num_train,13)
# set.seed(100)
# pmt = proc.time()
# model = xgboost(data = as.matrix(train_X),
#                 label = train_Y,
#                 eta = 0.05,
#                 max_depth = 5,
#                 nround=500,
#                 subsample = 1,
#                 colsample_bytree = 0.5,
#                 seed = 100,
#                 eval_metric = "merror",
#                 objective = "multi:softprob",
#                 num_class = 3,
#           
#                 silent = 1)
# show(proc.time() - pmt)