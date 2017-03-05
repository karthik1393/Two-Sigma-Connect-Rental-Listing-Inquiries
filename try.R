library(dplyr)
library(data.table)
library(tidyr)
library(matrix)
str(rent)
dumm<-select(rent,-c(7,12))
names(dumm)
dumm<-data.frame(dumm,stringsAsFactors=FALSE)
str(dumm)
droplevels.factor(dumm$interest_level)
dumm$interest_level<-as.character(dumm$interest_level)
train_y<-dumm$interest_level
str(train_y)
train_y[train_y=="low"]=1
train_y[train_y=="medium"]=2
train_y[train_y=="high"]=3
train_y<-as.factor(train_y)
head(train_y)
as.numeric(train_y)
class(train_y)
rent2<-rent
rent2$feature_count<-unlist(lapply(rent$features,length))
rent2$photos_count<-unlist(lapply(rent$photos,length))
rent2$created<-as.POSIXct(rent2$created)
rent2$year<-substr(rent2$created,1,4)
rent2$month<-substr(rent2$created,6,7)
rent2$days<-substr(rent2$created,9,10)
rent2$description_count<-stri_count(rent2$description,regex="\\S+")
rent2$des_count<-sapply(strsplit(rent$description, " "), length)
class(rent2$interest_level)
rent2$manger_id<-as.numeric(as.factor(rent2$manager_id))
rent2$interest_level[rent2$interest_level=="low"]=1
rent2$interest_level[rent2$interest_level=="medium"]=2
rent2$interest_level[rent2$interest_level=="high"]=3
rent2$interest_level<-as.factor(rent2$interest_level)
names(rent2)

class(rent2$interest_level)
##xgboost
names(num_train1)
str(num_train)
names(rent_num)
rent_num1<-select(rent_num,-15)
rent_num1<-sparse.model.matrix(~.-1,rent_num1)
num_train1<-select(num_train,-16)
train_x<-sparse.model.matrix(interest_level~.-1,data=num_train1)
y<-num_train1$interest_level
View(y)
levels(num_train1$interest_level)
num_train1$interest_level<-as.factor(num_train1$interest_level)
class(y)
View(num_train1$interest_level)
y<-as.numeric(as.character(y))
model = xgboost(data = train_x,
                label = y,
                eta = 0.1,
                max_depth = 6,
                nround=600,
                subsample = 0.7,
                colsample_bytree = 0.7,
                min_child_weight=1,
                seed = 100,
                eval_metric = "mlogloss",
                objective = "multi:softprob",
                num_class = 3,
               silent = 1)
?xgboost
xgb_pre<-predict(model,rent_num1)
xgb_matrix<-matrix(xgb_pre,nrow=nrow(rent_test),byrow=TRUE)
head(xgb_matrix)
write.csv(xgb_matrix,"xgb2.csv")

num_train1$manager_id<-as.numeric(num_train1$manager_id)
head(num_train1$manager_id)
preprocesspar<-preProcess(num_train1[,10],method=c("range"))
names(num_train1)
as.character.numeric_version(num_train1$manager_id)
d<-table(rent2$manger_id,rent2$interest_level)
d
e<-rowSums(d)
new<-cbind(d,e)
new1<-as.data.frame(new)
new1$low_frac<-(new1$`1`/e)
head(new1)
new1$med_frac<-(new1$`2`/e)
new1$high_frac<-(new1$`3`/e)
View(new1)
new1$manager_skill<-new1$high_frac*2+new1$med_frac
names(rent2)
rent3<-rent2 %>% group_by(rent2$manger_id) %>% summarise(mean=mean(rent2[,c(1,2,8,10,13,16,17,19,20,21,22,23)]))
rent3<-aggregate(x=rent2[,-15],by=list(rent2$manger_id),FUN=mean)
View(rent3)
sort(rent2$manger_id)     
tapply(rent2,rent2$manger_id,mean)
head(rent2$interest_level)
i<-merge(rent2,new1,by.x="manger_id",by.y="manger_id")
str(i)
class(i$interest_level)
i$month<-as.numeric(i$month)
i$days<-as.numeric(i$days)
i$year<-as.numeric(i$year)
set.seed(190)
i_rf<-randomForest(interest_level~bedrooms+bathrooms+price+latitude+longitude+feature_count+photos_count+month+days+description_count+manger_id+manager_skill,data=i,ntree=501)
plot(i_rf)
print(i_rf)
i_pre<-predict(i_rf,newdata=rent_test,type="prob")

