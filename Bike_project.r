library(ggplot2)
library(ggplot.multistats)

library(corrgram)
#Importing the csv file
data_bike=read.csv("D:\\SnehaDS\\day.csv")
head(data_bike,5)

data_bike<-subset(data_bike,select=-c(casual,registered))
head(data_bike,5)

dim(data_bike)
summary(data_bike)
#Structure of data_bikeset
str(data_bike)
#Rename the columns
names(data_bike)<-c('rec_id','datetime','season','year','month','holiday','weekday','workingday','weather_condition','temp','atemp','humidity','windspeed','total_count')
#Read the data
head(data_bike,5)
#Typecasting the datetime and numerical attributes to category

data_bike$datetime<- as.Date(data_bike$datetime)
data_bike$year<-as.factor(data_bike$year)
data_bike$month<-as.factor(data_bike$month)
data_bike$season <- as.factor(data_bike$season)
data_bike$holiday<- as.factor(data_bike$holiday)
data_bike$weekday<- as.factor(data_bike$weekday)
data_bike$workingday<- as.factor(data_bike$workingday)
data_bike$weather_condition<- as.factor(data_bike$weather_condition)

missing_val<-data.frame(apply(data_bike,2,function(x){sum(is.na(x))}))
names(missing_val)[1]='missing_val'
missing_val

#column plot for season wise monthly distribution of counts
ggplot(data_bike,aes(x=month,y=total_count,fill=season))+theme_bw()+geom_col()+
  labs(x='Month',y='Total_Count',title='Season wise monthly distribution of counts')
#column plot for weekday wise monthly distribution of counts
ggplot(data_bike,aes(x=month,y=total_count,fill=weekday))+theme_bw()+geom_col()+
  labs(x='Month',y='Total_Count',title='Weekday wise monthly distribution of counts')

ggplot(data_bike,aes(x=year,y=total_count,fill=year))+geom_violin()+theme_bw()+
  labs(x='Year',y='Total_Count',title='Yearly wise distribution of counts')

#oUTLIER Analysis
#boxplot for total_count_outliers
par(mfrow=c(1, 1))#divide graph area in 1 columns and 1 rows
boxplot(data_bike$total_count,main='Total_count',sub=paste(boxplot.stats(data_bike$total_count)$out))

#box plots for outliers
par(mfrow=c(2,2))
#Box plot for temp outliers
boxplot(data_bike$temp, main="Temp",sub=paste(boxplot.stats(data_bike$temp)$out))
#Box plot for humidity outliers
boxplot(data_bike$humidity,main="Humidity",sub=paste(boxplot.stats(data_bike$humidity)$out))
#Box plot for windspeed outliers
boxplot(data_bike$windspeed,main="Windspeed",sub=paste(boxplot.stats(data_bike$windspeed)$out))

#load the DMwR library
library(DMwR)
#create subset for windspeed and humidity variable
wind_hum<-subset(data_bike,select=c('windspeed','humidity'))
#column names of wind_hum
cnames<-colnames(wind_hum)
for(i in cnames){
  val=wind_hum[,i][wind_hum[,i] %in% boxplot.stats(wind_hum[,i])$out] #outlier values
  wind_hum[,i][wind_hum[,i] %in% val]= NA  # Replace outliers with NA 
}
#Imputating the missing values using mean imputation method
wind_hum$windspeed[is.na(wind_hum$windspeed)]<-mean(wind_hum$windspeed,na.rm=T) 
wind_hum$humidity[is.na(wind_hum$humidity)]<-mean(wind_hum$humidity,na.rm=T)

#Remove the windspeed and humidity variable in order to replace imputated data
new_df<-subset(data_bike,select=-c(windspeed,humidity))
#Combined new_df and wind_hum data frames
data_bike<-cbind(new_df,wind_hum)
head(data_bike,5)

corrgram(data_bike[,10:14], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#load the purrr library for functions and vectors
library(purrr)
#Split the dataset based on simple random resampling
train_index<-sample(1:nrow(data_bike),0.7*nrow(data_bike))
train_data<-data_bike[train_index,]
test_data<-data_bike[-train_index,]
dim(train_data)
dim(test_data)

#Create a new subset for train attributes 
train<-subset(train_data,select=c('season','year','month','holiday', 'weekday','workingday','weather_condition','temp','humidity','windspeed','total_count'))
#Create a new subset for test attributes
test<-subset(test_data,select=c('season','year','month','holiday','weekday','workingday','weather_condition','temp','humidity','windspeed','total_count'))
head(train,5)
head(test,5)

#create a new subset for train categorical attributes
train_cat_attributes<-subset(train,select=c('season','holiday','workingday','weather_condition','year'))
#create a new subset for test categorical attributes
test_cat_attributes<-subset(test,select=c('season','holiday','workingday','weather_condition','year'))
#create a new subset for train numerical attributes
train_num_attributes<-subset(train,select=c('weekday','month','temp','humidity','windspeed','total_count'))
#create a new subset for test numerical attributes
test_num_attributes<-subset(test,select=c('weekday','month','temp', 'humidity','windspeed','total_count'))

library(caret)
#other variables along with target variable to get dummy variables
othervars=c('month','weekday','temp','humidity','windspeed','total_count')
set.seed(2626)
#Categorical variables
vars=setdiff(colnames(train),c(train$total_count,othervars))
#formula pass through encoder to get dummy variables
f = paste('~', paste(vars, collapse = ' + '))
#encoder is encoded the categorical variables to numeric
encoder=dummyVars(as.formula(f), train)
#Predicting the encode attributes
encode_attributes=predict(encoder,train)
#Binding the train_num_attributes and encode_attributes
train_encoded_attributes=cbind(train_num_attributes,encode_attributes)
head(train_encoded_attributes,5)

set.seed(5662)
#Categorical variables
vars<-setdiff(colnames(test),c(test$total_count,othervars))
#formula pass through encoder to get dummy variables
f<- paste('~',paste(vars,collapse='+'))
#Encoder is encoded the categorical variables to numeric
encoder<-dummyVars(as.formula(f),test)
#Predicting the encoder attributes
encode_attributes<-predict(encoder,test)
#Binding the test_num_attributes and encode_attributes
test_encoded_attributes<-cbind(test_num_attributes,encode_attributes)
head(test_encoded_attributes,5)

#Set seed to reproduce the results of random sampling
set.seed(672)
#training the lr_model
lr_model<-lm(train_encoded_attributes$total_count~.,train_encoded_attributes[,-c(6)])
#Summary of the model
summary(lr_model)

set.seed(6872)
options(warn=-1)
#predict the lr_model
lm_predict<- predict(lr_model,test_encoded_attributes[,-c(6)])
head(lm_predict,5)

set.seed(688)
#Root mean squared error
rmse<-RMSE(lm_predict, test_encoded_attributes$total_count)
print(rmse)
#Mean squared error
mae<-MAE(lm_predict, test_encoded_attributes$total_count)
print(mae)

set.seed(568)
#load the rpart library for decision trees
library(rpart)
y_test<-test_encoded_attributes$total_count
#rpart.control to contro the performance of model
rpart.control<-rpart.control(minbucket = 2,cp = 0.01,maxcompete = 3, maxsurrogate = 4, usesurrogate = 2, xval = 3,surrogatestyle = 0, maxdepth = 10) 
#training the dtr model
dtr<-rpart(train_encoded_attributes$total_count~.,data=train_encoded_attributes[,-c(6)],control=rpart.control,method='anova',cp=0.01)
#Summary of dtr model
dtr

#predict the trained model
dtr_predict<-predict(dtr,test_encoded_attributes[,-c(6)])
head(dtr_predict,5)

#Root mean squared error
rmse<-RMSE(y_test,dtr_predict)
print(rmse)
#Mean absolute error
mae<-MAE(y_test,dtr_predict)
print(mae)

set.seed(6788271)
#load the randomForest library
library(randomForest)
#training the model
rf_model<-randomForest(total_count~.,train_encoded_attributes,importance=TRUE,ntree=200)
rf_model

set.seed(7889)
#Predicting the model
rf_predict<-predict(rf_model,test_encoded_attributes[,-c(6)])
head(rf_predict,5)

set.seed(667)
#Root mean squared error
rmse<-RMSE(y_test,rf_predict)
print(rmse)
mae<-MAE(y_test,rf_predict)
print(mae)

Bike_predictions=data.frame(y_test,rf_predict)
write.csv(Bike_predictions,'Bike_Renting_R.CSV',row.names=F)
Bike_predictions