#for mapvalues() function
library(plyr)
#for fast data manipulation(functions like mutate,select,arrange,filter,...)
library(dplyr)
#for data visualisation
library(ggplot2)
#READING THE DATASETs IN R 
df<-read.csv(file="C:/Users/abhak/Documents/train.csv",TRUE,",")
head(df)
dfa<-read.csv(file="C:/Users/abhak/Documents/test.csv",TRUE,",")
head(dfa)
#A QUICK VIEW AT STRUCTURE AND SUMMARY OF BOTH DATASETS
str(df)
str(dfa)
summary(df)
summary(dfa)
#DIMENSION OF DATASETS
#df is a dataframe with 8523 obs. of  12 variables.
dim(df)
#BMStest is a dataframe with 8523 obs. of  11 variables.
dim(dfa)
## add another column(Item_outlet_Sales)(the dependent variable), to the test dataset.
dfa$Item_Outlet_Sales <- NA
#Appending the train and test dataset
BMSdata <- rbind(df, dfa)
#DIMENSION OF NEW DATASETS
dim(BMSdata)
#Checking the missing(NA) values of the data
summary(is.na(BMSdata))
#SOME DATA VISUALISTION
hist(BMSdata$Item_Weight)
hist(BMSdata$Item_Visibility)
hist(BMSdata$Item_Outlet_Sales)
ggplot(BMSdata, aes(Outlet_Identifier, Item_Weight)) + geom_boxplot()
ggplot(BMSdata, aes(Item_Type, Item_Weight)) + geom_boxplot()
ggplot(BMSdata, aes(Item_Fat_Content, Item_Weight)) + geom_boxplot()
boxplot(BMSdata$Item_Visibility)
ggplot(BMSdata, aes(Outlet_Identifier,Item_Visibility)) + geom_boxplot()
ggplot(BMSdata, aes(Item_Type,Item_Visibility)) + geom_boxplot()
ggplot(BMSdata, aes(Outlet_Identifier,Item_Visibility)) + geom_boxplot()
ggplot(BMSdata, aes(Item_Outlet_Sales,Item_Visibility,col = Item_Type)) + geom_point()
ggplot(BMSdata, aes(Item_Outlet_Sales,Item_Visibility,col = Outlet_Type)) + geom_point()
#DATA CLEANING
#IMPUTING THE MISSING VALUES FOR DATA SET USING KNN IMPUTATION
library(VIM)
BMSdata[BMSdata$Outlet_Size =="","Outlet_Size"] <- NA
summary(BMSdata)# all the empty values were replaced by NA
imputdata1 <- BMSdata
imputdata1 <- kNN(BMSdata, variable = c("Item_Weight","Outlet_Size"), k = 90)
summary(imputdata1)
ncol(imputdata1) #you will see there are two additional logical columns that got created we have to remove them
imputdata1 <- subset(imputdata1,select = Item_Identifier:Item_Outlet_Sales)
summary(imputdata1)
plot(imputdata1$Item_MRP,imputdata1$Item_Weight)
BMSdata <- imputdata1
BMSdata$Item_Fat_Content <- mapvalues(BMSdata$Item_Fat_Content, from = c("LF","Low Fat","low fat","Regular"), to = c("lf","lf","lf","reg"))
levels(BMSdata$Item_Fat_Content)
#SOME DATA VISUALISTION again on Dataset
boxplot(BMSdata$Item_Weight)#we can clearly see there are no outliers for this variable.
boxplot(BMSdata$Item_Visibility)
boxplot(BMSdata$Item_Outlet_Sales)
#DETECTING OUTLIERS: BY using winsorisation
dataoutlier <- BMSdata 
bench <- 0.09459 + 1.5*IQR(BMSdata$Item_Visibility)
bench
dataoutlier$Item_Visibility[dataoutlier$Item_Visibility > bench] <- bench
boxplot(dataoutlier$Item_Visibility)
BMSdata <- dataoutlier
#Adding new level in Item_Fat_Content "None".
levels(BMSdata$Item_Fat_Content) <- c(levels(BMSdata$Item_Fat_Content), "None")
## Based on Item_Type, for "health and Hygiene", "Household" and "Others",
## we will change the Item_Fat_Content factor to "None".
BMSdata[which(BMSdata$Item_Type=="Health and Hygiene"),]$Item_Fat_Content="None"
BMSdata[which(BMSdata$Item_Type=="Household"), ]$Item_Fat_Content = "None"
BMSdata[which(BMSdata$Item_Type=="Others"), ]$Item_Fat_Content = "None"
BMSdata$Item_Fat_Content <- as.factor(BMSdata$Item_Fat_Content)
table(BMSdata$Item_Fat_Content)
# we will know how old outlet is.
BMSdata$Outlet_Year <- 2020 - BMSdata$Outlet_Establishment_Year
table(BMSdata$Outlet_Year)
BMSdata$Outlet_Year <- as.factor(BMSdata$Outlet_Year)
## Visualizing Item_MRP with ggplot
ggplot(BMSdata, aes(Item_MRP)) + geom_density(adjust = 1/5)
## It is obvious that we would be better off by converting Item_MRP to Categorical variable
BMSdata$price <- "low"
BMSdata$price[BMSdata$Item_MRP >200] <-"high"
BMSdata$price[BMSdata$Item_MRP>70 & BMSdata$Item_MRP <=200] <- "medium"
summary(BMSdata)
# data cleaning is over
# Dividing data into train and test
BMStrain <- BMSdata[1:8523, ]
BMStest <- BMSdata[8524:14204, ]
model1 <- lm(Item_Outlet_Sales~., data = BMStrain[-c(1,7,8)])
summary(model1)
model2 <- lm(log(Item_Outlet_Sales)~., data = BMStrain[-c(1,7,8)])
summary(model2)
model3 <- lm(sqrt(Item_Outlet_Sales)~., data = BMStrain[-c(1,7,8)])
summary(model3)
par(mfrow=c(1,1))#Create a multi-paneled plotting window.
plot(model1)
# Now we will check RMSE value on test_dataset
library(Metrics)
rmse(df$Item_Outlet_Sales, model1$fitted.values)
rmse(df$Item_Outlet_Sales, exp(model2$fitted.values))
rmse(df$Item_Outlet_Sales, sqrt(model2$fitted.values))
head(BMSdata)
#Prediction on test_dataset
BMStest2<- BMStest[c(1:11,13,14)]
prediction <- predict(model2,newdata = BMStest2)
BMStest2$Item_Outlet_Sales <-exp(prediction)
#Item_Identifier <-  BMStest$Item_Identifier
#Outlet_Identifier <-  BMStest$Outlet_Identifier
output.df <- as.data.frame(Item_Identifier)
output.df$Outlet_Identifier <- Outlet_Identifier 
output.df$Item_Outlet_Sales <- exp(prediction)
library(car)
some(output.df)#Sample a Few Elements of an Object
