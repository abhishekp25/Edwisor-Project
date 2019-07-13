#Clean the environment
rm(list = ls())

#Seting the  working directory
setwd("D:/Rfiles")

#Loading the librarires which would be needed
libraries = c("rpart.plot","plyr","dplyr", "ggplot2","rpart",'propagate',"DMwR","randomForest","usdm","DataCombine")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Read the csv file and making insant as index column
df = read.csv(file = "bikedata.csv", header = T,row.names="instant")


#####EXPLORE THE DATA

#number of rows and columns
dim(df)

#Observe top 5 rows
head(df)

#Structure of variables
str(df)

#renaming the column
colnames(df)[colnames(df)=="dteday"] <- "dateday"
colnames(df)[colnames(df)=="mnth"] <- "month"
colnames(df)[colnames(df)=="yr"] <- "year"
colnames(df)[colnames(df)=="cnt"] <- "count"


#Transform data types

#making factor
df$season = as.factor(df$season)
df$year = as.factor(df$year)
df$month = as.factor(df$month)
df$holiday = as.factor(df$holiday)
df$weekday = as.factor(df$weekday)
df$workingday = as.factor(df$workingday)
df$weathersit = as.factor(df$weathersit)
#for numeric 

df$temp = as.numeric(df$temp)
df$atemp = as.numeric(df$atemp)
df$hum = as.numeric(df$hum)
df$windspeed = as.numeric(df$windspeed)
df$casual = as.numeric(df$casual)
df$registered = as.numeric(df$registered)
df$count = as.numeric(df$count)


#checking the skewness and kurtosis of the numerical columns
skewness(df$temp)
skewness(df$atemp)
skewness(df$hum)
skewness(df$windspeed)
skewness(df$casual)
skewness(df$registered)
skewness(df$count)

kurtosis(df$temp)
kurtosis(df$atemp)
kurtosis(df$hum)
kurtosis(df$windspeed)
kurtosis(df$casual)
kurtosis(df$registered)
kurtosis(df$count)

# the skew limit is -1 to 1 , so from numercial variables temp , atemp , hum , windspeed, casual , registered , cnt .
#so thia casual (1.2 ) is highy skewed and others are modererly skewed .
#so it means that there might be chnaces of having outliers in casual


###################################  analysis

#Check the distribution of numerical data using histogram
hist1 = ggplot(data = df, aes(x =temp)) + ggtitle("Distribution of : temp") + geom_histogram(bins = 25)
hist2 = ggplot(data = df, aes(x =atemp)) + ggtitle("Distribution of: atemp") + geom_histogram(bins = 25)
hist3 = ggplot(data = df, aes(x =hum)) + ggtitle("Distribution of: hum") + geom_histogram(bins = 25)
hist4 = ggplot(data = df, aes(x =windspeed)) + ggtitle("Distribution of : windspeed") + geom_histogram(bins = 25)
hist5 = ggplot(data = df, aes(x =casual)) + ggtitle("Distribution of : casual") + geom_histogram(bins = 25)
hist6 = ggplot(data = df, aes(x =registered)) + ggtitle("Distribution of : registered") + geom_histogram(bins = 25)

#making a grid
gridExtra::grid.arrange(hist1,hist2,ncol=1)
gridExtra::grid.arrange(hist3,hist4,ncol=1)
gridExtra::grid.arrange(hist5,hist6,ncol=1)

#Distribution of factor data using bar plot
bar1 = ggplot(data = df, aes(x = season)) + geom_bar() + ggtitle("season") + theme_dark()
bar2 = ggplot(data = df, aes(x = year)) + geom_bar() +  ggtitle("year") + theme_dark()
bar3 = ggplot(data = df, aes(x = month)) + geom_bar() + ggtitle("Month") + theme_dark()
bar4 = ggplot(data = df, aes(x = holiday)) + geom_bar() + ggtitle("holiday") + theme_dark()
bar5 = ggplot(data = df, aes(x = weekday)) + geom_bar() + ggtitle("weekday") + theme_dark()
bar6 = ggplot(data = df, aes(x = workingday)) + geom_bar() + ggtitle("workingday") + theme_dark()
bar7 = ggplot(data = df, aes(x = weathersit)) + geom_bar() + ggtitle("weathersit") + theme_dark()

#making a grid
gridExtra::grid.arrange(bar1,bar2,ncol=1)
gridExtra::grid.arrange(bar3,bar4,ncol=1)
gridExtra::grid.arrange(bar5,bar6,bar7,ncol=2)



######MISSING VALUE ANALYSIS
#Get number of missing values(use of sapply)
sapply(df,function(x){sum(is.na(x))})

# we dont  have any missing value here

##################################outliers

#Get the data with only numeric columns
numeric_index = sapply(df, is.numeric)
numeric_data = df[,numeric_index]

#Get the data with only factor columns
factor_data = df[,!numeric_index]

#Check for outliers using boxplots

for(i in 1:ncol(numeric_data)) {
  assign(paste0("box",i), ggplot(data = df, aes_string(y = numeric_data[,i]))
         +stat_boxplot(geom = "errorbar", width = 1)
         +geom_boxplot(outlier.colour = "red", fill = "grey", outlier.size = 1) 
         +labs(y = colnames(numeric_data[i]))
         +ggtitle(paste("Boxplot: ",colnames(numeric_data[i]))))
}


#Arrange the plots in grids
gridExtra::grid.arrange(box1,box2,ncol=2)
gridExtra::grid.arrange(box3,box4,ncol=2)
gridExtra::grid.arrange(box5,box6,ncol=2)
gridExtra::grid.arrange(box7,ncol=1)

#boxplot(numeric_data)
# we have outliers in windspeed , casual column
# we will Replace all outlier data with NA
#but first we need to check which method to use for replacing the NA


#CHECKING WHICH METHOD TO USE

#Create missing value and impute using mean, median and knn
df1 =df
df2= df
df3= df


# we are using casual becase it has max missing value , 

df1[["casual"]][2]
df2[["casual"]][2]
df3[["casual"]][2]


# here the value we have choosen to remove is 131

df1[["casual"]][2] =NA
df2[["casual"]][2]= NA
df3[["casual"]][2] = NA

# checking for different values

#mean

df1[["casual"]][2] = mean(df1$casual, na.rm = T)
df1[["casual"]][2] #the value we got is 849.1589


#median


df2[["casual"]][2] = median(df1$casual, na.rm = T)
df2[["casual"]][2] #the value we got is 721


#KNN
df3 = knnImputation(data = df3, k = 3)
df3[["casual"]][2]  # we got the value 349  , so we going ahead with KNN



#Replacing all outlier data with NA
numeric_columns = colnames(numeric_data)

for(i in numeric_columns){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(paste(i,length(val)))
  df[,i][df[,i] %in% val] = NA
}

# KNN INPUTATION METHOD TO REPLACE THE OUTLIERS on DF

df[,9:15] = knnImputation(df[ ,9:15], k = 3)

# CHECKING BOXPLOT ------------------------------AGAIN--------
for(i in 1:ncol(numeric_data)) {
  assign(paste0("box_after",i), ggplot(data = df, aes_string(y = numeric_data[,i]))
         +stat_boxplot(geom = "errorbar", width = 1)
         +geom_boxplot(outlier.colour = "red", fill = "grey", outlier.size = 1) 
         +labs(y = colnames(numeric_data[i]))
         +ggtitle(paste("Boxplot: ",colnames(numeric_data[i]))))
}

#Arrange the plots in grids
gridExtra::grid.arrange(box_after1,box_after2,ncol=2)
gridExtra::grid.arrange(box_after3,box_after4,ncol=2)
gridExtra::grid.arrange(box_after5,box_after6,ncol=2)
gridExtra::grid.arrange(box_after7,ncol=1)


#again outliers on hum and windspeed (as these are very less in number so we can ignore them)

val_windspeed = df$windspeed[df$windspeed %in% boxplot.stats(df$windspeed)$out]
df = df[which(!df$windspeed%in%val_windspeed),]


val_hum = df$hum[df$hum %in% boxplot.stats(df$hum)$out]
df = df[which(!df$hum%in%val_hum),]


# CASUAL STILL HAS OUTLIER BUT WINDSPEED  and hum HASNT
numeric_index = sapply(df, is.numeric)
numeric_data = df[,numeric_index]


#######FEATURE SELECTION

#usdm library  # done for the checking correlation
#Check for multicollinearity 
vifcor(numeric_data)

#Check for multicollinearity using corelation graph
corrdf = cor(numeric_data,numeric_data, method = "spearman" )

# dropping the casual and registerd  
to_drop <- c("casual","registered","atemp")
df=df[ ,  -which(names(df) %in% to_drop)]



# removing the date column as it wont be used in model
df=df[ ,  -which(names(df) %in% "dateday")]


########DECISION TREE


#Splitting the  data (80-20 percent)
set.seed(1)
train_index = sample(1:nrow(df), 0.8*nrow(df))        
train = df[train_index,]
test = df[-train_index,]


#Build decsion tree using rpart
dt_model = rpart(count~ ., data = train, method = "anova")
# here we can try any method other than anova ,
#one of "anova", "poisson", "class" or "exp".
#If method is missing then the routine tries to make an intelligent guess. 

#Ploting the tree
rpart.plot(dt_model)


#Perdict for test cases 

dt_predictions = predict(dt_model, test[,-11])

#dataframe_pred_dt= data.frame((dt_predictions))

#Create data frame for actual and predicted values
df_pred_dt = data.frame("actual"= test[,11], "dt_pred"=dt_predictions)


# analyse relationship between actual and predicted count
ggplot(df_pred_dt, aes(x= actual ,y=dt_predictions)) +
  geom_point()+
  geom_smooth()

############# ERROR AND R SQ ###################



#calculate MAE
MAE = function(y_true, y_pred){
  mean(abs((y_true - y_pred)/y_true))
}

#R2
R2 =function(y_true, y_pred){
  rss =sum((y_pred - y_true) ^ 2)  ## residual sum of squares
  tss =sum((y_true - mean(y_true)) ^ 2)  ## total sum of squares
  rsq =1 - rss/tss
  
}



#accuracy and error(decision tree)

error_dt = MAE(test[,11], dt_predictions)
print("error decision tree ")
print(error_dt)
print("accuracy of decision tree ")
error_dt_per = (error_dt*100)
print(100-(error_dt_per))

#0.181949
#81.8051

r2_value_dt = R2(test[,11], dt_predictions)
print("R2 value for  decision tree ")
print(r2_value_dt)


########RANDOM FOREST


#Training the model using training data
rf_model = randomForest(count~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-11])


#Create data frame for actual and predicted values
df_pred_rf = data.frame("actual"= test[,11], "rf_predictions"=rf_predictions)

# analyse relationship between actual and predicted count
ggplot(df_pred_rf, aes(x= actual ,y=rf_predictions)) +
  geom_point()+
  geom_smooth()



#errors and accuracy
error_rf= MAE(test[,11], rf_predictions)
error_rf_per = error_rf*100
print(error_rf)
print("accuracy of Random forest ")
print(100-(error_rf_per))


r2_value_rf = R2(test[,11], rf_predictions)
print("R2 value for  random forest ")
print(r2_value_rf)




########LINEAR REGRESSION
#check multicollearity
library(usdm)
#converting multilevel categorical variable into ineger again
df$season=as.integer(df$season)
df$month=as.integer(df$month)
df$year=as.integer(df$year)
df$holiday=as.integer(df$holiday)
df$weekday=as.integer(df$weekday)
df$workingday=as.integer(df$workingday)
df$weathersit=as.integer(df$weathersit)

vif(df[,1:11])

vifcor(df[,1:11], th = 0.9)

# develop Linear Regression  model

set.seed(1)
train_index = sample(1:nrow(df), 0.8*nrow(df))        
train_lm = df[train_index,]
test_lm = df[-train_index,]

#run regression model
lm_model = lm(count ~., data = train_lm)

#Summary of the model
summary(lm_model)


# observe the  residuals and   coefficients  of the linear regression model
# Predict  the Test data 

#Predict
lm_predictions = predict(lm_model, test_lm[,-11])


#Create data frame for actual and predicted values
df_pred_lm = data.frame("actual"= test[,11], "lm_predictions"=lm_predictions)

# analyse relationship between actual and predicted count
ggplot(df_pred_lm, aes(x= actual ,y=lm_predictions)) +
  geom_point()+
  geom_smooth()

# Evaluate Linear Regression Model
#MAE

error_lm = MAE(test_lm[,11], lm_predictions)
error_lm_per = error_lm*100
print("accuracy  of linear model")
print(100-(error_lm_per))


r2_value_lm = R2(test[,11], lm_predictions)
print("R2  value for linear model ")
print(r2_value_lm)



#############
print("accuracy of decision tree ")
print(100-(error_dt_per))
print("R2 sq value for  decision tree ")
print(r2_value_dt)



print("accuracy of Random forest ")
print(100-(error_rf_per))

print("R2 sq value for  random forest ")
print(r2_value_rf)





print("accuracy  of linear model")
print(100-(error_lm_per))

print("R2 sq value for linear model ")
print(r2_value_lm)





write.csv(df_pred_rf, file = 'output_bikerent_R.csv', row.names = FALSE, quote=FALSE)
 


