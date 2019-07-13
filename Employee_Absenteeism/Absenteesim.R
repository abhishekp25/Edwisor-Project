#Clean the environment
rm(list = ls())

#Seting the  working directory
setwd("D:/Rfiles")

#Loading the librarires which would be needed
libraries = c("rpart.plot","plyr","dplyr", "ggplot2","rpart","DMwR","randomForest","usdm","DataCombine")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Read the csv file
absent = read.csv(file = "Absent.csv", header = T)





#####EXPLORE THE DATA


#number of rows and columns
dim(absent)

#Observe top 5 rows
head(absent)

#Structure of variables
str(absent)

#Transform data types
absent$ID = as.factor(as.character(absent$ID))
##as reason cant be zero
absent$Reason.for.absence[absent$Reason.for.absence %in% 0] = 20
absent$Reason.for.absence = as.factor(as.character(absent$Reason.for.absence))
#as month cant be zero
absent$Month.of.absence[absent$Month.of.absence %in% 0] = NA
absent$Month.of.absence = as.factor(as.character(absent$Month.of.absence))
absent$Day.of.the.week = as.factor(as.character(absent$Day.of.the.week))
absent$Seasons = as.factor(as.character(absent$Seasons))
absent$Disciplinary.failure = as.factor(as.character(absent$Disciplinary.failure))
absent$Education = as.factor(as.character(absent$Education))
absent$Son = as.factor(as.character(absent$Son))
absent$Social.drinker = as.factor(as.character(absent$Social.drinker))
absent$Social.smoker = as.factor(as.character(absent$Social.smoker))
absent$Pet = as.factor(as.character(absent$Pet))



#Make a copy of data
df = absent


######MISSING VALUE ANALYSIS
#Get number of missing values(use of sapply)
sapply(df,function(x){sum(is.na(x))})
missing_values = data.frame(sapply(df,function(x){sum(is.na(x))}))

#Get the rownames as new column
missing_values$Variables = row.names(missing_values)

#Reseting the row names 
row.names(missing_values) = NULL

#Renaming the column
names(missing_values)[1] = "Miss_perc"

#Calculate missing percentage(so that we can see the highest percentage)
missing_values$Miss_perc = ((missing_values$Miss_perc/nrow(absent)) *100)

#Reorder the columns
missing_values = missing_values[,c(2,1)]

#Sort the rows according to decreasing missing percentage
missing_values = missing_values[order(-missing_values$Miss_perc),]

# we cannot drop any column because there isn't any column having missing value greater than 30%


#Create missing value and impute using mean, median and knn
df1 =df
df2= df
df3= df


# we are using Body mass index becase it has max missing value , 

  df1[["Body.mass.index"]][10]
  df2[["Body.mass.index"]][10]
  df3[["Body.mass.index"]][10]
  
  
  # here the value we have choosen to remove is 29
  
  df1[["Body.mass.index"]][10] =NA
  df2[["Body.mass.index"]][10]= NA
  df3[["Body.mass.index"]][10] = NA
  
  # checking for different values
  
  #mean

  df1[["Body.mass.index"]][10] = mean(df1$Body.mass.index, na.rm = T)
  df1[["Body.mass.index"]][10] #the value we got is 26.68
  
  
  #median
  

  df2[["Body.mass.index"]][10] = median(df2$Body.mass.index, na.rm = T)
  df2[["Body.mass.index"]][10] #the value we got is 25
  
  
  #KNN
  df3 = knnImputation(data = df3, k = 5)
  df3[["Body.mass.index"]][10]  # we got the value 29.17163  , so we going ahead with KNN
  

  
# implementing KNN on original dataframe    
df = knnImputation(data = df, k = 5)

#Check if any missing values
sum(is.na(df))





######GRAPHS
# numerical data
numeric_index = sapply(df, is.numeric)
numeric_data = df[,numeric_index]

#Distribution of factor data using bar plot
bar1 = ggplot(data = df, aes(x = ID)) + geom_bar() + ggtitle("ID") + theme_dark()
bar2 = ggplot(data = df, aes(x = Reason.for.absence)) + geom_bar() +  ggtitle("Reason for absence") + theme_dark()
bar3 = ggplot(data = df, aes(x = Month.of.absence)) + geom_bar() + ggtitle("Month") + theme_dark()
bar4 = ggplot(data = df, aes(x = Disciplinary.failure)) + geom_bar() + ggtitle("Disciplinary failure") + theme_dark()
bar5 = ggplot(data = df, aes(x = Education)) + geom_bar() + ggtitle("Education") + theme_dark()
bar6 = ggplot(data = df, aes(x = Son)) + geom_bar() + ggtitle("Son") + theme_dark()
bar7 = ggplot(data = df, aes(x = Social.smoker)) + geom_bar() + ggtitle("Social smoker") + theme_dark()

#making a grid
gridExtra::grid.arrange(bar1,bar2,ncol=1)
gridExtra::grid.arrange(bar3,bar4,ncol=1)
gridExtra::grid.arrange(bar5,bar6,bar7,ncol=2)

#Check the distribution of numerical data using histogram
hist1 = ggplot(data = numeric_data, aes(x =Transportation.expense)) + ggtitle("Distribution of : Transportation.expense") + geom_histogram(bins = 25)
hist2 = ggplot(data = numeric_data, aes(x =Height)) + ggtitle("Distribution of: Height") + geom_histogram(bins = 25)
hist3 = ggplot(data = numeric_data, aes(x =Body.mass.index)) + ggtitle("Distribution of: Body mass index") + geom_histogram(bins = 25)
hist4 = ggplot(data = numeric_data, aes(x =Absenteeism.time.in.hours)) + ggtitle("Distribution of : Absenteeism time in hours") + geom_histogram(bins = 25)
hist5 = ggplot(data = numeric_data, aes(x =Age)) + ggtitle("Distribution of : Age") + geom_histogram(bins = 25)
hist6 = ggplot(data = numeric_data, aes(x =Weight)) + ggtitle("Distribution of : Weight") + geom_histogram(bins = 25)

#making a grid
gridExtra::grid.arrange(hist1,hist2,ncol=1)
gridExtra::grid.arrange(hist3,hist4,ncol=1)
gridExtra::grid.arrange(hist5,hist6,ncol=1)







#####OUTLIER ANALYSIS
# here we will replace the outliers with Knn method.
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
gridExtra::grid.arrange(box3,box4,ncol=2)
gridExtra::grid.arrange(box5,box6,ncol=2)
gridExtra::grid.arrange(box7,box8,ncol=2)
gridExtra::grid.arrange(box9,ncol=2)

#Get the names of numeric columns
numeric_columns = colnames(numeric_data)

#Replacing all outlier data with NA
for(i in numeric_columns){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(paste(i,length(val)))
  df[,i][df[,i] %in% val] = NA
}

#Check number of missing values
sapply(df,function(x){sum(is.na(x))})

#Get number of missing values after replacing outliers as NA
missing_values_out = data.frame(sapply(df,function(x){sum(is.na(x))}))
missing_values_out$Columns = row.names(missing_values_out)
row.names(missing_values_out) = NULL
names(missing_values_out)[1] = "Missing_percentage"
missing_values_out$Missing_percentage = ((missing_values_out$Missing_percentage/nrow(absent)) *100)
missing_values_out = missing_values_out[,c(2,1)]
missing_values_out = missing_values_out[order(-missing_values_out$Missing_percentage),]
missing_values_out

#Compute the NA values using KNN imputation
df = knnImputation(df, k = 5)

#Check if any missing values
sum(is.na(df))







#######FEATURE SELECTION

#usdm library  # done for the checking correlation
#Check for multicollinearity 
vifcor(numeric_data)

#Check for multicollinearity using corelation graph
corrdf = cor(numeric_data,numeric_data, method = "spearman" )

#As body mass index has high correlation with weight , so we are dropping body mass index
# and 'distance' because it has very low correaltion with target variable 
to_drop <- c("Body.mass.index","Distance.from.Residence.to.Work")
df1=df[ ,  -which(names(df) %in% to_drop)]





######FEATURE SCALING

#hist(df$Absenteeism.time.in.hours)
df = df1
#Remove dependent variable
numeric_index = sapply(df,is.numeric)
numeric_data = df[,numeric_index]
numeric_columns = names(numeric_data)
numeric_columns = numeric_columns[-9]



#Normalization of continuous variables
for(i in numeric_columns){
  print(i)
  df[,i] = (df[,i] - min(df[,i]))/
    (max(df[,i]) - min(df[,i]))
}

#Get the names of factor variables
factor_columns = names(factor_data)


########DECISION TREE


#Splitting the  data (80-20 percent)
set.seed(1)
train_index = sample(1:nrow(df), 0.8*nrow(df))        
train = df[train_index,]
test = df[-train_index,]


#Build decsion tree using rpart
dt_model = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")
# here we can try any method other than anova ,
#one of "anova", "poisson", "class" or "exp".
#If method is missing then the routine tries to make an intelligent guess. 

#Ploting the tree
rpart.plot(dt_model)


#Perdict for test cases 
dt_predictions = predict(dt_model, test[,-19])

df3= data.frame((dt_predictions))

#Create data frame for actual and predicted values
df_pred = data.frame("actual"= test[,19], "dt_pred"=dt_predictions)


# calculating error (mae)
mae_dt = regr.eval(test[,19], dt_predictions , stats = c ('mae'))
print(mae_dt)
print("accuracy ")
print((1-(mae_dt))*100)


########RANDOM FOREST


#Training the model using training data
rf_model = randomForest(Absenteeism.time.in.hours~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-19])

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)


# calculating error (mae)
mae_rf = regr.eval(test[,19], rf_predictions , stats = c ('mae'))
print(mae_rf)
print("accuracy ")
print((1-(mae_rf))*100)




########LINEAR REGRESSION

train_for_lr = train
test_for_lr = test

train_for_lr$ID = NULL
test_for_lr$ID = NULL
train_for_lr$Reason.for.absence = NULL
test_for_lr$Reason.for.absence = NULL

#we are removing these two columns because they are creating new levels
##Training the model using training data
lr_model = lm(formula = Absenteeism.time.in.hours~., data = train_for_lr)
# summary of lr_model
summary(lr_model)


#Predict the test cases
lr_predictions = predict(lr_model, test_for_lr[,-19])

#Creating a new dataframe for actual and predicted values
df_pred = cbind(df_pred,lr_predictions)


# calculating error (mae)
mae_lr = regr.eval(test[,19], lr_predictions , stats = c ('mae'))
print(mae_lr)
print("accuracy ")
print((1-(mae_lr))*100)


