#Clean the environment
rm(list = ls()) 

#Set working directory
setwd("/Users/monikawadhwani/Desktop/PROJECT")

#Load the librarires
libraries = c("plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine","gridExtra")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Read the csv file
df = read.csv(file = "day.csv", header = T, sep = ",", na.strings = c(" ", "", "NA"))

########DATA EXPLORATORY ANALYSIS#########
#First few rows
head(df)

#Dimensions of data
dim(df)

#Column names
names(df)

#Structure of variables
str(df)

#########FEATURE ENGINEERING#############
#Create columns
df$normalised_temp <- df$temp*39
df$normalised_feel_temp <- df$atemp*50
df$normalised_windspeed <- df$windspeed*67
df$normalised_hum = df$hum * 100

df$specific_season = factor(x = df$season, levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
df$specific_yr = factor(x = df$yr, levels = c(0,1), labels = c("2011","2012"))
df$specific_holiday = factor(x = df$holiday, levels = c(0,1), labels = c("Working day","Holiday"))
df$specific_weathersit = factor(x = df$weathersit, levels = c(1,2,3,4), 
                               labels = c("Clear","Cloudy/Mist","Rain/Snow/Fog","Heavy Rain/Snow/Fog"))

df$weathersit = as.factor(df$weathersit)
df$season = as.factor(df$season)
df$dteday = as.character(df$dteday)
df$mnth = as.factor(df$mnth)
df$weekday = as.factor(as.character(df$weekday))
df$workingday = as.factor(as.character(df$workingday))
df$yr = as.factor(df$yr)
df$holiday = as.factor(df$holiday)

###########MISSING VALUES#############
missing_values = sapply(df, function(x){sum(is.na(x))})


#Check the distribution of categorical Data using bar graph
bar1 = ggplot(data = df, aes(x = specific_season)) + geom_bar() + ggtitle("Count of Season")
bar2 = ggplot(data = df, aes(x = specific_weathersit)) + geom_bar() + ggtitle("Count of Weather")
bar3 = ggplot(data = df, aes(x = specific_holiday)) + geom_bar() + ggtitle("Count of Holiday")
bar4 = ggplot(data = df, aes(x = workingday)) + geom_bar() + ggtitle("Count of Working day")
# ## Plotting plots together
gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)

#Check the distribution of numerical data using histogram
hist1 = ggplot(data = df, aes(x =normalised_temp)) + ggtitle("Distribution of Temperature") + geom_histogram(bins = 25)
hist2 = ggplot(data = df, aes(x =normalised_hum)) + ggtitle("Distribution of Humidity") + geom_histogram(bins = 25)
hist3 = ggplot(data = df, aes(x =normalised_feel_temp)) + ggtitle("Distribution of Feel Temperature") + geom_histogram(bins = 25)
hist4 = ggplot(data = df, aes(x =normalised_windspeed)) + ggtitle("Distribution of Windspeed") + geom_histogram(bins = 25)
gridExtra::grid.arrange(hist1,hist2,hist3,hist4,ncol=2)

#Check the distribution of numerical data using scatterplot
scat1 = ggplot(data = df, aes(x =normalised_temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike COunt")
scat2 = ggplot(data = df, aes(x =normalised_hum, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike COunt")
scat3 = ggplot(data = df, aes(x =normalised_feel_temp, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point() + xlab("Feel Temperature") + ylab("Bike COunt")
scat4 = ggplot(data = df, aes(x =normalised_windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike COunt")
gridExtra::grid.arrange(scat1,scat2,scat3,scat4,ncol=2)

#Check for outliers in data using boxplot
cnames = colnames(df[,c("normalised_temp","normalised_feel_temp","normalised_windspeed","normalised_hum")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = df)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn3,gn2,gn4,ncol=2)

#Remove outliers in Windspeed
val = df[,19][df[,19] %in% boxplot.stats(df[,19])$out]
df = df[which(!df[,19] %in% val),]

#Check for multicollinearity using VIF
df = df[,c("instant","temp","atemp","hum","windspeed")]
vifcor(df)

#Check for collinearity using corelation graph
corrgram(df, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Remove the unwanted variables

df <- subset(df, select = -c(holiday,instant,dteday,atemp,casual,registered,normalised_temp,normalised_feel_temp,normalised_windspeed,
                             normalised_hum,specific_season,specific_yr,specific_holiday,specific_weathersit))

rmExcept(keepers = "df")


################## LINEAR REGRESSION ###################
#MAPE: 12.17%
#RMSE: 673
#Accuracy: 87.83%
#MAE: 494
#Adjusted R squared: 0.8373
#F-statistic: 110.2

#Divide the data into train and test
set.seed(123)
train_index = sample(1:nrow(df), 0.8 * nrow(df))
train = df[train_index,]
test = df[-train_index,]

#Train the data using linear regression
model_lr = lm(formula = cnt~., data = train)

#Check the summary of the model
summary(model_lr)

#Predict the test cases
predictions_lr = predict(model_lr, test[,-10])

#Create dataframe for actual and predicted values
df_1 = cbind(df,predictions_lr)
head(df_1)

#Calculate MAPE
regr.eval(trues = test[,10], preds = predictions_lr, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], predictions_lr)

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="green")
lines(predictions_lr,col="blue")

#Predict a sample data
predict(model_lr,test[2,])


#############DECISION TREE#################
#MAPE: 17.47%
#MAE: 684
#RMSE: 864.8
#Accuracy: 82.53%



#rpart for regression
model_dt = rpart(cnt ~ ., data = train, method = "anova")

#Predict the test cases
predictions_dt = predict(model_dt, test[,-10])

#Create dataframe for actual and predicted values
df_1 = data.frame("actual"=test[,10], "pred"=predictions_dt)
head(df_1)

#calculate MAPE
regr.eval(trues = test[,10], preds = predictions_dt, stats = c("mae","mse","rmse","mape"))

#calculate MAPE
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test[,10], predictions_dt)

################RANDOM FOREST#####################
#MAPE: 10.68%
#MAE: 392
#RMSE: 535
#Accuracy: 89.32%

#Train the data using random forest
model_rf = randomForest(cnt~., data = train, ntree = 500)

#Predict the test cases
predictions_rf = predict(rf_model, test[,-10])

#Create dataframe for actual and predicted values
df_1 = cbind(df_1,predictions_rf)
head(df_1)

#Calculate MAPE
regr.eval(trues = test[,10], preds = predictions_rf, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], predictions_rf)

