wd <- "C:\\Users\\46650\\Desktop\\Coursework\\"
setwd(wd)
#install.packages("VIM")
library(VIM)

# Load the data
data <- fread("C:\\Users\\46650\\Desktop\\Coursework\\database.csv",stringsAsFactors = T) 
data <- data[,c("Latitude","Longitude","Depth","Magnitude")]
#Check the data type
str(data)
#Missing value analysis
aggr(data)

#Correlation analysis
corrplot::corrplot(corr = cor(data))

#Split the data in to train set and test set
set(1)
fold <- sample(1:length(data$Latitude),round(length(data$Latitude)*0.8))
train_data <- data[fold,]
test_data <- data[-fold,]

#Build Random Forest model
#install.packages("randomForest")
library(randomForest)
random <- randomForest(Magnitude~Latitude+Longitude+Depth,train_data,
                       ntree = 1000,  #Number of trees
                       #mtry=3,       #Number of features of each tree
                       #importance=T, #Calculate variables' feature importance
                       #proximity=T  #Calculate the similarity between the observations
)
plot(random)        #Tree quantity variation error diagram determine the ntree


random <- randomForest(Magnitude~Latitude+Longitude+Depth,train_data,
                       ntree = 200,  #Number of trees
                       #mtry=3,       #Number of features of each tree
                       #importance=T, #Calculate variables' feature importance
                       #proximity=T  #Calculate the similarity between the observations
)
random$importance   #Variables' feature importance
random$mse        
pred<-predict(random,newdata=test_data)  
accuracy <- mean (1-abs((pred-test_data$Magnitude)/test_data$Magnitude))  # accuracy of prediction
accuracy

#Train set prediction
train_pred <- predict(random,train_data)
#install.packages("pROC")
library(pROC)
train_roc <- roc(train_data$Magnitude,as.numeric(train_pred))
plot(train_roc,print.auc=T,auc.polygon=T,grid=c(0.1,0.2),
     grid.col=c("green","red"),max.auc.polygon=T,
     auc.polygon.col="skyblue",print.thres=T,main="ROC Curve")



