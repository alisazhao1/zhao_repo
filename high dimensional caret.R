setwd('~/Downloads/')

#install.packages('caret')
#install.packages('tidyverse')
#install.packages('e1071', dependencies=TRUE)

library(caret)
library(tidyverse) 

#import dataset 
data = read.csv('orientation_data_sub-02_good_trials.csv',header = TRUE )
data = data[,2:ncol(data)]
data_pcr = data[,2:(ncol(data)-1)]
# X_data = data[,2:(ncol(data)-1)]
#Y_data = data[,ncol(data)]
data$y1 =  as.factor(data$orientations ==  0)
data$y2 = as.factor(data$orientations == 45)
data$y3 =  as.factor(data$orientations == 90)
data$y4 = as.factor(data$orientations == 135)

#Step 1: splitting training set and test set
train_index <- sample(1:nrow(data), 0.1 * nrow(data))
train <- data[train_index,]
test <- data[-train_index,]

#Step 2: run models using caret, k =5
train_control <- trainControl(method = "cv", number = 5)

head(train)
Model1 <- train(y1~. - y2 -y3 -y4, train, 
                method = "glm", 
                family=binomial, 
                preProcess=c("center", "scale", "pca"), 
                trControl = train_control)

#Step 3: calculate sensitivity and accuracy of the trainig set
pred.tr1 = predict(Model1,data=train,type="raw") 
confmat.tr1  <- confusionMatrix(train$y1, pred.tr1)
confmat.tr1

#calculate sensitivity and accuracy of the testing set
pred.te1 = predict(Model,newdata=test,type="raw") 
confmat.te1  <- confusionMatrix(test$y1, pred.te1)
confmat.te1

##model 2

#repeat step 2 and 3 with y2 as DV

#Step 2: run models using caret, k =5
Model2 <- train(y2~. - y1 -y3 -y4, train, 
               method = "glm", 
               family=binomial, 
               preProcess=c("center", "scale", "pca"), 
               trControl = train_control)

#Step 3: calculate sensitivity and accuracy
#trainig set
pred.tr2 = predict(Model2,data=train,type="raw") 
confmat.tr2  <- confusionMatrix(train$y2, pred.tr2)
confmat.tr2

# testing set
pred.te2 = predict(Model2,newdata=test,type="raw") 
confmat.te2  <- confusionMatrix(test$y2, pred.te2)

##model 3
#repeat step 2 and 3 with y3 as DV

#Step2: run models using caret, k =5
Model3 <- train(y3~. - y1 -y2 -y4, train, 
               method = "glm", 
               family=binomial, 
               preProcess=c("center", "scale", "pca"), 
               trControl = train_control)

#Step 3: calculate sensitivity and accuracy 
#trainig set
pred.tr3 = predict(Model3,data=train,type="raw") 
confmat.tr3  <- confusionMatrix(train$y3, pred.tr3)
confmat.tr3

#testing set
pred.te3 = predict(Model3,newdata=test,type="raw") 
confmat.te3  <- confusionMatrix(test$y3, pred.te3)
confmat.te3

##model 4
#repeat step 2 and 3 with y4 as DV

#Step2: run models using caret, k =5
Model4 <- train(y4~. - y1 -y2 -y3, train, 
                method = "glm", 
                family=binomial, 
                preProcess=c("center", "scale", "pca"), 
                trControl = train_control)

#Step 3: calculate sensitivity and accuracy
#trainig set
pred.tr4 = predict(Model4,data=train,type="raw") 
confmat.tr4  <- confusionMatrix(train$y4, pred.tr4)
confmat.tr4

#testing set
pred.te4 = predict(Model4,newdata=test,type="raw") 
confmat.te4  <- confusionMatrix(test$y4, pred.te4)
confmat.te4



