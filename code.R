library(dplyr) # for data manipulation
library(stringr) # for data manipulation
library(caret) # for sampling
library(caTools) # for train/test split
library(ggplot2) # for data visualization
library(corrplot) # for correlations
# function to set plot height and width


fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}
# loading the data
df = file.choose()


df=read.csv(df)

#basic data exploration
head(df)

str(df)

#Describing Data other alternative to str
glimpse(df)

summary(df)

# checking missing values
colSums(is.na(df))

# checking class imbalance
table(df$Class)

#percentage of fraud and not fraud
prop.table(table(df$Class))

#data visualization

labels=c("Not_Fraud","Fraud")
labels=paste(labels,round(100*prop.table(table(df$Class)),2))
labels=paste(labels,"%")
pie(table(df$Class),col=c("red","yellow"),main="FRAUD AND NOT FRAUD PERCENTAGE")

#ggplot
fig(12, 8)
common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data = df, aes(x = factor(Class), 
                      y = prop.table(stat(count)), fill = factor(Class),
                      label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_x_discrete(labels = c("no fraud", "fraud"))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'Class', y = 'Percentage') +
  ggtitle("Distribution of class labels") +
  common_theme



#Distribution of transaction amount by class
fig(14, 8)
ggplot(df, aes(x = factor(Class), y = Amount)) + geom_jitter() + 
  labs(x = 'Class', y = 'Amount') +
  ggtitle("Distribution of transaction amount by class") + common_theme

#Correlation of anonymised variables and 'Amount
fig(14, 8)
correlations <- cor(df,method="pearson")
corrplot(correlations,order = "alphabet", number.cex = .9, method = 'pie', type = "full", tl.cex=0.8,tl.col = "black")


#Data Preparation

#Remove 'Time' variable
df <- df[,-1]

head(df)

#Change 'Class' variable to factor
df$Class <- as.factor(df$Class)
levels(df$Class) <- c("Not_Fraud", "Fraud")

#Scale numeric variables

df[,-30] <- scale(df[,-30])

head(df)

#split into test and train
set.seed(123)
split <- sample.split(df$Class, SplitRatio = 0.7)
train <-  subset(df, split == TRUE)
test <- subset(df, split == FALSE)
dim(train)
dim(test)
# class ratio initially
table(train$Class)
# downsampling
set.seed(9560)

down_train <- downSample(x = train[, -ncol(train)],
                         y = train$Class)
table(down_train$Class)
# upsampling
set.seed(9560)
up_train <- upSample(x = train[, -ncol(train)],
                     y = train$Class)

table(up_train$Class)
#decision tree

library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Class ~ . , df, method = 'class')
predicted_val <- predict(decisionTree_model, df, type = 'class')
probability <- predict(decisionTree_model, df, type = 'prob')
rpart.plot(decisionTree_model)


#fitting random forest classifier model 
library(randomForest)
set.seed(123)
classifier = randomForest(x = train[-30],
                          y = train$Class,
                          ntree = 50)
# Predicting the Test set results
y_pred = predict(classifier, newdata = test[-31])
# Making the Confusion Matrix
library(caret)
confusionMatrix(test$Class,y_pred)
x = up_train[, -30]
y = up_train[,30]
library(Rborist)
rf_fit <- Rborist(x, y, ntree = 1000, minNode = 20, maxLeaf = 13)
rf_pred <- predict(rf_fit, test[,-30], ctgCensus = "prob")
prob <- rf_pred$prob

library(ROSE)
roc.curve(test$Class, prob[,2], plotit = TRUE)

#logistic model
Logistic_Model=glm(Class~.,test,family=binomial())
summary(Logistic_Model)

plot(Logistic_Model)

reg_model <- predict(Logistic_Model , test , type ="response")

table(Actual = test$Class , Predicted = reg_model> 0.5)
accuracy <- (56839 + 73) / (56839 + 73 + 45 + 4)
accuracy <- round((accuracy * 100),2)
accuracy


library(pROC)
lr.predict <- predict(Logistic_Model,train, probability = TRUE)


auc.gbm = roc(train$Class, lr.predict, plot = TRUE, col = "blue")


