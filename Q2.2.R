library(MASS)
library(caret)
library(e1071)
library(class)
library(rpart)
library(rpart.plot)
library(party)
library(AUC)

#Read File
setwd("C:/Users/Malvika/Documents/Machine Learning/FinalProject")
hmda.dumm = readRDS('hmda_dummy.RDS')
hmda = readRDS('hmda_cleaned.RDS')


#Divide into train and test
set.seed(1)
training = sample.int(407182, 285027)
train = hmda.dumm[training ,]
test = hmda.dumm[-training ,]

#----------------------------------------------------------------------------------
#Q2.2 Non-Linear Classification

# Classifier 1: QDA

#The column hoepa_status_ name is not giving much information since there are only 54 values for
#hoepa loan as against 284973 values for not a hoepa loan. 
#Since this column causes a rank deficiency error if included, I have removed the hoepa column
#before running qda.

train = train[, -70]
test = test[, -70]

#Q2.2.1 First Non-linear Classifier in scenario 1 
qda.exclude = qda(action_taken_name~. -applicant_income_000s -loan_amount_000s, data = train)

#Classifier performance on train
predict_qda.train.exclude = predict(qda.exclude, train[,-87])
confusionMatrix(predict_qda.train.exclude$class, train$action_taken_name) 
#Inference: Accuracy rate = 59.51 ; Specificity = 62.11%

#Classifier performance on test
predict_qda.test.exclude = predict(qda.exclude, test[, -87])
confusionMatrix(predict_qda.test.exclude$class, test$action_taken_name) 
#Inference: Acuuracy rate = 59.35% ; Specificity = 61.67%

#Q2.2.2 First Non-linear Classifier in scenario 2
qda.include = qda(action_taken_name~., data = train)

#Classifier performance on train
predict_qda.train.include = predict(qda.include, train[, -87])
confusionMatrix(predict_qda.train.include$class, train$action_taken_name) 
#Inference: Accuracy rate = 55.79% ; Specificity = 68%

#Classifier performance on test
predict_qda.test.include = predict(qda.include, test[, -87])
confusionMatrix(predict_qda.test.include$class, test$action_taken_name) 
#Inference: Accuracy rate = 55.68% ; Specificity = 67.91%

#-------------------------------------------------------------------------

# Classifier 2: 

#Decision Tree

#Q2.2.3 Second Non-Linear classifier in scenario 1
tree.exclude = rpart(action_taken_name~. -applicant_income_000s -loan_amount_000s, data = train, method ="class")
rpart.plot(tree.exclude)

#Classifier performance on train
predict_tree.train.exclude = predict(tree.exclude, train[,-87])
predict_tree.train.exclude = 1*(predict_tree.train.exclude[,2] > 0.5)
confusionMatrix(predict_tree.train.exclude, train$action_taken_name) 

#Inference: Accuracy rate = 78.85% ; Specificity = 5%

#Classifier performance on test 
predict_tree.test.exclude = predict(tree.exclude, test[, -87])
predict_tree.test.exclude = 1*(predict_tree.test.exclude[,2] > 0.5)
confusionMatrix(predict_tree.test.exclude, test$action_taken_name) 

#Inference: Accuracy rate = 78.63% ; Specificity = 5.11%

#Prune tree
prune_train.exclude = ctree(action_taken_name~. -applicant_income_000s -loan_amount_000s, data=train)

#Performance on test
pred_prune.test.exclude = predict(prune_train.exclude, newdata=test[, -87])
pred_prune.test.exclude = 1*(pred_prune.test.exclude>0.5)
pred_prune.test.exclude = as.vector(pred_prune.test.exclude)
confusionMatrix(pred_prune.test.exclude, test$action_taken_name)

#Inference: Accuracy rate = 79.41% ; Specificity = 15%

#Q2.2.4 Second Non-Linear classifier in scenario 2
tree.include = rpart(action_taken_name~., data = train, method = "class")
rpart.plot(tree.include)

#Classifier performance on train
predict_tree.train.include = predict(tree.include, train[,-87])
predict_tree.train.include = 1*(predict_tree.train.include[,2] > 0.5)
confusionMatrix(predict_tree.train.include, train$action_taken_name) 
#Accuracy rate = 78.63% ; Specificity = 4%

#Classifier performance on test 
predict_tree.test.include = predict(tree.include, test[, -87])
predict_tree.test.include = 1*(predict_tree.test.include[,2] > 0.5)
confusionMatrix(predict_tree.test.include, test$action_taken_name) 
#Accuracy rate = 78.46% ; Specificity = 3.91%

#Prune tree
prune_train.include = ctree(action_taken_name~., data=train)

#Performance on test
pred_prune.test.include = predict(prune_train.include, newdata=test[, -87])
pred_prune.test.include = 1*(pred_prune.test.include>0.5)
pred_prune.test.include = as.vector(pred_prune.test.include)
confusionMatrix(pred_prune.test.include, test$action_taken_name)
#Accuracy rate = 79.41% ; Specificity = 15.18%

#-------------------------------------------------------------------

# Classifier 3

#KNN

#Scenario 1: Including all variables
#For KNN, the model is fit on 10% of the data because of run time constraints
set.seed(1)
training1 = sample.int(407182, 20000)
train1 = hmda.dumm[training1 ,]
cl = as.factor(train1[, 88])
accuracy_list = c()
predicted = c()
actual = c()
for(i in seq(10)){
  testing = sample.int(407182, 5000)
  test1 = hmda.dumm[testing, ]
  predict.test = knn(train = train1, test = test1 , cl , k=5)
  predicted = c(predicted, predict.test)
  actual = c(actual, test1$action_taken_name)
  misclassfied.test.include = sum(test1$action_taken_name != predict.test)
  accuracy_list[i] = 1 - misclassfied.test.include/5000
  
}
predicted = 1*(predicted == 2)
confusionMatrix(predicted, as.factor(actual))
print(mean(accuracy_list))

#Inference: Accuracy rate = 74.92%  ; Specificity = 12.33%

#Scenario 2: Exclude 2 variables

train2= train1[, -c(5,7)]
cl= as.factor(train2[, 86])
accuracy_list = c()
predicted1 = c()
actual1 = c()
for(i in seq(10)){
  testing = sample.int(407182, 5000)
  test1 = hmda.dumm[testing, ]
  test1 = test1[, -c(5,7)]
  predict.test = knn(train = train2, test = test1 , cl , k=5)
  predicted1 = c(predicted1, predict.test)
  actual1 = c(actual1, test1$action_taken_name)
  misclassfied.test.exclude = sum(test1$action_taken_name != predict.test)
  accuracy_list[i] = 1 - misclassfied.test.exclude/5000
}
predicted1 = 1*(predicted1 == 2)
confusionMatrix(predicted1, as.factor(actual1))
print(mean(accuracy_list))

#Inference: Accuracy rate = 75.12%   Specificity = 11.07%
#-----------------------------------------------------------------------

#Q2.2.7 Non-Linear Comparisons (refer to PDF)

#Q2.2.8 Non-Linear classifier -- overfitting 
#(1)QDA

#Scenario 1
x = roc(predict_qda.test.exclude$class, factor(test$action_taken_name))
auc_qda.ex = auc(x, min =0, max =1) #0.6018

#Scenario 2
x = roc(predict_qda.test.include$class, factor(test$action_taken_name))
auc_qda.ex = auc(x, min =0, max =1) #0.6007

#(2) Prune Tree
#Scenario 1
x = roc(pred_prune.test.exclude, factor(test$action_taken_name))
auc_prune.ex = auc(x, min =0, max =1) #0.5631

#Scenario 2
x = roc(pred_prune.test.include, factor(test$action_taken_name))
auc_prune.in = auc(x, min =0, max =1) #0.5631

#(3) KNN
#Scenario 1
x = roc(predicted1, factor(actual1))
auc_knn.ex = auc(x, min =0, max =1) #0.5203

#Scenario 2
x = roc(predicted, factor(actual))
auc_knn.in = auc(x, min =0, max =1) #0.5231

#--------------------------------------------------------------

#Q2.2.9 Non-Linear Classifier - overall performance (refer to PDF)


#Q2.3 Analysis -- overall observations (refer to PDF)


#Q2.4 Analysis - graphical support

#PCA

prin.comp = prcomp(hmda.dumm[,-89], rank = 2)
x = data.frame(prin.comp$x)
x$color = as.factor(hmda.dumm$action_taken_name)
gg = ggplot(x, aes(x=PC1, y=PC2)) + geom_point(aes(col = color))
plot(gg)

#Inference (refer to PDF)