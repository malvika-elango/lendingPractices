library(randomForest)
library(caret)
library(gbm)
library(e1071)

#Read file
hmda = readRDS('hmda_cleaned.RDS')
hmda.dumm = readRDS('hmda_dummy.RDS')

#Divide into train and test
set.seed(1)
training = sample.int(407182, 300000)
train.dumm = hmda.dumm[training ,]
test.dumm = hmda.dumm[-training ,]

#--------------------------------------------------------------------------

#Q3.1 Cross Validation with more complex methods 

#Q3.1.1 Random Forest
set.seed(1)
training1 = sample.int(407182, 50000)
random.train = hmda[training1 ,]
mtry = c(2, 3, 4, 5, 6)
for(i in mtry){
random.forest = randomForest(as.factor(action_taken_name)~., data = random.train, importance = TRUE, mtry = i, ntree =500)
pred.forest.train = predict(random.forest, random.train[, -19])
misclassified = sum(random.train$action_taken_name != pred.forest.train) 
print(misclassified)
}
#Inference: mtry = 6 gives the lowest train error

#Model with mtry = 6
random_final = randomForest(as.factor(action_taken_name)~., data = random.train, importance = TRUE, mtry = 6, ntree = 500)

#Checking important variables
varImpPlot(random.forest) 

#Testing on test set
test1 = hmda[-training1 , ]

#Reducing number of observations on test
random.test = test1[sample(nrow(test1), 30000),]

#Classifier performance on test
pred.forest.test = predict(random_final, random.test[, -19])
confusionMatrix(pred.forest.test, random.test$action_taken_name)

#Inference: Accuracy rate = 79.54%  ; Specificity = 16.17%

#---------------------------------------------------------------------

#Q3.1.2 Boosting 
# use the matrix hmda.dumm
# require JOUSBoost
# make response variables c(-1, 1)
require('JOUSBoost')
train.dumm$action_taken_name = 1*(train.dumm$action_taken_name == 1) - 1*(train.dumm$action_taken_name == 0)
test.dumm$action_taken_name = 1*(test.dumm$action_taken_name == 1) - 1*(test.dumm$action_taken_name == 0)
imp = c(4,7,5,59,60,66,67) #the top five important features found from random forest

#Fit model
adaboost.fit = JOUSBoost::adaboost(X = as.matrix(train.dumm[, imp]), y = train.dumm$action_taken_name, tree_depth = 1, n_rounds = 100)

#Classifier performance on train
pred.boost.train = predict(adaboost.fit, train.dumm[,-89])
confusionMatrix(pred.boost.train, train.dumm$action_taken_name)
#Inference: Accuracy rate = 78.66%    ; Specificity = 6.95%

#Classifier performance on test
pred.boost = predict(adaboost.fit, test.dumm[,-89])
confusionMatrix(pred.boost, test.dumm$action_taken_name) 
#Inference: Accuracy rate = 78.43% ; Specificity =  6.80%

#------------------------------------------------------------

#Q3.1.3 SVM (radial) using 2 parameters
set.seed(1)
training.svm = sample.int(407182, 10000)
train.svm = hmda[training.svm ,]
test.svm = hmda[-training.svm ,]
test.svm = test.svm[sample(nrow(test.svm), 8000),]
cost = c(0.001, 0.1, 1, 5,10 )
gamma = c(0.5, 1, 2, 3, 4)
best.gamma = c()
accuracy.radial = c()
set.seed(2)
for (i in seq(5)){
  max_acc = 0
  max_gamma = 0
  for(j in gamma){
    radial.svmfit = svm(action_taken_name ~ applicant_income_000s + loan_amount_000s, data = train.svm, kernel = "radial", cost = cost[i], gamma = j)
    pred.radial = predict(radial.svmfit, train.svm[, -19])
    pred.radial = as.vector(pred.radial)
    pred.radial = 1*(pred.radial>0.5)
    acc = sum(train.svm$action_taken_name == pred.radial)
    if (acc > max_acc){
      max_acc = acc
      max_gamma = j
    }
  }
  accuracy.radial[i] = max_acc
  best.gamma[i] = max_gamma
}

#Inference: cost = 10 and gamma = 4 give the highest correct classificaton rate

#SVM using the above inference
svm.final = svm(action_taken_name ~ applicant_income_000s + loan_amount_000s, data = train.svm, kernel = "radial", cost = 10, gamma = 4)

#Classifier performance on test
predict.final = predict(svm.final, newdata = test.svm[, -19])
predict.final = as.vector(predict.final)
predict.final = 1*(predict.final>0.5)
confusionMatrix(predict.final, test.svm$action_taken_name)

#Inference: Accuracy rate = 78.87%  ; Specificity = 1.7%

