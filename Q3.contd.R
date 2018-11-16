library(glmnet)
library(caret)
hmda.dumm = readRDS('hmda_dummy.RDS')

#Divide into train and test
set.seed(1)
training = sample.int(407182, 300000)
train.dumm = hmda.dumm[training ,]
test.dumm = hmda.dumm[-training ,]

train.dumm$action_taken_name = 1*(train.dumm$action_taken_name==1)-1*(train.dumm$action_taken_name==0)
test.dumm$action_taken_name = 1*(test.dumm$action_taken_name==1)-1*(test.dumm$action_taken_name==0)

#Q3.1.4 Lasso
max_correct = 0
lambda = c(0.0005, 0.001, 0.005, 0.01, 0.05)
for (i in lambda) {
  lasso.mod = glmnet(as.matrix(train.dumm[, -88]), as.matrix(train.dumm[, 88]), alpha = 1, lambda = i, family = "binomial")
  pred.train.mod = predict(lasso.mod, newx = as.matrix(train.dumm[, -88]))
  pred.train.mod = sign(pred.train.mod)
  correct = sum(train.dumm$action_taken_name==pred.train.mod)
  if (correct > max_correct) {
    max_correct = correct
    best.lam = i 
  }
}

#Inference: #Best Lambda = 0.0005 ; Accuracy = 79%

#Classifier performance on test
lasso.mod = glmnet(as.matrix(train.dumm[, -88]), as.matrix(train.dumm[, 88]), alpha = 1, lambda = best.lam, family = "binomial")
pred.test.mod = predict(lasso.mod, newx = as.matrix(test.dumm[, -88]))
pred.test.mod = sign(pred.test.mod)
confusionMatrix(pred.test.mod, test.dumm$action_taken_name) 

# Inference: Accuracy = 78.81%  ; Specificity = 9.30%


#Q3.1.5 Ridge
max_correct = 0
lambda = c(0.005, 0.001, 0.05, 0.01, 0.5)
for (i in lambda) {
  ridge.mod = glmnet(as.matrix(train.dumm[, -88]), as.matrix(train.dumm[, 88]), alpha = 0, lambda = i, family = "binomial")
  pred.train.mod = predict(ridge.mod, newx = as.matrix(train.dumm[, -88]))
  pred.train.mod = sign(pred.train.mod)
  correct = sum(train.dumm$action_taken_name==pred.train.mod)
  if (correct > max_correct) {
    max_correct = correct
    best.lam = i 
  }
}

#Inference: Best lambda 0.001 ; Accuracy = 79.09%

#Classifier performance on test 
ridge.mod = glmnet(as.matrix(train.dumm[, -88]), as.matrix(train.dumm[, 88]), alpha = 0, lambda = best.lam, family = "binomial")
pred.test.mod = predict(ridge.mod, newx = as.matrix(test.dumm[, -88]))
pred.test.mod = sign(pred.test.mod)
confusionMatrix(pred.test.mod, test.dumm$action_taken_name) 
#Inference: Accuracy = 78.82%   ; Specificity = 9.63%







