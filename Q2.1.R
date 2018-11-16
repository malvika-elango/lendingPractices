library(MASS)
library(caret)
library(e1071)
library(class)
library(naivebayes)
library(AUC)

#Read File
hmda.dumm = readRDS('hmda_dummy.RDS')
hmda = readRDS('hmda_cleaned.RDS')

#Checking correlation
c = cor(hmda.dumm)
View(c)  
#Inference: There was nothing more than 90 percent pairwise correlation. Although, population & 
#number of owner occupied units and number of 1 to 4 family units and number of owner occupied units
#had a correlation of 0.84 and 0.88 respectively. 

#----------------------------------------------------------------

#Divide into train and test
set.seed(1)
training = sample.int(407182, 285027)
train = hmda.dumm[training ,]
test = hmda.dumm[-training ,]

#-----------------------------------------------------------------

#Q2.1 Linear Classification

#Q2.1.1 First Linear Classifier in scenario 1

#LDA
lda.exclude = lda(action_taken_name~. -applicant_income_000s -loan_amount_000s, data = train)

#(1)Classifier performance on train data 
pred_train.exclude = predict(lda.exclude, data=train[, -88])
confusionMatrix(pred_train.exclude$class, train$action_taken_name)

#Inference: Accuracy = 79.05%    ; Specificity = 12.81%


#(2)Classifier performance on test data 
pred_test.exclude = predict(lda.exclude, newdata =test[,-88])
confusionMatrix(pred_test.exclude$class, test$action_taken_name) 

#Inference: Accuracy rate 78.77%  ; Specificity = 12.60%


#Q2.1.2 First Linear Classifier in scenario 2

#LDA
lda.include = lda(action_taken_name~., data = train)

#(1)Classifier performance on train data
pred_train.include = predict(lda.include, data = train[, -88])
confusionMatrix(pred_train.include$class, train$action_taken_name)

#Inference: Accuracy rate 79.08% ; Specificity = 12.94%

#(2)Classifier performance on test data 
pred_test.include = predict(lda.include, newdata = test[, -88])
confusionMatrix(pred_test.include$class, test$action_taken_name) 

#Inference: Accuracy rate is 78.79% ; Specificity = 12.66%

#----------------------------------------------------------------------

#Q2.1.3 Second Linear Classifier in scenario 1

#Logistic Regression
glm.exclude = glm(action_taken_name~. -applicant_income_000s -loan_amount_000s, data = train, family = binomial)

#(1)Classifier performace on train data 
pred_train.exclude.glm = predict(glm.exclude, data = train[, -88], type = "response")
pred_train.exclude.glm = as.vector(pred_train.exclude.glm)
pred_train.exclude.glm = 1*(pred_train.exclude.glm>0.5)
confusionMatrix(pred_train.exclude.glm, train$action_taken_name)

#Inference: Accuracy rate = 79.06% ; Specificity = 9.8%

#(2)Classifier performance on test set
pred_test.exclude.glm = predict(glm.exclude, newdata = test[, -88], type = "response")
pred_test.exclude.glm = as.vector(pred_test.exclude.glm)
pred_test.exclude.glm = 1*(pred_test.exclude.glm>0.5)
confusionMatrix(pred_test.exclude.glm, test$action_taken_name) 
#Inference: Accuracy rate = 78.83% ; Specificity = 9.70%


#Q2.1.4 Second Linear Classifier in scenario 2
#Logistic Regression
glm.include = glm(action_taken_name~., data = train, family = binomial)

#(1)Classifier performace on train data 
pred_train.include.glm = predict(glm.include, data = train[, -88], type = "response")
pred_train.include.glm = as.vector(pred_train.include.glm)
pred_train.include.glm = 1*(pred_train.include.glm>0.5)
confusionMatrix(pred_train.include.glm, train$action_taken_name)
#Inference: Accuracy rate = 79% ; Specificity = 10%

#(2)Classifier performance on test set
pred_test.include.glm = predict(glm.include, newdata = test[, -88], type = "response")
pred_test.include.glm = as.vector(pred_test.include.glm)
pred_test.include.glm = 1*(pred_test.include.glm>0.5)
confusionMatrix(pred_test.include.glm, test$action_taken_name) 
#Inference: Accuracy rate = 78.87% ; Specificity = 9.83%

#----------------------------------------------------------------------

#Q2.1.5 Third Linear Classifier in scenario 1

naive.train = hmda[training,]
naive.test = hmda[-training ,]

#NaiveBayes Classifier
naive.exclude = naiveBayes(action_taken_name~. -applicant_income_000s -loan_amount_000s, data = naive.train)

#(1)Classifier performance on train data
pred_train.exclude.naive = predict(naive.exclude, naive.train[, -19], type = "raw")
pred_train.exclude.naive = 1*(pred_train.exclude.naive[,2]>0.5)
confusionMatrix(pred_train.exclude.naive, naive.train$action_taken_name) 
#Inference: Accuracy rate = 73.24% ;  Specificity = 36.23%

#(2)Classifier performance on test data
pred_test.exclude.naive = predict(naive.exclude, naive.test[, -19], type = "raw")
pred_test.exclude.naive = 1*(pred_test.exclude.naive[,2]>0.5)
confusionMatrix(pred_test.exclude.naive, naive.test$action_taken_name)
#Inference: Accuracy rate = 73.15% ; Specificity = 36% 

#Q2.1.6 Third Linear Classifier in scenario 2
naive.include = naiveBayes(action_taken_name~., data = naive.train)

#(1)Classifier performance on train data
pred_train.include.naive = predict(naive.include, naive.train[, -19], type="raw")
pred_train.include.naive = 1*(pred_train.include.naive[,2]>0.5)
confusionMatrix(pred_train.include.naive, naive.train$action_taken_name)
#Inference: Accuracy rate = 73.24% ; Specificity = 36.23%

#(2)Classifier performance on test data
pred_test.include.naive = predict(naive.include, naive.test[, -19], type ="raw")
pred_test.include.naive = 1*(pred_test.include.naive[,2]>0.5)
confusionMatrix(pred_test.include.naive, naive.test$action_taken_name) 
#Inference: Accuracy rate = 73.15% ; Specificity = 36%

#---------------------------------------------------------------------

#Q2.1.7 Linear Classifier - comparisons (refer to PDF)


#Q2.1.8 Linear Classifier - overfitting

#(1)LDA

#Scenario 1
x = roc(pred_test.exclude$class, factor(test$action_taken_name))
auc_lda.ex = auc(x, min =0, max =1) #0.5500

#Scenario 2
x = roc(pred_test.include$class, factor(test$action_taken_name))
auc_lda.in = auc(x, min =0, max =1) #0.5504

#(2) Logistic Regression
#Scenario 1
x = roc(pred_test.exclude.glm, factor(test$action_taken_name))
auc_glm.ex = auc(x, min =0, max =1) #0.5400

#Scenario 2
x = roc(pred_test.include.glm, factor(test$action_taken_name))
auc_glm.in = auc(x, min =0, max =1) #0.5407

#(3) Naive Bayes
#Scenario 1
x = roc(pred_test.exclude.naive, factor(test$action_taken_name))
auc_naive.ex = auc(x, min =0, max =1) #0.5979

#Scenario 2
x = roc(pred_test.include.naive, factor(test$action_taken_name))
auc_naive.in = auc(x, min =0, max =1) #0.5979

#Inference: Although the overall accuracy rate of Naive Bayes is lower compared to other
#models, it has the best AUC of 0.59. However, there does exist the presence of overfitting across
#all models which is evident from the low AUC values. 

#Q2.1.9 Linear Classifier - overall performance (refer to PDF)
