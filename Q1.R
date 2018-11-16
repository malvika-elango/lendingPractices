library(plotly)
library(ggplot2)

#Read File
setwd("C:/Users/Malvika/Documents/Machine Learning/FinalProject")
hmda <- readRDS('hmda_lar.RDS')

#Remove irrelevant columns
hmda = hmda[, c(3:12,15,17,18,19,20,21,22,38,43,44,45,46,47)]

#Remove irrelevant rows
hmda = hmda[hmda$action_taken_name=='Application denied by financial institution' | hmda$action_taken_name=='Loan originated' ,]

#Rename outcomes
hmda$action_taken_name[hmda$action_taken_name=='Application denied by financial institution'] = 'denied'
hmda$action_taken_name[hmda$action_taken_name=='Loan originated'] = 'accepted'

#Remove row with all na values 
hmda = hmda[rowSums(is.na(hmda))!= ncol(hmda),]

#Divide data into train and test
set.seed(1)
training = sample.int(411328, 287930)
train = hmda[training ,]
test = hmda[-training ,]

#---------------------------------------

#Q1. Exploratory Data Analysis -- TRAIN

#Percentage accepted vs percentage denied - Train

#(1) Overall

#Numerical
percentage.denied = (length(which(train$action_taken_name=='denied'))/length(train$action_taken_name))*100
print(percentage.denied)
percentage.accept = (length(which(train$action_taken_name=='accepted'))/length(train$action_taken_name))*100
print(percentage.accept)

#Graphical
percentage.overall = c(percentage.accept, percentage.denied)
barplot(percentage.overall, main = "Overall percentage accepted vs denied - train", ylab = "percentage", names.arg = c("denied", "accepted"), ylim = c(0,100))

#Inference: #In the train set, the overall percentage accepted is 77.65% and overall percentage denied
#is 22.35%. 

#---------------------------------

#(2) Across seven states

#Numerical
states = c("CA", "AL", "TX", "MA", "NY", "IA", "CO")
accepted = c()
denied = c()
for (i in seq(length(states))){
  state = states[i]
  accept_ = length(which((train$state_abbr==state)&(train$action_taken_name=='accepted')))
  denied_ = length(which((train$state_abbr==state)&(train$action_taken_name=='denied')))
  total = length(which(train$state_abbr==state))
  percentage.state.accept = (accept_/total)*100
  percentage.state.denied = (denied_/total)*100
  print(state)
  accepted[i] = percentage.state.accept
  denied[i] = percentage.state.denied
  print(percentage.state.accept)
  print(percentage.state.denied)
  }
print(accepted)
print(denied)

#Graphical
accepted = signif(accepted, digits = 4)
denied = signif(denied, digits = 4)
data.state = data.frame(type = rep(c("accepted", "denied"), each =7), States = rep(c("CA", "AL", "TX", "MA", "NY","IA", "CO"), 2),percentage = c(accepted, denied)) 
ggplot(data=data.state, aes(x = States, y = percentage, fill = type)) + geom_bar(stat = "identity") + geom_text(aes(y = percentage, label = percentage), vjust =1.6, color = "white", size = 3.5)

#Inference: IA has the highest accepted rate and AL has the highest denied rate. 

#-------------------------------

#(3) By agency name 

#Numerical

unique(hmda$agency_name)

#Consumer Financial Protection Bureau
total.agency1 = length(which(train$agency_name=="Consumer Financial Protection Bureau"))
agency1.accept = length(which((train$agency_name=="Consumer Financial Protection Bureau")&(train$action_taken_name=="accepted")))
agency1.denied =  length(which((train$agency_name=="Consumer Financial Protection Bureau")&(train$action_taken_name=="denied")))                       
agency1.accept.percentage = (agency1.accept/total.agency1)*100 #74.84%
agency1.denied.percentage = (agency1.denied/total.agency1)*100 #25.16%

#National Credit Union Administration
total.agency2 = length(which(train$agency_name=="National Credit Union Administration"))
agency2.accept = length(which((train$agency_name=="National Credit Union Administration")&(train$action_taken_name=="accepted")))
agency2.denied =  length(which((train$agency_name=="National Credit Union Administration")&(train$action_taken_name=="denied")))                       
agency2.accept.percentage = (agency2.accept/total.agency2)*100 #80.62%
agency2.denied.percentage = (agency2.denied/total.agency2)*100 #19.38%

#Federal Reserve System
total.agency3 = length(which(train$agency_name=="Federal Reserve System"))
agency3.accept = length(which((train$agency_name=="Federal Reserve System")&(train$action_taken_name=="accepted")))
agency3.denied =  length(which((train$agency_name=="Federal Reserve System")&(train$action_taken_name=="denied")))                       
agency3.accept.percentage = (agency3.accept/total.agency3)*100 #84.75%
agency3.denied.percentage = (agency3.denied/total.agency3)*100 #15.25%

#Department of Housing and Urban Development
total.agency4 = length(which(train$agency_name=="Department of Housing and Urban Development"))
agency4.accept = length(which((train$agency_name=="Department of Housing and Urban Development")&(train$action_taken_name=="accepted")))
agency4.denied =  length(which((train$agency_name=="Department of Housing and Urban Development")&(train$action_taken_name=="denied")))                       
agency4.accept.percentage = (agency4.accept/total.agency4)*100 #76.33%
agency4.denied.percentage = (agency4.denied/total.agency4)*100 #23.67%

#Office of the Comptroller of the Currency
total.agency5 = length(which(train$agency_name=="Office of the Comptroller of the Currency"))
agency5.accept = length(which((train$agency_name=="Office of the Comptroller of the Currency")&(train$action_taken_name=="accepted")))
agency5.denied =  length(which((train$agency_name=="Office of the Comptroller of the Currency")&(train$action_taken_name=="denied")))                       
agency5.accept.percentage = (agency5.accept/total.agency5)*100 #81.92%
agency5.denied.percentage = (agency5.denied/total.agency5)*100 #18.08%

#Federal Deposit Insurance Corporation
total.agency6 = length(which(train$agency_name=="Federal Deposit Insurance Corporation"))
agency6.accept = length(which((train$agency_name=="Federal Deposit Insurance Corporation")&(train$action_taken_name=="accepted")))
agency6.denied =  length(which((train$agency_name=="Federal Deposit Insurance Corporation")&(train$action_taken_name=="denied")))                       
agency6.accept.percentage = (agency6.accept/total.agency6)*100 #85.34%
agency6.denied.percentage = (agency6.denied/total.agency6)*100 #14.66%

#Graphical 
agency.name = c("CFPB", "NCUA", "FRS", "HUD", "OCC","FDIC")
accepted = c(agency1.accept.percentage, agency2.accept.percentage, agency3.accept.percentage, agency4.accept.percentage, agency5.accept.percentage, agency6.accept.percentage)
denied = c(agency1.denied.percentage, agency2.denied.percentage, agency3.denied.percentage, agency4.denied.percentage, agency5.denied.percentage, agency6.denied.percentage)
data.agency = data.frame(agency.name, accepted, denied)
plot.agency = plot_ly(data.agency, x = ~agency.name, y = ~accepted, type = "bar", name="accepted")%>%add_trace(y = ~denied, name="denied")%>%layout(yaxis = list(title = "percentage"), barmode = "stack")
plot.agency

#Inference: CFPB has the highest denied rate and FDIC has the highest accepted rate
#---------------------------------------------

#Exploratory data analysis -- TEST

#(1) Overall
#Numerical
percentage.denied.test = (length(which(test$action_taken_name=='denied'))/length(test$action_taken_name))*100
percentage.accept.test = (length(which(test$action_taken_name=='accepted'))/length(test$action_taken_name))*100

#Graphical
percentage.overall.test = c(percentage.accept.test, percentage.denied.test)
barplot(percentage.overall.test, main = "Overall percentage accepted vs denied - test", ylab = "percentage", names.arg = c("denied", "accepted"), ylim = c(0,100))

#Inference: #In the test set, the overall percentage accepted is 77.35% and overall percentage denied
#is 22.64%. 
#-------------------------------------------

#(2) Across seven states

#Numerical
states = c("CA", "AL", "TX", "MA", "NY", "IA", "CO")
accepted = c()
denied = c()
for (i in seq(length(states))){
  state = states[i]
  accept_ = length(which((test$state_abbr==state)&(test$action_taken_name=='accepted')))
  denied_ = length(which((test$state_abbr==state)&(test$action_taken_name=='denied')))
  total = length(which(test$state_abbr==state))
  percentage.state.accept = (accept_/total)*100
  percentage.state.denied = (denied_/total)*100
  print(state)
  accepted[i] = percentage.state.accept
  denied[i] = percentage.state.denied
  print(percentage.state.accept)
  print(percentage.state.denied)
}
print(accepted)
print(denied)

#Graphical
accepted = signif(accepted, digits = 4)
denied = signif(denied, digits = 4)
data.state = data.frame(type = rep(c("accepted", "denied"), each =7), States = rep(c("CA", "AL", "TX", "MA", "NY","IA", "CO"), 2),percentage = c(accepted, denied)) 
ggplot(data=data.state, aes(x = States, y = percentage, fill = type)) + geom_bar(stat = "identity") + geom_text(aes(y = percentage, label = percentage), vjust =1.6, color = "white", size = 3.5)

#Inference: AL has the highest denied rate and IA has the highest accepted rate (same as train observation)
#--------------------------------------------

#(3) By Agency Name

#Numerical

#Consumer Financial Protection Bureau
test.total.agency1 = length(which(test$agency_name=="Consumer Financial Protection Bureau"))
test.agency1.accept = length(which((test$agency_name=="Consumer Financial Protection Bureau")&(test$action_taken_name=="accepted")))
test.agency1.denied =  length(which((test$agency_name=="Consumer Financial Protection Bureau")&(test$action_taken_name=="denied")))                       
test_agency1.accept.percentage = (test.agency1.accept/test.total.agency1)*100 #74.48%
test_agency1.denied.percentage = (test.agency1.denied/test.total.agency1)*100 #25.54%

#National Credit Union Administration
test.total.agency2 = length(which(test$agency_name=="National Credit Union Administration"))
test.agency2.accept = length(which((test$agency_name=="National Credit Union Administration")&(test$action_taken_name=="accepted")))
test.agency2.denied =  length(which((test$agency_name=="National Credit Union Administration")&(test$action_taken_name=="denied")))                       
test_agency2.accept.percentage = (test.agency2.accept/test.total.agency2)*100 #80.37%
test_agency2.denied.percentage = (test.agency2.denied/test.total.agency2)*100 #19.63%

#Federal Reserve System
test.total.agency3 = length(which(test$agency_name=="Federal Reserve System"))
test.agency3.accept = length(which((test$agency_name=="Federal Reserve System")&(test$action_taken_name=="accepted")))
test.agency3.denied =  length(which((test$agency_name=="Federal Reserve System")&(test$action_taken_name=="denied")))                       
test_agency3.accept.percentage = (test.agency3.accept/test.total.agency3)*100 #83.49%
test_agency3.denied.percentage = (test.agency3.denied/test.total.agency3)*100 #16.51%

#Department of Housing and Urban Development
test.total.agency4 = length(which(test$agency_name=="Department of Housing and Urban Development"))
test.agency4.accept = length(which((test$agency_name=="Department of Housing and Urban Development")&(test$action_taken_name=="accepted")))
test.agency4.denied =  length(which((test$agency_name=="Department of Housing and Urban Development")&(test$action_taken_name=="denied")))                       
test_agency4.accept.percentage = (test.agency4.accept/test.total.agency4)*100 #76.09%
test_agency4.denied.percentage = (test.agency4.denied/test.total.agency4)*100 #23.91%

#Office of the Comptroller of the Currency
test.total.agency5 = length(which(test$agency_name=="Office of the Comptroller of the Currency"))
test.agency5.accept = length(which((test$agency_name=="Office of the Comptroller of the Currency")&(test$action_taken_name=="accepted")))
test.agency5.denied =  length(which((test$agency_name=="Office of the Comptroller of the Currency")&(test$action_taken_name=="denied")))                       
test_agency5.accept.percentage = (test.agency5.accept/test.total.agency5)*100 #82%
test_agency5.denied.percentage = (test.agency5.denied/test.total.agency5)*100 #18%

#Federal Deposit Insurance Corporation
test.total.agency6 = length(which(test$agency_name=="Federal Deposit Insurance Corporation"))
test.agency6.accept = length(which((test$agency_name=="Federal Deposit Insurance Corporation")&(test$action_taken_name=="accepted")))
test.agency6.denied =  length(which((test$agency_name=="Federal Deposit Insurance Corporation")&(test$action_taken_name=="denied")))                       
test_agency6.accept.percentage = (test.agency6.accept/test.total.agency6)*100 #85.03%
test_agency6.denied.percentage = (test.agency6.denied/test.total.agency6)*100 #14.97%

#Graphical 
agency.name = c("CFPB", "NCUA", "FRS", "HUD", "OCC","FDIC")
accepted = c(test_agency1.accept.percentage, test_agency2.accept.percentage, test_agency3.accept.percentage, test_agency4.accept.percentage, test_agency5.accept.percentage, test_agency6.accept.percentage)
denied = c(test_agency1.denied.percentage, test_agency2.denied.percentage, test_agency3.denied.percentage, test_agency4.denied.percentage, test_agency5.denied.percentage, test_agency6.denied.percentage)
data.agency = data.frame(agency.name, accepted, denied)
plot.agency = plot_ly(data.agency, x = ~agency.name, y = ~accepted, type = "bar", name="accepted")%>%add_trace(y = ~denied, name="denied")%>%layout(yaxis = list(title = "percentage"), barmode = "stack")
plot.agency

#Inference: CFPB has the highest denied rate and FDIC has the highest accepted rate (same as train observation)

