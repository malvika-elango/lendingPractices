
#Read File
setwd("C:/Users/Malvika/Documents/Machine Learning/FinalProject")
hmda <- readRDS('hmda_lar.RDS')

#Remove specified columns
hmda = hmda[, c(3:12,15,17,18,19,20,21,22,38,43,44,45,46,47)]

#Keep specified rows
hmda = hmda[hmda$action_taken_name=='Application denied by financial institution' | hmda$action_taken_name=='Loan originated' ,]

#Rename outcomes
hmda$action_taken_name[hmda$action_taken_name=='Application denied by financial institution'] = 'denied'
hmda$action_taken_name[hmda$action_taken_name=='Loan originated'] = 'accepted'

#Remove row with all na values 
hmda = hmda[rowSums(is.na(hmda))!= ncol(hmda),]

#-------------------------------------

#Data cleaning

#Delete irrelevant columns
hmda = hmda[, -c(9, 10, 22)]

#Check NA's
sapply(hmda, function(x) sum(is.na(x)))

#Delete rows with missing popualtion and minority population values
hmda = hmda[-which(is.na(hmda$population)),]
hmda = hmda[-which(is.na(hmda$minority_population)),]

#Replace missing values of applicant income, number of owner occupied units and number of 1
#to 4 family units with respective means
hmda$applicant_income_000s[is.na(hmda$applicant_income_000s)] = mean(hmda$applicant_income_000s, na.rm = TRUE)
hmda$number_of_owner_occupied_units[is.na(hmda$number_of_owner_occupied_units)] = mean(hmda$number_of_owner_occupied_units, na.rm = TRUE)
hmda$number_of_1_to_4_family_units[is.na(hmda$number_of_1_to_4_family_units)] = mean(hmda$number_of_1_to_4_family_units, na.rm = TRUE)

#Create mode function
get.mode = function(v){
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

#Replace missing values of hud_median_family_income with the mod of hud_median_family_income 
#of respective msamd_name
msmd.name = c(unique(hmda$msamd_name))

for (i in msmd.name){
  hmda$hud_median_family_income[is.na(hmda$hud_median_family_income)]= get.mode(hmda$hud_median_family_income[which(hmda$msamd_name==i)])
}


#Convert character variables to factor
cols = c("state_name", "property_type_name", "owner_occupancy_name", "msamd_name", "loan_type_name", "loan_purpose_name", "lien_status_name", "hoepa_status_name", "applicant_sex_name", "applicant_race_name_1", "applicant_ethnicity_name", "agency_name", "action_taken_name")
hmda[cols]=lapply(hmda[cols], factor)
sapply(hmda, class)
hmda$action_taken_name = 1*(hmda$action_taken_name == 'accepted')

#Remove column msamd_name since this column does not seem to provide any useful information anymore
hmda = hmda[, -11]

#Save clean file
saveRDS(hmda, 'hmda_cleaned.RDS')

#'One Hot Encoding' to convert categorical variables
hmda.dumm = as.data.frame(model.matrix(~.,data = hmda))
hmda.dumm = hmda.dumm[, -1]

#Save dummy version
saveRDS(hmda.dumm, 'hmda_dummy.RDS')
