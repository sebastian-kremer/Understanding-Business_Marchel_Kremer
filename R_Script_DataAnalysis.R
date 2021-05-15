
library(dplyr)
library(readr)
library(caret)
library(lmtest)

data <- read.csv("dataset_31_credit-g.csv")

summary(data$age)

data <- within(data, {   
  Age.group <- NA # need to initialize variable
  Age.group[age < 20] <- "0-20"
  Age.group[age >=20 & age <30] <- "20-29"
  Age.group[age >=30 & age <40] <- "30-39"
  Age.group[age >=40 & age <50] <- "40-49"
  Age.group[age >=50 & age <60] <- "50-59"
  Age.group[age >=60 & age <70] <- "60-69"
  Age.group[age >=70 ] <- "over 70"
  
  
} )

table(data$Age.group)


data$Gender <- NA
data$Gender[data$personal_status == "'female div/dep/mar'"] <- "female"
data$Gender[data$personal_status == "'male single'"] <- "male"
data$Gender[data$personal_status == "'male div/sep'"] <- "male"
data$Gender[data$personal_status == "'male mar/wid'"] <- "male"


data$Class[data$class == "good"] <- 1
data$Class[data$class == "bad"] <- 0

# Logistic Regression

set.seed(987654321)

data_full_training <- createDataPartition(data$Class,
                                          p = 0.7, 
                                          list = FALSE) 
data_full_train <- data[data_full_training,]
data_full_test <- data[-data_full_training,]


data_logit1 <- glm(Class ~ checking_status + duration + credit_history + purpose +
                     credit_amount + savings_status + employment + installment_commitment + 
                     other_parties + residence_since + property_magnitude + age + other_payment_plans +
                     housing + existing_credits + job + num_dependents + own_telephone + foreign_worker +
                     Gender,
                   family =  binomial(link = "logit"),
                   data = data_full_train)

summary(data_logit1)

table(data$Gender)

data_men <- data[data$Gender == "male",]
data_women <- data[data$Gender == "female",]


#We want in our training dataset 250 females and 250 males

data_full_training_men <- createDataPartition(data_men$Class,
                                              p = 250/690, 
                                              list = FALSE)

data_full_training_women <- createDataPartition(data_women$Class,
                                                p = 250/310, 
                                                list = FALSE)

data_men_train <- data_men[data_full_training_men,]
data_women_train <- data_women[data_full_training_women,]

data_men_test <- data_men[-data_full_training_men,]
data_women_test <- data_women[-data_full_training_women,]

data_full_train2 <- rbind(data_men_train,data_women_train)
data_full_test2 <- rbind(data_men_test, data_women_test)

table(data_full_train$Gender)

data_logit2 <- glm(Class ~ checking_status + duration + credit_history + purpose +
                     credit_amount + savings_status + employment + installment_commitment + 
                     other_parties + residence_since + property_magnitude + age + other_payment_plans +
                     housing + existing_credits + job + num_dependents + own_telephone + foreign_worker +
                     Gender,
                   family =  binomial(link = "logit"),
                   data = data_full_train2)

summary(data_logit2)


ggplot(data = data, aes(x = age, y = duration)) +
  geom_point()

ggplot(data = data, aes(x = age, y = credit_amount)) +
  geom_point()

table(data$Gender)
table(data$Gender, data$residence_since)

#GENDER


# Residence since
tab <- with(data, table(Gender, residence_since))

prop.table(tab, margin = 1)

# credit history
tab <- with(data, table(Gender, credit_history))

prop.table(tab, margin = 1)

#purpose
tab <- with(data, table(Gender, purpose))

prop.table(tab, margin = 1)




#savings_status
tab <- with(data, table(Gender, savings_status))

prop.table(tab, margin = 1)

#employment
tab <- with(data, table(Gender, employment))

prop.table(tab, margin = 1)


#installment commitment
tab <- with(data, table(Gender,installment_commitment))

prop.table(tab, margin = 1)


#other_parties
tab <- with(data, table(Gender, other_parties))

prop.table(tab, margin = 1)


#property_magnitude
tab <- with(data, table(Gender, property_magnitude))

prop.table(tab, margin = 1)


#housing
tab <- with(data, table(Gender, housing))

prop.table(tab, margin = 1)
