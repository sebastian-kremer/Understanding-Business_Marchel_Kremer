
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

