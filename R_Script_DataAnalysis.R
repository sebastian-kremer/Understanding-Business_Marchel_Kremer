
library(dplyr)
library(readr)
library(caret)
library(lmtest)
library(reshape2)

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
table(data_full_train2$Gender)

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
tab1 <- with(data_full_train2, table(Gender, residence_since))

prop.table(tab1, margin = 1)

summary(tab1)

df_1 <- data.frame(
  Residence_since=c("1 year","2 years","3 years","4 years"),  
  Female=c(0.17,0.26,0.11,0.46), 
  Male = c(0.12,0.34,0.20,0.34))

plot1_data <- melt(df_1, id.vars = "Residence_since")
colnames(plot1_data) <- c("Residence_since", "Gender", "Percentage")

# Barplot
ggplot(plot1_data, aes(x=Residence_since, y = Percentage, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Model 2")

#Model 3
#250 Women: 0.40% 4 years, 0.15% 3 years, 0.3% 2 years, 0.15% 1 year
#250 Men: 0.40% 4 years, 0.15% 3 years, 0.3% 2 years, 0.15% 1 year

#0.4% = 100, 0.3% = 75, 0.15% = 37.5

data_women_1 <- data_women[data_women$residence_since == 1,]
data_women_2 <- data_women[data_women$residence_since == 2,]
data_women_3 <- data_women[data_women$residence_since == 3,]
data_women_4 <- data_women[data_women$residence_since == 4,]

data_men_1 <- data_men[data_men$residence_since == 1,]
data_men_2 <- data_men[data_men$residence_since == 2,]
data_men_3 <- data_men[data_men$residence_since == 3,]
data_men_4 <- data_men[data_men$residence_since == 4,]

data_full_training_men_1 <- createDataPartition(data_men_1$Class,
                                              p = 45/80, 
                                              list = FALSE)

data_full_training_men_2 <- createDataPartition(data_men_2$Class,
                                                p = 75/225, 
                                                list = FALSE)

data_full_training_men_3 <- createDataPartition(data_men_3$Class,
                                                p = 30/114, 
                                                list = FALSE)

data_full_training_men_4 <- createDataPartition(data_men_4$Class,
                                                p = 100/271, 
                                                list = FALSE)

data_full_training_women_1 <- createDataPartition(data_women_1$Class,
                                                p = 45/50, 
                                                list = FALSE)

data_full_training_women_2 <- createDataPartition(data_women_2$Class,
                                                p = 75/83, 
                                                list = FALSE)

data_full_training_women_3 <- createDataPartition(data_women_3$Class,
                                                p = 30/35, 
                                                list = FALSE)

data_full_training_women_4 <- createDataPartition(data_women_4$Class,
                                                p = 100/142, 
                                                list = FALSE)

data_men_train_1 <- data_men_1[data_full_training_men_1,]
data_men_train_2 <- data_men_2[data_full_training_men_2,]
data_men_train_3 <- data_men_3[data_full_training_men_3,]
data_men_train_4 <- data_men_4[data_full_training_men_4,]

data_women_train_1 <- data_women_1[data_full_training_women_1,]
data_women_train_2 <- data_women_2[data_full_training_women_2,]
data_women_train_3 <- data_women_3[data_full_training_women_3,]
data_women_train_4 <- data_women_4[data_full_training_women_4,]

data_men_test_1 <- data_men_1[-data_full_training_men_1,]
data_men_test_2 <- data_men_2[-data_full_training_men_2,]
data_men_test_3 <- data_men_3[-data_full_training_men_3,]
data_men_test_4 <- data_men_4[-data_full_training_men_4,]

data_women_test_1 <- data_women_1[-data_full_training_women_1,]
data_women_test_2 <- data_women_2[-data_full_training_women_2,]
data_women_test_3 <- data_women_3[-data_full_training_women_3,]
data_women_test_4 <- data_women_4[-data_full_training_women_4,]

data_full_train3 <- rbind(data_men_train_1,data_men_train_2,
                          data_men_train_3,data_men_train_4,
                          data_women_train_1,data_women_train_2,
                          data_women_train_3,data_women_train_4)
data_full_test3 <- rbind(data_men_test_1, data_men_test_2,
                         data_men_test_3,data_men_test_4,
                         data_women_test_1,data_women_test_2,
                         data_women_test_3,data_women_test_4)

tab2 <- with(data_full_train3, table(Gender, residence_since))

prop.table(tab2, margin = 1)

summary(tab2)

df_2 <- data.frame(
  Residence_since=c("1 year","2 years","3 years","4 years"),  
  Female=c(0.18,0.30,0.12,0.40), 
  Male = c(0.18,0.30,0.12,0.40))

plot2_data <- melt(df_2, id.vars = "Residence_since")
colnames(plot2_data) <- c("Residence_since", "Gender", "Percentage")

# Barplot
ggplot(plot2_data, aes(x=Residence_since, y = Percentage, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Model 3")



# credit history
tab2 <- with(data, table(Gender, credit_history))

prop.table(tab2, margin = 1)

#purpose
tab3 <- with(data, table(Gender, purpose))

prop.table(tab3, margin = 1)




#savings_status
tab4 <- with(data, table(Gender, savings_status))

prop.table(tab4, margin = 1)

#employment
tab <- with(data, table(Gender, employment))

prop.table(tab, margin = 1)


#installment commitment
tab5 <- with(data, table(Gender,installment_commitment))

prop.table(tab5, margin = 1)


#other_parties
tab6 <- with(data, table(Gender, other_parties))

prop.table(tab6, margin = 1)


#property_magnitude
tab7 <- with(data, table(Gender, property_magnitude))

prop.table(tab7, margin = 1)


#housing
tab8 <- with(data, table(Gender, housing))

prop.table(tab8, margin = 1)


#Model 4
#employment
#250 Women: 63 <1, 47 >=7, 85 1 to 4, 40 4 to 7, 15 unemployed
#250 Men: 63 <1, 47 >=7, 85 1 to 4, 40 4 to 7, 15 unemployed

tab <- with(data, table(Gender, employment))

prop.table(tab, margin = 1)

data_women_1 <- data_women[data_women$employment == "'<1'",]
data_women_2 <- data_women[data_women$employment == "'>=7'",]
data_women_3 <- data_women[data_women$employment == "'1<=X<4'",]
data_women_4 <- data_women[data_women$employment == "'4<=X<7'",]
data_women_5 <- data_women[data_women$employment == "unemployed",]

data_men_1 <- data_men[data_men$employment == "'<1'",]
data_men_2 <- data_men[data_men$employment == "'>=7'",]
data_men_3 <- data_men[data_men$employment == "'1<=X<4'",]
data_men_4 <- data_men[data_men$employment == "'4<=X<7'",]
data_men_5 <- data_men[data_men$employment == "unemployed",]


data_full_training_men_1 <- createDataPartition(data_men_1$Class,
                                                p = 63/86, 
                                                list = FALSE)

data_full_training_men_2 <- createDataPartition(data_men_2$Class,
                                                p = 47/206, 
                                                list = FALSE)

data_full_training_men_3 <- createDataPartition(data_men_3$Class,
                                                p = 85/232, 
                                                list = FALSE)

data_full_training_men_4 <- createDataPartition(data_men_4$Class,
                                                p = 39/127, 
                                                list = FALSE)

data_full_training_men_5 <- createDataPartition(data_men_5$Class,
                                                p = 15/39, 
                                                list = FALSE)

data_full_training_women_1 <- createDataPartition(data_women_1$Class,
                                                  p = 63/86, 
                                                  list = FALSE)

data_full_training_women_2 <- createDataPartition(data_women_2$Class,
                                                  p = 47/47, 
                                                  list = FALSE)

data_full_training_women_3 <- createDataPartition(data_women_3$Class,
                                                  p = 85/107, 
                                                  list = FALSE)

data_full_training_women_4 <- createDataPartition(data_women_4$Class,
                                                  p = 40/47, 
                                                  list = FALSE)

data_full_training_women_5 <- createDataPartition(data_women_5$Class,
                                                  p = 15/23, 
                                                  list = FALSE)


data_men_train_1 <- data_men_1[data_full_training_men_1,]
data_men_train_2 <- data_men_2[data_full_training_men_2,]
data_men_train_3 <- data_men_3[data_full_training_men_3,]
data_men_train_4 <- data_men_4[data_full_training_men_4,]
data_men_train_5 <- data_men_5[data_full_training_men_5,]

data_women_train_1 <- data_women_1[data_full_training_women_1,]
data_women_train_2 <- data_women_2[data_full_training_women_2,]
data_women_train_3 <- data_women_3[data_full_training_women_3,]
data_women_train_4 <- data_women_4[data_full_training_women_4,]
data_women_train_5 <- data_women_5[data_full_training_women_5,]

data_men_test_1 <- data_men_1[-data_full_training_men_1,]
data_men_test_2 <- data_men_2[-data_full_training_men_2,]
data_men_test_3 <- data_men_3[-data_full_training_men_3,]
data_men_test_4 <- data_men_4[-data_full_training_men_4,]
data_men_test_5 <- data_men_5[-data_full_training_men_5,]

data_women_test_1 <- data_women_1[-data_full_training_women_1,]
data_women_test_2 <- data_women_2[-data_full_training_women_2,]
data_women_test_3 <- data_women_3[-data_full_training_women_3,]
data_women_test_4 <- data_women_4[-data_full_training_women_4,]
data_women_test_5 <- data_women_5[-data_full_training_women_5,]

data_full_train4 <- rbind(data_men_train_1,data_men_train_2,
                          data_men_train_3,data_men_train_4,data_men_train_5,
                          data_women_train_1,data_women_train_2,
                          data_women_train_3,data_women_train_4,data_women_train_5)
data_full_test4 <- rbind(data_men_test_1, data_men_test_2,
                         data_men_test_3,data_men_test_4,data_men_test_5,
                         data_women_test_1,data_women_test_2,
                         data_women_test_3,data_women_test_4,data_women_test_5)

tab <- with(data_full_train4, table(Gender, employment))

prop.table(tab, margin = 1)
