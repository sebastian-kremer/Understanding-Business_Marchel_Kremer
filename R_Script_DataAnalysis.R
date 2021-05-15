
data <- read.csv("dataset_31_credit-g.csv")

summary(data$age)

data <- within(data, {   
  Age.group <- NA # need to initialize variable
  Age.group[age < 20] <- "under 20"
  Age.group[age >=20 & age <30] <- "20-29"
  Age.group[age >=30 & age <40] <- "30-39"
  Age.group[age >=40 & age <50] <- "40-49"
  Age.group[age >=50 & age <60] <- "50-59"
  Age.group[age >=60 & age <70] <- "60-69"
  Age.group[age >=70 ] <- "over 70"


} )

table(data$Age.group)


data$age_group[data$age >=0 & data$age < 10] <- "0-10"
data$age_group[data$age >=10 & data$age < 20] <- "10-20"
data$age_group[data$age >=20 & data$age < 30] <- "20-30"
data$age_group[data$age >=30 & data$age < 40] <- "30-40"
data$age_group[data$age >=40 & data$age < 50] <- "40-50"
data$age_group[data$age >=50 & data$age < 60] <- "50-60"
data$age_group[data$age >=60 & data$age < 70] <- "60-70"
data$age_group[data$age >=70 & data$age < 80] <- "70-80"

