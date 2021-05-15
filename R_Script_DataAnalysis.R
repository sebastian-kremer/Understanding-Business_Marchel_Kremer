
library(tidyverse)

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
  Age.group[age >=70 & age <30] <- "over 70"


} )

table(data$Age.group)


