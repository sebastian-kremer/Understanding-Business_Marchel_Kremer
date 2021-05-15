
library(tidyverse)

data <- read.csv("dataset_31_credit-g.csv")

if(data$age >= 0 & data$age < 10) {data$age_group <- "0-10"} else
if(data$age >= 10 & data$age < 20) {data$age_group <- "10-20"}
if(data$age >= 20 & data$age < 30) {data$age_group <- "20-30"}
if(data$age >= 30 & data$age < 40) {data$age_group <- "30-40"}
if(data$age >= 40 & data$age < 50) {data$age_group <- "40-50"}
if(data$age >= 50 & data$age < 60) {data$age_group <- "50-60"}
if(data$age >= 60 & data$age < 70) {data$age_group <- "60-70"}
if(data$age >= 70 & data$age < 80) {data$age_group <- "70-80"}
if(data$age >= 80 & data$age < 90) {data$age_group <- "80-90"}

data$age_group


data$age_group <- 