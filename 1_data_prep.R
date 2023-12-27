#select food data
install.packages('tidyverse', 'fs', 'readxl')
library(tidyverse)
library(fs)
library(readxl)
library(dplyr)

#import raw data
setwd("D:/r/KenyaDSM/data")
raw_foods <- as.data.frame(read_excel("food_composition_tables.xlsx", 4))

# clean data
raw_foods <- raw_foods[307:470, c(2, 12,14,16,17)] # select vegetables and nutrients
colnames(raw_foods) <- c('Vegetable', 'Calcium', 'Magnesium', 'Potassium', 'Sodium')

duplicated(raw_foods$Vegetable) #check for duplicates
is.null(raw_foods) #check for nulls
str(raw_foods) #check data types

raw_foods <- raw_foods[!(is.na(raw_foods$Vegetable) | raw_foods$Vegetable==""), ] # remove null foods
raw_foods <- raw_foods[!duplicated(raw_foods$Vegetable), ] # remove duplicate foods
raw_foods <- raw_foods %>% mutate_at(c('Calcium', 'Magnesium', 'Potassium', 'Sodium'), as.numeric) # convert nutrients to num
raw_foods <- raw_foods %>% mutate(across(where(is.numeric), round, 3)) # round off to 3.dp
write.csv(raw_foods, "veg_nutrients.csv", row.names=FALSE) # export csv
