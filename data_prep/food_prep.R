#set project directory
proj_dir <- 'D:/r/KenyaDSM'

#select food data
install.packages('tidyverse', 'fs', 'readxl')
library(tidyverse)
library(fs)
library(readxl)
library(dplyr)

#import raw data
setwd(paste(proj_dir,'data/foods', sep="/"))
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

# descriptive stats of veg nutrients
summary(raw_foods)
boxplot(raw_foods[,2:5])
# select potassium because of uniform distribution and reject sodium 
boxplot(raw_foods$Potassium)
# compare Mg and Ca since they are similar
hist(raw_foods$Magnesium)
hist(raw_foods$Calcium)
# select Magnesium for slightly more varied distribution between Mg and Ca

#export veg, potassium and magnesium data
write.csv(raw_foods[,c(1,3,4)], "veg_k_mg.csv", row.names=FALSE) # export csv