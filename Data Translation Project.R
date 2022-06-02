##############################################################
# Data Translation Project, Applied Econometrics
##############################################################
#3 Retail needs to worry about who has money to spend - what has changed about who is working and earning money?

#How to Answer the Question
##1. Cut-off of when COVID started (look into avg income industry)
  
# Load in Required Packages
library(vtable)
library(ipumsr)
library(tidyverse)
library(purrr)
library(lubridate)
library(fixest)
library(multcomp)
library(dplyr)

# Read in and Load the Data
help("ipumsr")
ddi <- read_ipums_ddi("cps_00002.xml")
data <- read_ipums_micro(ddi)

# Step 1: look into the data (vtable) and choose the variables by using select(); rename any variables if needed. 
#then use sumtable to see the distribution
# View the Data
vtable(data)

# Clean and Use Select to choose required variables to make final data set
cleandt <- data %>% dplyr :: select(YEAR, MONTH, EMPSTAT, OCC, IND, UHRSWORK1, WKSTAT)
cleandt <- cleandt %>% drop_na()
cleandt <- cleandt %>% mutate(HRSMAIN = UHRSWORK1)

# We filter for Employment Status at 10 (employed) and 20 (unemployed)
cleandt <- cleandt %>% filter(EMPSTAT %in% c(10,20:22))

# Create a variable called date, that denotes for the different months in the years 2019 to 2022, use ggplot to visualize a change
#in Income Wage between 2019 to 2020
cleandt <- cleandt %>% mutate(date = ym(paste(YEAR,MONTH)))

# Join the Industry names and cleandt to align broader Industries
indnames <- read_csv('indnames.csv')
vtable(indnames)
cleandt <- cleandt %>% rename(ind = IND)
finaldt <- full_join(cleandt, indnames)
vtable(finaldt)

# Step 2: Visualize the difference in the count of employed and unemployed people between those of the different industries
finaldt$industry <- ifelse(finaldt$indname == 'Retail Trade', 1, 0)
emp_rate_by_month <- finaldt %>%
  group_by(industry, date) %>%
  summarize(emp_rate = 1 - mean(EMPSTAT %in% 20:22))
vtable(emp_rate_by_month)
sumtable(emp_rate_by_month)
emp_rate_by_month <- emp_rate_by_month %>% 
  drop_na(industry) %>%
  drop_na(date)
emp_rate_by_month %>% ggplot(aes(x = date, y = emp_rate, color = industry)) + geom_point() + labs(y = "Employed Rate", x = "Date") + 
  geom_vline(xintercept = as.Date("2020-03-15"))

# Step 3: Create bins of the Emp_Rate_By_Month
bindata <- emp_rate_by_month  %>% mutate(emp_bins = cut(emp_rate, breaks = 50))
sumtable(bindata)

# Step 4: use ggplot  -> visual representative of step 3
bindata %>% ggplot(aes(x = emp_bins)) + geom_bar() + geom_vline(xintercept = '(0.96,0.962]') + theme(axis.text.x = element_blank())

# Step 5: rdrobust -> will select a window and weighting kernel to weight observations more near the cutoff 


# Step 6: rdplot -> to plot what rdrobust did


