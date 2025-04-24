install.packages('tidyverse')
install.packages('dplyr')
library(tidyverse)
library(readr)
library(dplyr)

customer_churn <- read_csv("C:/Users/PMLS/Downloads/customer_churn.csv")
View(customer_churn)
filtered_churn <- customer_churn %>% filter(Churn == 'Yes')
View(filtered_churn)

#Create a new column ChargeGap as TotalCharges - (MonthlyCharges * Tenure). Handle any NAs appropriately.
dataset <- customer_churn %>% mutate(ChargeGap = TotalCharges - (MonthlyCharges * Tenure), na.rm = 1)
View(dataset)

#Filter customers who have been with the company for more than 2 years (Tenure > 24)
#and are still active (Churn == "No").
tenure_churn <- dataset %>% filter((Tenure > 24) & (Churn == "No"))
View(tenure_churn)

#Find the average MonthlyCharges for each ContractType (e.g., "Month-to-Month", "One
#year", "Two year").

monthly_charges <- dataset %>% group_by(ContractType) %>% summarise(avg = mean(MonthlyCharges))
View(monthly_charges)

#Categorize customers based on Age:
#"Youth": Age < 25
#"Adult": 25 ≤ Age ≤ 55
#"Senior": Age > 55

customers_age <- dataset %>% mutate(AgeGroup  = case_when(
  Age < 25 ~ "Youth",
  Age <= 25 & Age <= 55 ~ "Adult",
  TRUE ~ "Senior"
))
View(dataset)

#• Find the top 5 cities with the highest number of churned customers.
dataset <- dataset %>% filter(arrange(desc(City)) %>%  head(5)
View(dataset)

#• Extract only the names and cities of customers whose TotalCharges > 3000, are on a
#Month-to-Month contract, and have churned.
filtered <- customer_churn %>% select(Name, City) %>% filter(TotalCharges > 3000, ContractType == "Month-to-Month", Churn == "Yes") 
View(filtered)

#• Create a table that shows the average tenure and total revenue (sum(TotalCharges)) for
  #each ContractType.
tenure_revenue <- dataset %>% group_by(ContractType) %>% summarise(Avg_tenure = mean(Tenure) , TotalRevenue = sum(TotalCharges))
View(tenure_revenue)


