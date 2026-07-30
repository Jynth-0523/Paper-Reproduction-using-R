# This script is to calculate real returns in the paper.

source("R/01_import_clean.R")

# Construct real returns
data$r_bill_rate = (1 + data$bill_rate)/(1 + data$inflation) - 1
data$r_bond_tr = (1 + data$bond_tr)/(1 + data$inflation) - 1
data$r_eq_tr = (1 + data$eq_tr)/(1 + data$inflation) - 1
data$r_housing_tr = (1+ data$housing_tr)/(1 + data$inflation) - 1

# Construct excess returns relative to bills
data$excess_bond_tr = data$r_bond_tr - data$r_bill_rate
data$excess_eq_tr = data$r_eq_tr - data$r_bill_rate
data$excess_housing_tr = data$r_housing_tr - data$r_bill_rate

# Variables required for the common sample
return_vars = c(
  "r_bill_rate",
  "r_bond_tr",
  "r_eq_tr",
  "r_housing_tr"
)

# TRUE when all four returns are available
data$sample = complete.cases(data[return_vars]) #full sample

# TRUE for observations from 1950 onward
data$tp_post1950 = data$year >= 1950 

# Check sample sizes
sum(data$sample) 
sum(data$sample & data$tp_post1950)






