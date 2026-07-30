# This script is to reproduce the Table II: Global Real Returns

source("R/02_construct_returns.R")
library(gt)

# Define the two samples
full_rows = data$sample
post1950_rows = data$sample &data$tp_post1950

# For real returns

# Mean real returns for full sample
full_real_mean = colMeans(data[full_rows,return_vars])*100

# Mean real returns for post-1950 sample
post1950_real_mean = colMeans(data[post1950_rows,return_vars])*100

# Standard deviation of real returns for full sample
full_real_sd = sapply(data[full_rows,return_vars], sd)*100

# Standard deviation of real returns for post-1950 sample
post1950_real_sd = sapply(data[post1950_rows,return_vars],sd)*100

# Function for geometric mean returns
geometric_mean = function(x){exp(mean(log(1+x)))-1}

# Geometric mean real returns for full sample
full_real_geomean = sapply(data[full_rows,return_vars],geometric_mean)*100

# Geometric mean real returns for post1950 sample
post1950_real_geomean = sapply(data[post1950_rows,return_vars],geometric_mean)*100

# Excess-return variables
excess_vars = c("excess_bond_tr","excess_eq_tr","excess_housing_tr")

# Mean excess returns for full sample
full_excess_mean = colMeans(data[full_rows,excess_vars])*100

# Mean excess returns for post1950 sample
post1950_excess_mean = colMeans(data[post1950_rows,excess_vars])*100

# Standard deviation of excess returns for full sample
full_excess_sd = sapply(data[full_rows,excess_vars],sd)*100

# Standard deviation of excess returns for post1950 sample
post1950_excess_sd = sapply(data[post1950_rows,excess_vars],sd)*100

# Geometric mean excess returns for full sample
full_excess_geomean = sapply(data[full_rows,excess_vars],geometric_mean)*100

# Geometric mean excess returns for post1950 sample
post1950_excess_geomean = sapply(data[post1950_rows,excess_vars],geometric_mean)*100

# For nominal returns
nominal_vars = c("bill_rate","bond_tr","eq_tr","housing_tr")

# Mean nominal returns for full sample
full_nominal_mean = colMeans(data[full_rows,nominal_vars])*100

# Mean nominal returns for post1950 sample
post1950_nominal_mean = colMeans(data[post1950_rows,nominal_vars])*100

# Standard deviation of nominal returns for full sample
full_nominal_sd = sapply(data[full_rows,nominal_vars],sd)*100

# Standard deviation of nominal returns for post1950 sample
post1950_nominal_sd = sapply(data[post1950_rows,nominal_vars],sd)*100

# Geometric mean of nominal returns for full sample
full_nominal_geomean = sapply(data[full_rows,nominal_vars],geometric_mean)*100

# Geometric mean of nominal returns for full sample
post1950_nominal_geomean = sapply(data[post1950_rows,nominal_vars],geometric_mean)*100

# Panel A: Full Sample

# Table column names
table_columns = c("Real Bills",
                  "Real Bonds",
                  "Real Equity",
                  "Real Housing",
                  "Nominal Bills",
                  "Nominal Bonds",
                  "Nominal Equity",
                  "Nominal Housing")

#Table row names
table_rows = c("Mean return p.a.",
               "Standard deviation",
               "Geometric Mean",
               "Mean excess return p.a.",
               "Standard deviation",
               "Geometric Mean",
               "Observations")

# Create Panel A: Full Sample
full_panel = rbind(
  c(full_real_mean,full_nominal_mean),
  c(full_real_sd,full_nominal_sd),
  c(full_real_geomean,full_nominal_geomean),
  c(NA,full_excess_mean,rep(NA,4)),
  c(NA,full_excess_sd,rep(NA,4)),
  c(NA,full_excess_geomean,rep(NA,4)),
  rep(sum(full_rows),8)
)

colnames(full_panel) = table_columns
rownames(full_panel) = table_rows

#Display results with two decimals
round(full_panel,2)

View(round(full_panel,2))

# Create Panel B: Post1950 Sample
post1950_panel = rbind(
  c(post1950_real_mean,post1950_nominal_mean),
  c(post1950_real_sd,post1950_nominal_sd),
  c(post1950_real_geomean,post1950_nominal_geomean),
  c(NA,post1950_excess_mean,rep(NA,4)),
  c(NA,post1950_excess_sd,rep(NA,4)),
  c(NA,post1950_excess_geomean,rep(NA,4)),
  rep(sum(post1950_rows),8)
)

colnames(post1950_panel) = table_columns
rownames(post1950_panel) = table_rows

round(post1950_panel,2)

View(round(post1950_panel,2))

# Combine the two numeric panels
table2_numeric = rbind(
  full_panel,
  post1950_panel
)

# Function for formatting each panel
format_panel = function(panel){
  
  # Confirm that NA values appear only in the expected positions
  expected_na = matrix(
    FALSE,
    nrow = nrow(panel),
    ncol = ncol(panel)
  )
  
  expected_na[4:6,1] = TRUE
  expected_na[4:6,5:8] = TRUE
  
  stopifnot(all(is.na(panel) == expected_na))
  
  # Convert numbers to character values with two decimal places
  display_panel = matrix(
    sprintf("%.2f",panel),
    nrow = nrow(panel),
    ncol = ncol(panel),
    dimnames = dimnames(panel)
  )
  
  # Bill excess returns: not applicable
  display_panel[4:6,1] = "."
  
  # Nominal excess-return area: blank
  display_panel[4:6,5:8] = ""
  
  # Display observations as integers
  display_panel[7, ] = as.character(
    as.integer(panel[7, ])
  )
  
  display_panel
  
}

# Format both panels
full_panel_display = format_panel(full_panel)
post1950_panel_display = format_panel(post1950_panel)

# Stack the formatted panels
table2_values = rbind(
  full_panel_display,
  post1950_panel_display
)

# Delete the original row names
rownames(table2_values) = NULL

#Data used to create the paper_style table
table2_data = data.frame(
  Sample = rep(
    c("Full sample: ","Post-1950: "), each = length(table_rows)
  ),
  Statistic = rep(table_rows,2),
  table2_values,
  check.names = FALSE
)

# Check the size of table2_data
dim(table2_data)

View(table2_data)

# Create the basic gt table
table2_gt = gt(
  table2_data,
  groupname_col = "Sample",
  rowname_col = "Statistic"
)

table2_gt

table2_gt = table2_gt|>             # add label "Real Returns"
  tab_spanner(
    label = "Real returns",
    columns = c(
      'Real Bills',
      'Real Bonds',
      'Real Equity',
      'Real Housing'
    )
  ) |>                              # add label "Nominal Returns"
  tab_spanner(
    label = "Nominal Returns",
    columns = c(
      'Nominal Bills',
      'Nominal Bonds',
      'Nominal Equity',
      'Nominal Housing'
    ) 
  ) |>                              # Shorten column names
      cols_label(
        'Real Bills' = "Bills",
        'Real Bonds' = "Bonds",
        'Real Equity' = "Equity",
        'Real Housing' = "Housing",
        'Nominal Bills' = "Bills",
        'Nominal Bonds' = "Bonds",
        'Nominal Equity' = "Equity",
        'Nominal Housing' = "Housing"
      )
                             

table2_gt

# Create table title and bottom note

table2_gt |>
  tab_header(
    title = md("**Table II:** *Global real returns*"
    )
  ) |>
  tab_source_note(
    source_note = md(
      paste0(
        "*Note:* Annual global returns in 16 countries, equally weighted. ",
        "Period coverage differs across countries. ",
        "Consistent coverage within countries: each country-year observation ",
        "used to compute the statistics in this table has data for all four ",
        "asset returns. Excess returns are computed relative to bills."
      )
    )
  )

table2_gt

gtsave(
  table2_gt,
  "output/table2_global_real_returns.html"
)

file.exists(
  "output/table2_global_real_returns.html"
)













