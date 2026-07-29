# install.packages("haven") #Use haven to read .dta file

library(haven)

main = read_dta("data_raw/rore_public_main.dta")
supp = read_dta("data_raw/rore_public_supplement.dta")

dim(main)
dim(supp)

head(main[ ,c("iso","country","year")])   #Select country_id, country_name, year and check the fist 6 lines

#Merge the main and supplementary datasets by country and year
key = c("iso","year")

#Confirm each country-year appears only once
stopifnot(!anyDuplicated(main[key]))
stopifnot(!anyDuplicated(supp[key]))

#keep only new supplementary variables to avoid duplicate columns.
supp_only = supp[ , c(key,setdiff(names(supp),names(main)))]
data = merge(
  main,
  supp_only,
  by = key,
  all = TRUE,
  sort = FALSE
)

#sort by country and year
data <- data[order(data$iso, data$year), ]

#Check the merged dataset
dim(data)
head(data[ , c("iso","country","year")])

