#### city planning data analysis ####
#### december 2018 ####



getwd() # this directory should be the one with your data files in it!
# if not, use "setwd()" to set the working directory



#### load libraries ####

library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(psych)
library(tcltk2)
library(bit64)
library(scales)
library(forcats)
library(reshape2)



#### read joined ACS 2013-2017 CA (Sacramento county only) census data ####

# assign column classes correctly
raw_acs_2013_2017_joined_data_ca <- 
  fread("acs_2013_2017_joined_data_ca.txt", 
        colClasses = 
          list(character = 'GEOID'))

working_acs_2013_2017_data_ca <-
  raw_acs_2013_2017_joined_data_ca

rm(raw_acs_2013_2017_joined_data_ca)


#### remove rows not in sacramento county ####
working_county_acs <- 
  working_acs_2013_2017_data_ca[
    !is.na(
      working_acs_2013_2017_data_ca$B02008e1), 
    ]

rm(working_acs_2013_2017_data_ca)



#### read data on which tracts are in sacramento city ####
raw_sac_city_tracts <- 
  fread("arcgis/tracts_in_sac_export.txt", 
        colClasses = 
          list(character = 'GEOID'))

working_sac_city_tracts <- 
  raw_sac_city_tracts

rm(raw_sac_city_tracts)



#### remove rows not in sacramento city ####
GEOIDs_sac_tracts <- 
  working_sac_city_tracts$GEOID

working_sac_city_acs <-   
  working_county_acs[
    which(
      working_county_acs$GEOID 
      %in% 
        GEOIDs_sac_tracts
      )
    ]

rm(working_county_acs)



#### import trees per tract, NLCD data, and life expectancy data
raw_outcome_data <- 
  fread("outcome_data_2018_12_12.txt", 
        colClasses = 
          list(character = 'GEOID'))

working_outcome_data <- 
  raw_outcome_data



#### join to the rest of the data ####
data_for_analysis <- 
  merge(x = working_sac_city_acs, 
        y = working_outcome_data, 
        by = "GEOID", 
        all.x = TRUE)



#### variable naming ####

# total population
data_for_analysis$total_population_B01001e1 <- 
  data_for_analysis$B01001e1

# population density
data_for_analysis$population_density_B01001_ALAND <- 
  (1000000*
     data_for_analysis$total_population_B01001e1/
     data_for_analysis$ALAND.x)

# total for race purposes
data_for_analysis$total_for_race_B02001e1 <- 
  data_for_analysis$B02001e1
data_for_analysis$total_for_race_margin_B02001m1 <- 
  data_for_analysis$B02001m1

# white
data_for_analysis$white_number_B02008e1 <- 
  data_for_analysis$B02008e1
data_for_analysis$white_margin_B02008m1 <- 
  data_for_analysis$B02008e1
data_for_analysis$white_percent <- 
  data_for_analysis$white_number_B02008e1/data_for_analysis$total_for_race_B02001e1

# nhopi
data_for_analysis$nhopi_number_B02012e1 <-
  data_for_analysis$B02012e1 
data_for_analysis$nhopi_margin_B02012m1 <- 
  data_for_analysis$B02012m1
data_for_analysis$nhopi_percent <- 
  data_for_analysis$nhopi_number_B02012e1/
  data_for_analysis$total_for_race_B02001e1
  
# native american alaska native
data_for_analysis$aian_number_B02010e1 <-
  data_for_analysis$B02010e1 
data_for_analysis$aian_margin_B02010m1 <- 
  data_for_analysis$B02010m1
data_for_analysis$aian_percent <- 
  data_for_analysis$aian_number_B02010e1/
  data_for_analysis$total_for_race_B02001e1

# asian
data_for_analysis$asian_number_B02011e1 <-
  data_for_analysis$B02011e1 
data_for_analysis$asian_margin_B02011m1 <- 
  data_for_analysis$B02011m1
data_for_analysis$asian_percent <- 
  data_for_analysis$asian_number_B02011e1/
  data_for_analysis$total_for_race_B02001e1

# african american
data_for_analysis$black_number_B02009e1 <-
  data_for_analysis$B02009e1 
data_for_analysis$black_margin_B02009m1 <- 
  data_for_analysis$B02009m1
data_for_analysis$black_percent <- 
  data_for_analysis$black_number_B02009e1/
  data_for_analysis$total_for_race_B02001e1

# hispanic/latinx
data_for_analysis$total_for_ethnicity_B03003e1 <- 
  data_for_analysis$B03003e1
data_for_analysis$margin_for_ethnicity_B03003m1 <- 
  data_for_analysis$B03003m1

data_for_analysis$hisp_lat_number_B03003e3 <-
  data_for_analysis$B03003e3 
data_for_analysis$hisp_lat_margin_B03003m3 <-
  data_for_analysis$B03003m3

data_for_analysis$not_hisp_lat_number_B03003e2 <-
  data_for_analysis$B03003e2 
data_for_analysis$not_hisp_lat_margin_B03003m2 <-
  data_for_analysis$B03003m2

data_for_analysis$hisp_lat_percent_B03003 <- 
  data_for_analysis$hisp_lat_number_B03003e3/
  data_for_analysis$total_for_ethnicity_B03003e1

# % below poverty
data_for_analysis$poverty_total_B17001e1 <- 
  data_for_analysis$B17001e1
data_for_analysis$poverty_total_margin_B17001m1 <- 
  data_for_analysis$B17001m1

data_for_analysis$poverty_below_number_B17001e2 <- 
  data_for_analysis$B17001e2
data_for_analysis$poverty_below_margin_B17001m2 <- 
  data_for_analysis$B17001m2

data_for_analysis$poverty_above_number_B17001e31 <- 
  data_for_analysis$B17001e31
data_for_analysis$poverty_above_margin_B17001m31 <- 
  data_for_analysis$B17001m31

data_for_analysis$poverty_below_percent_B17001 <- 
  data_for_analysis$poverty_below_number_B17001e2/
  data_for_analysis$poverty_total_B17001e1

# unemployment rate
data_for_analysis$employment_16_plus_total_S2301e1 <- 
  data_for_analysis$S2301e1
data_for_analysis$employment_16_plus_margin_S2301m1 <- 
  data_for_analysis$S2301m1
data_for_analysis$unemployment_rate_S2301e4 <- 
  data_for_analysis$S2301e4
data_for_analysis$unemployment_rate_margin_S2301m4 <- 
  data_for_analysis$S2301m4

# educational attainment
data_for_analysis$education_total_B15003e1 <- 
  data_for_analysis$B15003e1
data_for_analysis$education_total_margin_B15003m1 <- 
  data_for_analysis$B15003m1

data_for_analysis$completed_high_school_number_B15003e17_25 <- 
  (data_for_analysis$B15003e17 + 
     data_for_analysis$B15003e18 + 
     data_for_analysis$B15003e19 + 
     data_for_analysis$B15003e20 + 
     data_for_analysis$B15003e21 + 
     data_for_analysis$B15003e22 + 
     data_for_analysis$B15003e23 + 
     data_for_analysis$B15003e24 + 
     data_for_analysis$B15003e25)

data_for_analysis$did_not_complete_high_school_number_B15003e2_16 <- 
  (data_for_analysis$B15003e2 + 
     data_for_analysis$B15003e3 + 
     data_for_analysis$B15003e4 + 
     data_for_analysis$B15003e5 + 
     data_for_analysis$B15003e6 + 
     data_for_analysis$B15003e7 + 
     data_for_analysis$B15003e8 + 
     data_for_analysis$B15003e9 + 
     data_for_analysis$B15003e10 + 
     data_for_analysis$B15003e11 + 
     data_for_analysis$B15003e12 + 
     data_for_analysis$B15003e13 + 
     data_for_analysis$B15003e14 + 
     data_for_analysis$B15003e15 + 
     data_for_analysis$B15003e16) # figure out margins later

data_for_analysis$completed_high_school_percent_B15003 <- 
  data_for_analysis$completed_high_school_number_B15003e17_25/
  data_for_analysis$B15003e1

# median age
data_for_analysis$median_age_B01002e2 <- 
  data_for_analysis$B01002e2
data_for_analysis$median_age_margin_B01002m2 <- 
  data_for_analysis$B01002m2
  
# linguistic isolation
data_for_analysis$ling_iso_total_households_S1602e1 <- 
  data_for_analysis$S1602e1
data_for_analysis$ling_iso_total_households_S1602m1 <- 
  data_for_analysis$S1602m1

data_for_analysis$ling_iso_number_S1602e3 <- 
  data_for_analysis$S1602e3
data_for_analysis$ling_iso_number_margin_S1602m3 <- 
  data_for_analysis$S1602m3

data_for_analysis$ling_iso_percent_S1602e4 <- 
  data_for_analysis$S1602e4
data_for_analysis$ling_iso_percent_margin_S1602m4 <- 
  data_for_analysis$S1602m4
  
# housing tenure
data_for_analysis$housing_tenure_total_B25003e1 <- 
  data_for_analysis$B25003e1
data_for_analysis$housing_tenure_total_margin_B25003m1 <- 
  data_for_analysis$B25003m1

data_for_analysis$owner_occupy_number_B25003e2 <- 
  data_for_analysis$B25003e2
data_for_analysis$owner_occupy_margin_B25003m2 <- 
  data_for_analysis$B25003m2

data_for_analysis$renter_occupy_number_B25003e3 <- 
  data_for_analysis$B25003e3
data_for_analysis$renter_occupy_margin_B25003m3 <- 
  data_for_analysis$B25003m3
  
data_for_analysis$owner_occupy_percent_B25003 <- 
  data_for_analysis$owner_occupy_number_B25003e2/
  data_for_analysis$housing_tenure_total_B25003e1

data_for_analysis$renter_occupy_percent_B25003 <- 
  data_for_analysis$renter_occupy_number_B25003e3/
  data_for_analysis$housing_tenure_total_B25003e1

# median household income
data_for_analysis$median_household_income_B19013e1 <- 
  data_for_analysis$B19013e1
data_for_analysis$median_household_income_margin_B19013m1 <- 
  data_for_analysis$B19013m1

# median household value
data_for_analysis$median_household_value_B25077e1 <- 
  data_for_analysis$B25077e1
data_for_analysis$median_household_value_margin_B25077m1 <- 
  data_for_analysis$B25077m1

# age below 5
data_for_analysis$age_total_B01001e1 <- 
  data_for_analysis$B01001e1
data_for_analysis$age_total_margin_B010001m1 <- 
  data_for_analysis$B01001m1

data_for_analysis$age_below_5_number_B01001e3e27 <- 
  (data_for_analysis$B01001e3 + 
  data_for_analysis$B01001e27)

data_for_analysis$age_over_5_number_B01001 <- 
  (data_for_analysis$B01001e4 + 
     data_for_analysis$B01001e5 + 
     data_for_analysis$B01001e6 + 
     data_for_analysis$B01001e7 + 
     data_for_analysis$B01001e8 + 
     data_for_analysis$B01001e9 + 
     data_for_analysis$B01001e10 + 
     data_for_analysis$B01001e11 + 
     data_for_analysis$B01001e12 + 
     data_for_analysis$B01001e13 + 
     data_for_analysis$B01001e14 + 
     data_for_analysis$B01001e15 + 
     data_for_analysis$B01001e16 + 
     data_for_analysis$B01001e17 + 
     data_for_analysis$B01001e18 + 
     data_for_analysis$B01001e19 + 
     data_for_analysis$B01001e20 + 
     data_for_analysis$B01001e21 + 
     data_for_analysis$B01001e22 + 
     data_for_analysis$B01001e23 + 
     data_for_analysis$B01001e24 + 
     data_for_analysis$B01001e25 +
     data_for_analysis$B01001e28 + 
     data_for_analysis$B01001e29 + 
     data_for_analysis$B01001e30 + 
     data_for_analysis$B01001e31 + 
     data_for_analysis$B01001e32 + 
     data_for_analysis$B01001e33 + 
     data_for_analysis$B01001e34 + 
     data_for_analysis$B01001e35 + 
     data_for_analysis$B01001e36 + 
     data_for_analysis$B01001e37 + 
     data_for_analysis$B01001e38 + 
     data_for_analysis$B01001e39 + 
     data_for_analysis$B01001e40 + 
     data_for_analysis$B01001e41 + 
     data_for_analysis$B01001e42 + 
     data_for_analysis$B01001e43 + 
     data_for_analysis$B01001e44 + 
     data_for_analysis$B01001e45 + 
     data_for_analysis$B01001e46 + 
     data_for_analysis$B01001e47 + 
     data_for_analysis$B01001e48 + 
     data_for_analysis$B01001e49)

data_for_analysis$age_under_5_percent_B01001 <- 
  data_for_analysis$age_below_5_number_B01001e3e27/
  data_for_analysis$age_total_B01001e1




#### quick analysis table ####


#### variable naming ####
data_quick_analysis <- data_for_analysis[,c("GEOID","B01001e1")]
View(data_quick_analysis)

# total population
data_quick_analysis$total_population_B01001e1 <- 
  data_for_analysis$B01001e1

# population density
data_quick_analysis$population_density_B01001_ALAND <- 
  (1000000*
     data_for_analysis$total_population_B01001e1/
     data_for_analysis$ALAND.x)

# total for race purposes
data_quick_analysis$total_for_race_B02001e1 <- 
  data_for_analysis$B02001e1
data_quick_analysis$total_for_race_margin_B02001m1 <- 
  data_for_analysis$B02001m1

# white
data_quick_analysis$white_number_B02008e1 <- 
  data_for_analysis$B02008e1
data_quick_analysis$white_margin_B02008m1 <- 
  data_for_analysis$B02008e1
data_quick_analysis$white_percent <- 
  data_for_analysis$white_number_B02008e1/
  data_for_analysis$total_for_race_B02001e1

# nhopi
data_quick_analysis$nhopi_number_B02012e1 <-
  data_for_analysis$B02012e1 
data_quick_analysis$nhopi_margin_B02012m1 <- 
  data_for_analysis$B02012m1
data_quick_analysis$nhopi_percent <- 
  data_for_analysis$nhopi_number_B02012e1/
  data_for_analysis$total_for_race_B02001e1

# native american alaska native
data_quick_analysis$aian_number_B02010e1 <-
  data_for_analysis$B02010e1 
data_quick_analysis$aian_margin_B02010m1 <- 
  data_for_analysis$B02010m1
data_quick_analysis$aian_percent <- 
  data_for_analysis$aian_number_B02010e1/
  data_for_analysis$total_for_race_B02001e1

# asian
data_quick_analysis$asian_number_B02011e1 <-
  data_for_analysis$B02011e1 
data_quick_analysis$asian_margin_B02011m1 <- 
  data_for_analysis$B02011m1
data_quick_analysis$asian_percent <- 
  data_for_analysis$asian_number_B02011e1/
  data_for_analysis$total_for_race_B02001e1

# african american
data_quick_analysis$black_number_B02009e1 <-
  data_for_analysis$B02009e1 
data_quick_analysis$black_margin_B02009m1 <- 
  data_for_analysis$B02009m1
data_quick_analysis$black_percent <- 
  data_for_analysis$black_number_B02009e1/
  data_for_analysis$total_for_race_B02001e1

# hispanic/latinx
data_quick_analysis$total_for_ethnicity_B03003e1 <- 
  data_for_analysis$B03003e1
data_quick_analysis$margin_for_ethnicity_B03003m1 <- 
  data_for_analysis$B03003m1

data_quick_analysis$hisp_lat_number_B03003e3 <-
  data_for_analysis$B03003e3 
data_quick_analysis$hisp_lat_margin_B03003m3 <-
  data_for_analysis$B03003m3

data_quick_analysis$not_hisp_lat_number_B03003e2 <-
  data_for_analysis$B03003e2 
data_quick_analysis$not_hisp_lat_margin_B03003m2 <-
  data_for_analysis$B03003m2

data_quick_analysis$hisp_lat_percent_B03003 <- 
  data_for_analysis$hisp_lat_number_B03003e3/
  data_for_analysis$total_for_ethnicity_B03003e1

# % below poverty
data_quick_analysis$poverty_total_B17001e1 <- 
  data_for_analysis$B17001e1
data_quick_analysis$poverty_total_margin_B17001m1 <- 
  data_for_analysis$B17001m1

data_quick_analysis$poverty_below_number_B17001e2 <- 
  data_for_analysis$B17001e2
data_quick_analysis$poverty_below_margin_B17001m2 <- 
  data_for_analysis$B17001m2

data_quick_analysis$poverty_above_number_B17001e31 <- 
  data_for_analysis$B17001e31
data_quick_analysis$poverty_above_margin_B17001m31 <- 
  data_for_analysis$B17001m31

data_quick_analysis$poverty_below_percent_B17001 <- 
  data_for_analysis$poverty_below_number_B17001e2/
  data_for_analysis$poverty_total_B17001e1

# unemployment rate
data_quick_analysis$employment_16_plus_total_S2301e1 <- 
  data_for_analysis$S2301e1
data_quick_analysis$employment_16_plus_margin_S2301m1 <- 
  data_for_analysis$S2301m1
data_quick_analysis$unemployment_rate_S2301e4 <- 
  data_for_analysis$S2301e4
data_quick_analysis$unemployment_rate_margin_S2301m4 <- 
  data_for_analysis$S2301m4

# educational attainment
data_quick_analysis$education_total_B15003e1 <- 
  data_for_analysis$B15003e1
data_quick_analysis$education_total_margin_B15003m1 <- 
  data_for_analysis$B15003m1

data_quick_analysis$completed_high_school_number_B15003e17_25 <- 
  (data_for_analysis$B15003e17 + 
     data_for_analysis$B15003e18 + 
     data_for_analysis$B15003e19 + 
     data_for_analysis$B15003e20 + 
     data_for_analysis$B15003e21 + 
     data_for_analysis$B15003e22 + 
     data_for_analysis$B15003e23 + 
     data_for_analysis$B15003e24 + 
     data_for_analysis$B15003e25)

data_quick_analysis$did_not_complete_high_school_number_B15003e2_16 <- 
  (data_for_analysis$B15003e2 + 
     data_for_analysis$B15003e3 + 
     data_for_analysis$B15003e4 + 
     data_for_analysis$B15003e5 + 
     data_for_analysis$B15003e6 + 
     data_for_analysis$B15003e7 + 
     data_for_analysis$B15003e8 + 
     data_for_analysis$B15003e9 + 
     data_for_analysis$B15003e10 + 
     data_for_analysis$B15003e11 + 
     data_for_analysis$B15003e12 + 
     data_for_analysis$B15003e13 + 
     data_for_analysis$B15003e14 + 
     data_for_analysis$B15003e15 + 
     data_for_analysis$B15003e16) # figure out margins later

data_quick_analysis$completed_high_school_percent_B15003 <- 
  data_for_analysis$completed_high_school_number_B15003e17_25/
  data_for_analysis$B15003e1

# median age
data_quick_analysis$median_age_B01002e2 <- 
  data_for_analysis$B01002e2
data_quick_analysis$median_age_margin_B01002m2 <- 
  data_for_analysis$B01002m2

# linguistic isolation
data_quick_analysis$ling_iso_total_households_S1602e1 <- 
  data_for_analysis$S1602e1
data_quick_analysis$ling_iso_total_households_S1602m1 <- 
  data_for_analysis$S1602m1

data_quick_analysis$ling_iso_number_S1602e3 <- 
  data_for_analysis$S1602e3
data_quick_analysis$ling_iso_number_margin_S1602m3 <- 
  data_for_analysis$S1602m3

data_quick_analysis$ling_iso_percent_S1602e4 <- 
  data_for_analysis$S1602e4
data_quick_analysis$ling_iso_percent_margin_S1602m4 <- 
  data_for_analysis$S1602m4

# housing tenure
data_quick_analysis$housing_tenure_total_B25003e1 <- 
  data_for_analysis$B25003e1
data_quick_analysis$housing_tenure_total_margin_B25003m1 <- 
  data_for_analysis$B25003m1

data_quick_analysis$owner_occupy_number_B25003e2 <- 
  data_for_analysis$B25003e2
data_quick_analysis$owner_occupy_margin_B25003m2 <- 
  data_for_analysis$B25003m2

data_quick_analysis$renter_occupy_number_B25003e3 <- 
  data_for_analysis$B25003e3
data_quick_analysis$renter_occupy_margin_B25003m3 <- 
  data_for_analysis$B25003m3

data_quick_analysis$owner_occupy_percent_B25003 <- 
  data_for_analysis$owner_occupy_number_B25003e2/
  data_for_analysis$housing_tenure_total_B25003e1

data_quick_analysis$renter_occupy_percent_B25003 <- 
  data_for_analysis$renter_occupy_number_B25003e3/
  data_for_analysis$housing_tenure_total_B25003e1

# median household income
data_quick_analysis$median_household_income_B19013e1 <- 
  data_for_analysis$B19013e1
data_quick_analysis$median_household_income_margin_B19013m1 <- 
  data_for_analysis$B19013m1

# median household value
data_quick_analysis$median_household_value_B25077e1 <- 
  data_for_analysis$B25077e1
data_quick_analysis$median_household_value_margin_B25077m1 <- 
  data_for_analysis$B25077m1

# age below 5
data_quick_analysis$age_total_B01001e1 <- 
  data_for_analysis$B01001e1
data_quick_analysis$age_total_margin_B010001m1 <- 
  data_for_analysis$B01001m1

data_quick_analysis$age_below_5_number_B01001e3e27 <- 
  (data_for_analysis$B01001e3 + 
     data_for_analysis$B01001e27)

data_quick_analysis$age_over_5_number_B01001 <- 
  (data_for_analysis$B01001e4 + 
     data_for_analysis$B01001e5 + 
     data_for_analysis$B01001e6 + 
     data_for_analysis$B01001e7 + 
     data_for_analysis$B01001e8 + 
     data_for_analysis$B01001e9 + 
     data_for_analysis$B01001e10 + 
     data_for_analysis$B01001e11 + 
     data_for_analysis$B01001e12 + 
     data_for_analysis$B01001e13 + 
     data_for_analysis$B01001e14 + 
     data_for_analysis$B01001e15 + 
     data_for_analysis$B01001e16 + 
     data_for_analysis$B01001e17 + 
     data_for_analysis$B01001e18 + 
     data_for_analysis$B01001e19 + 
     data_for_analysis$B01001e20 + 
     data_for_analysis$B01001e21 + 
     data_for_analysis$B01001e22 + 
     data_for_analysis$B01001e23 + 
     data_for_analysis$B01001e24 + 
     data_for_analysis$B01001e25 +
     data_for_analysis$B01001e28 + 
     data_for_analysis$B01001e29 + 
     data_for_analysis$B01001e30 + 
     data_for_analysis$B01001e31 + 
     data_for_analysis$B01001e32 + 
     data_for_analysis$B01001e33 + 
     data_for_analysis$B01001e34 + 
     data_for_analysis$B01001e35 + 
     data_for_analysis$B01001e36 + 
     data_for_analysis$B01001e37 + 
     data_for_analysis$B01001e38 + 
     data_for_analysis$B01001e39 + 
     data_for_analysis$B01001e40 + 
     data_for_analysis$B01001e41 + 
     data_for_analysis$B01001e42 + 
     data_for_analysis$B01001e43 + 
     data_for_analysis$B01001e44 + 
     data_for_analysis$B01001e45 + 
     data_for_analysis$B01001e46 + 
     data_for_analysis$B01001e47 + 
     data_for_analysis$B01001e48 + 
     data_for_analysis$B01001e49)

data_quick_analysis$age_under_5_percent_B01001 <- 
  data_for_analysis$age_below_5_number_B01001e3e27/
  data_for_analysis$age_total_B01001e1



#### add outcomes ####
data_quick_analysis <- 
  merge(x = data_quick_analysis, 
        y = working_outcome_data, 
        by = "GEOID", 
        all.x = TRUE)

data_quick_analysis$mean_tree_canopy <- data_quick_analysis$MEAN

data_quick_analysis$city_tree_count <- data_quick_analysis$Count_

data_quick_analysis$median_household_value_B25077e1 <- as.numeric(data_quick_analysis$median_household_value_B25077e1)

#### correlation analyses ####

names(data_quick_analysis)
# make dataframe for the correlation dataset
correlation_matrix_data  <-  data_quick_analysis[,c("population_density_B01001_ALAND","median_household_income_B19013e1","median_household_value_B25077e1","white_percent","nhopi_percent","aian_percent","asian_percent","black_percent","hisp_lat_percent_B03003","completed_high_school_percent_B15003","median_age_B01002e2", "poverty_below_percent_B17001","unemployment_rate_S2301e4","ling_iso_percent_S1602e4","renter_occupy_percent_B25003","age_under_5_percent_B01001","mean_tree_canopy","city_tree_count","life_expect")]

View(correlation_matrix_data)

# create the correlation matrix
cormat  <-  round(cor(correlation_matrix_data[,1:19], use = "pairwise.complete.obs"),2) # rounds to 2 decimal places

# view and write to file
View(cormat)
write.csv(cormat, file = "cormat.csv")

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

print(ggheatmap)

life_expec <- 

#### national data ####

#### aian ####
aian_data_sac_alone <- 
  fread(file = "data_for_race_tables_raw_ish/data_for_race_tables/aian_data_for_r_sac_alone.csv")

aian_data_sac_alone$Nation_or_Tribe <- 
  factor(aian_data_sac_alone$Nation_or_Tribe)

aian_data_sac_alone$AIorAN <- 
  ifelse(aian_data_sac_alone$Nation_or_Tribe %in% 
           c("Alaskan Athabascan",
             "Aleut",
             "Inupiat",
             "Tlingit-Haida",
             "Tsimshian",
             "Yup'ik",
             "Alaska Native, no nation or tribe specified"), 
         "Alaska Native", 
         ifelse(aian_data_sac_alone$Nation_or_Tribe %in% 
                  c("Native American or Alaska Native, no nation or tribe specified"), 
                "", 
                "Native American"))

aian_data_sac_alone$AIorAN <- 
  factor(aian_data_sac_alone$AIorAN, 
         levels = c("Native American", 
                    "Alaska Native", 
                    ""))

ggplot(aian_data_sac_alone, 
       aes(x=fct_inorder(Nation_or_Tribe), y=Number)) + 
  geom_bar(position=position_dodge(), 
           stat="identity", colour='black', 
           fill = "green") +
  geom_errorbar(aes(
    ymin = Number - Raw_Error, 
    ymax = Number + Raw_Error), 
    width=.2, 
    position = position_dodge(.9)
  ) + 
  geom_text(aes(label=comma(Number), 
                y = Number + Raw_Error + 150), 
            vjust=1, 
            hjust = 0.5, 
            family = "serif") +
  labs(x = paste(expression("First Nation or tribal identification among those classified as Native American, American Indian, or Alaska Native alone or in any combination in the American Community Survey 2013–2017 five-year estimates (Table B02017) (",italic("n "),"= 10,738 +/- 1,118)")), y = "Number (margin of error)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5), 
        text = element_text (size = 14, 
                             family = "serif"), 
        axis.title.x = element_text(margin=margin(30,0,0,0))) + 
  facet_grid(~AIorAN, space="free", scales="free")



#### nhopi subgroups ####

nhopi_data_sac_alone <- 
  fread(file = "data_for_race_tables_raw_ish/data_for_race_tables/nhopi_data_for_r_sac_alone.csv")

nhopi_data_sac_alone$Group <- 
  factor(nhopi_data_sac_alone$Group, 
         levels = c("Melanesian", 
                    "Micronesian", 
                    "Polynesian", 
                    ""))

ggplot(nhopi_data_sac_alone, 
       aes(x=fct_inorder(Nationality), 
           y=Number)) + 
  geom_bar(position=position_dodge(), 
           stat="identity", 
           colour='black', 
           fill = "green") +
  geom_errorbar(aes(
    ymin = Number - Raw_Error, 
    ymax = Number + Raw_Error), 
    width=.2, 
    position = position_dodge(.9)
  ) + 
  geom_text(aes(label=comma(Number), 
                y = Number + Raw_Error + 250), 
            vjust=1, 
            hjust = 0.5, 
            family = "serif") +
  labs(x = expression(paste("National origin of those classified as Native Hawaiian or Other Pacific Islander alone or in any combination in the American Community Survey 2013–2017 five-year estimates (Table B02019) (", italic("n ")," = 12,560) (",italic("n "),"of nationalities = 12,560 +/- 1,360)")), y = "Number (margin of error)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5), 
        text = element_text (size = 14, 
                             family = "serif"), 
        axis.title.x = element_text(margin=margin(30,0,0,0))) + 
  facet_grid(~Group, space="free", scales="free")



#### asian subgroups ####
asian_bar_graph_data <- 
  fread(file = 
          "data_for_race_tables_raw_ish/data_for_race_tables/
        asian_data_for_r.csv")


ggplot(asian_bar_graph_data, 
       aes(x=as.factor(Nationality), y=Percent, fill=Geography)) + 
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(
    ymin = Percent - Percentage_Error, 
    ymax = Percent + Percentage_Error), 
    width=.2, 
    position = position_dodge(.9)
    ) + 
  labs(x = expression(paste("Percentage and standard error of nationalities among those classified as Asian alone or in any combination in the American Community Survey 2013–2017 five-year estimates (Table B02018) (",italic("n "),"of nationalities = 108,676 +/- 2,222)")), 
       y = "Number (margin of error)") + 
  scale_fill_manual(values = c("Sacramento" = "green", "United States" = "purple")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        text = element_text (size = 14, family = "serif"), 
        axis.title.x = element_text(margin=margin(15,0,0,0)))

asian_data_sac_alone <- 
  fread(file = "data_for_race_tables_raw_ish/data_for_race_tables/asian_data_for_r_sac_alone.csv")

asian_data_sac_alone$Nationality <- 
  factor(asian_data_sac_alone$Nationality)

ggplot(asian_data_sac_alone, 
       aes(x=fct_inorder(Nationality), y=Number)) + 
  geom_bar(position=position_dodge(), stat="identity", colour='black', fill = "green") +
  geom_errorbar(aes(
    ymin = Number - Raw_Error, 
    ymax = Number + Raw_Error), 
    width=.2, 
    position = position_dodge(.9)
  ) + 
  geom_text(aes(label=comma(Number), y = Number + 2500), vjust=1, hjust = 0.5, family = "serif") +
  labs(x = paste(expression("National origin of those classified as Asian alone or in any combination in the American Community Survey 2013–2017 five-year estimates (Table B02018) (",italic("n "),"= 108,676 +/- 2,222)")), y = "Number (margin of error)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        text = element_text (size = 14, family = "serif"), 
        axis.title.x = element_text(margin=margin(15,0,0,0)), 
        axis.title.y = element_text (size = 14, family = "serif"))



#### hispanic latino groups ####
latino_data_sac_alone <- 
  fread(file = "data_for_race_tables_raw_ish/data_for_race_tables/latino_hispanic_for_r_sac_alone.csv")

latino_data_sac_alone$Group <- 
  factor(latino_data_sac_alone$Group, 
         levels = c("", 
                    "Central American", 
                    "South American", 
                    "Other Hispanic or Latino"))

ggplot(latino_data_sac_alone, 
       aes(x=fct_inorder(Ethnicity), 
           y=Number)) + 
  geom_bar(position=position_dodge(), 
           stat="identity", 
           colour='black', 
           fill = "green") +
  geom_errorbar(aes(
    ymin = Number - Raw_Error, 
    ymax = Number + Raw_Error), 
    width=.2, 
    position = position_dodge(.9)
  ) + 
  geom_text(aes(label=comma(Number), 
                y = Number + Raw_Error + 4000), 
            vjust=1, 
            hjust = 0.5, 
            family = "serif") +
  labs(x = expression(paste("National origin of those classified as Hispanic or Latino alone or in any combination in the American Community Survey 2013–2017 five-year estimates (Table B03001) (",italic("n "),"= 138,483 +/- 2.706)")), y = "Number (margin of error)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5), 
        text = element_text (size = 14, 
                             family = "serif"), 
        axis.title.x = element_text(margin=margin(30,0,0,0))) + 
  facet_grid(~Group, space="free", scales="free")







