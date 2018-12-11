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



#### variable naming ####

# total population
working_sac_city_acs$total_population_B01001e1 <- 
  working_sac_city_acs$B01001e1

# population density
working_sac_city_acs$population_density_B01001_ALAND <- 
  (1000000*
     working_sac_city_acs$total_population_B01001e1/
     working_sac_city_acs$ALAND)

# total for race purposes
working_sac_city_acs$total_for_race_B02001e1 <- 
  working_sac_city_acs$B02001e1
working_sac_city_acs$total_for_race_margin_B02001m1 <- 
  working_sac_city_acs$B02001m1

# white
working_sac_city_acs$white_number_B02008e1 <- 
  working_sac_city_acs$B02008e1
working_sac_city_acs$white_margin_B02008m1 <- 
  working_sac_city_acs$B02008e1
working_sac_city_acs$white_percent <- 
  working_sac_city_acs$number_white/
  working_sac_city_acs$total_for_race_B02001e1

# nhopi
working_sac_city_acs$nhopi_number_B02012e1 <-
  working_sac_city_acs$B02012e1 
working_sac_city_acs$nhopi_margin_B02012m1 <- 
  working_sac_city_acs$B02012m1
working_sac_city_acs$nhopi_percent <- 
  working_sac_city_acs$nhopi_number_B02012e1/
  working_sac_city_acs$total_for_race_B02001e1
  
# native american alaska native
working_sac_city_acs$aian_number_B02010e1 <-
  working_sac_city_acs$B02010e1 
working_sac_city_acs$aian_margin_B02010m1 <- 
  working_sac_city_acs$B02010m1
working_sac_city_acs$aian_percent <- 
  working_sac_city_acs$aian_number_B02010e1/
  working_sac_city_acs$total_for_race_B02001e1

# asian
working_sac_city_acs$asian_number_B02011e1 <-
  working_sac_city_acs$B02011e1 
working_sac_city_acs$asian_margin_B02011m1 <- 
  working_sac_city_acs$B02011m1
working_sac_city_acs$asian_percent <- 
  working_sac_city_acs$asian_number_B02011e1/
  working_sac_city_acs$total_for_race_B02001e1

# african american
working_sac_city_acs$black_number_B02009e1 <-
  working_sac_city_acs$B02009e1 
working_sac_city_acs$black_margin_B02009m1 <- 
  working_sac_city_acs$B02009m1
working_sac_city_acs$black_percent <- 
  working_sac_city_acs$black_number_B02009e1/
  working_sac_city_acs$total_for_race_B02001e1

# hispanic/latinx
working_sac_city_acs$total_for_ethnicity_B03003e1 <- 
  working_sac_city_acs$B03003e1
working_sac_city_acs$margin_for_ethnicity_B03003m1 <- 
  working_sac_city_acs$B03003m1

working_sac_city_acs$hisp_lat_number_B03003e3 <-
  working_sac_city_acs$B03003e3 
working_sac_city_acs$hisp_lat_margin_B03003m3 <-
  working_sac_city_acs$B03003m3

working_sac_city_acs$not_hisp_lat_number_B03003e2 <-
  working_sac_city_acs$B03003e2 
working_sac_city_acs$not_hisp_lat_margin_B03003m2 <-
  working_sac_city_acs$B03003m2

working_sac_city_acs$hisp_lat_percent_B03003 <- 
  working_sac_city_acs$hisp_lat_number_B03003e3/
  working_sac_city_acs$total_for_ethnicity_B03003e1

# % below poverty
working_sac_city_acs$poverty_total_B17001e1 <- 
  working_sac_city_acs$B17001e1
working_sac_city_acs$poverty_total_margin_B17001m1 <- 
  working_sac_city_acs$B17001m1

working_sac_city_acs$poverty_below_number_B17001e2 <- 
  working_sac_city_acs$B17001e2
working_sac_city_acs$poverty_below_margin_B17001m2 <- 
  working_sac_city_acs$B17001m2

working_sac_city_acs$poverty_above_number_B17001e31 <- 
  working_sac_city_acs$B17001e31
working_sac_city_acs$poverty_above_margin_B17001m31 <- 
  working_sac_city_acs$B17001m31

working_sac_city_acs$poverty_below_percent_B17001 <- 
  working_sac_city_acs$poverty_below_number_B17001e2/
  working_sac_city_acs$poverty_total_B17001e1

# unemployment rate
working_sac_city_acs$employment_16_plus_total_S2301e1 <- 
  working_sac_city_acs$S2301e1
working_sac_city_acs$employment_16_plus_margin_S2301m1 <- 
  working_sac_city_acs$S2301m1
working_sac_city_acs$unemployment_rate_S2301e4 <- 
  working_sac_city_acs$S2301e4
working_sac_city_acs$unemployment_rate_margin_S2301m4 <- 
  working_sac_city_acs$S2301m4

# educational attainment
working_sac_city_acs$education_total_B15003e1 <- 
  working_sac_city_acs$B15003e1
working_sac_city_acs$education_total_margin_B15003m1 <- 
  working_sac_city_acs$B15003m1

working_sac_city_acs$completed_high_school_number_B15003e17_25 <- 
  (working_sac_city_acs$B15003e17 + 
     working_sac_city_acs$B15003e18 + 
     working_sac_city_acs$B15003e19 + 
     working_sac_city_acs$B15003e20 + 
     working_sac_city_acs$B15003e21 + 
     working_sac_city_acs$B15003e22 + 
     working_sac_city_acs$B15003e23 + 
     working_sac_city_acs$B15003e24 + 
     working_sac_city_acs$B15003e25)

working_sac_city_acs$did_not_complete_high_school_number_B15003e2_16 <- 
  (working_sac_city_acs$B15003e2 + 
     working_sac_city_acs$B15003e3 + 
     working_sac_city_acs$B15003e4 + 
     working_sac_city_acs$B15003e5 + 
     working_sac_city_acs$B15003e6 + 
     working_sac_city_acs$B15003e7 + 
     working_sac_city_acs$B15003e8 + 
     working_sac_city_acs$B15003e9 + 
     working_sac_city_acs$B15003e10 + 
     working_sac_city_acs$B15003e11 + 
     working_sac_city_acs$B15003e12 + 
     working_sac_city_acs$B15003e13 + 
     working_sac_city_acs$B15003e14 + 
     working_sac_city_acs$B15003e15 + 
     working_sac_city_acs$B15003e16) # figure out margins later

working_sac_city_acs$completed_high_school_percent_B15003 <- 
  working_sac_city_acs$completed_high_school_number_B15003e17_25/
  working_sac_city_acs$B15003e1

# median age
working_sac_city_acs$median_age_B01002e2 <- 
  working_sac_city_acs$B01002e2
working_sac_city_acs$median_age_margin_B01002m2 <- 
  working_sac_city_acs$B01002m2
  
# linguistic isolation
working_sac_city_acs$ling_iso_total_households_S1602e1 <- 
  working_sac_city_acs$S1602e1
working_sac_city_acs$ling_iso_total_households_S1602m1 <- 
  working_sac_city_acs$S1602m1

working_sac_city_acs$ling_iso_number_S1602e3 <- 
  working_sac_city_acs$S1602e3
working_sac_city_acs$ling_iso_number_margin_S1602m3 <- 
  working_sac_city_acs$S1602m3

working_sac_city_acs$ling_iso_percent_S1602e4 <- 
  working_sac_city_acs$S1602e4
working_sac_city_acs$ling_iso_percent_margin_S1602m4 <- 
  working_sac_city_acs$S1602m4
  
# housing tenure
working_sac_city_acs$housing_tenure_total_B25003e1 <- 
  working_sac_city_acs$B25003e1
working_sac_city_acs$housing_tenure_total_margin_B25003m1 <- 
  working_sac_city_acs$B25003m1

working_sac_city_acs$owner_occupy_number_B25003e2 <- 
  working_sac_city_acs$B25003e2
working_sac_city_acs$owner_occupy_margin_B25003m2 <- 
  working_sac_city_acs$B25003m2

working_sac_city_acs$renter_occupy_number_B25003e3 <- 
  working_sac_city_acs$B25003e3
working_sac_city_acs$renter_occupy_margin_B25003m3 <- 
  working_sac_city_acs$B25003m3
  
working_sac_city_acs$owner_occupy_percent_B25003 <- 
  working_sac_city_acs$owner_occupy_number_B25003e2/
  working_sac_city_acs$housing_tenure_total_B25003e1

working_sac_city_acs$renter_occupy_percent_B25003 <- 
  working_sac_city_acs$renter_occupy_number_B25003e3/
  working_sac_city_acs$housing_tenure_total_B25003e1

# median household income
working_sac_city_acs$median_household_income_B19013e1 <- 
  working_sac_city_acs$B19013e1
working_sac_city_acs$median_household_income_margin_B19013m1 <- 
  working_sac_city_acs$B19013m1

# median household value
working_sac_city_acs$median_household_value_B25077e1 <- 
  working_sac_city_acs$B25077e1
working_sac_city_acs$median_household_value_margin_B25077m1 <- 
  working_sac_city_acs$B25077m1

# age below 5
working_sac_city_acs$age_total_B01001e1 <- 
  working_sac_city_acs$B01001e1
working_sac_city_acs$age_total_margin_B010001m1 <- 
  working_sac_city_acs$B01001m1

working_sac_city_acs$age_below_5_number_B01001e3e27 <- 
  (working_sac_city_acs$B01001e3 + 
  working_sac_city_acs$B01001e27)

working_sac_city_acs$age_over_5_number_B01001 <- 
  (working_sac_city_acs$B01001e4 + 
     working_sac_city_acs$B01001e5 + 
     working_sac_city_acs$B01001e6 + 
     working_sac_city_acs$B01001e7 + 
     working_sac_city_acs$B01001e8 + 
     working_sac_city_acs$B01001e9 + 
     working_sac_city_acs$B01001e10 + 
     working_sac_city_acs$B01001e11 + 
     working_sac_city_acs$B01001e12 + 
     working_sac_city_acs$B01001e13 + 
     working_sac_city_acs$B01001e14 + 
     working_sac_city_acs$B01001e15 + 
     working_sac_city_acs$B01001e16 + 
     working_sac_city_acs$B01001e17 + 
     working_sac_city_acs$B01001e18 + 
     working_sac_city_acs$B01001e19 + 
     working_sac_city_acs$B01001e20 + 
     working_sac_city_acs$B01001e21 + 
     working_sac_city_acs$B01001e22 + 
     working_sac_city_acs$B01001e23 + 
     working_sac_city_acs$B01001e24 + 
     working_sac_city_acs$B01001e25 +
     working_sac_city_acs$B01001e28 + 
     working_sac_city_acs$B01001e29 + 
     working_sac_city_acs$B01001e30 + 
     working_sac_city_acs$B01001e31 + 
     working_sac_city_acs$B01001e32 + 
     working_sac_city_acs$B01001e33 + 
     working_sac_city_acs$B01001e34 + 
     working_sac_city_acs$B01001e35 + 
     working_sac_city_acs$B01001e36 + 
     working_sac_city_acs$B01001e37 + 
     working_sac_city_acs$B01001e38 + 
     working_sac_city_acs$B01001e39 + 
     working_sac_city_acs$B01001e40 + 
     working_sac_city_acs$B01001e41 + 
     working_sac_city_acs$B01001e42 + 
     working_sac_city_acs$B01001e43 + 
     working_sac_city_acs$B01001e44 + 
     working_sac_city_acs$B01001e45 + 
     working_sac_city_acs$B01001e46 + 
     working_sac_city_acs$B01001e47 + 
     working_sac_city_acs$B01001e48 + 
     working_sac_city_acs$B01001e49)

working_sac_city_acs$age_under_5_percent_B01001 <- 
  working_sac_city_acs$age_below_5_number_B01001e3e27/
  working_sac_city_acs$age_total_B01001e1

#### correlation analysis ####
working_sac_city_acs$GEOID
