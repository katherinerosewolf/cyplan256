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

#### STOPPED HERE ####


# median household inome (renaming)
working$median_household_income_B19013 <- 
  acs_constructed_variables$B19013e1

# median household value (renaming)
acs_constructed_variables$median_household_value_B25077 <- 
  acs_constructed_variables$B25077e1

# percent white (renaming)
acs_constructed_variables$percent_white_B03002 <- 
  acs_constructed_variables$B03002e3/
  acs_constructed_variables$B03002e1

# population density (people/km2, ALAND in m2)
acs_constructed_variables$population_density_B01001_ALAND <- 
  (1000000*
     acs_constructed_variables$B01001e1/
     acs_constructed_variables$ALAND)

# percent of population with high school education or more
acs_constructed_variables$percent_high_school_plus_B15003 <- 
  (
    (
      acs_constructed_variables$B15003e17 + 
        acs_constructed_variables$B15003e18 + 
        acs_constructed_variables$B15003e19 + 
        acs_constructed_variables$B15003e20 + 
        acs_constructed_variables$B15003e21 + 
        acs_constructed_variables$B15003e22 + 
        acs_constructed_variables$B15003e23 + 
        acs_constructed_variables$B15003e24 + 
        acs_constructed_variables$B15003e25
    )
    /acs_constructed_variables$B15003e1
  )

# median age
acs_constructed_variables$median_age_B01002 <- 
  acs_constructed_variables$B01002e1


# income
median_household_income

# Native American etc.
total_for_black <-
number_black <-
percent_black <-

# Asian
percent_hispanic_latinx <-

# Black African American
total_for_black <-
number_black <-
percent_black <-

# Hispanic/Latinx
total_for_hispanic_latinx <-
number_hispanic_latinx <-
percent_hispanic_latinx <-

# % below poverty
total_for_poverty <-
number_above_poverty <-
number_below_poverty <-
percent_below_poverty <- 

# educational attainment
total_for_education <-
number_completed_high_school <-
percent_completed_high_school <-

# population density
total_for_population_density <-
area_for_population_density <-
population_density <-




# 