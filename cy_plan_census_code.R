# Oil and Gas Disposal Well Analysis for Kansas
# Katherine Wolf
# November 2018

#### check working directory ####

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



#### create necessary functions ####

# notin function
`%notin%`  <-  function(x,y) !(x %in% y) # define "not-in" function

# function to force R to stop
my_wait <- function() {
  tt <- tktoplevel()
  tkpack(tkbutton(
    tt, 
    text = 'continue', 
    command = 
      function()tkdestroy(tt)),
          side='bottom')
  tkbind(tt, '<Key>', 
         function()tkdestroy(tt))
    tkwait.window(tt)
}



# #### raw data import ####
# 
# # well data import
# ks_wells_2018_11_01 <- read.csv(file = "ks_wells_2018_11_01.txt", stringsAsFactors = FALSE) # import raw data
# save(ks_wells_2018_11_01, file = "ks_wells_2018_11_01.rdata") # save as an rdata file
# 
# # UIC data import
# ks_uic_2018_09_04 <- fread(file = "KS_UIC_archive_2018_09_04.txt") # import raw data
# save(ks_uic_2018_09_04, file = "ks_uic_2018_09_04.rdata") # save as an rdata file
#
# # ACS data import
# BG_METADATA_2016 <-
#   fread("BG_METADATA_2016.txt")
# X00_COUNTS <- 
#   fread("X00_COUNTS.txt")
# X01_AGE_AND_SEX <- 
#   fread("X01_AGE_AND_SEX.txt")
# X02_RACE <- 
#   fread("X02_RACE.txt")
# X03_HISPANIC_OR_LATINO_ORIGIN <- 
#   fread("X03_HISPANIC_OR_LATINO_ORIGIN.txt")
# X07_MIGRATION <- 
#   fread("X07_MIGRATION.txt")
# X08_COMMUTING <- 
#   fread("X08_COMMUTING.txt")
# X09_CHILDREN_HOUSEHOLD_RELATIONSHIP <- 
#   fread("X09_CHILDREN_HOUSEHOLD_RELATIONSHIP.txt")
# X11_HOUSEHOLD_FAMILY_SUBFAMILIES <- 
#   fread("X11_HOUSEHOLD_FAMILY_SUBFAMILIES.txt")
# X12_MARITAL_STATUS_AND_HISTORY <- 
#   fread("X12_MARITAL_STATUS_AND_HISTORY.txt")
# X14_SCHOOL_ENROLLMENT <- 
#   fread("X14_SCHOOL_ENROLLMENT.txt")
# X15_EDUCATIONAL_ATTAINMENT <- 
#   fread("X15_EDUCATIONAL_ATTAINMENT.txt")
# X16_LANGUAGE_SPOKEN_AT_HOME <- 
#   fread("X16_LANGUAGE_SPOKEN_AT_HOME.txt")
# X17_POVERTY <- 
#   fread("X17_POVERTY.txt")
# X19_INCOME <- 
#   fread("X19_INCOME.txt")
# X20_EARNINGS <- 
#   fread("X20_EARNINGS.txt")
# X21_VETERAN_STATUS <- 
#   fread("X21_VETERAN_STATUS.txt")
# X22_FOOD_STAMPS <- 
#   fread("X22_FOOD_STAMPS.txt")
# X23_EMPLOYMENT_STATUS <- 
#   fread("X23_EMPLOYMENT_STATUS.txt")
# X24_INDUSTRY_OCCUPATION <- 
#   fread("X24_INDUSTRY_OCCUPATION.txt")
# X25_HOUSING_CHARACTERISTICS <- 
#   fread("X25_HOUSING_CHARACTERISTICS.txt")
# X27_HEALTH_INSURANCE <- 
#   fread("X27_HEALTH_INSURANCE.txt")
# X99_IMPUTATION <- 
#   fread("X99_IMPUTATION.txt")
# ACS_2016_5YR_BG_20_KANSAS <- 
#   fread("ACS_2016_5YR_BG_20_KANSAS.txt")
# 
# # save as rdata files
# save(BG_METADATA_2016,
#      file = "BG_METADATA_2016.rdata")
# save(X00_COUNTS,
#      file = "X00_COUNTS.rdata")
# save(X01_AGE_AND_SEX,
#      file = "X01_AGE_AND_SEX.rdata")
# save(X02_RACE,
#      file = "X02_RACE.rdata")
# save(X03_HISPANIC_OR_LATINO_ORIGIN,
#      file = "X03_HISPANIC_OR_LATINO_ORIGIN.rdata")
# save(X07_MIGRATION,
#      file = "X07_MIGRATION.rdata")
# save(X08_COMMUTING,
#      file = "X08_COMMUTING.rdata")
# save(X09_CHILDREN_HOUSEHOLD_RELATIONSHIP,
#      file = "X09_CHILDREN_HOUSEHOLD_RELATIONSHIP.rdata")
# save(X11_HOUSEHOLD_FAMILY_SUBFAMILIES,
#      file = "X11_HOUSEHOLD_FAMILY_SUBFAMILIES.rdata")
# save(X12_MARITAL_STATUS_AND_HISTORY,
#      file = "X12_MARITAL_STATUS_AND_HISTORY.rdata")
# save(X14_SCHOOL_ENROLLMENT,
#      file = "X14_SCHOOL_ENROLLMENT.rdata")
# save(X15_EDUCATIONAL_ATTAINMENT,
#      file = "X15_EDUCATIONAL_ATTAINMENT.rdata")
# save(X16_LANGUAGE_SPOKEN_AT_HOME,
#      file = "X16_LANGUAGE_SPOKEN_AT_HOME.rdata")
# save(X17_POVERTY,
#      file = "X17_POVERTY.rdata")
# save(X19_INCOME,
#      file = "X19_INCOME.rdata")
# save(X20_EARNINGS,
#      file = "X20_EARNINGS.rdata")
# save(X21_VETERAN_STATUS,
#      file = "X21_VETERAN_STATUS.rdata")
# save(X22_FOOD_STAMPS,
#      file = "X22_FOOD_STAMPS.rdata")
# save(X23_EMPLOYMENT_STATUS,
#      file = "X23_EMPLOYMENT_STATUS.rdata")
# save(X24_INDUSTRY_OCCUPATION,
#      file = "X24_INDUSTRY_OCCUPATION.rdata")
# save(X25_HOUSING_CHARACTERISTICS,
#      file = "X25_HOUSING_CHARACTERISTICS.rdata")
# save(X27_HEALTH_INSURANCE,
#      file = "X27_HEALTH_INSURANCE.rdata")
# save(X99_IMPUTATION,
#      file = "X99_IMPUTATION.rdata")
# save(ACS_2016_5YR_BG_20_KANSAS,
#      file = "ACS_2016_5YR_BG_20_KANSAS.rdata")
#
# # convert everything to a data table
# BG_METADATA_2016_data_table <- 
#   data.table(BG_METADATA_2016)
# X00_COUNTS_data_table <- 
#   data.table(X00_COUNTS)
# X01_AGE_AND_SEX_data_table <- 
#   data.table(X01_AGE_AND_SEX)
# X02_RACE_data_table <- 
#   data.table(X02_RACE)
# X03_HISPANIC_OR_LATINO_ORIGIN_data_table <- 
#   data.table(X03_HISPANIC_OR_LATINO_ORIGIN)
# X07_MIGRATION_data_table <- 
#   data.table(X07_MIGRATION)
# X08_COMMUTING_data_table <- 
#   data.table(X08_COMMUTING)
# X09_CHILDREN_HOUSEHOLD_RELATIONSHIP_data_table <- 
#   data.table(X09_CHILDREN_HOUSEHOLD_RELATIONSHIP)
# X11_HOUSEHOLD_FAMILY_SUBFAMILIES_data_table <- 
#   data.table(X11_HOUSEHOLD_FAMILY_SUBFAMILIES)
# X12_MARITAL_STATUS_AND_HISTORY_data_table <- 
#   data.table(X12_MARITAL_STATUS_AND_HISTORY)
# X14_SCHOOL_ENROLLMENT_data_table <- 
#   data.table(X14_SCHOOL_ENROLLMENT)
# X15_EDUCATIONAL_ATTAINMENT_data_table <- 
#   data.table(X15_EDUCATIONAL_ATTAINMENT)
# X16_LANGUAGE_SPOKEN_AT_HOME_data_table <- 
#   data.table(X16_LANGUAGE_SPOKEN_AT_HOME)
# X17_POVERTY_data_table <- 
#   data.table(X17_POVERTY)
# X19_INCOME_data_table <- 
#   data.table(X19_INCOME)
# X20_EARNINGS_data_table <- 
#   data.table(X20_EARNINGS)
# X21_VETERAN_STATUS_data_table <- 
#   data.table(X21_VETERAN_STATUS)
# X22_FOOD_STAMPS_data_table <- 
#   data.table(X22_FOOD_STAMPS)
# X23_EMPLOYMENT_STATUS_data_table <- 
#   data.table(X23_EMPLOYMENT_STATUS)
# X24_INDUSTRY_OCCUPATION_data_table <- 
#   data.table(X24_INDUSTRY_OCCUPATION)
# X25_HOUSING_CHARACTERISTICS_data_table <- 
#   data.table(X25_HOUSING_CHARACTERISTICS)
# X27_HEALTH_INSURANCE_data_table <- 
#   data.table(X27_HEALTH_INSURANCE)
# X99_IMPUTATION_data_table <- 
#   data.table(X99_IMPUTATION)
# ACS_2016_5YR_BG_20_KANSAS_data_table <- 
#   data.table(ACS_2016_5YR_BG_20_KANSAS)
# 
# # save data tables
# save(BG_METADATA_2016_data_table,
#      file="BG_METADATA_2016_data_table.rdata")
# save(X00_COUNTS_data_table,
#      file="X00_COUNTS_data_table.rdata")
# save(X01_AGE_AND_SEX_data_table,
#      file="X01_AGE_AND_SEX_data_table.rdata")
# save(X02_RACE_data_table,
#      file="X02_RACE_data_table.rdata")
# save(X03_HISPANIC_OR_LATINO_ORIGIN_data_table,
#      file="X03_HISPANIC_OR_LATINO_ORIGIN_data_table.rdata")
# save(X07_MIGRATION_data_table,
#      file="X07_MIGRATION_data_table.rdata")
# save(X08_COMMUTING_data_table,
#      file="X08_COMMUTING_data_table.rdata")
# save(X09_CHILDREN_HOUSEHOLD_RELATIONSHIP_data_table,
#      file="X09_CHILDREN_HOUSEHOLD_RELATIONSHIP_data_table.rdata")
# save(X11_HOUSEHOLD_FAMILY_SUBFAMILIES_data_table,
#      file="X11_HOUSEHOLD_FAMILY_SUBFAMILIES_data_table.rdata")
# save(X12_MARITAL_STATUS_AND_HISTORY_data_table,
#      file="X12_MARITAL_STATUS_AND_HISTORY_data_table.rdata")
# save(X14_SCHOOL_ENROLLMENT_data_table,
#      file="X14_SCHOOL_ENROLLMENT_data_table.rdata")
# save(X15_EDUCATIONAL_ATTAINMENT_data_table,
#      file="X15_EDUCATIONAL_ATTAINMENT_data_table.rdata")
# save(X16_LANGUAGE_SPOKEN_AT_HOME_data_table,
#      file="X16_LANGUAGE_SPOKEN_AT_HOME_data_table.rdata")
# save(X17_POVERTY_data_table,
#      file="X17_POVERTY_data_table.rdata")
# save(X19_INCOME_data_table,
#      file="X19_INCOME_data_table.rdata")
# save(X20_EARNINGS_data_table,
#      file="X20_EARNINGS_data_table.rdata")
# save(X21_VETERAN_STATUS_data_table,
#      file="X21_VETERAN_STATUS_data_table.rdata")
# save(X22_FOOD_STAMPS_data_table,
#      file="X22_FOOD_STAMPS_data_table.rdata")
# save(X23_EMPLOYMENT_STATUS_data_table,
#      file="X23_EMPLOYMENT_STATUS_data_table.rdata")
# save(X24_INDUSTRY_OCCUPATION_data_table,
#      file="X24_INDUSTRY_OCCUPATION_data_table.rdata")
# save(X25_HOUSING_CHARACTERISTICS_data_table,
#      file="X25_HOUSING_CHARACTERISTICS_data_table.rdata")
# save(X27_HEALTH_INSURANCE_data_table,
#      file="X27_HEALTH_INSURANCE_data_table.rdata")
# save(X99_IMPUTATION_data_table,
#      file="X99_IMPUTATION_data_table.rdata")
# save(ACS_2016_5YR_BG_20_KANSAS_data_table,
#      file="ACS_2016_5YR_BG_20_KANSAS_data_table.rdata")
#
#
#
# # load data tables
# load(file="BG_METADATA_2016_data_table.rdata")
# load(file="X00_COUNTS_data_table.rdata")
# load(file="X01_AGE_AND_SEX_data_table.rdata")
# load(file="X02_RACE_data_table.rdata")
# load(file="X03_HISPANIC_OR_LATINO_ORIGIN_data_table.rdata")
# load(file="X07_MIGRATION_data_table.rdata")
# load(file="X08_COMMUTING_data_table.rdata")
# load(file="X09_CHILDREN_HOUSEHOLD_RELATIONSHIP_data_table.rdata")
# load(file="X11_HOUSEHOLD_FAMILY_SUBFAMILIES_data_table.rdata")
# load(file="X12_MARITAL_STATUS_AND_HISTORY_data_table.rdata")
# load(file="X14_SCHOOL_ENROLLMENT_data_table.rdata")
# load(file="X15_EDUCATIONAL_ATTAINMENT_data_table.rdata")
# load(file="X16_LANGUAGE_SPOKEN_AT_HOME_data_table.rdata")
# load(file="X17_POVERTY_data_table.rdata")
# load(file="X19_INCOME_data_table.rdata")
# load(file="X20_EARNINGS_data_table.rdata")
# load(file="X21_VETERAN_STATUS_data_table.rdata")
# load(file="X22_FOOD_STAMPS_data_table.rdata")
# load(file="X23_EMPLOYMENT_STATUS_data_table.rdata")
# load(file="X24_INDUSTRY_OCCUPATION_data_table.rdata")
# load(file="X25_HOUSING_CHARACTERISTICS_data_table.rdata")
# load(file="X27_HEALTH_INSURANCE_data_table.rdata")
# load(file="X99_IMPUTATION_data_table.rdata")
# load(file="ACS_2016_5YR_BG_20_KANSAS_data_table.rdata")
# 
# # define merge function
# merge_data_tables <- function(x, y) x[y, on = "GEOID"]
# 
# # perform the merge function en masse
# acs_all_data_tables <- Reduce(
#   merge_data_tables,
#   list(X00_COUNTS_data_table,
#        X01_AGE_AND_SEX_data_table,
#        X02_RACE_data_table,
#        X03_HISPANIC_OR_LATINO_ORIGIN_data_table,
#        X07_MIGRATION_data_table,
#        X08_COMMUTING_data_table,
#        X09_CHILDREN_HOUSEHOLD_RELATIONSHIP_data_table,
#        X11_HOUSEHOLD_FAMILY_SUBFAMILIES_data_table,
#        X12_MARITAL_STATUS_AND_HISTORY_data_table,
#        X14_SCHOOL_ENROLLMENT_data_table,
#        X15_EDUCATIONAL_ATTAINMENT_data_table,
#        X16_LANGUAGE_SPOKEN_AT_HOME_data_table,
#        X17_POVERTY_data_table,
#        X19_INCOME_data_table,
#        X20_EARNINGS_data_table,
#        X21_VETERAN_STATUS_data_table,
#        X22_FOOD_STAMPS_data_table,
#        X23_EMPLOYMENT_STATUS_data_table,
#        X24_INDUSTRY_OCCUPATION_data_table,
#        X25_HOUSING_CHARACTERISTICS_data_table,
#        X27_HEALTH_INSURANCE_data_table,
#        X99_IMPUTATION_data_table)
#   )
# 
# # save compiled acs data table
# save(acs_all_data_tables, 
#      file = "acs_all_data_tables.rdata")







#### load necessary files ####

# load well data
load(file = "ks_wells_2018_11_01.rdata")

# load UIC data
load(file = "ks_uic_2018_09_04.rdata")

# # load ACS data
# load(file = "BG_METADATA_2016.rdata")
# load(file = "X00_COUNTS.rdata")
# load(file = "X01_AGE_AND_SEX.rdata")
# load(file = "X02_RACE.rdata")
# load(file = "X03_HISPANIC_OR_LATINO_ORIGIN.rdata")
# load(file = "X07_MIGRATION.rdata")
# load(file = "X08_COMMUTING.rdata")
# load(file = "X09_CHILDREN_HOUSEHOLD_RELATIONSHIP.rdata")
# load(file = "X11_HOUSEHOLD_FAMILY_SUBFAMILIES.rdata")
# load(file = "X12_MARITAL_STATUS_AND_HISTORY.rdata")
# load(file = "X14_SCHOOL_ENROLLMENT.rdata")
# load(file = "X15_EDUCATIONAL_ATTAINMENT.rdata")
# load(file = "X16_LANGUAGE_SPOKEN_AT_HOME.rdata")
# load(file = "X17_POVERTY.rdata")
# load(file = "X19_INCOME.rdata")
# load(file = "X20_EARNINGS.rdata")
# load(file = "X21_VETERAN_STATUS.rdata")
# load(file = "X22_FOOD_STAMPS.rdata")
# load(file = "X23_EMPLOYMENT_STATUS.rdata")
# load(file = "X24_INDUSTRY_OCCUPATION.rdata")
# load(file = "X25_HOUSING_CHARACTERISTICS.rdata")
# load(file = "X27_HEALTH_INSURANCE.rdata")
# load(file = "X99_IMPUTATION.rdata")
# load(file = "ACS_2016_5YR_BG_20_KANSAS.rdata")

# load collected ACS data from TIGER attribute tables
load(file = "acs_all_data_tables.rdata")

# load ACS shapefile data from its attribute table
load(file="ACS_2016_5YR_BG_20_KANSAS_data_table.rdata")

# load ACS metadata
load(file = "BG_METADATA_2016.rdata")



#### census data ####

# match "GEOID_Data" in ACS_2016_5YR_BG_20_KANSAS_data_table to "GEOID"

# making working dataset
ACS_2016_5YR_BG_20_KANSAS_data_table_working <-
  ACS_2016_5YR_BG_20_KANSAS_data_table

# convert factors with different levels to characters
ACS_2016_5YR_BG_20_KANSAS_data_table_working$GEOID_Data <- 
  as.character(ACS_2016_5YR_BG_20_KANSAS_data_table_working$GEOID_Data)

# preserve the old GEOID column for posterity as "GEOID_simple"
ACS_2016_5YR_BG_20_KANSAS_data_table_working$GEOID_simple <- 
  ACS_2016_5YR_BG_20_KANSAS_data_table_working$GEOID

# replace GEOID column in working data
ACS_2016_5YR_BG_20_KANSAS_data_table_working$GEOID <- 
  ACS_2016_5YR_BG_20_KANSAS_data_table_working$GEOID_Data

# check type
str(ACS_2016_5YR_BG_20_KANSAS_data_table_working$GEOID)

# do the merge
acs_2016_5_year_with_geography <- merge(acs_all_data_tables, 
               ACS_2016_5YR_BG_20_KANSAS_data_table_working, 
               by = "GEOID")

# final full ACS data file saved here in .rdata format (12.7 GB)
save(acs_2016_5_year_with_geography, 
     file = "acs_2016_5_year_with_geography.rdata")

# final full ACS data file saved here in portable .csv format
write.csv(acs_2016_5_year_with_geography, 
          file = "acs_2016_5_year_with_geography.csv")

# save data key
acs_2016_5_year_metadata <-   # rename the metadata
  BG_METADATA_2016   
save(acs_2016_5_year_metadata,   # save the metadata as an r file
     file="acs_2016_5_year_metadata")
write.csv(acs_2016_5_year_metadata,   # save the metadat as a .csv file
          file="acs_2016_5_year_metadata.csv")

# # load final data without regenerating
# load(file="acs_2016_5_year_with_geography.rdata")
# load(file="acs_metadata.rdata")

# View variable names
View(acs_2016_5_year_metadata)



#### pull ACS data I need ####
total_pop <- 
  acs_2016_5_year_with_geography[
    ,c(
      "GEOID", 
      "B01001e1"
      )
    ]
sex_cat <- acs_2016_5_year_with_geography[
  ,c(
    "GEOID", 
    "B01001e2", 
    "B01001e26"
    )
  ]
sex_age_cat <- acs_2016_5_year_with_geography[
  ,c(
    "GEOID",
    "B01001e3",
    "B01001e4",
    "B01001e5",
    "B01001e6",
    "B01001e7",
    "B01001e8",
    "B01001e9",
    "B01001e10",
    "B01001e11",
    "B01001e12",
    "B01001e13",
    "B01001e14",
    "B01001e15",
    "B01001e16",
    "B01001e17",
    "B01001e18",
    "B01001e19",
    "B01001e20",
    "B01001e21",
    "B01001e22",
    "B01001e23",
    "B01001e24",
    "B01001e25",
    "B01001e27", # 26 is total population of women and omitted
    "B01001e28",
    "B01001e29",
    "B01001e30",
    "B01001e31",
    "B01001e32",
    "B01001e33",
    "B01001e34",
    "B01001e35",
    "B01001e36",
    "B01001e37",
    "B01001e38",
    "B01001e39",
    "B01001e40",
    "B01001e41",
    "B01001e42",
    "B01001e43",
    "B01001e44",
    "B01001e45",
    "B01001e46",
    "B01001e47",
    "B01001e48",
    "B01001e49"
    )
  ]
age_median <- acs_2016_5_year_with_geography[
  ,c(
    "GEOID",
    "B01002e1",
    "B01002e2",
    "B01002e3"
    )
  ]
race_cat <- acs_2016_5_year_with_geography[
  ,c(
    "GEOID",
    "B03002e1",
    "B03002e2",
    "B03002e3",
    "B03002e4",
    "B03002e5",
    "B03002e6",
    "B03002e7",
    "B03002e8",
    "B03002e9",
    "B03002e10",
    "B03002e11",
    "B03002e12",
    "B03002e13",
    "B03002e14",
    "B03002e15",
    "B03002e16",
    "B03002e17",
    "B03002e18",
    "B03002e19",
    "B03002e20",
    "B03002e21")]
education_cat <- acs_2016_5_year_with_geography[
  ,c(
    "GEOID", 
    "B15003e1", 
    "B15003e2", 
    "B15003e3", 
    "B15003e4", 
    "B15003e5", 
    "B15003e6", 
    "B15003e7", 
    "B15003e8", 
    "B15003e9", 
    "B15003e10", 
    "B15003e11", 
    "B15003e12", 
    "B15003e13", 
    "B15003e14", 
    "B15003e15", 
    "B15003e16", 
    "B15003e17", 
    "B15003e18", 
    "B15003e19", 
    "B15003e20", 
    "B15003e21", 
    "B15003e22", 
    "B15003e23", 
    "B15003e24", 
    "B15003e25"
    )
  ]
poverty_ratio_cat<-acs_2016_5_year_with_geography[
  ,c(
    "GEOID", 
    "C17002e1", 
    "C17002e2", 
    "C17002e3", 
    "C17002e4", 
    "C17002e5", 
    "C17002e6", 
    "C17002e7", 
    "C17002e8"
     )
  ]
income_house_cat <- 
  acs_2016_5_year_with_geography[
    ,c(
      "GEOID", 
      "B19001e1", 
      "B19001e2", 
      "B19001e3", 
      "B19001e4", 
      "B19001e5", 
      "B19001e6", 
      "B19001e7", 
      "B19001e8", 
      "B19001e9", 
      "B19001e10", 
      "B19001e11", 
      "B19001e12", 
      "B19001e13", 
      "B19001e14", 
      "B19001e15", 
      "B19001e16", 
      "B19001e17"
      )
    ]
income_house_median<-acs_2016_5_year_with_geography[
  ,c(
    "GEOID", 
    "B19013e1"
    )
  ]
earnings_sex_cat <- 
  acs_2016_5_year_with_geography[
    ,c(
      "GEOID", 
      "B20001e1", 
      "B20001e2", 
      "B20001e3", 
      "B20001e4", 
      "B20001e5", 
      "B20001e6", 
      "B20001e7", 
      "B20001e8", 
      "B20001e9", 
      "B20001e10", 
      "B20001e11", 
      "B20001e12", 
      "B20001e13", 
      "B20001e14", 
      "B20001e15", 
      "B20001e16", 
      "B20001e17", 
      "B20001e18", 
      "B20001e19", 
      "B20001e20", 
      "B20001e21", 
      "B20001e22", 
      "B20001e23", 
      "B20001e24", 
      "B20001e25", 
      "B20001e26", 
      "B20001e27", 
      "B20001e28", 
      "B20001e29", 
      "B20001e30", 
      "B20001e31", 
      "B20001e32", 
      "B20001e33", 
      "B20001e34", 
      "B20001e35", 
      "B20001e36", 
      "B20001e37", 
      "B20001e38", 
      "B20001e39", 
      "B20001e40", 
      "B20001e41", 
      "B20001e42", 
      "B20001e43"
      )
    ]
earnings_median <- 
  acs_2016_5_year_with_geography[
    ,c(
      "GEOID", 
      "B20002e1", 
      "B20002e2", 
      "B20002e3"
      )
    ]
employment_cat <- 
  acs_2016_5_year_with_geography[
    ,c(
      "GEOID",
      "B23025e1",
      "B23025e2",
      "B23025e3",
      "B23025e4",
      "B23025e5",
      "B23025e6",
      "B23025e7"
      )
    ]
home_value_cat <- 
  acs_2016_5_year_with_geography[
    ,c(
      "GEOID", 
      "B25075e1", 
      "B25075e2", 
      "B25075e3", 
      "B25075e4", 
      "B25075e5", 
      "B25075e6", 
      "B25075e7", 
      "B25075e8", 
      "B25075e9", 
      "B25075e10", 
      "B25075e11", 
      "B25075e12", 
      "B25075e13", 
      "B25075e14", 
      "B25075e15", 
      "B25075e16", 
      "B25075e17", 
      "B25075e18", 
      "B25075e19", 
      "B25075e20", 
      "B25075e21", 
      "B25075e22", 
      "B25075e23", 
      "B25075e24", 
      "B25075e25", 
      "B25075e26", 
      "B25075e27"
      )
    ]
health_insurance_cat <- 
  acs_2016_5_year_with_geography[
    ,c(
      "GEOID", 
      "B27010e1", 
      "B27010e2", 
      "B27010e3", 
      "B27010e4", 
      "B27010e5", 
      "B27010e6", 
      "B27010e7", 
      "B27010e8", 
      "B27010e9", 
      "B27010e10", 
      "B27010e11", 
      "B27010e12", 
      "B27010e13", 
      "B27010e14", 
      "B27010e15", 
      "B27010e16", 
      "B27010e17", 
      "B27010e18", 
      "B27010e19", 
      "B27010e20", 
      "B27010e21", 
      "B27010e22", 
      "B27010e23", 
      "B27010e24", 
      "B27010e25", 
      "B27010e27", 
      "B27010e28", 
      "B27010e29", 
      "B27010e30", 
      "B27010e31", 
      "B27010e32", 
      "B27010e33", 
      "B27010e34", 
      "B27010e35", 
      "B27010e36", 
      "B27010e37", 
      "B27010e38", 
      "B27010e39", 
      "B27010e40", 
      "B27010e41", 
      "B27010e42", 
      "B27010e43", 
      "B27010e44", 
      "B27010e45", 
      "B27010e46", 
      "B27010e47", 
      "B27010e48", 
      "B27010e49", 
      "B27010e50", 
      "B27010e51", 
      "B27010e52", 
      "B27010e53", 
      "B27010e54", 
      "B27010e55", 
      "B27010e56", 
      "B27010e57", 
      "B27010e58", 
      "B27010e59", 
      "B27010e60", 
      "B27010e61", 
      "B27010e62", 
      "B27010e63", 
      "B27010e64", 
      "B27010e65", 
      "B27010e66"
      )
    ]
home_value_median <- 
  acs_2016_5_year_with_geography[
    ,c(
      "GEOID", 
      "B25077e1"
      )
    ]
geography_area <- 
  acs_2016_5_year_with_geography[
    ,c(
      "GEOID", 
      "STATEFP", 
      "COUNTYFP", 
      "TRACTCE", 
      "BLKGRPCE", 
      "NAMELSAD", 
      "MTFCC", 
      "FUNCSTAT", 
      "ALAND", 
      "AWATER", 
      "INTPTLAT", 
      "INTPTLON", 
      "Shape_Length", 
      "Shape_Area", 
      "GEOID_simple"
      )
    ]

# merge all of the above together
ftdm1 <- merge(total_pop, sex_cat)
ftdm2 <- merge(ftdm1, sex_age_cat)
ftdm3 <- merge(ftdm2, age_median)
ftdm4 <- merge(ftdm3, race_cat)
ftdm5 <- merge(ftdm4, education_cat)
ftdm6 <- merge(ftdm5, poverty_ratio_cat)
ftdm7 <- merge(ftdm6, income_house_cat)
ftdm8 <- merge(ftdm7, income_house_median)
ftdm9 <- merge(ftdm8, earnings_sex_cat)
ftdm10 <- merge(ftdm9, earnings_median)
ftdm11 <- merge(ftdm10, employment_cat)
ftdm12 <- merge(ftdm11, home_value_cat)
ftdm13 <- merge(ftdm12, home_value_median)
ftdm14 <- merge(ftdm13, health_insurance_cat)
select_acs_data <- merge(ftdm14, geography_area)

# # View resulting dataframe with selected ACS variables
# View(select_acs_data)

# save select_acs_data
save(select_acs_data,
     file = "select_acs_data.rdata")

# # load selected variables
# load(file = "select_acs_data.rdata")


### making variables ####
# making variables
# from Silva:
# median household income (dollars), 
# median household value (dollars), 
# percent of population identifying as White only, 
# population density (population per square mile), 
# percent of population with a high school education/GED or higher, and 
# population median age
# from RMF:  age categories

# make set to modify
acs_constructed_variables <- select_acs_data

# median household inome (renaming)
acs_constructed_variables$median_household_income_B19013 <- 
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

# age categories
acs_constructed_variables$age_below_5 <- 
  acs_constructed_variables$B01001e27 +   # women <5
  acs_constructed_variables$B01001e3      # men <5
acs_constructed_variables$age_5_to_19 <-
  acs_constructed_variables$B01001e28 +   # women 5-9
  acs_constructed_variables$B01001e29 +   # women 10-14
  acs_constructed_variables$B01001e30 +   # women 15-17
  acs_constructed_variables$B01001e31 +   # women 18-19
  acs_constructed_variables$B01001e4 +   # men 5-9
  acs_constructed_variables$B01001e5 +   # men 10-14
  acs_constructed_variables$B01001e6 +   # men 15-17
  acs_constructed_variables$B01001e7     # men 18-19
acs_constructed_variables$age_20_to_64 <-
  acs_constructed_variables$B01001e32 + 
  acs_constructed_variables$B01001e33 + 
  acs_constructed_variables$B01001e34 + 
  acs_constructed_variables$B01001e35 + 
  acs_constructed_variables$B01001e36 + 
  acs_constructed_variables$B01001e37 + 
  acs_constructed_variables$B01001e38 + 
  acs_constructed_variables$B01001e39 + 
  acs_constructed_variables$B01001e40 + 
  acs_constructed_variables$B01001e41 + 
  acs_constructed_variables$B01001e42 + 
  acs_constructed_variables$B01001e43 + 
  acs_constructed_variables$B01001e7 + 
  acs_constructed_variables$B01001e8 + 
  acs_constructed_variables$B01001e9 + 
  acs_constructed_variables$B01001e10 + 
  acs_constructed_variables$B01001e11 + 
  acs_constructed_variables$B01001e12 + 
  acs_constructed_variables$B01001e13 + 
  acs_constructed_variables$B01001e14 + 
  acs_constructed_variables$B01001e15 + 
  acs_constructed_variables$B01001e16 + 
  acs_constructed_variables$B01001e17 + 
  acs_constructed_variables$B01001e18 + 
  acs_constructed_variables$B01001e19
acs_constructed_variables$age_65_plus <- 
  acs_constructed_variables$B01001e44 + 
  acs_constructed_variables$B01001e45 + 
  acs_constructed_variables$B01001e46 + 
  acs_constructed_variables$B01001e47 + 
  acs_constructed_variables$B01001e48 + 
  acs_constructed_variables$B01001e49 + 
  acs_constructed_variables$B01001e20 + 
  acs_constructed_variables$B01001e21 + 
  acs_constructed_variables$B01001e22 + 
  acs_constructed_variables$B01001e23 + 
  acs_constructed_variables$B01001e24 + 
  acs_constructed_variables$B01001e25
  
# save constructed variables
save(acs_constructed_variables, 
     file = "acs_constructed_variables.rdata")

# make constructed variables into .csv
write.csv(acs_constructed_variables, 
          file = "acs_constructed_variables.csv")

# load constructed variables to avoid the above
load(file = "acs_constructed_variables.rdata")
View(acs_constructed_variables)










#### well data ####

ks_clean <- ks_wells_2018_11_01 # make well data file for cleaning



#### formatting changes in main well data ####

# convert dates to dates
ks_clean$permit_as_date  <-  
  as.Date(ks_clean$PERMIT, "%d-%b-%Y") # permit date
ks_clean$spud_as_date  <-  
  as.Date(ks_clean$SPUD, "%d-%b-%Y") # spud date
ks_clean$completion_as_date  <-  
  as.Date(ks_clean$COMPLETION, "%d-%b-%Y") # completion date
ks_clean$plugging_as_date  <-  
  as.Date(ks_clean$PLUGGING, "%d-%b-%Y") # plugging date
ks_clean$modified_as_date  <-  
  as.Date(ks_clean$MODIFIED, "%d-%b-%Y") # modified date
ks_clean$API_NUMBER  <-  
  as.character(ks_clean$API_NUMBER) # make API into a character



#### add analysis categories ####

# categorizing the well as SWD ('swd'), INJ ('inj'), or Class I ('ci')
ks_clean$well_type <- NA

# categorizing the well as in the uic database ('yes') or no ('no')
ks_clean$uic <- NA

# categorizing the well as active ('active'), inactive ('inactive'), drilled ('drill'), future ('future'), or never drilled or used as a saltwater disposal well ('canceled')
ks_clean$activity <- NA

# categorizing the well as plugged ('plugged') or unknown by STATUS or STATUS2
ks_clean$plug_status <- NA

# categorizing the well as plugged or not by plugging_as_date
ks_clean$plug_date_binary <- NA

# categorizing as plugged or not by the above two categories
ks_clean$plug_overall <- NA

# classifying as having or not having an api ('yes' or 'no')
ks_clean$has_api <- NA

# whether the well was classified via either
# (1) a STATUS of "SWD" or "SWD-P&A" ('status');
# (2) a status2 of "Converted to SWD Well" ('status2'); or
# (3) manual comment review ('comments')
ks_clean$assignment_source <- NA

# classifying the detailed well type (see data documentation for possible values)
ks_clean$detailed_well_type <- NA

# raw notes on manual well assignments
ks_clean$assignment_notes <- NA

# whether the comments of the intial well have already been reviewed ('yes', 'NA')
ks_clean$comments_examined <- NA

# whether I reviewed documents from the Kansas Geological Society ('yes', 'no')
ks_clean$kgs_available_documents_verified <- NA

save(ks_clean, 
     file = "ks_clean.rdata") # save results of all these conversions



##### counting duplicates #####

# count unique APIs
number_of_unique_APIs <- length(unique(ks_clean$API_NUMBER))
save(number_of_unique_APIs, file = "number_of_unique_APIs.rdata")

# raw well count
raw_well_count <- nrow(ks_clean)
save(raw_well_count, file = "raw_well_count.rdata")

# overall counts by API
ks_by_API_count <- table(ks_clean$API_NUMBER)
save(ks_by_API_count, file = "ks_by_API_count.rdata")

# view rows with duplicated APIs
index <- 
  duplicated(ks_clean$API_NUMBER) | duplicated(ks_clean$API_NUMBER, 
                                               fromLast = TRUE)
ks_API_dups <- ks_clean[index,]
save(ks_API_dups, file = "ks_API_dups.rdata")

# view counts of duplicates per API
ks_API_dup_counts <- table(ks_API_dups$API_NUMBER)
save(ks_API_dup_counts, file = "ks_dup_API_count.rdata")

# count unique KIDs
unique_KID_count <- length(unique(ks_clean$KID))
save(unique_KID_count, file = "unique_KID_count.rdata")

# counts by well status for rows without APIs
ks_wells_no_API <- 
  ks_clean[which(ks_clean$API_NUMBER == ""),] # isolate rows for wells sans APIs
save(ks_wells_no_API, 
     file = "ks_wells_no_API.rdata") # save the above
ks_wells_no_API_by_status <- 
  table(ks_wells_no_API$STATUS) # table of counts of wells sans API by status
save(ks_wells_no_API_by_status, 
     file = "ks_wells_no_API_by_status.rdata") # save table of counts



##### checking ambiguous wells for inclusion or exclusion #####

# get counts by well status1 (main status)
ks_well_counts_by_status1 <- 
  table(ks_clean$STATUS) # get counts
ks_well_counts_by_status1 <- 
  as.data.frame(ks_well_counts_by_status1) # convert to dataframe
save(ks_well_counts_by_status1, 
     file = "ks_well_counts_by_status1.rdata")
write.csv(ks_well_counts_by_status1, 
          file = "ks_well_counts_by_status1.csv") # write for excel file

# get counts by well status2
ks_well_counts_by_status2 <- 
  table(ks_clean$STATUS2)
ks_well_counts_by_status2 <- 
  as.data.frame(ks_well_counts_by_status2) # convert to dataframe
save(ks_well_counts_by_status2, 
     file = "ks_well_counts_by_status2.rdata")
write.csv(ks_well_counts_by_status2, 
          file = "ks_well_counts_by_status2.csv")

# make vectors of status1s and status2s
ks_all_status1s <- 
  sort(unique(ks_clean$STATUS)) # vector of all STATUS values
ks_all_status2s <- 
  sort(unique(ks_clean$STATUS2)) # vector of all STATUS2 values
save(ks_all_status1s, 
     file = "ks_all_status1s.rdata")
save(ks_all_status2s, # save status1s to file
     file = "ks_all_status2s.rdata") # save status2s to file
write.csv(ks_all_status1s, 
          file = "ks_all_status1s.csv") # csv of status1s for excel
write.csv(ks_all_status2s, 
          file = "ks_all_status2s.csv") # csv of status2s for excel

# make vectors of status1s for further investigation or not
ks_status1_check_further <- 
  sort(c("INTENT",
         "OTHER()",
         "OTHER(NULL)",
         "OTHER(OTHER)",
         "OTHER(TA)",
         "OTHER(TEMP ABD)",
         "OTHER-P&A()",
         "OTHER-P&A(TA)"))

ks_potential_disposal_include_status1s <- 
  sort(c("OTHER()",
         "OTHER(1O&1SWD)",
         "OTHER(CBM/SWD)",
         "OTHER(CLASS ONE (OLD))",
         "OTHER(CLASS1)",
         "OTHER(NHDW)",
         "OTHER(NULL)",
         "OTHER(OIL,SWD)",
         "OTHER(OTHER)",
         "OTHER(SWD-P&A)",
         "OTHER(TA)",
         "OTHER(TEMP ABD)",
         "OTHER-P&A()",
         "OTHER-P&A(CLASS ONE (OLD))",
         "OTHER-P&A(OIL-SWD)",
         "OTHER-P&A(TA)",
         "SWD",
         "SWD-P&A"))

ks_definitely_include_status1s <- 
  sort(c("OTHER(1O&1SWD)",
         "OTHER(CBM/SWD)",
         "OTHER(CLASS ONE (OLD))",
         "OTHER(CLASS1)",
         "OTHER(NHDW)",
         "OTHER(OIL,SWD)",
         "OTHER(SWD-P&A)",
         "OTHER-P&A(CLASS ONE (OLD))",
         "OTHER-P&A(OIL-SWD)",
         "SWD",
         "SWD-P&A"))

ks_potential_disposal_exclude_status1s <- 
  sort(c("INTENT",
         "INJ",
         "INJ-P&A",
         "OTHER(INJ or EOR)",
         "OTHER-P&A(INJ OR )",
         "OTHER-P&A(INJ or EOR)",
         "CBM",
         "CBM-P&A",
         "D&A",
         "EOR",
         "EOR-P&A",
         "GAS",
         "GAS-P&A",
         "LOC",
         "O&G",
         "O&G-P&A",
         "OIL",
         "OIL-P&A",
         "OTHER-P&A(2 OIL)",
         "OTHER-P&A(CATH)",
         "OTHER-P&A(COREHOLE)",
         "OTHER-P&A(GAS-INJ)",
         "OTHER-P&A(GAS-STG)",
         "OTHER-P&A(GSW)",
         "OTHER-P&A(LH)",
         "OTHER-P&A(OBS)",
         "OTHER-P&A(OIL&GAS-INJ)",
         "OTHER-P&A(SHUT-IN)",
         "OTHER-P&A(STRAT)",
         "OTHER-P&A(WATER)",
         "OTHER(2OIL)",
         "OTHER(ABD LOC)",
         "OTHER(CATH)",
         "OTHER(COREHOLE)",
         "OTHER(GAS-INJ)",
         "OTHER(GAS-STG)",
         "OTHER(GAS INJ)",
         "OTHER(GAS SHUT-IN)",
         "OTHER(GSW)",
         "OTHER(HELIUM)",
         "OTHER(LH)",
         "OTHER(Monitor)",
         "OTHER(MONITOR)",
         "OTHER(OBS)",
         "OTHER(OBSERVATION)",
         "OTHER(OIL&GAS-INJ)",
         "OTHER(Oil)",
         "OTHER(OIL/GAS)",
         "OTHER(SHUT-IN)",
         "OTHER(STRAT)",
         "OTHER(WATER)"))

# check lengths of vectors
length(ks_potential_disposal_include_status1s) # count included statii
length(ks_potential_disposal_exclude_status1s) # count excluded statii
length(ks_all_status1s) # above two numbers should sum to this number



#### selecting rows based on status2 ####
# make vector of status2s to include regardless of status1
ks_potential_disposal_include_status2s <- 
  sort(c("Converted to SWD Well")) 

# vector of status2s that don't guarantee inclusion
ks_potential_disposal_exclude_status2s <- 
  setdiff(ks_all_status2s,
          ks_potential_disposal_include_status2s)
ks_potential_disposal_exclude_status2s



#### investigating rows based on comments ####
ks_potential_disposal_comments <-   # make vector of just comments
  ks_clean$COMMENTS

# below identifies rows with comments containing the strings 
# "swd", "disp", "salt", "class", or "waste"
positions_of_possible_comments_to_include <-
  grep("swd|disp|salt|class|waste", 
       ks_potential_disposal_comments, 
       ignore.case = TRUE)

comments_to_review <-   # makes vector of identified comments
  ks_clean$COMMENTS[positions_of_possible_comments_to_include]

# makes .csv file of identified comments
write.csv(comments_to_review,   # makes .csv file of identified comments
          file = "comments_to_review.csv")

# makes dataframe of entire rows matching comments
rows_requiring_comment_investigation <-
  ks_clean[which(ks_clean$COMMENTS %in% comments_to_review),]

# make dataframe of most relevant columns of entire rows matching comments
rows_requiring_comment_investigation_simple <- 
  rows_requiring_comment_investigation[
    ,c("KID",
       "API_NUMBER",
       "activity",
       "well_type",
       "assignment_source",
       "detailed_well_type",
       "assignment_notes",
       "comments_examined",
       "kgs_available_documents_verified",
       "STATUS",
       "STATUS2",
       "COMMENTS"
       )
    ]

# make .csv files of rows matching comments and simple rows matching comments
write.csv(rows_requiring_comment_investigation,
          file = "rows_requiring_comment_investigation.csv")
write.csv(rows_requiring_comment_investigation_simple, 
          file = "rows_requiring_comment_investigation_simple.csv")



#### manual well assignment space ####
# cat("Please assign the ambiguous wells manually in an Excel file.  Afterward, find the window that just opened and click on the 'continue' button to continue the program.  The next prompt asks you to choose the file with the well assignments.")
# my_wait() # runs function to force the program to wait for input

# # import manual assignments
# manual_well_assignment_csv_file <- # asks user to choose the correct .csv file
#   file.choose()
# 
# raw_manual_well_assignments_dataframe <- # converts above .csv to a dataframe
#   read.csv(manual_well_assignment_csv_file)

raw_manual_well_assignments_dataframe <- # converts above .csv to a dataframe
  read.csv(
    file = 
      "rows_requiring_comment_investigation_simple_2018_11_24_back_to_r.csv")

# View(raw_manual_well_assignments_dataframe) # view the import



#### creating the semi-final well list #### 
# This section combines all the manually assigned wells as well as the wells
# pulled due solely due to their STATUS or STATUS2.

# pull well KIDs identified manually or by status1 or status2
kids_from_manual_assignments <-   # KIDs of manually added wells
  ks_clean$KID[which(ks_clean$KID %in% 
                       raw_manual_well_assignments_dataframe$KID)]

kids_status1s <-   # KIDs of wells IDed via status1
  ks_clean$KID[which(ks_clean$STATUS %in% 
                       ks_definitely_include_status1s)]

kids_status2s <-   # KIDs of wells IDed via status2
  ks_clean$KID[which(ks_clean$STATUS2 %in% 
                       ks_potential_disposal_include_status2s)]

# length(kids_from_manual_assignments) # count of manual KIDs
# length(kids_status1s)  # count of status1 KIDs
# length(kids_status2s)  # count of status2 KIDs

# combine the lists
kids_semi_final_list <- c(kids_from_manual_assignments, 
                         kids_status1s,
                         kids_status2s)

# convert the list of three vectors into just one vector
kids_semi_final_list <- 
  unlist(kids_semi_final_list)

kids_semi_final_list <-   # delete duplicates
  unique(kids_semi_final_list)   

ks_semi_final_wells <-   # make dataframe of data from selected wells
  ks_clean[which(ks_clean$KID %in% 
                   kids_semi_final_list),]

# View(ks_semi_final_wells)

# save data to disk
save(ks_semi_final_wells, file = "ks_semi_final_wells.rdata")
write.csv(ks_semi_final_wells, file = "ks_semi_final_wells.csv")



#### create working semi-final dataset ####

ks_semi_final_wells_working <-   # make safe working dataset
  ks_semi_final_wells


# status1s

ks_semi_final_status1s <-   # make vector of status1s in semi-final dataset
  sort(unique(ks_semi_final_wells$STATUS))

# View(ks_semi_final_status1s)   # view vector of status1s

# View(table(ks_semi_final_wells_working$STATUS))   # view table of status1s

write.csv(ks_semi_final_status1s,   # write .csv of status1s
          file = "ks_semi_final_status1s.csv")


# status2s

ks_semi_final_status2s <-   # make vector of status2s in semi-final dataset
  sort(unique(ks_semi_final_wells$STATUS2))

# View(ks_semi_final_status2s)   # view vector of status2s

# View(table(ks_semi_final_wells_working$STATUS2))   # view table of status2s

write.csv(ks_semi_final_status2s,   # write .csv of status2s
          file = "ks_semi_final_status2s.csv")



#### CATEGORY ASSIGNMENTS IN SEMI-FINAL DATASET ####

#### assign has api ####
# assign it
ks_semi_final_wells_working$has_api <-
  ifelse(ks_semi_final_wells_working$API_NUMBER == "", "no", "yes")

# View(ks_semi_final_wells_working)



#### assign well_type ####

# View(table(ks_semi_final_wells_working$STATUS)) view possible status1s

# ks_definitely_include_status1s <- # reminder of status1s chosen to keep
#   sort(c("OTHER(1O&1SWD)",
#          "OTHER(CBM/SWD)",
#          "OTHER(CLASS ONE (OLD))",
#          "OTHER(CLASS1)",
#          "OTHER(NHDW)",
#          "OTHER(OIL,SWD)",
#          "OTHER(SWD-P&A)",
#          "OTHER-P&A(CLASS ONE (OLD))",
#          "OTHER-P&A(OIL-SWD)",
#          "SWD",
#          "SWD-P&A"))

swd_status1s <-   # make vector of status1s that mean swd well
  sort(c("OTHER(1O&1SWD)",
         "OTHER(CBM/SWD)",
         "OTHER(OIL,SWD)",
         "OTHER(SWD-P&A)",
         "OTHER-P&A(OIL-SWD)",
         "SWD",
         "SWD-P&A"))

class1_status1s <-   # make vector of status1s that mean class1 well
  sort(c("OTHER(CLASS ONE (OLD))",
         "OTHER(CLASS1)",
         "OTHER(NHDW)",
         "OTHER-P&A(CLASS ONE (OLD))"))

swd_status2s <-   # make vector of status2s that mean swd well
  sort(c("Converted to SWD Well"))

ks_semi_final_wells_working <-   # assign class1
  within(ks_semi_final_wells_working, 
         well_type[STATUS %in% class1_status1s] <- 'class1')

ks_semi_final_wells_working <-   # assign swd
  within(ks_semi_final_wells_working, 
         well_type[STATUS %in% swd_status1s | 
                     STATUS2 %in% swd_status2s] <- 'swd')



#### assign activity ####

# assign inactive statuses
inactive_status1s <-   # make vector of pa status1s
  sort(c("EOR-P&A",
         "GAS-P&A",
         "INJ-P&A",
         "O&G-P&A",
         "OIL-P&A",
         "OTHER-P&A(CLASS ONE (OLD))",
         "OTHER-P&A(LH)",
         "OTHER-P&A(OIL-SWD)",
         "OTHER-P&A(OIL&GAS-INJ)",
         "OTHER-P&A(STRAT)",
         "OTHER-P&A(TA)",
         "OTHER-P&A(WATER)",
         "OTHER(SWD-P&A)",
         "SWD-P&A",
         "D&A",
         "OTHER(LH)",
         "OTHER(TA)"))

inactive_status2s <-   # make vector of pa status2s
  sort(c("Approved for Plugging - CP-1 Received",
         "Expired Plugging Application (CP-1)",
         "Inactive Well",
         "Injection Authorization Terminated",
         "Injection Authorization Terminated - INACTIVE CODE",
         "KCC Fee Fund Plugging",
         "Plugged and Abandoned",
         "Re-Plugged (non Fee-Fund)",
         "Unplugged Former Injection Well"))

ks_semi_final_wells_working <-   # assign inactive wells
  within(ks_semi_final_wells_working, 
         activity[STATUS %in% inactive_status1s |
                    STATUS2 %in% inactive_status2s] <- 'inactive')


# assign future statuses
future_status2s <-   # make vector of future status2s
  sort(c("Approved Intent to Drill",
         "DEVELOPMENT",
         "ON LIST",
         "Pending Injection Application"))

ks_semi_final_wells_working <-   # assign inactive wells
  within(ks_semi_final_wells_working, 
         activity[STATUS %in% future_status2s] <- 'future')


# assign cancel statuses
cancel_status2s <-   # make vector of cancel status2s
  sort(c("Cancelled API Number",
         "Expired Intent to Drill (C-1)",
         "UIC Application Denied",
         "UIC Application Dismissed",
         "UIC Application Withdrawn"))

ks_semi_final_wells_working <-   # assign cancel wells
  within(ks_semi_final_wells_working, 
         activity[STATUS %in% cancel_status2s] <- 'cancel')


# assign drill statuses
drill_status2s <-   # make vector of drill status2s
  sort(c("Spudded",
         "Well Drilled"))

ks_semi_final_wells_working <-   # assign drill wells
  within(ks_semi_final_wells_working, 
         activity[STATUS %in% drill_status2s] <- 'drill')



#### assign plug ####

# assign plug_status
plug_status1s <-   # make vector of plug status1s
  sort(c("EOR-P&A",
         "GAS-P&A",
         "INJ-P&A",
         "O&G-P&A",
         "OIL-P&A",
         "OTHER-P&A(CLASS ONE (OLD))",
         "OTHER-P&A(LH)",
         "OTHER-P&A(OIL-SWD)",
         "OTHER-P&A(OIL&GAS-INJ)",
         "OTHER-P&A(STRAT)",
         "OTHER-P&A(TA)",
         "OTHER-P&A(WATER)",
         "OTHER(SWD-P&A)",
         "SWD-P&A"))

plug_status2s <-   # make vector of plug status2s
  sort(c("Plugged and Abandoned",
         "Re-Plugged (non Fee-Fund)"))

ks_semi_final_wells_working <-   # assign plugged by status
  within(ks_semi_final_wells_working,
         plug_status[STATUS %in% plug_status1s | 
                       STATUS2 %in% plug_status2s] <-
           'has_plug_status')

ks_semi_final_wells_working <-   # assign rest to not status
  within(ks_semi_final_wells_working,
         plug_status[is.na(plug_status)] <-
           'no_plug_status')

# assign plug_date
ks_semi_final_wells_working <-   # assign plugged by date
  within(ks_semi_final_wells_working, 
         plug_date_binary[!is.na(plugging_as_date)] <-
           'has_plug_date')

ks_semi_final_wells_working <-   # assign rest to no plug date
  within(ks_semi_final_wells_working,
         plug_date_binary[is.na(plug_date_binary)] <-
           'no_plug_date')

View(ks_semi_final_wells_working)

# assign overall plugged
ks_semi_final_wells_working <-   # assign overall plug
  within(ks_semi_final_wells_working,
         plug_overall[plug_status == 
                        "has_plug_status" | 
                       plug_date_binary == "has_plug_date"] <-
           'plugged')

ks_semi_final_wells_working <-   # assign overall plug
  within(ks_semi_final_wells_working,
         plug_overall[is.na(plug_overall)] <-
           'not_plugged')

View(ks_semi_final_wells_working)



#### assign UIC status ####

uic_kids <-   # pull KIDs of uic wells in UIC database
  unique(ks_uic_2018_09_04$KGS_ID)

# assign wells as in or not in the uic
ks_semi_final_wells_working$uic <-   
  ifelse(ks_semi_final_wells_working$KID %in% uic_kids,
         'in_uic',
         'not_in_uic')

View(ks_semi_final_wells_working)

# View(ks_uic)
# kids<-unique(ks_uic$KGS_ID)
# kids
#
# ks_wells_in_uic_data_only <- subset(ks_clean, KID %in% kids)
# View(ks_wells_in_uic_data_only)
# uic_statuses<-table(ks_wells_in_uic_data_only$STATUS)
# View(uic_statuses)
# write.csv(uic_statuses,file="uic_statuses.csv")


#### deal with comments ####
ks_semi_final_wells_working <- merge(ks_semi_final_wells_working, core_manual, by = "KID", all.x = TRUE, all.y = TRUE)
ks_semi_final_wells_working <- within(ks_semi_final_wells_working, well_type[man_well_type == 'swd'] <- 'swd')
ks_semi_final_wells_working <- within(ks_semi_final_wells_working, well_type[man_well_type == 'class1'] <- 'class1')
ks_semi_final_wells_working <- within(ks_semi_final_wells_working, well_type[man_well_type == 'swd_class1'] <- 'swd_class1')
ks_semi_final_wells_working <- within(ks_semi_final_wells_working, activity[man_activity == 'pa'] <- 'pa')
ks_semi_final_wells_working <- within(ks_semi_final_wells_working, activity[man_activity == 'never_completed'] <- 'never_completed')
ks_semi_final_wells_working <- within(ks_semi_final_wells_working, other[STATUS %in% c("SWD","SWD-P&A")] <- 'no')

# assign not-others to "yes"
ks_semi_final_wells_working$other[is.na(ks_semi_final_wells_working$other)]  <-  "yes"
ks_semi_final_wells_working$activity[is.na(ks_semi_final_wells_working$activity)]  <-  "not_pa"
ks_semi_final_wells_working$manual[is.na(ks_semi_final_wells_working$man_well_type)] <- "no"
ks_semi_final_wells_working$manual[is.na(ks_semi_final_wells_working$manual)] <- "yes"

View(ks_semi_final_wells_working)

View(table(ks_semi_final_wells_working$well_type,ks_semi_final_wells_working$has_api))

save(ks_semi_final_wells_working, file = "ks_semi_final_wells_working.rdata")



#### handling duplicates ####



#### deleting those with duplicate APIs
# make dataset without wells without apis
ks_only_apis <- ks_semi_final_wells_working
ks_only_apis <- ks_semi_final_wells_working[!(is.na(ks_semi_final_wells_working$API_NUMBER) | ks_semi_final_wells_working$API_NUMBER==""),]
View(ks_only_apis)

# find duplicate APIs
ks_final_index  <-  duplicated(ks_only_apis$API_NUMBER) | duplicated(ks_only_apis$API_NUMBER, fromLast = TRUE)
ks_API_dups <- ks_only_apis[ks_final_index, ]
# View(ks_API_dups)

# remove duplicate APIs, keeping those last modified
ks_bye_dup_API <- subset(ks_only_apis, ave(modified_as_date, API_NUMBER, FUN = max) == modified_as_date)
View(ks_bye_dup_API)


#### dealing with API duplication based on 4-digit activity code

# delete extraneous four digits
ks_bye_dup_API$API_NUMBER_SIMPLE <- 
  substr(ks_bye_dup_API$API_NUMBER, 0, 12)
View(ks_bye_dup_API)

# find duplicate APIs without extraneous four digits
ks_simple_API_index <- 
  duplicated(ks_bye_dup_API$API_NUMBER_SIMPLE) | duplicated(ks_bye_dup_API$API_NUMBER_SIMPLE, fromLast = TRUE)
ks_simple_API_dups <- 
  ks_bye_dup_API[ks_simple_API_index, ]
View(ks_simple_API_dups)

# remove duplicate APIs, keeping those last modified
ks_bye_dup_API_simple <- subset(ks_bye_dup_API, ave(modified_as_date, API_NUMBER_SIMPLE, FUN = max) == modified_as_date)

View(ks_bye_dup_API_simple)

# re-find duplicates
ks_simple_API_index_2 <- duplicated(ks_bye_dup_API_simple$API_NUMBER_SIMPLE) | duplicated(ks_bye_dup_API_simple$API_NUMBER_SIMPLE, fromLast = TRUE)
ks_simple_API_dups_2 <- ks_bye_dup_API_simple[ks_simple_API_index_2, ]
View(ks_simple_API_dups_2)

ks_bye_dup_API_simple <- ks_bye_dup_API_simple[order(ks_bye_dup_API_simple$API_NUMBER_SIMPLE,ks_bye_dup_API_simple$other),]
View(ks_bye_dup_API_simple)

true_no_dup_API <- ks_bye_dup_API_simple[!duplicated(ks_bye_dup_API_simple$API_NUMBER_SIMPLE),]
View(true_no_dup_API)

# drop never-completeds
no_never_complete <- subset(true_no_dup_API, !(activity == "never_completed"))
View(no_never_complete)

# remove classIs
ks_no_dup_api_or_ci <- subset(no_never_complete, !(well_type == "class1"))
View(ks_no_dup_api_or_ci)


# find duplicated latitudes and longitudes
ks_lat_long_dup_index <- duplicated(ks_no_dup_api_or_ci[c("LATITUDE","LONGITUDE")]) | duplicated(ks_no_dup_api_or_ci[c("LATITUDE","LONGITUDE")], fromLast = TRUE)
ks_lat_long_dup <- ks_no_dup_api_or_ci[ks_lat_long_dup_index, ]
View(ks_lat_long_dup)


View(ks_no_dup_API_or_cI)



# calling it the final thesis data
ks_final_thesis_data <- ks_no_dup_api_or_ci
save(ks_final_thesis_data, file = "ks_final_thesis_data.rdata")
View(ks_final_thesis_data)


table(ks_final_thesis_data$well_type,ks_final_thesis_data$activity)
table(ks_final_thesis_data$other,ks_final_thesis_data$well_type)
 
ks_set_for_mapping <- ks_final_thesis_data[,c("KID","API_NUMBER_SIMPLE","LATITUDE","LONGITUDE","well_type")]
# View(ks_set_for_mapping)
write.csv2(ks_set_for_mapping, file = "ks_set_for_mapping.csv")



# import mapping results back
ks_join_results <- read.csv(file = "well_join_2018_10_30.txt",stringsAsFactors = FALSE)
# View(ks_join_results)
save(ks_join_results, file = "ks_join_results.rdata")
#
# # load needed files if starting from here
load(file = "ks_final_thesis_data.rdata")
load(file = "ks_join_results.rdata")
 

ks_join_to_edit <- ks_join_results

# remove columns not needed in the final analysis dataset
ks_join_to_edit$API_NUMBER <- NULL
ks_join_to_edit$LATITUDE <- NULL
ks_join_to_edit$LONGITUDE <- NULL
ks_join_to_edit$well_type <- NULL
ks_join_to_edit$Join_Count <- NULL
ks_join_to_edit$OBJECTID_1 <- NULL
ks_join_to_edit$TARGET_FID <- NULL
 
ks_wells_and_block_groups <- merge(x = ks_final_thesis_data, y = ks_join_to_edit, by = "KID", all.x = TRUE)
View(ks_wells_and_block_groups)

# currently active classification
ks_wells_and_block_groups$current_active <- NA

# use status
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, current_active[STATUS == "INJ-P&A" | STATUS == "OTHER-P&A(CLASS ONE (OLD))" | STATUS == " OTHER-P&A(INJ OR )" | STATUS == " OTHER-P&A(INJ or EOR)" | STATUS == "OTHER-P&A(OIL-SWD)" | STATUS == " OTHER-P&A(TA)" | STATUS == "SWD-P&A"]  <-  'plugged_abandoned')

# use presence of plugged date
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, current_active[!is.na(plugging_as_date)]  <-  'plugged_abandoned')

# make rest active
ks_wells_and_block_groups$current_active[is.na(ks_wells_and_block_groups$current_active)]  <-  "presumed_active"
View(ks_wells_and_block_groups)

ks_wells_and_block_groups$swd <- 1

ks_well_counts <- aggregate (ks_wells_and_block_groups$swd~GEOID_Data,ks_wells_and_block_groups,sum)
View(ks_well_counts)

# rename GEOID_Data to GEOID in well data
ks_well_counts$GEOID <- ks_well_counts$GEOID_Data
save(ks_well_counts, file = "ks_well_counts.rdata")
load(file = "ks_well_counts.rdata")
 
# load ACS variables
load(file = "acs_constructed_variables.rdata")

# # limit ks block groups to ks
# ks_acs <- acs_constructed_variables[which(acs_constructed_variables$STATEFP=='20'),]
# save(ks_acs, file = "ks_acs.rdata")
load(file = "ks_acs.rdata")

# merge well counts and acs!  hey-o!
KS_FINAL_DATASET <- merge(ks_well_counts, ks_acs, by = "GEOID", all = TRUE)
View(KS_FINAL_DATASET)
save(KS_FINAL_DATASET, file = "KS_FINAL_DATASET.rdata")
write.csv(KS_FINAL_DATASET, file = "KS_FINAL_DATASET.csv")


#### Analyses! ####

# load files if starting from here
load(file = "KS_FINAL_DATASET.rdata")
ks_analysis_dataset <- KS_FINAL_DATASET
View(ks_analysis_dataset)

#### swd well block groups only ####

# make variable for sum of swd wells per block group
ks_analysis_dataset$any_swd_sum <- ks_analysis_dataset[,c("ks_wells_and_block_groups$swd")]
str(ks_analysis_dataset$any_swd_sum)
ks_analysis_dataset <- within(ks_analysis_dataset,any_swd_sum[is.na(ks_analysis_dataset$any_swd_sum)] <- 0)
ks_analysis_dataset[,c("ks_wells_and_block_groups$swd")] <- NULL
View(ks_analysis_dataset)

sum(ks_analysis_dataset$any_swd_sum)


# make new binary variable for have versus not have swd
ks_analysis_dataset$any_swd_binary <- NA
ks_analysis_dataset <- within(ks_analysis_dataset, any_swd_binary[any_swd_sum == 0] <- 'no')
ks_analysis_dataset <- within(ks_analysis_dataset, any_swd_binary[any_swd_sum > 0] <- 'yes')
table(ks_analysis_dataset$any_swd_binary)

names(ks_analysis_dataset)

# drop 12 without people
ks_analysis_dataset <- ks_analysis_dataset %>%
  filter(population_density_B01001_ALAND > 0)

#### correlation matrix of ACS variables ####
names(ks_analysis_dataset) # get variable names

# make dataframe for the correlation dataset
correlation_matrix_data  <-  ks_analysis_dataset[,c("median_household_income_B19013","median_household_value_B25077","percent_white_B03002","population_density_B01001_ALAND","percent_high_school_plus_B15003","median_age_B01002","any_swd_sum","any_swd_binary","GEOID_simple")]

# create the correlation matrix
correlation_matrix  <-  round(cor(correlation_matrix_data[,1:6], use = "pairwise.complete.obs"),2) # rounds to 2 decimal places

# view and write to file
View(correlation_matrix)
write.csv(correlation_matrix, file = "correlation_matrix.csv")

# count observations going into the correlation matrix
counts_pairwise_correlations  <-  count.pairwise(correlation_matrix_data[,1:6], y = NULL,diagonal=TRUE)

# view and write to file
View(counts_pairwise_correlations)
write.csv(counts_pairwise_correlations, file = "counts_pairwise_correlations.csv")


#### checking normality ####

par(mfrow=c(2,2))

# income
density_hh_income <- density(ks_analysis_dataset$median_household_income_B19013, na.rm = TRUE)
plot(density_hh_income, main = "Median Household Income Density Plot")

density_household_value <- density(ks_analysis_dataset$median_household_value_B25077, na.rm = TRUE)
plot(density_household_value, main = "Median Household Value Density Plot")

density_percent_white <- density(ks_analysis_dataset$percent_white_B03002, na.rm = TRUE)
plot(density_percent_white, main = "Percent White Density Plot")

density_pop_density <- density(ks_analysis_dataset$population_density_B01001_ALAND, na.rm = TRUE)
plot(density_pop_density, main = "Population Density Density Plot")

density_percent_high_school <- density(ks_analysis_dataset$percent_high_school_plus_B15003, na.rm = TRUE)
plot(density_percent_high_school, main = "Percent Completed High School Density Plot")

density_median_age <- density(ks_analysis_dataset$median_age_B01002, na.rm = TRUE)
plot(density_median_age, main = "Median Age Density Plot")

density_swd <- density(ks_analysis_dataset$any_swd_sum, na.rm = TRUE)
plot(density_swd, main = "Number of Disposal Wells per Block Group Density Plot")






#### analysis by presence and absence of swd ####
# number of block groups with and without swd wells
table(ks_analysis_dataset$any_swd_binary)



## median household income
# median
aggregate(median_household_income_B19013~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$median_household_income_B19013, na.rm = TRUE)

# interquartile range
aggregate(median_household_income_B19013~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$median_household_income_B19013, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$median_household_income_B19013))
aggregate(median_household_income_B19013~any_swd_binary, data=ks_analysis_dataset, FUN=length)

# t test
t.test(ks_analysis_dataset$median_household_income_B19013[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$median_household_income_B19013[ks_analysis_dataset$any_swd_binary=="no"])


## median household value
# median
aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$median_household_value_B25077, na.rm = TRUE)

# interquartile range
aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$median_household_value_B25077, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$median_household_value_B25077))
aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=length)

# t test
t.test(ks_analysis_dataset$median_household_value_B25077[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$median_household_value_B25077[ks_analysis_dataset$any_swd_binary=="no"])


## percent white
# median
aggregate(percent_white_B03002~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$percent_white_B03002, na.rm = TRUE)

# interquartile range
aggregate(percent_white_B03002~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$percent_white_B03002, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$percent_white_B03002))
aggregate(percent_white_B03002~any_swd_binary, data=ks_analysis_dataset, FUN=length)

# t test
t.test(ks_analysis_dataset$percent_white_B03002[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$percent_white_B03002[ks_analysis_dataset$any_swd_binary=="no"])


## population density
# median
aggregate(population_density_B01001_ALAND~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$population_density_B01001_ALAND, na.rm = TRUE)

# interquartile range
aggregate(population_density_B01001_ALAND~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$population_density_B01001_ALAND, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$population_density_B01001_ALAND))

# t test
t.test(ks_analysis_dataset$population_density_B01001_ALAND[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$population_density_B01001_ALAND[ks_analysis_dataset$any_swd_binary=="no"])


## percent high school or more
# median
aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$percent_high_school_plus_B15003, na.rm = TRUE)

# interquartile range
aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$percent_high_school_plus_B15003, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$percent_high_school_plus_B15003))
aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=length)

# t test
t.test(ks_analysis_dataset$percent_high_school_plus_B15003[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$percent_high_school_plus_B15003[ks_analysis_dataset$any_swd_binary=="no"])


## median age
# median
aggregate(median_age_B01002~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$median_age_B01002, na.rm = TRUE)

# interquartile range
aggregate(median_age_B01002~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$median_age_B01002, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$median_age_B01002))
aggregate(median_age_B01002~any_swd_binary, data=ks_analysis_dataset, FUN=length)

# t test
t.test(ks_analysis_dataset$median_age_B01002[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$median_age_B01002[ks_analysis_dataset$any_swd_binary=="no"])



#### Poisson
poisson_model <- glm(formula = any_swd_sum ~ median_household_value_B25077 + percent_white_B03002 + population_density_B01001_ALAND + percent_high_school_plus_B15003 + median_age_B01002, family = "poisson", data = ks_analysis_dataset)
summary(poisson_model)

poisson_model <- glm(formula = any_swd_sum ~ median_household_income_B19013 + median_household_value_B25077 + percent_white_B03002 + population_density_B01001_ALAND + percent_high_school_plus_B15003 + median_age_B01002, family = "poisson", data = ks_analysis_dataset)
summary(poisson_model)
confint(poisson_model)

ks_analysis_dataset <- within(ks_analysis_dataset, any_swd_binary[ks_analysis_dataset$any_swd_binary == "yes"]  <-  1)
ks_analysis_dataset <- within(ks_analysis_dataset, any_swd_binary[ks_analysis_dataset$any_swd_binary == "no"]  <-  0)
View(ks_analysis_dataset$any_swd_binary)
ks_analysis_dataset$any_swd_binary <- as.numeric(ks_analysis_dataset$any_swd_binary)
View(ks_analysis_dataset$any_swd_binary)

#### logistic model r
logistic_model <- glm(formula = any_swd_binary ~ median_household_income_B19013 + median_household_value_B25077 + percent_white_B03002 + population_density_B01001_ALAND + percent_high_school_plus_B15003 + median_age_B01002, family = "binomial", data = ks_analysis_dataset)
summary(logistic_model)
confint(logistic_model)




write.csv(ks_analysis_dataset, file = "ks_analysis_dataset.csv")