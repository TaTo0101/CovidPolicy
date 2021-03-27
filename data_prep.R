########################## Constructing the dataset ####################################
######### Author: Tony Gottschalg
######### Date 20.11.2020 

###### Load Packages ####
libs = c("magrittr", "ggplot2", "tidyr", "tibble",
         "lubridate", "dplyr", "ggthemes", "RColorBrewer", "ggpubr",
         "xtable")
if(any(libs%in%installed.packages() == F)){
  libs.1 = libs[which(libs%in%installed.packages() == F)]
  lapply(libs.1, FUN = install.packages, dependencies = T)
}
lapply(libs, library, character.only = T, quiet = T)


##### Load the various datasets ####

setwd("./Data") # CD for easier handling

owid_data = read.csv("covid19_owid_071220.csv") %>% as_tibble()
gmr_data = read.csv("Global_Mobility_Report.csv") %>% as_tibble()
doc_density = read.csv("doctor_density_oecd.csv") %>% as_tibble()

# Hong Kong data set includes no non NA's for the total_cases variable
# thus it will be removed
owid_data = owid_data %>% filter(location != "Hong Kong")

# France & Sweden test data is not contained in the owid dataset
france_test = read.csv("france_test_data.csv", sep = ";") %>% as_tibble()
sweden_test = read.csv("sweden_test_data.csv") %>% as_tibble()

# Sadly, the data does not contain the full range of dates where data on
# testing was recorded. We therefore use weekly data compiled by the 
# european centre for disease prevention and control

weekly_test = read.csv("weekly_test_data_france.csv") %>% as_tibble()

##################################### convert data types and eliminate NA's #######

#### owid ####
backup = owid_data
filter_factor_owid = c("iso_code", "continent", "location", "tests_units")

backup = backup %>% 
  mutate(across(any_of(filter_factor_owid), factor))

backup = backup %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))



#### Include France and Sweden test data####

## First reformat the national data so it can be joined with the owid data

# France
france_test = france_test %>%
  filter(clage90 == 0) #data is seperated by age group

france_test = france_test %>%
  mutate(jour = as.Date(jour, format = "%d.%m.%Y"))
france_test = rename(france_test, date = jour, total_tests = Test)

france_test = france_test %>% select(date, total_tests)
france_test = france_test %>% mutate(location = factor("France")) %>%
  relocate(date, location, total_tests)

# Sweden
sweden_test = sweden_test %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         class = factor(class))
sweden_test = sweden_test %>% group_by(date) %>%
  summarise(total_tests = sum(count)) %>% ungroup()

sweden_test = sweden_test %>% mutate(location = factor("Sweden")) %>%
  relocate(date, location, total_tests)

# Weekly data
weekly_test = weekly_test %>% rename(location = ï..country)

weekly_test = weekly_test %>%
  filter(location %in% c("France", "Sweden"))

# We have data for france starting from week 20 and for sweden from week 17
weekly_test = weekly_test %>% filter(year_week != "2021-W01")

# we use the end date of the week to add dates
k = c(seq.Date(as.Date("2020-02-28"), as.Date("2021-01-01"), by = 7),
  seq.Date(as.Date("2020-01-24"), as.Date("2021-01-01"), by = 7))
weekly_test$date = k

france_early = france_test %>% select(date) %>% slice_head(n = 1)
sweden_early = sweden_test %>% select(date) %>% slice_head(n = 1)


weekly_test = weekly_test %>% filter(date < pull(france_early))
weekly_test = weekly_test %>% select(date, location, tests_done)
weekly_test = rename(weekly_test, total_tests = tests_done)

# We divide the weekly data naively by 7 and transform them to day data
france_weekly = weekly_test %>%
  filter(location == "France", date <= pull(france_early))
sweden_weekly = weekly_test %>%
  filter(location == "Sweden", date <= pull(sweden_early),
         date >= as.Date("2020-02-15"))

# France: 22.02-08.05, , Sweden: (even earlier) 15.02-24.04

dates_france = backup %>% select(date) %>%
  filter(date <= max(france_weekly$date),
         date >= min(france_weekly$date-6)) %>% unique()

dates_sweden = backup %>% select(date) %>%
  filter(date <= max(sweden_weekly$date),
         date >= min(sweden_weekly$date-6)) %>% unique()

# We divide every weekly test number by 7 to get daily values
# where we but a higher weight on Mondays (1/5 instead of 1/7)
# weight monday: 1/5, weight rest 2/15

help_fun_1 = function(x){
  mon = round(x/5)
  rest = round(x * 2/15)
  return(c(mon, rep(rest,6)))
}

test_france_daily = sapply(france_weekly$total_tests, help_fun_1) %>%
  as.vector()
test_sweden_daily = sapply(sweden_weekly$total_tests, help_fun_1) %>%
  as.vector()

weekly_daily_france = tibble(dates_france,
                             location = factor("France"),
                             total_tests = test_france_daily)

weekly_daily_sweden = tibble(dates_sweden,
                             location = factor("Sweden"),
                             total_tests = test_sweden_daily)
# There are gaps between the weekly daily data and the actual daily data
# France misses: 09.05-12.05, Sweden misses: None
# We impute the days by using a rolling 7 day mean
means_test = france_test$total_tests[1:7]
for(i in 0:3){
  means_test = round(append(mean(means_test[1:7]),
                           means_test))
}
dates_france_impute = seq.Date(as.Date("2020-05-09"),
                               as.Date("2020-05-12"), 1)
imputed_france = tibble(date = dates_france_impute,
                            location = factor("France"),
                            total_tests = means_test[1:4])
weekly_daily_france = rbind(weekly_daily_france, imputed_france)

france_test = rbind(weekly_daily_france, france_test)
sweden_test = rbind(weekly_daily_sweden, sweden_test)

# For France we are still missing data on the week 15.02-21.02 (6 days)
# We again employ a rolling 7 day average
means_test = france_test$total_tests[1:7]
for(i in 0:5){
  means_test = round(append(mean(means_test[1:7]),
                      means_test))
}

dates_france_impute_2 = seq.Date(as.Date("2020-02-15"),
                                 as.Date("2020-02-21"), 1)
imputed_france_2 = tibble(date = dates_france_impute_2,
                        location = factor("France"),
                        total_tests = means_test[1:7])
france_test = rbind(imputed_france_2, france_test)

len_france = dim(france_test)[1]
len_owid = dim(backup %>% filter(location == "France",
                                    date >= "2020-02-15"))[1]
backup = backup %>% mutate(total_tests = 
                       replace(total_tests,
                               location == "France" & date >= "2020-02-15",
                               france_test$total_tests[1:len_owid]))
backup = backup %>%
  mutate(total_tests = replace(total_tests,
                               location == "Sweden" & date >= "2020-02-15",
                               sweden_test$total_tests[1:len_owid]))

##### Convert Germany & Spain Weekly data to daily ####

weekly_test = backup %>% filter(location %in% c("Spain", "Germany"))

# We have data for Germany starting from week 10 and for Spain from week 16
dates_weekly = weekly_test %>% group_by(location) %>%
  filter(is.na(total_tests) != TRUE) %>% 
  summarize(date_min = min(date)-6, date_max = max(date)) 

# Note that Germany's data is for the Sunday of a week, whereas
# Spain's data is for Thursday except for the first data point, which is Monday

# we use the end date of the week to add dates for Germany, and the fourth
# date of a week for Spain
k = c(seq.Date(pull(dates_weekly, date_min)[1],
               pull(dates_weekly, date_max)[1], by = 1),
      seq.Date(pull(dates_weekly, date_min)[2],
               pull(dates_weekly, date_max)[2], by = 1))

# Select only the relevant dates for each country,
weekly_test = weekly_test %>% filter(
  (location == "Germany" & date >= pull(dates_weekly, date_min)[1]+6 &
     date <= pull(dates_weekly, date_max)[1]) |
  (location == "Spain" & date >= pull(dates_weekly, date_min)[2]+6 &
     date <= pull(dates_weekly, date_max)[2]) 
                       )
# We do as follows: Extract the test data and divide them in such a way
# that we get 7 data points per date with a stronger weight on Monday
# weight monday: 1/5, weight rest 2/15
# A special case is the first data point of Spain which we will divide manually

# For Germany
help_fun_germany = function(x){
  mon = round(x/5)
  rest = round(x * 2/15)
  return(c(mon, rep(rest,6)))
}

# For Spain
help_fun_spain = function(x){
  mon = round(x/5)
  rest = round(x * 2/15)
  return(c(rep(rest, 3), mon, rep(rest,3)))
}

# Select only the dates without NAs in total_tests
# except for the first date for Spain, since this will be calculated seperately
weekly_test_nNA = weekly_test %>%
  filter(is.na(total_tests) != TRUE,
         date != as.Date("2020-04-13"),
         date != as.Date("2020-04-23"))

# Create the the total_tests data
daily_germany = weekly_test_nNA %>%
  filter(location == "Germany") %>%
  pull(total_tests) %>%
  sapply(help_fun_germany) %>% as.vector()

daily_spain = weekly_test_nNA %>%
  filter(location == "Spain") %>%
  pull(total_tests) %>%
  sapply(help_fun_spain) %>% as.vector()

insert_spain_1 = weekly_test %>%
  filter(date == as.Date("2020-04-13"), location == "Spain") %>%
  pull(total_tests)
insert_spain_2 = weekly_test %>%
  filter(date == as.Date("2020-04-23"), location == "Spain") %>%
  pull(total_tests)

help_fun_spain_2 = function(x){
  mon = round(2*x/10)
  rest = round(x * 4/45)
  return(c(rep(rest, 6), mon, rep(rest,3)))
}

insert_spain_1 = help_fun_germany(insert_spain_1)
insert_spain_2 = help_fun_spain_2(insert_spain_2)

daily_spain = c(insert_spain_1, insert_spain_2, daily_spain)

# Insert test data in owid data set (backup)
early_germany = pull(dates_weekly, date_min)[1]
late_germany = pull(dates_weekly, date_max)[1]
early_spain = pull(dates_weekly, date_min)[2]
late_spain = pull(dates_weekly, date_max)[2]

#len_owid_germany = dim(backup %>% filter(location == "Germany",
#                                 date >= "early_germany"))[1]

backup = backup %>% mutate(total_tests = replace(total_tests,
                                     location == "Germany" &
                                       date >= early_germany &
                                       date <= late_germany,
                                     daily_germany))

backup = backup %>% mutate(total_tests = replace(total_tests,
                                        location == "Spain" &
                                          date >= early_spain & 
                                          date <= late_spain,
                                        daily_spain))


#### Impute via rolling mean ####

# custom rollmean function
cust_rollmean = function(x, dates){ 
  
  # get first non-NA value after "2020-02-15"
  index_start = which(dates == "2020-02-15")
  non_Na = min(which(is.na(x[-(1:(index_start-1))]) != TRUE) + index_start - 1) # Note that an empty set returns Inf, therefore warnings occur
  
  non_Na = ifelse(non_Na == Inf, 10^10, non_Na) # To control unexpected behavior
  
  # If non_Na - index_start > 100 we return just the vector, since this country will be omitted anyway in such a case
  check_1 = non_Na - index_start > 100
  
  # Secondly if there are no NAs we just return the vector
  check_2 = non_Na - index_start == 0
  
  # Now apply either rolling mean or return just the vector
    if(check_1 == TRUE | check_2 == TRUE){
      return(x)
    }else{
      # select values up to first non NA and the 14 following values
      # to calculate rolling mean
      x_new = x[1:(non_Na+14)]
      
      # Calculate mean
      for(i in non_Na:2){
        x_new[i-1] = round(mean(x_new[i:(i+14)], na.rm = TRUE)) 
      }
      
      # return mean values and other x values after 23
      return(c(x_new[1:(non_Na-1)], x[-(1:(non_Na-1))]))
    }  
}  

test = backup %>% group_by(location) %>%
  mutate(total_tests = cust_rollmean(total_tests, date))

backup = backup %>% group_by(location) %>%
  mutate(total_tests = cust_rollmean(total_tests, date))

#### Total cases Imputation #####

# We assume that for a country which reports not more than 3 cases of covid19
# the number of total cases before the first cases where reported and the
# first date of the dataset should be 0 when looking at a date 14 weeks prior to
# this date and between 14 weeks prior this date and the date of the first reported cases
# the number should be equal to the number of the earliest reported cases
# However, this justifies the simplification to set total_cases equal to 0 for those
# countries between 2020-02-15 and the date where the first cases were reported
# Justification: Measures are enacted on the number of reported total cases

# Countries where the first recorded number of cases exceeds 3
backup %>% filter(date >= as.Date("2020-02-15")) %>%
  select(total_cases, date, location) %>% drop_na() %>%
  slice_min(n = 1, order_by = "date") %>%
  filter(date > as.Date("2020-02-15")) %>% filter(total_cases > 3)

# According to the numbers the following countries might had more unreported
# cases: Ecuador, Myanmar, Tajikistan
# Since those countries will be omitted anyway due to different reasons
# we can savely set the number of total_cases equal to 0 for all countries
# prior to their first reported cases.

# Get the countries and earliest dates for those who didn't report cases prior to 2020-02-15
# plus for those countries that reported cases before
# since truncate before 2020-02-15 anyway, it doesn't matter if we set
# the cases values for those countries equal to 0
first_countries = backup %>% select(total_cases, date, location) %>%
  filter(is.na(total_cases), date == as.Date("2020-02-15")) %>%
  slice_min(n = 1, order_by = "date")

first_date = backup %>% select(total_cases, date, location) %>%
  filter(location %in% first_countries$location) %>% drop_na() %>%
  slice_min(n = 1, order_by = "date")

# We practically repeat the procedure from the rolling mean
# custom cases replace function
cust_case = function(x){
  
  # If not more than 23 values are NA (till 2020-02-15) return just the vector
  # or if there are more than 200 NAs (will be deleted anyway)
  if(sum(is.na(x)) < 24 | sum(is.na(x)) >= 200){
    return(x)
  }else{
    # select values up to first non NA 
    index = min(which(!(is.na(x))))
    x_new = x[1:index]
    
    # Set value to 0
    x_new = rep(0, length(x_new))
    
    # return 0 values and other x values after 23
    return(c(x_new[1:index], x[-(1:index)]))
  }  
} 

test = backup %>% group_by(location) %>%
  mutate(total_cases = cust_case(total_cases)) %>% ungroup()

# Check if everything worked as intended
check1 = test %>% select(total_cases, date, location) %>%
  filter(is.na(total_cases), date == as.Date("2020-02-15")) %>%
  slice_min(n = 1, order_by = "date")

test %>% select(total_cases, date, location) %>%
  filter(location %in% first_countries$location) %>% drop_na() %>%
  slice_min(n = 1, order_by = "date") %>% filter(date >= as.Date("2020-02-15"))

backup = test

# Only three countries remain, which will be omitted anyway due to their lack
# of cases during the year 2020

#### Finalize Owid####

owid_data = backup

#### doctor density ####

doc_density = rename(doc_density, iso_code = ï..LOCATION,
                     date = TIME)

filter_factor_doc = c("iso_code", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY",
                      "Flag.Codes")
backup = doc_density

backup = backup %>% mutate(across(any_of(filter_factor_doc), factor))

backup = backup %>% mutate(date = ymd(backup$date, truncated = 2L))

# Since the last date in doc_density is one year before the first date in the owid_data
# dataset, assume that the density did not change from the last date point

values = backup %>% group_by(iso_code) %>% select(iso_code, Value, date) %>%
  slice_max(date) %>% ungroup()

# change date value
min_date = min(gmr_data$date)
backup = values %>% mutate(date = as.Date(min_date))
backup = rename(backup, doc_density = Value) # density per 1000 pop

doc_density = backup

#### gmr ####

backup = gmr_data

filter_factor_gmr = c("country_region_code", "location", "sub_region_1",
                      "sub_region_2", "metro_area", "iso_3166_2_code")

filter_median_gmr = c("retail_and_recreation_percent_change_from_baseline",
                       "grocery_and_pharmacy_percent_change_from_baseline",
                       "parks_percent_change_from_baseline",
                       "transit_stations_percent_change_from_baseline",
                       "workplaces_percent_change_from_baseline",
                       "residential_percent_change_from_baseline")

backup = rename(backup, location = country_region)

backup = backup %>%
  mutate(across(any_of(filter_factor_gmr), factor))

backup = backup %>% mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Since we don't have data on sub-region for the other variables we only use 
# the total country baseline change. Also we drop other variables that are not needed.
backup = backup %>% filter(sub_region_1 == "") %>%
  select(date, location, any_of(filter_median_gmr))

# The following countries will be discarded from the main dataset, since
# it seems that their values have been multiple times mended and are some-
# times lower than the main timeline (275 days) or even higher but I cannot
# deduce a pattern in it. 

backup %>% group_by(location) %>%
  summarise(date_length = n()) %>% filter(date_length != 275) %>%
  pull(location)

identifier = backup %>% group_by(location) %>%
  summarise(date_length = n()) %>% filter(date_length != 275) %>%
  pull(location)

backup = backup %>% filter(!(location %in% identifier))

gmr_data = backup


##################################### Merge Datasets ######

#### Censor Note ####
# GMR starts at 2020-02-15, so the owid_data set will be censored. At this point
# a total of 27 countries already exhibited a strict positive number of cases

test = owid_data %>% filter(date == as.Date("2020-02-15"),
                            !(location %in% c("International", "World"))) %>%
  select(total_cases, location) %>% drop_na()
sum(test$total_cases) # Total number of cases
slice_max(test, order_by = total_cases, n = 10) # selection of 10 countries with highest number of total cases at this point

filter(test, location != "China") %>%
  summarise(mean_ex_china = mean(total_cases))

filter(test, location != "China") %>%
  summarise(median_ex_china = median(total_cases))

country_list_cens = pull(test, location) # country list

#### Merging ####

backup = inner_join(owid_data, gmr_data, by = c("date", "location"))
backup = inner_join(backup, select(doc_density, iso_code, doc_density),
                    by = "iso_code")

##################################### Creating Final Dataset ####

not_needed = c("reproduction_rate", "weekly_icu_admissions", 
               "weekly_icu_admissions_per_million", "weekly_hosp_admissions",
               "weekly_hosp_admissions_per_million", "positive_rate", "tests_per_case",
               "tests_units", "stringency_index", "population", "diabetes_prevalence",
               "handwashing_facilities",
               "aged_70_older", "extreme_poverty",
               "new_tests", "new_tests_per_thousand",
               "total_cases_per_million", "total_deaths_per_million",
               "new_tests_smoothed_per_thousand", "new_tests_smoothed",
               "total_tests_per_thousand")

backup = backup %>% select(!(all_of(not_needed)))
data = backup
data = data %>% filter(location != "Turkey")

#### Looking at NA's ####
data_rel = data %>% filter(date <= "2020-09-20")
NA_s = data_rel %>% group_by(location) %>%
  select_if(function(x) any(is.na(x))) %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(cols = -location)

NA_total = ungroup(data_rel) %>% select(where(is.numeric)) %>%
  select_if(function(x) any(is.na(x))) %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(cols = everything())

#Variables with most overall NA's
NA_total %>% slice_max(n = 15, order_by = value)

# Countries with most overall NA's
NA_s %>% group_by(location) %>% summarise(NA_loc = sum(value)) %>%
  slice_max(n = 10, order_by = NA_loc)

# total tests

loc_NA_total_tests = data_rel %>% group_by(location) %>%
  summarize(NA_tests = sum(is.na(total_tests))) %>%
  arrange(desc(NA_tests)) %>% mutate(perc = NA_tests / 275 * 100) %>%
  filter(NA_tests != 0)

# total cases
loc_NA_total_cases = data_rel %>% group_by(location) %>%
  summarize(NA_cases = sum(is.na(total_cases))) %>%
  arrange(desc(NA_cases)) %>% mutate(perc = NA_cases / 275 * 100) %>%
  filter(NA_cases != 0)

# parks_percent_change_from_baseline
loc_NA_total_gmr_parks = data_rel %>% group_by(location) %>%
  summarize(NA_gmr_parks = sum(is.na(parks_percent_change_from_baseline))) %>%
  arrange(desc(NA_gmr_parks)) %>% mutate(perc = NA_gmr_parks / 275 * 100) %>%
  filter(NA_gmr_parks != 0)

# grocery_and_pharmacy_percent_change_from_baseline
loc_NA_total_gmr_grocery = data_rel %>% group_by(location) %>%
  summarize(NA_gmr_grocery = sum(is.na(grocery_and_pharmacy_percent_change_from_baseline))) %>%
  arrange(desc(NA_gmr_grocery)) %>% mutate(perc = NA_gmr_grocery / 275 * 100) %>%
  filter(NA_gmr_grocery != 0)

# workplaces_percent_change_from_baseline
loc_NA_total_gmr_workplaces = data_rel %>% group_by(location) %>%
  summarize(NA_gmr_workplaces = sum(is.na(workplaces_percent_change_from_baseline))) %>%
  arrange(desc(NA_gmr_workplaces)) %>% mutate(perc = NA_gmr_workplaces / 275 * 100) %>%
  filter(NA_gmr_workplaces != 0)

# residential_percent_change_from_baseline
loc_NA_total_gmr_residential = data_rel %>% group_by(location) %>%
  summarize(NA_gmr_residential = sum(is.na(residential_percent_change_from_baseline))) %>%
  arrange(desc(NA_gmr_residential)) %>% mutate(perc = NA_gmr_residential / 275 * 100) %>%
  filter(NA_gmr_residential != 0)

# tests check whether missing values are due to a pattern
data %>% filter(location %in% loc_NA_total_tests$location) %>%
  select(date, total_tests, location) %>% drop_na() %>%
  group_by(location) %>% slice_min(n=1, order_by = date) %>%
  filter(date > as.Date("2020-02-15"))

# countries in Europe that can be used for imputing Germany test data
data %>% filter(continent == "Europe", date <= "2020-03-01") %>%
  select(total_tests, date, location) %>%
  drop_na() %>% select(location) %>% unique()

# cases check whether missing values are due to a pattern
data %>% filter(location %in% loc_NA_total_cases$location) %>%
  select(date, total_cases, location) %>% drop_na() %>%
  group_by(location) %>% slice_min(n=1, order_by = date) 

# gmr_parks check whether missing values are due to a pattern
data %>% filter(location %in% loc_NA_total_gmr_parks$location) %>%
  select(date, parks_percent_change_from_baseline, location) %>%
  drop_na() %>%
  group_by(location) %>% slice_min(n=1, order_by = date) 

##### NA Decisions ####

# We will drop the variables intensive care patients per day and 
# hospitalized cases of Covid-19 per day, since there are more then 10
# countries with no data whatsoever 

NA_s %>% group_by(location) %>%
  filter(name %in% c("icu_patients", "hosp_patients"), value == 275) %>%
  select(location) %>% unique()

filter_NA_1 = c("icu_patients", "icu_patients_per_million",
                "hosp_patients", "hosp_patients_per_million")
data = data %>% select(!(all_of(filter_NA_1)))

# Impute the non pattern NAs of the remaining variables with NAs

# Let us take a quick look at the countries with more than 5 NAs for total_test
NA_total_tests_np = data %>% filter(location %in% c("South Africa", "India", "Austria"), is.na(total_tests)) %>%
  select(total_tests, date, location) 
qplot(data = NA_total_tests_np,x = date, y = location, col = location)

# We writ a function that does the following:
# 1.) Find all NA values for a given total_test vector
# 2.) Stepwisely calculate a 7 day rolling mean for each of those points, starting with the last (time) value 
# 3.) Impute those values and return a non NA vector.
# The same function will also be applied to the missings for the gmr variables

cust_rollmean_spec = function(x, loca){ 
  
  # get NA values
  NA_index = which(is.na(x))
  
  # If empty, return vector
  if(length(NA_index) == 0){
    print(paste(loca, "has been skipped entirely since there are no NAs"))
    return(x)
  }
  
  # If all NAs occur within the last 7 days of the time series, return the whole vector
  last_seven = (length(x)-7):length(x)
  if(all(NA_index %in% last_seven) == TRUE){
    print(paste(loca, "has been skipped entirely since all NAs are at the end."))
    return(x)
  }
  
  # check whether the last NA value is superseded by 7 non NA values. If true, check whether it is enough to skip this value. 
  # If another NA value is in the environment of this value return a message that at least one NA value has not been imputed by 7 but less
  # observations
  last_NA = NA_index[length(NA_index)] # Get last NA index
  check_1 = ifelse(length(x[last_NA:length(x)]) > 8, TRUE, FALSE) # Check whether there are more then 7 non NA values to the right
  counter = 0 # Count how many values have been skipped this way
  
  while(check_1 == FALSE){
    NA_index = NA_index[-length(NA_index)] # Skip this value
    last_NA = NA_index[length(NA_index)] # Get last NA index
    check_1 = ifelse(length(x[last_NA:length(x)]) > 8, TRUE, FALSE)
    
    penult_NA = NA_index[length(NA_index)-1] # Get penultimate NA index
    check_2 = ifelse(any(is.na(x[(penult_NA + 1):(penult_NA + 8)])), TRUE, FALSE)
    counter = ifelse(check_2 == FALSE, counter+1, counter)
  }
  
  if(counter > 0){
   print(paste(counter, "value(s) was(were) not imputed using a 7 day window. The issue arised for:", location[1]))
  }
  
  
  ### Now impute the NA values via rolling mean. 
  for(i in rev(NA_index)){
    x[i] = round(mean(x[(i+1):(i+8)], na.rm = TRUE)) 
  }
  
  # return imputed vector
  return(x)
}  

data = data %>% group_by(location) %>% mutate(total_tests = cust_rollmean_spec(total_tests, location)) %>%
  ungroup()

# Calculate whether there are still NAs and display countries and dates
remaining_NA_test = data %>% group_by(location) %>%
  summarize(NA_tests = sum(is.na(total_tests))) %>%
  arrange(desc(NA_tests)) %>% mutate(perc = NA_tests / 275 * 100) %>%
  filter(NA_tests != 0)

data %>% filter(location %in% remaining_NA_test$location, is.na(total_tests)) %>%
  select(location, total_tests, date) 

# Those dates will not be used anyway, so we can consider the variable total_tests as NA free.

## GMR parks

NA_total_parks = data %>%
  filter(is.na(parks_percent_change_from_baseline)) %>%
  select(parks_percent_change_from_baseline, date, location) 
qplot(data = NA_total_parks,x = date, y = location, col = location)

# Impute values
data = data %>% group_by(location) %>%
  mutate(parks_percent_change_from_baseline = cust_rollmean_spec(parks_percent_change_from_baseline, location)) %>%
  ungroup()

# Check for remaining NAs

data %>% group_by(location) %>%
  summarize(NA_gmr_parks = sum(is.na(parks_percent_change_from_baseline))) %>%
  arrange(desc(NA_gmr_parks)) %>% mutate(perc = NA_gmr_parks / 275 * 100) %>%
  filter(NA_gmr_parks != 0)

## GMR Grocery

NA_total_grocery = data %>%
  filter(is.na(grocery_and_pharmacy_percent_change_from_baseline)) %>%
  select(grocery_and_pharmacy_percent_change_from_baseline, date, location) 
qplot(data = NA_total_grocery,x = date, y = location, col = location)

# Impute values
data = data %>% group_by(location) %>%
  mutate(grocery_and_pharmacy_percent_change_from_baseline = cust_rollmean_spec(grocery_and_pharmacy_percent_change_from_baseline, location)) %>%
  ungroup()

data %>% group_by(location) %>%
  summarize(NA_gmr_grocery = sum(is.na(grocery_and_pharmacy_percent_change_from_baseline))) %>%
  arrange(desc(NA_gmr_grocery)) %>% mutate(perc = NA_gmr_grocery / 275 * 100) %>%
  filter(NA_gmr_grocery != 0)

## GMR Workplaces

NA_total_workplaces = data %>%
  filter(is.na(workplaces_percent_change_from_baseline)) %>%
  select(workplaces_percent_change_from_baseline, date, location) 
qplot(data = NA_total_workplaces,x = date, y = location, col = location)

# Impute values
data = data %>% group_by(location) %>%
  mutate(workplaces_percent_change_from_baseline = cust_rollmean_spec(workplaces_percent_change_from_baseline, location)) %>%
  ungroup()

data %>% group_by(location) %>%
  summarize(NA_gmr_grocery = sum(is.na(workplaces_percent_change_from_baseline))) %>%
  arrange(desc(NA_gmr_grocery)) %>% mutate(perc = NA_gmr_grocery / 275 * 100) %>%
  filter(NA_gmr_grocery != 0)

## GMR Residential

NA_total_residential = data %>%
  filter(is.na(residential_percent_change_from_baseline)) %>%
  select(residential_percent_change_from_baseline, date, location) 
qplot(data = NA_total_residential,x = date, y = location, col = location)

# Impute values
data = data %>% group_by(location) %>%
  mutate(residential_percent_change_from_baseline = cust_rollmean_spec(residential_percent_change_from_baseline, location)) %>%
  ungroup()

data %>% group_by(location) %>%
  summarize(NA_gmr_grocery = sum(is.na(residential_percent_change_from_baseline))) %>%
  arrange(desc(NA_gmr_grocery)) %>% mutate(perc = NA_gmr_grocery / 275 * 100) %>%
  filter(NA_gmr_grocery != 0)

##### Final NA Check ####

NA_s = data %>% group_by(location) %>%
  select_if(function(x) any(is.na(x))) %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(cols = -location)

NA_total = ungroup(data) %>% select(where(is.numeric)) %>%
  select_if(function(x) any(is.na(x))) %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(cols = everything())

# Total Tests has still 5 NAs. Display country and time.
NA_total_tests = data %>%
  filter(is.na(total_tests)) %>%
  select(total_tests, date, location) 
qplot(data = NA_total_tests,x = date, y = location, col = location)
# As we can see there are still two countries remaining, Mexico and Australia, but the dates are after Nov 20,
# hence we can ignore them. 

##################################### Finalization and list of countries #####

# Subset final data set to include only data < "2020-11-13" to remove remaining NAs in total test

data = data %>% filter(date < "2020-11-13")

# Create total_deaths variable
data = data %>% group_by(location) %>% mutate(total_deaths = cumsum(new_deaths)) %>% ungroup()

##### Create percentage change variable #####

# We want to see whether we can justify the use of log differences instead of percentage changes
# Create function that calculates percentage or log differences for input variable

diff_log_perc = function(x, spec = "percentage", lag = 1){
  # Compute percentage difference if spec == "percentage" otherwise compute log differences
  if(spec == "percentage"){
    diff = diff(x, lag = lag) # lag difference
    perc_change = diff/x[-((length(x)-lag+1):length(x))] # percentage change/100, at n: (x_n-x_{n-1})/x_{n-1}
    perc_change = replace(perc_change, is.na(perc_change) | perc_change == Inf | perc_change == (-Inf), 0) # Replace values equal to NA with 0, cases where we divided by 0
    perc_change = c(rep(NA, lag), perc_change) # the number of elements reduces by lag, hence NA
    return(perc_change)
  }else{
    log_diff = diff(log(x), lag = lag) # log differences
    log_diff = replace(log_diff, is.na(log_diff) | log_diff == Inf | log_diff == (-Inf), 0) # Replace values equal to NA with 0, cases where we applied log(0)
    log_diff = c(rep(NA, lag), log_diff) # First change does not exist, hence NA
    return(log_diff)
  }
}

# Create new variables, log_diff and perc_chg for total_cases, total_deaths with lag 1, 7 and 14
backup = data
backup = backup %>% group_by(location) %>% mutate(perc_case_1 = diff_log_perc(total_cases),
                           perc_death_1 = diff_log_perc(total_deaths),
                           log_case_1 = diff_log_perc(total_cases, "log"),
                           log_death_1 = diff_log_perc(total_deaths, "log"),
                           perc_case_7 = diff_log_perc(total_cases, lag = 7),
                           perc_death_7 = diff_log_perc(total_deaths, lag = 7),
                           log_case_7 = diff_log_perc(total_cases, "log", lag = 7),
                           log_death_7 = diff_log_perc(total_deaths, "log", lag = 7),
                           perc_case_14 = diff_log_perc(total_cases, lag = 14),
                           perc_death_14 = diff_log_perc(total_deaths, lag = 14),
                           log_case_14 = diff_log_perc(total_cases, "log", lag = 14),
                           log_death_14 = diff_log_perc(total_deaths, "log", lag = 14)) %>% ungroup()

# Calculate difference between log diff and percentage change and store in new variables
backup = backup %>% group_by(location) %>%
  mutate(diff_cases_1 = abs(perc_case_1 - log_case_1),
         diff_deaths_1 = abs(perc_death_1 - log_death_1),
         diff_cases_7 = abs(perc_case_7 - log_case_7),
         diff_deaths_7 = abs(perc_death_7 - log_death_7),
         diff_cases_14 = abs(perc_case_14 - log_case_14),
         diff_deaths_14 = abs(perc_death_14 - log_death_14)) %>% ungroup()

# Calculate mean approximation error and check whether the error is smaller than 0.015 (1.5%)
errors_apprx = backup %>% summarise(mean_cases_1 = mean(diff_cases_1, na.rm = TRUE),
                     mean_deaths_1 = mean(diff_deaths_1, na.rm = TRUE),
                     error_cases_1 = mean_cases_1 < 0.015,
                     error_deaths_1 = mean_deaths_1 < 0.015,
                     mean_cases_7 = mean(diff_cases_7, na.rm = TRUE),
                     mean_deaths_7 = mean(diff_deaths_7, na.rm = TRUE),
                     error_cases_7 = mean_cases_7 < 0.015,
                     error_deaths_7 = mean_deaths_7 < 0.015,
                     mean_cases_14 = mean(diff_cases_14, na.rm = TRUE),
                     mean_deaths_14 = mean(diff_deaths_14, na.rm = TRUE),
                     error_cases_14 = mean_cases_14 < 0.015,
                     error_deaths_14 = mean_deaths_14 < 0.015) 
t(errors_apprx) # Print errors

# We conclude, that taking log differences is not justified. We therefore use percentage changes
# data = data %>% group_by(location) %>% mutate(chg_cases = diff_log_perc(total_cases, lag = 1),
#                                               chg_deaths = diff_log_perc(total_deaths, lag = 1),
#                                               chg_cases_7 = diff_log_perc(total_cases, lag = 7),
#                                               chg_deaths_14 = diff_log_perc(total_deaths, lag = 14)) %>%
#   ungroup()

data = data %>% group_by(location) %>% mutate(chg_cases = diff_log_perc(total_cases, lag = 1),
                                              chg_deaths = diff_log_perc(total_deaths, lag = 1)) %>%
  ungroup()

#### Create lag variable for both change variables & change in test data ####

# data = data %>% group_by(location) %>% mutate(lag_cases_7 = lag(chg_cases_7, k = 7),
#                                               lag_chg_deaths_14 = lag(chg_deaths_14, k = 14),
#                                               chg_test = diff_log_perc(total_tests, lag = 1),
#                                               chg_test_7 = diff_log_perc(total_tests, lag = 7),
#                                               chg_test_14 = diff_log_perc(total_tests, lag = 14),
#                                               lag_test = lag(chg_test, k = 1),
#                                               lag_test_7 = lag(chg_test_7, k = 7),
#                                               lag_test_14 = lag(chg_test_14, k = 14))

data = data %>% group_by(location) %>% mutate(chg_test = diff_log_perc(total_tests, lag = 1)) %>%
  ungroup()

#### Exclude US, Mexico, and Hungary due to issues with policy determination ####

data = data %>% filter(!(location %in% c("United States", "Hungary", "Mexico")))

#### Second data set without lag, diff induced NAs ####

data_2 = data %>% drop_na()

unique(data_2$date)[1] # earliest date in data set

##### Include Policy Dummies #####

# Creating a function that takes a date vector of a country and a data set that includes the
# location, date and type of measure and based on these inputs creates a dummy vector

dummy_maker = function(date_org, loc_org, data_measure, pol){
  
  loc_sub = unique(loc_org) # Get country name
  x = rep(0, length(date_org)) # create empty dummy vector
  data_sub = data_measure %>% filter(measure == pol, location == as.character(loc_sub)) # Subset of relevant policy data
  
  # Return vector of zeros if policy has not been implemented
  if(data_sub$implement == 0){
    return(as.integer(x))
  }else{
    data_start = data_sub$start_date # get start date of policy measure
    data_end = data_sub$end_date # get end date of policy measure
    
    x = replace(x, date_org >= data_start & date_org <= data_end, 1) # Set every date between start and end date equal to 1
    return(as.integer(x))
  }
}

# Load policy data and convert data types
policies = read.csv("./Data/policies_final.csv", sep = ";") %>% as_tibble() %>%
  rename(location = ï..loc) %>%
  mutate(location = factor(location),
         start_date = as.Date(start_date, format = "%d.%m.%Y"),
         end_date = as.Date(end_date, format = "%d.%m.%Y"),
         measure = factor(measure),
         implement = factor(implement)) %>%
  filter(!(location %in% c("United States", "Hungary", "Mexico")))

# Add Policy dummys and (NOT: lags to data set)

# data = data %>% mutate(events = dummy_maker(date, location, policies, "Events"),
#                            travel = dummy_maker(date, location, policies, "Travel"),
#                            distance = dummy_maker(date, location, policies, "Distance"),
#                            public = dummy_maker(date, location, policies, "Public"),
#                            lock = dummy_maker(date, location, policies, "Lock"),
#                            closure = dummy_maker(date, location, policies, "Closure"),
#                            visit = dummy_maker(date, location, policies, "Visit"),
#                            mask = dummy_maker(date, location, policies, "Mask"),
#                            events_7 = lag(dummy_maker(date, location, policies, "Events"), k = 7),
#                            travel_7 = lag(dummy_maker(date, location, policies, "Travel"), k = 7),
#                            distance_7 = lag(dummy_maker(date, location, policies, "Distance"), k = 7),
#                            public_7 = lag(dummy_maker(date, location, policies, "Public"), k = 7),
#                            lock_7 = lag(dummy_maker(date, location, policies, "Lock"), k = 7),
#                            closure_7 = lag(dummy_maker(date, location, policies, "Closure"), k = 7),
#                            visit_7 = lag(dummy_maker(date, location, policies, "Visit"), k = 7),
#                            mask_7 = lag(dummy_maker(date, location, policies, "Mask"), k = 7),
#                            events_14 = lag(dummy_maker(date, location, policies, "Events"), k = 14),
#                            travel_14 = lag(dummy_maker(date, location, policies, "Travel"), k = 14),
#                            distance_14 = lag(dummy_maker(date, location, policies, "Distance"), k = 14),
#                            public_14 = lag(dummy_maker(date, location, policies, "Public"), k = 14),
#                            lock_14 = lag(dummy_maker(date, location, policies, "Lock"), k = 14),
#                            closure_14 = lag(dummy_maker(date, location, policies, "Closure"), k = 14),
#                            visit_14 = lag(dummy_maker(date, location, policies, "Visit"), k = 14),
#                            mask_14 = lag(dummy_maker(date, location, policies, "Mask"), k = 14),
#                            retail_7 = lag(retail_and_recreation_percent_change_from_baseline, k = 7),
#                            retail_14 = lag(retail_and_recreation_percent_change_from_baseline, k = 14),
#                            grocery_7 = lag(grocery_and_pharmacy_percent_change_from_baseline, k = 7),
#                            grocery_14 = lag(grocery_and_pharmacy_percent_change_from_baseline, k = 14),
#                            parks_7 = lag(parks_percent_change_from_baseline, k = 7),
#                            parks_14 = lag(parks_percent_change_from_baseline, k = 14),
#                            transit_7 = lag(transit_stations_percent_change_from_baseline, k = 7),
#                            transit_14 = lag(transit_stations_percent_change_from_baseline, k = 14),
#                            workplace_7 = lag(workplaces_percent_change_from_baseline, k = 7),
#                            workplace_14 = lag(workplaces_percent_change_from_baseline, k = 14),
#                            residential_7 = lag(residential_percent_change_from_baseline, k = 7),
#                            residential_14 = lag(residential_percent_change_from_baseline, k = 14))


data = data %>% group_by(location) %>% 
  mutate(events = dummy_maker(date, location, policies, "Events"),
                       travel = dummy_maker(date, location, policies, "Travel"),
                       distance = dummy_maker(date, location, policies, "Distance"),
                       public = dummy_maker(date, location, policies, "Public"),
                       lock = dummy_maker(date, location, policies, "Lock"),
                       closure = dummy_maker(date, location, policies, "Closure"),
                       visit = dummy_maker(date, location, policies, "Visit"),
                       mask = dummy_maker(date, location, policies, "Mask")) %>%
  ungroup()

# Convert Dummys to factors and gmr vars to in
# data = data %>% mutate(across((starts_with("events") |
#                           starts_with("public") |
#                           starts_with("travel") |
#                           starts_with("lock") |
#                           starts_with("mask") |
#                           starts_with("distance") |
#                           starts_with("visit") | 
#                           starts_with("closure")), factor))

data = data %>% mutate(across((starts_with("retail") | 
                                starts_with("grocery") |
                                starts_with("parks") |
                                starts_with("transit") |
                                starts_with("workplace") |
                                starts_with("residential")), as.integer))

##### Save data set as R files ####

# rename gmr columns for easier handling
data = rename(data, retail = retail_and_recreation_percent_change_from_baseline,
              grocery = grocery_and_pharmacy_percent_change_from_baseline,
              parks = parks_percent_change_from_baseline,
              transit = transit_stations_percent_change_from_baseline,
              workplaces = workplaces_percent_change_from_baseline,
              residential = residential_percent_change_from_baseline)

# Drop variables that are not needed
drop_filter = c("new_cases", "new_cases_smoothed",
                "new_deaths", "new_deaths_smoothed", "new_cases_per_million",
                "new_cases_smoothed_per_million", "new_deaths_per_million",
                "new_deaths_smoothed_per_million", "iso_code")
data = data %>% select(!all_of(drop_filter))

# Finally, create a month dummy for time effects
ds = data$date
tmp = data.table::data.table(ds)[, month := format(ds, "%Y-%m")] # Create month variable
dummies_time = data.table::dcast(tmp, ds ~ month, length, value.var = "ds") # Create dummies
dummies_time[dummies_time == 27] = 1 # Set 27 to 1
colnames(dummies_time) = c("date", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")

data = left_join(data, dummies_time, by = "date")
data = data %>%  mutate(across(Feb:Nov, as.integer))

# Save data

saveRDS(data, file = "data_final_w_NA.Rdata")
#saveRDS(data_2, file = "data_final_wo_NA.Rdata")

##################################### Create Weekly data ####
##### Preformating

# Test Delete
data = data %>% group_by(location) %>%
  mutate(change_cases_7_log = diff_log_perc(total_cases, spec = "log", lag = 7),
         change_cases_7_per = diff_log_perc(total_cases, lag = 7),
         change_deaths_14_log = diff_log_perc(total_deaths, spec = "log", lag = 14),
         change_deaths_14_per = diff_log_perc(total_deaths, lag = 14)) %>% ungroup()  
# The first Monday in the data set is 2020-02-18 and the last is 2020-10-26
# subset data accordingly
data_weekly = data %>% filter(date >= "2020-02-18", date <= "2020-10-26")

# Create an integer identifier (is needed for calculation later)
data_weekly = data_weekly %>% mutate(week = week(date)) 

# Also create a month vector (will be used for time effect dummies)
data_weekly = data_weekly %>% mutate(month = month(date))

##### Confounders and Behavior variables ####
# We do the following: For the control variables and the behavior variables calculate the weekly mean
ctrl_beh_means = data_weekly %>% group_by(location, week) %>%
  summarise(across(population_density:doc_density, mean)) %>% ungroup()

##### Policy dummies ####
# Write a function that checks whether given a week vector of dummies either all are 0 or 1 and if 
# this is not the case set all values to 0

shift_fun = function(x){
  x_new = ifelse(all(x) == 1 | all(x) == 0, x, rep(0, length(x)))
  return(x_new)
}

data_weekly = data_weekly %>% group_by(location, week) %>%
  mutate(events = shift_fun(events),
         public = shift_fun(public),
         travel = shift_fun(travel),
         mask = shift_fun(mask),
         lock = shift_fun(lock),
         distance = shift_fun(distance),
         visit = shift_fun(visit),
         closure = shift_fun(closure)) %>%
  ungroup()

##### Combine everything ####

# Create a vector of dates for the beginnings of the weeks
min_date = min(unique(data_weekly$date))
max_date = max(unique(data_weekly$date))
week_seq = seq.Date(as.Date(min_date), as.Date(max_date), 7)
data_week = data_weekly %>% filter(date %in% week_seq)

# Drop ctrl. and behavior variables
keep_filter = c("continent", "location", "date", "total_cases", "total_tests", "total_deaths",
                "week", "month", "change_cases_7_log", "change_cases_7_per",
                "change_deaths_14_log", "change_deaths_14_per")

data_week = data_week %>% select(all_of(keep_filter), events:mask)

# Join with mean ctrl. and behavior vars
data_week = left_join(data_week, ctrl_beh_means, by = c("week", "location"))

##### Change variables ####
data_week = data_week %>% group_by(location) %>% mutate(chg_cases = diff_log_perc(total_cases, lag = 1),
                                     chg_deaths = diff_log_perc(total_deaths, lag = 1),
                                     chg_tests = diff_log_perc(total_tests, lag = 1)) %>% ungroup()
# reorder variables
data_week = data_week %>%
  select(continent:date, week, month, total_cases:total_deaths, chg_cases:chg_tests,
         events:mask, retail:residential, everything())

##### Save Weekly Data ####
saveRDS(data_week, file = "weekly_data.Rdata")

##################################### Plots & Tables Data Visualization ####
##### Final list of countries ####
setwd("C:/Users/TonyG/Documents/GitHub/PTCoV19/Programming")
list_of_countries = unique(data$location)
list_of_countries
# We create some plots to visualize the final data set.

# Countries in World Map
WorldData = map_data('world') %>% filter(region != "Antarctica") %>% fortify

df = data.frame(region=list_of_countries, 
                value=factor(rep(1, length(list_of_countries))), 
                stringsAsFactors=FALSE)
levels(df$region) = replace(levels(df$region), levels(df$region) == "United States", "USA") # In WorldData United States is identified via USA
identifier = ifelse(WorldData$region %in% list_of_countries, 1, 0)
WorldData = WorldData %>% mutate(identifier = as.factor(identifier))
WorldData = as_tibble(WorldData)
cnames = aggregate(cbind(long, lat) ~ region, data=WorldData, 
                   FUN=function(x)mean(range(x))) %>% filter(region %in% list_of_countries)

world = ggplot() + geom_map(data = WorldData, map = WorldData,
                            aes(x = long, y = lat, group = group, map_id=region),
                            fill = "white", colour = "#7f7f7f", size=0.5)+
  geom_map(data = df, map=WorldData,
           aes(fill=value, map_id=region),
           colour="#7f7f7f", size=0.5)+
  scale_y_continuous(breaks=c()) + ylab("") +
  scale_x_continuous(breaks=c()) + xlab("")+
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 80))+
  scale_fill_brewer(palette = "Accent")+
  theme_clean()+theme(legend.position = "none")
ggsave("World_map.png", plot = world, width = 9, height = 6)

# Countries per continent
number_countr = data %>% group_by(continent) %>% summarise(Count = n_distinct(location)) %>%
  ggplot(aes(x = continent, y = Count))+geom_point(col = "#7FC97F", size = 3)+
  scale_y_continuous(breaks = seq(min(1), max(21), by = 2),
                     limits = c(1, 21))+xlab("Continent")+
  ylab("Count")+theme_clean()

plot_tog = ggarrange(world, number_countr, labels = c("World Map", "Countries per Continent"),
                     ncol = 1, nrow = 2, heights = c(1,0.7))
ggexport(plot_tog, filename = "plot_world_count.pdf")
##### Summary of policies ####
Sys.setlocale("LC_TIME", "C") # Change date names to english

summary_policies = policies %>% group_by(measure) %>% 
  summarise(min__start_date = as.character(format(min(start_date, na.rm = TRUE), "%d %b")),
  max__start_date = as.character(format(max(start_date, na.rm = TRUE), "%d %b")),
  median__start_date = as.character(format(median(start_date, na.rm = TRUE), "%d %b")),
  min__end_date = as.character(format(min(end_date, na.rm = TRUE), "%d %b")),
  max__end_date = as.character(format(max(end_date, na.rm = TRUE), "%d %b")),
  median__end_date = as.character(format(median(end_date, na.rm = TRUE), "%d %b")),
  num_measures = sum(implement == 1))
summary_policies_table = xtable(summary_policies,
                                caption = "Summary statistics of end and start dates of measures. Year is always 2020.",
                                label = "tab:sum_pol",
                                align = c("l", "l", rep("c", 7)),
                                display = c("d", rep("s", 7), "d"))
#options(xtable.sanitize.text.function = identity)
colnames(summary_policies_table) = c("Type of Measure", "Earliest Start Date",
                                     "Ultimate Start Date",
                                     "Median Start Date", "Earliest End Date",
                                     "Ultimate End Date",
                                     "Median End Date", "No. of Countries")

print.xtable(summary_policies_table, type = "latex", 
             file = "./Tables/policy_summary.tex",
             floating = TRUE,
             include.rownames = FALSE,
             booktabs = TRUE,
             size = "small")


##### Covid cases/deaths changes for selected countries #####

# We select the following countries: 

country_filter = c("Australia", "Canada", "Colombia", "Germany", "India", "Israel", "Italy",
                   "Japan", "Sweden")


data_plot = data %>% filter(location %in% country_filter, date <= "2020-06-01") %>%
  select(date, location, events, travel, distance, public, lock, closure, visit, mask,
         total_cases, total_deaths)
# Create new change variables 7 and 14 day perc change of cases and deaths
data_plot = data_plot %>% group_by(location) %>%
  mutate(chg_cases_7 = diff_log_perc(total_cases, spec = "log", lag = 7),
         chg_deaths_7 = diff_log_perc(total_deaths,spec = "log", lag = 7)) %>%
  ungroup()

data_plot = data_plot %>% drop_na()

# Pivot data for changes in cases and deaths for plot
data_plot = pivot_longer(data_plot, cols = c("chg_cases_7", "chg_deaths_7"),
                    names_to = "changes", values_to = "values")

# Pivot data for policy lines
data_plot = data_plot %>% group_by(location) %>%
  mutate(event_date = ifelse(min(date[events == 1]) == Inf, NA, min(date[events == 1])),
         lock_date = ifelse(min(date[lock == 1]) == Inf, NA, min(date[lock == 1])),
         closure_date = ifelse(min(date[closure == 1]) == Inf, NA, min(date[closure == 1])),
         public_date = ifelse(min(date[public == 1]) == Inf, NA, min(date[public == 1]))) %>% ungroup()

data_plot = pivot_longer(data_plot, cols = c("event_date", "lock_date", "closure_date", "public_date"),
                         names_to = "policy", values_to = "values_dummy")

min_date = min(data_plot$date) # For breaks in plot
max_date = max(data_plot$date) # For breaks in plot

#plots = list() # empty list for the various plots that will be arranged in a grid

plot_country = function(loc){
  Sys.setlocale("LC_TIME", "C")
  data_plot %>% filter(location == loc) %>%
    ggplot(aes(x = date, y = (values * 100), col = changes))+
    geom_point(alpha = 0.6, size = 1.5, key_glyph = "blank")+geom_line(size = 0.8, alpha = 0.7,key_glyph = "rect")+
    geom_vline(aes(xintercept = values_dummy,
                   linetype = relevel(factor(policy),
                                      ref = "lock_date")), key_glyph = "path")+
    theme_clean()+scale_color_brewer(palette = "Dark2",
                                     name="Change of",
                                     labels=c("7 Days Cases", "7 Days Deaths"))+
    scale_linetype_discrete(name = "Policy",
                            labels = c("Lock", "Closure", "Public", "Events"),
                            breaks = c("lock_date", "closure_date", "public_date", "event_date"))+
    xlab("Date")+ylab("Change in %")+
    scale_x_date(date_breaks = "3 weeks", date_labels = "%d %b")+#scale_y_log10()+
  theme(legend.background = element_rect(fill="gray90", size=.5))+
    guides(col = guide_legend(order = 1), linetype = guide_legend(order = 2))
}

plots = lapply(country_filter, plot_country)

ggarrange(plotlist = plots, ncol = 3, nrow = 3, common.legend = TRUE,
          legend = "bottom", labels = country_filter,
          label.x = 0.3)
ggsave("change_policy_plot.pdf", scale = 2, device = "pdf")

##### Behavior changes for selected countries #####

# We select the following countries: 

country_filter = c("Australia", "Canada", "Colombia", "Germany", "India", "Israel", "Italy",
                   "Japan", "Sweden")

data_plot_2 = data %>% filter(location %in% country_filter, date <= "2020-07-01") %>%
  select(date, location, events, travel, distance, public, lock, closure, visit, mask,
         retail, grocery, parks, transit, workplaces, residential)

# Pivot data for behavior
data_plot_2 = pivot_longer(data_plot_2, cols = c("retail", "grocery", "parks",
                                                 "transit", "workplaces", "residential"),
                         names_to = "behavior", values_to = "values")

# Pivot data for policy lines
data_plot_2 = data_plot_2 %>% group_by(location) %>%
  mutate(event_date = ifelse(min(date[events == 1]) == Inf, NA, min(date[events == 1])),
         lock_date = ifelse(min(date[lock == 1]) == Inf, NA, min(date[lock == 1])),
         closure_date = ifelse(min(date[closure == 1]) == Inf, NA, min(date[closure == 1])),
         public_date = ifelse(min(date[public == 1]) == Inf, NA, min(date[public == 1]))) %>% ungroup()

data_plot_2 = pivot_longer(data_plot_2, cols = c("event_date", "lock_date", "closure_date", "public_date"),
                         names_to = "policy", values_to = "values_dummy")

min_date = min(data_plot_2$date) # For breaks in plot
max_date = max(data_plot_2$date) # For breaks in plot

#plots = list() # empty list for the various plots that will be arranged in a grid

plot_country_behavior = function(loc){
  Sys.setlocale("LC_TIME", "C")
  data_plot_2 %>% filter(location == loc) %>%
    ggplot(aes(x = date, y = values, col = behavior))+
    geom_line(size = 0.8, alpha = 0.7, key_glyph = "rect")+
    geom_vline(aes(xintercept = values_dummy,
                   linetype = relevel(factor(policy),
                                      ref = "lock_date")), key_glyph = "path")+
    geom_hline(yintercept = 0, col = "black", alpha = 0.6)+
    theme_clean()+scale_color_brewer(palette = "Dark2",
                                     name="Change from Baseline",
                                     labels=c("Grocery", "Parks", "Residential",
                                              "Retail", "Transit", "Workplaces"))+
    scale_linetype_discrete(name = "Policy",
                            labels = c("Lock", "Closure", "Public", "Events"),
                            breaks = c("lock_date", "closure_date", "public_date", "event_date"))+
    xlab("Date")+ylab("Change in %")+
    scale_x_date(date_breaks = "3 weeks", date_labels = "%d %b")+
    theme(legend.background = element_rect(fill="gray90", size=.5))
}

plots = lapply(country_filter, plot_country_behavior)

ggarrange(plotlist = plots, ncol = 3, nrow = 3, common.legend = TRUE,
          legend = "bottom", labels = country_filter,
          label.x = 0.3)
ggsave("change_behavior_plot.pdf", scale = 2, device = "pdf")


##### Calculate mean imputation error ####

# We write the following function: 
# For each country 
# After 2020-02-15 find a section containing 28 non NA values, save this section and index
# calculate the rolling mean starting from the last observation for those 28 values using 14 days 
# after the last observation of the section
# calculate approximation error for each element and save in a vector
# calculate mean of vector of errors

approx_error = function(loc, x, len, ran){
  
  # Empty vector for errors
  errors = c()
  
  # Get country names
  identifier = unique(loc)
  
  for(j in identifier){
    # Subset x to country
    x_sub = x[loc == j]
    
    # Check whether there is at least one NA index, if so skip this country
    if(all(!is.na(x_sub)) == T){
      print(paste(j, "has no NAs. Skipped"))
      next()
    }
    
    # Find len successional non NA's
    nas = which(is.na(x_sub)) # Get every index, which is NA
    nas = append(nas, length(x_sub)) # append last index to prevent false conclusion for vectors which exhibit only at certain locality NAs
    diffs = diff(nas) # Get adjacent differences of indices
    
    # If no such section exists skip
    if(any(diffs >= len) == F){
      print(paste(j, "has not such a section. Skipped."))
      next()
    }
    
    section_start = nas[min(which(diffs >= len))] + 1 # if the index of two NAs occurences is longer 41, then there exists a section with non NAs after the first of theses indices
    section_end = section_start + len-1 # end index of section
    rollmean_start = section_start + ran-1 # Start of rolling mean window
    
    # select values up to first non NA and the 14 following values
    # to calculate rolling mean
    x_new = x_sub[section_start:section_end] # get data on whole section
    
    # Calculate mean
    for(i in ran:2){
      x_new[i-1] = round(mean(x_new[i:(i+13)], na.rm = TRUE)) 
    }
    
    # Calculate approximation error
    errors = append(errors, abs(x_new[1:(ran-1)] - x_sub[section_start:(rollmean_start - 1)]))
    
  }
  
  # Calculate mean of approximation error
  return(mean(errors))
  
}

error = owid_data %>% filter(location %in% unique(data$location)) %>% 
  summarise(error = approx_error(location, total_tests, len = 35, ran = 21)) # len: Total length of data section, ran: Range of data that should be imputed
reference = owid_data %>% summarize(mean_test = mean(total_tests, na.rm = T))
error/reference * 100 # Proportion of error compared to the average size to total_test

##### Plot of policy proportion ####


data_plot_policy = data_weekly %>% group_by(week) %>% 
  summarise(across(events:mask, ~ sum(.x)/27), date = unique(date)) %>% ungroup()
data_plot_policy = data_plot_policy %>%
  pivot_longer(-c(week, date), names_to = "policy", values_to = "prop")

policies = c(events = "Events", travel = "Travel", distance = "Distance", public = "Public",
             lock = "Lock", closure = "Closure", mask = "Mask", visit = "Visit")
Sys.setlocale("LC_TIME", "C")
ggplot(data_plot_policy, aes(x = date, y = prop)) +
  geom_line(size = 0.8) +
  facet_wrap(vars(policy), ncol = 4,
             labeller = labeller(policy = policies)) +
  xlab("Date")+ylab("Proportion of countries")+theme_clean()+
  scale_x_date(date_breaks = "6 weeks", date_labels = "%d %b") +
  theme(strip.text.x = element_text(size = 12, face = "bold"), 
        strip.text.y = element_text(size = 12, face = "bold"))
ggsave("proportion_of_policies.pdf", scale = 2, device = "pdf")
