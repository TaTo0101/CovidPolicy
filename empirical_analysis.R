########################## Empirical Analysis ####################################
######### Author: Tony Gottschalg
######### Date 22.03.2021 

###### Load Packages ####
libs = c("magrittr", "ggplot2", "tidyr", "tibble",
         "lubridate", "dplyr", "ggthemes", "RColorBrewer", "ggpubr",
         "xtable", "pdynmc")
if(any(libs%in%installed.packages() == F)){
  libs.1 = libs[which(libs%in%installed.packages() == F)]
  lapply(libs.1, FUN = install.packages, dependencies = T)
}
lapply(libs, library, character.only = T, quiet = T)

##### Load data ####

data_weekly = readRDS(file = "weekly_data_test.Rdata") # With lag induced NA's
#data_2 = readRDS(file = "data_final_wo_NA.Rdata") # Without lag induced NA's

##### Subset relevant data ####
data_sub = data_weekly %>% select(!c("continent", "date"))

# Convert location to integer (necessary for calculations)
get_names = unique(data_sub$location)
new_names = 1:27 + 60
identifier_selector = data.frame(Name = get_names, Number = new_names)
data_sub = data_sub %>%
  mutate(location = plyr::mapvalues(location, from = get_names, to = new_names),
         location = as.integer(as.character(location)))
data_sub = data_sub %>% mutate(location = as.integer(as.character(location)))
data_sub = as.data.frame(data_sub)

# Drop NAs
data_sub = data_sub %>% drop_na()

# Apply Frisch-Waugh
W = data_sub %>% select("population_density", "median_age", "aged_65_older",
                        "gdp_per_capita", "cardiovasc_death_rate", "hospital_beds_per_thousand",
                        "life_expectancy", "doc_density") %>%
  as.matrix()
projection = W %*% solve(t(W) %*% W) %*% t(W)
orth_projection = diag(1, nrow = dim(projection)[1], ncol = dim(projection)[2]) - projection

# Apply orth. projection on every variable except confounders and dummies
data_to_orth = data_sub %>% select(total_cases:chg_tests, retail:change_deaths_14_per) %>%
  as.matrix()
# data_to_orth = data_sub %>% select(total_cases:chg_tests, retail:residential) %>%
#   as.matrix()
trans_data = orth_projection %*% data_to_orth

# Add week, location and month to transformed data
trans_data = as.data.frame(trans_data)
trans_data = trans_data %>% mutate(week = data_sub$week,
                                   month = data_sub$month,
                                   location = data_sub$location,
                                   events = data_sub$events,
                                   public = data_sub$public,
                                   visit = data_sub$visit,
                                   closure = data_sub$closure,
                                   mask = data_sub$mask,
                                   lock = data_sub$lock,
                                   travel = data_sub$travel,
                                   distance = data_sub$distance)
# Reorder
#trans_data = trans_data %>% select(location, week, month, everything())
trans_data = trans_data %>% select(location, week, month, chg_cases, chg_deaths, chg_tests,
                                   change_cases_7_log, change_cases_7_per, change_deaths_14_log,
                                   change_deaths_14_per, everything())

# Since pdynmc cannot omit lags of length 0:
trans_data_2 = trans_data %>% group_by(location) %>%
  mutate(across(total_cases:distance, ~ dplyr::lag(.x, k = 1))) %>% ungroup()
trans_data_2 = drop_na(trans_data_2)
trans_data_2 = as.data.frame(trans_data_2)

#################################### Dynamic Panel Data Model #####
#################################### Case Change ####
##### Basic Specification Cases ####

# instruments_control = c("population_density", "median_age", "aged_65_older",
#                         "gdp_per_capita", "cardiovasc_death_rate", "hospital_beds_per_thousand",
#                         "life_expectancy", "doc_density")
# control_lags = rep(0, length(instruments_control))

# Behavior affects the dependent variable, therefore those will be included as instruments
instruments_behavior = c("retail", "grocery", "parks", "transit", "workplaces", "residential")
behavior_lags = rep(0, length(instruments_behavior))

# The same can be said for the policy variables
instruments_policy = c("events", "travel", "distance", "public", "lock", "closure", "visit", "mask")
policy_lags = rep(0, length(instruments_policy))

# Lastly we include as information variables the test rate and the total cases/deaths
instrument_inf_cases = c("chg_test", "total_deaths")
inf_lags_cases =  c(0,1)#rep(0, length(instrument_inf_cases))

# Estimate Model
mod_1 = pdynmc(trans_data_2, # data
       varname.i = "location", # variable name of the group identifier
       varname.t = "week", # variable name of the time identifier
       use.mc.diff = TRUE, # Use differences as instruments?
       use.mc.lev = FALSE, # Use levels as instruments?
       use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
       include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
       varname.y = "chg_cases", # variable name of the dependent variable
       lagTerms.y = 1, # number of lags of the dependent variable
       #include.x = TRUE,
       #varname.reg.pre = control_filter,
       #lagTerms.reg.pre = control_lags,
       #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
       #varname.reg.toInstr = control_filter, # name of those covariates
       fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
       fur.con.diff = TRUE, # Include as differences?
       fur.con.lev = FALSE, # Include as levels?
       varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_cases), # names of the instruments
       lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_cases), # lags of those instruments
       include.dum = TRUE, # include time dummies as instrument?
       dum.diff = TRUE, #  include as differences?
       dum.lev = FALSE, # include as levels?
       varname.dum = "week", # variable name of the time dummies
       w.mat = "iid.err", # type of weighting matrix
       std.err = "corrected", # type of std.err.
       estimation = "twostep", # estimation procedure
       #inst.thresh = 10, # maximum number of instruments that will be used
       opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_1) # Results

# Serial test
mtest.fct(mod_1, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_1 = round(coef(summary(mod_1)),4) # Estimated coefficients
test_results_mod_1_slope = c(as.vector(round(summary(mod_1)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_1)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_1_hansen = c(round(as.vector(summary(mod_1)$hansenj$statistic),4),
                              round(as.vector(summary(mod_1)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_1_serial_cor = c(round(as.vector(mtest.fct(mod_1, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_1, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_1 = rbind(coefs_mod_1, test_stat_header, test_results_mod_1_slope,
                      test_results_mod_1_hansen, test_results_mod_1_serial_cor)

names_coefs = c("Retail", "Grocery", "Parks", "Transit", "Workplaces", "Residential", "Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths")
rownames(results_mod_1) = c("lag(% change Cases, 7)", paste0("lag(",names_coefs,", 7)"),
                            "lag(Total No. Deaths, 14)",
                            paste("Week",row.names(results_mod_1)[19:49], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_1) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_1 = xtable(results_mod_1,
                     caption = "Results of two-step GMM estimation of policy, behavior and information on % change in COVID-19 cases for all countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:cases_spec_1_full",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_1, type = "latex",
             file = "./Tables/Spec_1_Cases_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Only European Countries ####

# Create a subset of European countries
eu_names = data_weekly %>% filter(continent == "Europe") %>% select(location) %>% unique() 

numbers_eu = identifier_selector %>% filter(Name %in% eu_names$location) %>% select(Number)

data_eu = trans_data_2 %>% filter(location %in% numbers_eu$Number)

# Estimate Model
mod_2 = pdynmc(data_eu, # data
              varname.i = "location", # variable name of the group identifier
              varname.t = "week", # variable name of the time identifier
              use.mc.diff = TRUE, # Use differences as instruments?
              use.mc.lev = FALSE, # Use levels as instruments?
              use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
              include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
              varname.y = "chg_cases", # variable name of the dependent variable
              lagTerms.y = 1, # number of lags of the dependent variable
              #include.x = TRUE,
              #varname.reg.pre = control_filter,
              #lagTerms.reg.pre = control_lags,
              #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
              #varname.reg.toInstr = control_filter, # name of those covariates
              fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
              fur.con.diff = TRUE, # Include as differences?
              fur.con.lev = FALSE, # Include as levels?
              varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_cases), # names of the instruments
              lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_cases), # lags of those instruments
              include.dum = TRUE, # include time dummies as instrument?
              dum.diff = TRUE, #  include as differences?
              dum.lev = FALSE, # include as levels?
              varname.dum = "week", # variable name of the time dummies
              w.mat = "iid.err", # type of weighting matrix
              std.err = "corrected", # type of std.err.
              estimation = "twostep", # estimation procedure
              #inst.thresh = 10, # maximum number of instruments that will be used
              opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_2) # Results

# Serial test
mtest.fct(mod_2, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)

coefs_mod_2 = round(coef(summary(mod_2)),4) # Estimated coefficients
test_results_mod_2_slope = c(as.vector(round(summary(mod_2)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_2)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_2_hansen = c(round(as.vector(summary(mod_2)$hansenj$statistic),4),
                              round(as.vector(summary(mod_2)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_2_serial_cor = c(round(as.vector(mtest.fct(mod_2, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_2, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_2 = rbind(coefs_mod_2, test_stat_header, test_results_mod_2_slope,
                      test_results_mod_2_hansen, test_results_mod_2_serial_cor)

names_coefs = c("Retail", "Grocery", "Parks", "Transit", "Workplaces", "Residential", "Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths")
rownames(results_mod_2) = c("lag(% change Cases, 7)", paste0("lag(",names_coefs,", 7)"),
                            "lag(Total No. Deaths, 14)",
                            paste("Week",row.names(results_mod_2)[19:49], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_2) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_2 = xtable(results_mod_2,
                     caption = "Results of two-step GMM estimation of policy, behavior and information on % change in COVID-19 cases for all European countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:cases_spec_2_full",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_2, type = "latex",
             file = "./Tables/Spec_2_Cases_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Only Non-European Countries ####

# Create a subset of non European countries
non_eu_names = data_weekly %>% filter(continent != "Europe") %>% select(location) %>% unique() 

numbers_non_eu = identifier_selector %>% filter(Name %in% non_eu_names$location) %>% select(Number)

data_non_eu = trans_data_2 %>% filter(location %in% numbers_non_eu$Number)


# Estimate Model
mod_3 = pdynmc(data_non_eu, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_cases", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_cases), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_cases), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_3) # Results

# Serial test
mtest.fct(mod_3, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)

coefs_mod_3 = round(coef(summary(mod_3)),4) # Estimated coefficients
test_results_mod_3_slope = c(as.vector(round(summary(mod_3)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_3)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_3_hansen = c(round(as.vector(summary(mod_3)$hansenj$statistic),4),
                              round(as.vector(summary(mod_3)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_3_serial_cor = c(round(as.vector(mtest.fct(mod_3, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_3, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_3 = rbind(coefs_mod_3, test_stat_header, test_results_mod_3_slope,
                      test_results_mod_3_hansen, test_results_mod_3_serial_cor)

names_coefs = c("Retail", "Grocery", "Parks", "Transit", "Workplaces", "Residential", "Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths")
rownames(results_mod_3) = c("lag(% change Cases, 7)", paste0("lag(",names_coefs,", 7)"),
                            "lag(Total No. Deaths, 14)",
                            paste("Week",row.names(results_mod_3)[19:49], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_3) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_3 = xtable(results_mod_3,
                     caption = "Results of two-step GMM estimation of policy, behavior and information on % change in COVID-19 cases for all non-European countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:cases_spec_3_full",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_3, type = "latex",
             file = "./Tables/Spec_3_Cases_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")


##### Only Countries that were not heavily imputed ####

# Create a subset of non European countries
imput_names = c("Canada", "Poland", "Spain", "Luxembourg", "Estonia", "Slovenia", "Australia")

numbers_non_imput = identifier_selector %>% filter(!(Name %in% imput_names)) %>%
  select(Number)

data_non_impute = trans_data_2 %>% filter(location %in% numbers_non_imput$Number)


# Estimate Model
mod_4 = pdynmc(data_non_impute, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_cases", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_cases), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_cases), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_4) # Results

# Serial test
mtest.fct(mod_4, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)

coefs_mod_4 = round(coef(summary(mod_4)),4) # Estimated coefficients
test_results_mod_4_slope = c(as.vector(round(summary(mod_4)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_4)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_4_hansen = c(round(as.vector(summary(mod_4)$hansenj$statistic),4),
                              round(as.vector(summary(mod_4)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_4_serial_cor = c(round(as.vector(mtest.fct(mod_4, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_4, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_4 = rbind(coefs_mod_4, test_stat_header, test_results_mod_4_slope,
                      test_results_mod_4_hansen, test_results_mod_4_serial_cor)

names_coefs = c("Retail", "Grocery", "Parks", "Transit", "Workplaces", "Residential", "Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths")
rownames(results_mod_4) = c("lag(% change Cases, 7)", paste0("lag(",names_coefs,", 7)"),
                            "lag(Total No. Deaths, 14)",
                            paste("Week",row.names(results_mod_4)[19:49], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_4) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_4 = xtable(results_mod_4,
                     caption = "Results of two-step GMM estimation of policy, behavior and information on % change in COVID-19 cases for all countries except: Canada, Poland, Spain, Luxembourg, Estonia, Slovenia, and Australia. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:cases_spec_4_full",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_4, type = "latex",
             file = "./Tables/Spec_4_Cases_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

#################################### Death Change ####
##### Basic Specification Deaths ####

# Since pdynmc cannot omit lags of length 0:
trans_data_3 = trans_data %>% group_by(location) %>%
  mutate(across(total_cases:distance, ~ dplyr::lag(.x, k = 2))) %>% ungroup()
trans_data_3 = drop_na(trans_data_3)
trans_data_3 = as.data.frame(trans_data_3)

# Behavior affects the dependent variable, therefore those will be included as instruments
instruments_behavior = c("retail", "grocery", "parks", "transit", "workplaces", "residential")
behavior_lags = rep(0, length(instruments_behavior))

# The same can be said for the policy variables
instruments_policy = c("events", "travel", "distance", "public", "lock", "closure", "visit", "mask")
policy_lags = rep(0, length(instruments_policy))

# Lastly we include as information variables the test rate and the total cases/deaths
instrument_inf_deaths = c("chg_test", "total_cases")
inf_lags_deaths = rep(0, length(instrument_inf_deaths))


# Estimate Model
mod_5 = pdynmc(trans_data_3, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_deaths", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_deaths), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_deaths), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_5) # Results

# Serial test
mtest.fct(mod_5, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_5 = round(coef(summary(mod_5)),4) # Estimated coefficients
test_results_mod_5_slope = c(as.vector(round(summary(mod_5)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_5)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_5_hansen = c(round(as.vector(summary(mod_5)$hansenj$statistic),4),
                              round(as.vector(summary(mod_5)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_5_serial_cor = c(round(as.vector(mtest.fct(mod_5, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_5, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_5 = rbind(coefs_mod_5, test_stat_header, test_results_mod_5_slope,
                      test_results_mod_5_hansen, test_results_mod_5_serial_cor)

names_coefs = c("Retail", "Grocery", "Parks", "Transit", "Workplaces", "Residential", "Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Cases")
rownames(results_mod_5) = c("lag(% change Deaths, 14)", paste0("lag(",names_coefs,", 14)"),
                            paste("Week",row.names(results_mod_5)[18:48], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_5) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_5 = xtable(results_mod_5,
                     caption = "Results of two-step GMM estimation of policy, behavior and information on % change in COVID-19 deaths for all countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:deaths_spec_5_full",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_5, type = "latex",
             file = "./Tables/Spec_5_Deaths_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Only European Countries ####

# Create a subset of European countries
eu_names = data_weekly %>% filter(continent == "Europe") %>% select(location) %>% unique() 

numbers_eu = identifier_selector %>% filter(Name %in% eu_names$location) %>% select(Number)

data_eu_2 = trans_data_3 %>% filter(location %in% numbers_eu$Number)

# Estimate Model
mod_6 = pdynmc(data_eu_2, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_deaths", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_deaths), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_deaths), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_6) # Results

# Serial test
mtest.fct(mod_6, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_6 = round(coef(summary(mod_6)),4) # Estimated coefficients
test_results_mod_6_slope = c(as.vector(round(summary(mod_6)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_6)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_6_hansen = c(round(as.vector(summary(mod_6)$hansenj$statistic),4),
                              round(as.vector(summary(mod_6)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_6_serial_cor = c(round(as.vector(mtest.fct(mod_6, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_6, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_6 = rbind(coefs_mod_6, test_stat_header, test_results_mod_6_slope,
                      test_results_mod_6_hansen, test_results_mod_6_serial_cor)

names_coefs = c("Retail", "Grocery", "Parks", "Transit", "Workplaces", "Residential", "Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Cases")
rownames(results_mod_6) = c("lag(% change Deaths, 14)", paste0("lag(",names_coefs,", 14)"),
                            paste("Week",row.names(results_mod_6)[18:48], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_6) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_6 = xtable(results_mod_6,
                     caption = "Results of two-step GMM estimation of policy, behavior and information on % change in COVID-19 deaths for European countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:deaths_spec_6_full",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_6, type = "latex",
             file = "./Tables/Spec_6_Deaths_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Only Non-European Countries ####

# Create a subset of non European countries
non_eu_names = data_weekly %>% filter(continent != "Europe") %>% select(location) %>% unique() 

numbers_non_eu = identifier_selector %>% filter(Name %in% non_eu_names$location) %>% select(Number)

data_non_eu_2 = trans_data_3 %>% filter(location %in% numbers_non_eu$Number)


# Estimate Model
mod_7 = pdynmc(data_non_eu_2, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_deaths", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_deaths), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_deaths), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_7) # Results

# Serial test
mtest.fct(mod_7, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_7 = round(coef(summary(mod_7)),4) # Estimated coefficients
test_results_mod_7_slope = c(as.vector(round(summary(mod_7)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_7)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_7_hansen = c(round(as.vector(summary(mod_7)$hansenj$statistic),4),
                              round(as.vector(summary(mod_7)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_7_serial_cor = c(round(as.vector(mtest.fct(mod_7, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_7, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_7 = rbind(coefs_mod_7, test_stat_header, test_results_mod_7_slope,
                      test_results_mod_7_hansen, test_results_mod_7_serial_cor)

names_coefs = c("Retail", "Grocery", "Parks", "Transit", "Workplaces", "Residential", "Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Cases")
rownames(results_mod_7) = c("lag(% change Deaths, 14)", paste0("lag(",names_coefs,", 14)"),
                            paste("Week",row.names(results_mod_7)[18:48], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_7) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_7 = xtable(results_mod_7,
                     caption = "Results of two-step GMM estimation of policy, behavior and information on % change in COVID-19 deaths for non-European countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:deaths_spec_7_full",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_7, type = "latex",
             file = "./Tables/Spec_7_Deaths_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Only Countries that were not heavily imputed ####

# Create a subset of non European countries
imput_names = c("Canada", "Poland", "Spain", "Luxembourg", "Estonia", "Slovenia", "Australia")

numbers_non_imput = identifier_selector %>% filter(!(Name %in% imput_names)) %>%
  select(Number)

data_non_impute_2 = trans_data_3 %>% filter(location %in% numbers_non_imput$Number)


# Estimate Model
mod_8 = pdynmc(data_non_impute_2, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_deaths", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_deaths), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_deaths), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_8) # Results

# Serial test
mtest.fct(mod_8, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_8 = round(coef(summary(mod_8)),4) # Estimated coefficients
test_results_mod_8_slope = c(as.vector(round(summary(mod_8)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_8)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_8_hansen = c(round(as.vector(summary(mod_8)$hansenj$statistic),4),
                              round(as.vector(summary(mod_8)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_8_serial_cor = c(round(as.vector(mtest.fct(mod_8, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_8, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_8 = rbind(coefs_mod_8, test_stat_header, test_results_mod_8_slope,
                      test_results_mod_8_hansen, test_results_mod_8_serial_cor)

names_coefs = c("Retail", "Grocery", "Parks", "Transit", "Workplaces", "Residential", "Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Cases")
rownames(results_mod_8) = c("lag(% change Deaths, 14)", paste0("lag(",names_coefs,", 14)"),
                            paste("Week",row.names(results_mod_8)[18:48], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_8) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_8 = xtable(results_mod_8,
                     caption = "Results of two-step GMM estimation of policy, behavior and information on % change in COVID-19 deaths all countries except: Canada, Poland, Spain, Luxembourg, Estonia, Slovenia, and Australia. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:deaths_spec_8_full",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_8, type = "latex",
             file = "./Tables/Spec_8_Deaths_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

################################### Reduced Results tables ####
##### Cases 
# Get coefficent tables and round the values
coef_table_1 = round(coef(summary(mod_1)),4)[1:17,]
coef_table_2 = round(coef(summary(mod_2)),4)[1:17,]
coef_table_3 = round(coef(summary(mod_3)),4)[1:17,]
coef_table_4 = round(coef(summary(mod_4)),4)[1:17,]

# Add asterixes to coefficents
asterix_fun = function(x, y){
  x_vals = character(length(x))
  
  for(i in 1:length(x)){
    if(y[i] < 0.001){
      x_vals[i] = paste0(c(x[i], "***"), collapse = "")
    }else if(y[i] < 0.01){
      x_vals[i] = paste0(c(x[i], "**"), collapse = "")
    }else if(y[i] < 0.05){
      x_vals[i] = paste0(c(x[i], "*"), collapse = "")
    }else{
      x_vals[i] = paste0(c(x[i],""), collapse = "")
    }
  }
  return(x_vals)
}

estimates_1 = asterix_fun(coef_table_1[,1], coef_table_1[, 4])
estimates_2 = asterix_fun(coef_table_2[,1], coef_table_2[, 4])
estimates_3 = asterix_fun(coef_table_3[,1], coef_table_3[, 4])
estimates_4 = asterix_fun(coef_table_4[,1], coef_table_4[, 4])

# Add parenthesis to std.err
std_err_1 = paste0("(", coef_table_1[,2], ")")
std_err_2 = paste0("(", coef_table_2[,2], ")")
std_err_3 = paste0("(", coef_table_3[,2], ")")
std_err_4 = paste0("(", coef_table_4[,2], ")")

# Create columns
column_1 = paste0(estimates_1, " ", std_err_1)
column_2 = paste0(estimates_2, " ", std_err_2)
column_3 = paste0(estimates_3, " ", std_err_3)
column_4 = paste0(estimates_4, " ", std_err_4)

# Create Table
table_results_1 = data.frame(column_1, column_2, column_3, column_4)
colnames(table_results_1) = c("All", "Europe", "Non-Europe", "Reduced")
names_coefs = c("Retail", "Grocery", "Parks", "Transit", "Workplaces", "Residential", "Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths")
rownames(table_results_1) = c("lag(% change Cases, 7)", paste0("lag(",names_coefs,", 7)"))

# Create latex table
table_results_1_red = xtable(table_results_1, caption = "Reduced results of two-step GMM estimation of policy, behavior and information on \\% change in COVID-19 cases. Specifications are: All: All countries in sample; Europe: All European countries in sample; Non-Europe: All non-European countries in sample; Reduced: All countries except Canada, Poland, Spain, Luxembourg, Estonia, Slovenia, and Australia. \\\\ Windmeijer corrected standard errors in parenthesis. \\\\ ***: $p<0.001$; **: $p< 0.01$; *: $p< 0.05$",
       label = "tab_results:cases_reduced",
       align = c("l", "c", "c", "c", "c"))
print.xtable(table_results_1_red, type = "latex",
             file = "./Tables/Cases_Results_reduced.tex", floating = T,
             include.rownames = T,
             booktabs = TRUE,
             size = "small")

##### Deaths

# Get coefficent tables and round the values
coef_table_5 = round(coef(summary(mod_5)),4)[1:17,]
coef_table_6 = round(coef(summary(mod_6)),4)[1:17,]
coef_table_7 = round(coef(summary(mod_7)),4)[1:17,]
coef_table_8 = round(coef(summary(mod_8)),4)[1:17,]

# Add asterixes to coefficents
asterix_fun = function(x, y){
  x_vals = character(length(x))
  
  for(i in 1:length(x)){
    if(y[i] < 0.001){
      x_vals[i] = paste0(c(x[i], "***"), collapse = "")
    }else if(y[i] < 0.01){
      x_vals[i] = paste0(c(x[i], "**"), collapse = "")
    }else if(y[i] < 0.05){
      x_vals[i] = paste0(c(x[i], "*"), collapse = "")
    }else{
      x_vals[i] = paste0(c(x[i],""), collapse = "")
    }
  }
  return(x_vals)
}

estimates_5 = asterix_fun(coef_table_5[,1], coef_table_5[, 4])
estimates_6 = asterix_fun(coef_table_6[,1], coef_table_6[, 4])
estimates_7 = asterix_fun(coef_table_7[,1], coef_table_7[, 4])
estimates_8 = asterix_fun(coef_table_8[,1], coef_table_8[, 4])

# Add parenthesis to std.err
std_err_5 = paste0("(", coef_table_5[,2], ")")
std_err_6 = paste0("(", coef_table_6[,2], ")")
std_err_7 = paste0("(", coef_table_7[,2], ")")
std_err_8 = paste0("(", coef_table_8[,2], ")")

# Create columns
column_5 = paste0(estimates_5, " ", std_err_5)
column_6 = paste0(estimates_6, " ", std_err_6)
column_7 = paste0(estimates_7, " ", std_err_7)
column_8 = paste0(estimates_8, " ", std_err_8)

# Create Table
table_results_2 = data.frame(column_5, column_6, column_7, column_8)
colnames(table_results_2) = c("All", "Europe", "Non-Europe", "Reduced")
names_coefs = c("Retail", "Grocery", "Parks", "Transit", "Workplaces", "Residential", "Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Cases")
rownames(table_results_2) = c("lag(% change Deaths, 14)", paste0("lag(",names_coefs,", 14)"))

# Create latex table
table_results_2_red = xtable(table_results_2, caption = "Reduced results of two-step GMM estimation of policy, behavior and information on \\% change in COVID-19 deaths Specifications are: All: All countries in sample; Europe: All European countries in sample; Non-Europe: All non-European countries in sample; Reduced: All countries except Canada, Poland, Spain, Luxembourg, Estonia, Slovenia, and Australia. \\\\ Windmeijer corrected standard errors in parenthesis. \\\\ ***: $p<0.001$; **: $p< 0.01$; *: $p< 0.05$",
                             label = "tab_results:deaths_reduced",
                             align = c("l", "c", "c", "c", "c"))
print.xtable(table_results_2_red, type = "latex",
             file = "./Tables/Deaths_Results_reduced.tex", floating = T,
             include.rownames = T,
             booktabs = TRUE,
             size = "small")
################################### Behavior Change ####
##### Retail ####

# Create lags of relevant variables
data_retail = trans_data %>% group_by(location) %>%
  mutate(across(!c(week, month, retail), ~ dplyr::lag(.x, k = 1))) %>% ungroup()
data_retail = drop_na(data_retail)
data_retail = as.data.frame(data_retail)


# Policy instruments
instruments_policy = c("events", "travel", "distance", "public", "lock", "closure", "visit", "mask")
policy_lags = rep(0, length(instruments_policy))

# Information instruments
instrument_inf_cases = c("chg_test", "total_deaths", "total_cases")
inf_lags_cases = rep(0, length(instrument_inf_cases))

# Estimate Model
mod_retail = pdynmc(data_retail, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "retail", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_policy, instrument_inf_cases), # names of the instruments
               lagTerms.reg.fur = c(policy_lags, inf_lags_cases), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_retail) # Results

# Serial test
mtest.fct(mod_retail, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_retail = round(coef(summary(mod_retail)),4) # Estimated coefficients
test_results_mod_retail_slope = c(as.vector(round(summary(mod_retail)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_retail)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_retail_hansen = c(round(as.vector(summary(mod_retail)$hansenj$statistic),4),
                              round(as.vector(summary(mod_retail)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_retail_serial_cor = c(round(as.vector(mtest.fct(mod_retail, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_retail, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_retail = rbind(coefs_mod_retail, test_stat_header, test_results_mod_retail_slope,
                      test_results_mod_retail_hansen, test_results_mod_retail_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths", "Total No. Cases")
rownames(results_mod_retail) = c("lag(Retail, 7)", paste0("lag(",names_coefs,", 7)"),
                            paste("Week",row.names(results_mod_retail)[13:43], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_retail) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_retail = xtable(results_mod_retail,
                     caption = "Results of two-step GMM estimation of behavior and information on the frequency of visiting retail stores and recreation places for all countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:retail_full",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_retail, type = "latex",
             file = "./Tables/Retail_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Grocery ####

# Create lags of relevant variables
data_grocery = trans_data %>% group_by(location) %>%
  mutate(across(!c(week, month, grocery), ~ dplyr::lag(.x, k = 1))) %>% ungroup()
data_grocery = drop_na(data_grocery)
data_grocery = as.data.frame(data_grocery)

# Policy instruments
instruments_policy = c("events", "travel", "distance", "public", "lock", "closure", "visit", "mask")
policy_lags = rep(0, length(instruments_policy))

# Information instruments
instrument_inf_cases = c("chg_test", "total_deaths", "total_cases")
inf_lags_cases = rep(0, length(instrument_inf_cases))

# Estimate Model
mod_grocery = pdynmc(data_grocery, # data
                    varname.i = "location", # variable name of the group identifier
                    varname.t = "week", # variable name of the time identifier
                    use.mc.diff = TRUE, # Use differences as instruments?
                    use.mc.lev = FALSE, # Use levels as instruments?
                    use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
                    include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
                    varname.y = "grocery", # variable name of the dependent variable
                    lagTerms.y = 1, # number of lags of the dependent variable
                    #include.x = TRUE,
                    #varname.reg.pre = control_filter,
                    #lagTerms.reg.pre = control_lags,
                    #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
                    #varname.reg.toInstr = control_filter, # name of those covariates
                    fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
                    fur.con.diff = TRUE, # Include as differences?
                    fur.con.lev = FALSE, # Include as levels?
                    varname.reg.fur = c(instruments_policy, instrument_inf_cases), # names of the instruments
                    lagTerms.reg.fur = c(policy_lags, inf_lags_cases), # lags of those instruments
                    include.dum = TRUE, # include time dummies as instrument?
                    dum.diff = TRUE, #  include as differences?
                    dum.lev = FALSE, # include as levels?
                    varname.dum = "week", # variable name of the time dummies
                    w.mat = "iid.err", # type of weighting matrix
                    std.err = "corrected", # type of std.err.
                    estimation = "twostep", # estimation procedure
                    #inst.thresh = 10, # maximum number of instruments that will be used
                    opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_grocery) # Results

# Serial test
mtest.fct(mod_grocery, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_grocery = round(coef(summary(mod_grocery)),4) # Estimated coefficients
test_results_mod_grocery_slope = c(as.vector(round(summary(mod_grocery)$slopef$statistic, 4)),
                                  as.vector(round(summary(mod_grocery)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_grocery_hansen = c(round(as.vector(summary(mod_grocery)$hansenj$statistic),4),
                                   round(as.vector(summary(mod_grocery)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_grocery_serial_cor = c(round(as.vector(mtest.fct(mod_grocery, 2)$statistic), 4),
                                       round(as.vector(mtest.fct(mod_grocery, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_grocery = rbind(coefs_mod_grocery, test_stat_header, test_results_mod_grocery_slope,
                           test_results_mod_grocery_hansen, test_results_mod_grocery_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths", "Total No. Cases")
rownames(results_mod_grocery) = c("lag(Grocery, 7)", paste0("lag(",names_coefs,", 7)"),
                                 paste("Week",row.names(results_mod_grocery)[13:43], "Dummy"), "",
                                 c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_grocery) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_grocery = xtable(results_mod_grocery,
                          caption = "Results of two-step GMM estimation of behavior and information on the frequency of visiting grocery stores and pharmacies for all countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                          label = "tab_results:grocery_full",
                          align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_grocery, type = "latex",
             file = "./Tables/Grocery_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")


##### Parks ####

# Create lags of relevant variables
data_parks = trans_data %>% group_by(location) %>%
  mutate(across(!c(week, month, parks), ~ dplyr::lag(.x, k = 1))) %>% ungroup()
data_parks = drop_na(data_parks)
data_parks = as.data.frame(data_parks)

# Policy instruments
instruments_policy = c("events", "travel", "distance", "public", "lock", "closure", "visit", "mask")
policy_lags = rep(0, length(instruments_policy))

# Information instruments
instrument_inf_cases = c("chg_test", "total_deaths", "total_cases")
inf_lags_cases = rep(0, length(instrument_inf_cases))

# Estimate Model
mod_parks = pdynmc(data_parks, # data
                     varname.i = "location", # variable name of the group identifier
                     varname.t = "week", # variable name of the time identifier
                     use.mc.diff = TRUE, # Use differences as instruments?
                     use.mc.lev = FALSE, # Use levels as instruments?
                     use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
                     include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
                     varname.y = "parks", # variable name of the dependent variable
                     lagTerms.y = 1, # number of lags of the dependent variable
                     #include.x = TRUE,
                     #varname.reg.pre = control_filter,
                     #lagTerms.reg.pre = control_lags,
                     #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
                     #varname.reg.toInstr = control_filter, # name of those covariates
                     fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
                     fur.con.diff = TRUE, # Include as differences?
                     fur.con.lev = FALSE, # Include as levels?
                     varname.reg.fur = c(instruments_policy, instrument_inf_cases), # names of the instruments
                     lagTerms.reg.fur = c(policy_lags, inf_lags_cases), # lags of those instruments
                     include.dum = TRUE, # include time dummies as instrument?
                     dum.diff = TRUE, #  include as differences?
                     dum.lev = FALSE, # include as levels?
                     varname.dum = "week", # variable name of the time dummies
                     w.mat = "iid.err", # type of weighting matrix
                     std.err = "corrected", # type of std.err.
                     estimation = "twostep", # estimation procedure
                     #inst.thresh = 10, # maximum number of instruments that will be used
                     opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_parks) # Results

# Serial test
mtest.fct(mod_parks, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_parks = round(coef(summary(mod_parks)),4) # Estimated coefficients
test_results_mod_parks_slope = c(as.vector(round(summary(mod_parks)$slopef$statistic, 4)),
                                   as.vector(round(summary(mod_parks)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_parks_hansen = c(round(as.vector(summary(mod_parks)$hansenj$statistic),4),
                                    round(as.vector(summary(mod_parks)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_parks_serial_cor = c(round(as.vector(mtest.fct(mod_parks, 2)$statistic), 4),
                                        round(as.vector(mtest.fct(mod_parks, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_parks = rbind(coefs_mod_parks, test_stat_header, test_results_mod_parks_slope,
                            test_results_mod_parks_hansen, test_results_mod_parks_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths", "Total No. Cases")
rownames(results_mod_parks) = c("lag(Parks, 7)", paste0("lag(",names_coefs,", 7)"),
                                  paste("Week",row.names(results_mod_parks)[13:43], "Dummy"), "",
                                  c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_parks) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_parks = xtable(results_mod_parks,
                           caption = "Results of two-step GMM estimation of behavior and information on the frequency of visiting parks for all countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                           label = "tab_results:parks_full",
                           align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_parks, type = "latex",
             file = "./Tables/Parks_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Transit ####

# Create lags of relevant variables
data_transit = trans_data %>% group_by(location) %>%
  mutate(across(!c(week, month, transit), ~ dplyr::lag(.x, k = 1))) %>% ungroup()
data_transit = drop_na(data_transit)
data_transit = as.data.frame(data_transit)

# Policy instruments
instruments_policy = c("events", "travel", "distance", "public", "lock", "closure", "visit", "mask")
policy_lags = rep(0, length(instruments_policy))

# Information instruments
instrument_inf_cases = c("chg_test", "total_deaths", "total_cases")
inf_lags_cases = rep(0, length(instrument_inf_cases))

# Estimate Model
mod_transit = pdynmc(data_transit, # data
                   varname.i = "location", # variable name of the group identifier
                   varname.t = "week", # variable name of the time identifier
                   use.mc.diff = TRUE, # Use differences as instruments?
                   use.mc.lev = FALSE, # Use levels as instruments?
                   use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
                   include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
                   varname.y = "transit", # variable name of the dependent variable
                   lagTerms.y = 1, # number of lags of the dependent variable
                   #include.x = TRUE,
                   #varname.reg.pre = control_filter,
                   #lagTerms.reg.pre = control_lags,
                   #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
                   #varname.reg.toInstr = control_filter, # name of those covariates
                   fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
                   fur.con.diff = TRUE, # Include as differences?
                   fur.con.lev = FALSE, # Include as levels?
                   varname.reg.fur = c(instruments_policy, instrument_inf_cases), # names of the instruments
                   lagTerms.reg.fur = c(policy_lags, inf_lags_cases), # lags of those instruments
                   include.dum = TRUE, # include time dummies as instrument?
                   dum.diff = TRUE, #  include as differences?
                   dum.lev = FALSE, # include as levels?
                   varname.dum = "week", # variable name of the time dummies
                   w.mat = "iid.err", # type of weighting matrix
                   std.err = "corrected", # type of std.err.
                   estimation = "twostep", # estimation procedure
                   #inst.thresh = 10, # maximum number of instruments that will be used
                   opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_transit) # Results

# Serial test
mtest.fct(mod_transit, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_transit = round(coef(summary(mod_transit)),4) # Estimated coefficients
test_results_mod_transit_slope = c(as.vector(round(summary(mod_transit)$slopef$statistic, 4)),
                                 as.vector(round(summary(mod_transit)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_transit_hansen = c(round(as.vector(summary(mod_transit)$hansenj$statistic),4),
                                  round(as.vector(summary(mod_transit)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_transit_serial_cor = c(round(as.vector(mtest.fct(mod_transit, 2)$statistic), 4),
                                      round(as.vector(mtest.fct(mod_transit, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_transit = rbind(coefs_mod_transit, test_stat_header, test_results_mod_transit_slope,
                          test_results_mod_transit_hansen, test_results_mod_transit_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths", "Total No. Cases")
rownames(results_mod_transit) = c("lag(Transit, 7)", paste0("lag(",names_coefs,", 7)"),
                                paste("Week",row.names(results_mod_transit)[13:43], "Dummy"), "",
                                c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_transit) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_transit = xtable(results_mod_transit,
                         caption = "Results of two-step GMM estimation of behavior and information on the frequency of using public transit for all countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                         label = "tab_results:transit_full",
                         align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_transit, type = "latex",
             file = "./Tables/Transit_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Workplaces ####

# Create lags of relevant variables
data_workplaces = trans_data %>% group_by(location) %>%
  mutate(across(!c(week, month, workplaces), ~ dplyr::lag(.x, k = 1))) %>% ungroup()
data_workplaces = drop_na(data_workplaces)
data_workplaces = as.data.frame(data_workplaces)

# Policy instruments
instruments_policy = c("events", "travel", "distance", "public", "lock", "closure", "visit", "mask")
policy_lags = rep(0, length(instruments_policy))

# Information instruments
instrument_inf_cases = c("chg_test", "total_deaths", "total_cases")
inf_lags_cases = rep(0, length(instrument_inf_cases))

# Estimate Model
mod_workplaces = pdynmc(data_workplaces, # data
                     varname.i = "location", # variable name of the group identifier
                     varname.t = "week", # variable name of the time identifier
                     use.mc.diff = TRUE, # Use differences as instruments?
                     use.mc.lev = FALSE, # Use levels as instruments?
                     use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
                     include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
                     varname.y = "workplaces", # variable name of the dependent variable
                     lagTerms.y = 1, # number of lags of the dependent variable
                     #include.x = TRUE,
                     #varname.reg.pre = control_filter,
                     #lagTerms.reg.pre = control_lags,
                     #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
                     #varname.reg.toInstr = control_filter, # name of those covariates
                     fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
                     fur.con.diff = TRUE, # Include as differences?
                     fur.con.lev = FALSE, # Include as levels?
                     varname.reg.fur = c(instruments_policy, instrument_inf_cases), # names of the instruments
                     lagTerms.reg.fur = c(policy_lags, inf_lags_cases), # lags of those instruments
                     include.dum = TRUE, # include time dummies as instrument?
                     dum.diff = TRUE, #  include as differences?
                     dum.lev = FALSE, # include as levels?
                     varname.dum = "week", # variable name of the time dummies
                     w.mat = "iid.err", # type of weighting matrix
                     std.err = "corrected", # type of std.err.
                     estimation = "twostep", # estimation procedure
                     #inst.thresh = 10, # maximum number of instruments that will be used
                     opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_workplaces) # Results

# Serial test
mtest.fct(mod_workplaces, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_workplaces = round(coef(summary(mod_workplaces)),4) # Estimated coefficients
test_results_mod_workplaces_slope = c(as.vector(round(summary(mod_workplaces)$slopef$statistic, 4)),
                                   as.vector(round(summary(mod_workplaces)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_workplaces_hansen = c(round(as.vector(summary(mod_workplaces)$hansenj$statistic),4),
                                    round(as.vector(summary(mod_workplaces)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_workplaces_serial_cor = c(round(as.vector(mtest.fct(mod_workplaces, 2)$statistic), 4),
                                        round(as.vector(mtest.fct(mod_workplaces, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_workplaces = rbind(coefs_mod_workplaces, test_stat_header, test_results_mod_workplaces_slope,
                            test_results_mod_workplaces_hansen, test_results_mod_workplaces_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths", "Total No. Cases")
rownames(results_mod_workplaces) = c("lag(Workplaces, 7)", paste0("lag(",names_coefs,", 7)"),
                                  paste("Week",row.names(results_mod_workplaces)[13:43], "Dummy"), "",
                                  c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_workplaces) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_workplaces = xtable(results_mod_workplaces,
                           caption = "Results of two-step GMM estimation of behavior and information on the frequency of traveling to workplaces for all countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                           label = "tab_results:workplaces_full",
                           align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_workplaces, type = "latex",
             file = "./Tables/Workplaces_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Residential ####

# Create lags of relevant variables
data_residential = trans_data %>% group_by(location) %>%
  mutate(across(!c(week, month, residential), ~ dplyr::lag(.x, k = 1))) %>% ungroup()
data_residential = drop_na(data_residential)
data_residential = as.data.frame(data_residential)

# Policy instruments
instruments_policy = c("events", "travel", "distance", "public", "lock", "closure", "visit", "mask")
policy_lags = rep(0, length(instruments_policy))

# Information instruments
instrument_inf_cases = c("chg_test", "total_deaths", "total_cases")
inf_lags_cases = rep(0, length(instrument_inf_cases))

# Estimate Model
mod_residential = pdynmc(data_residential, # data
                        varname.i = "location", # variable name of the group identifier
                        varname.t = "week", # variable name of the time identifier
                        use.mc.diff = TRUE, # Use differences as instruments?
                        use.mc.lev = FALSE, # Use levels as instruments?
                        use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
                        include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
                        varname.y = "residential", # variable name of the dependent variable
                        lagTerms.y = 1, # number of lags of the dependent variable
                        #include.x = TRUE,
                        #varname.reg.pre = control_filter,
                        #lagTerms.reg.pre = control_lags,
                        #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
                        #varname.reg.toInstr = control_filter, # name of those covariates
                        fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
                        fur.con.diff = TRUE, # Include as differences?
                        fur.con.lev = FALSE, # Include as levels?
                        varname.reg.fur = c(instruments_policy, instrument_inf_cases), # names of the instruments
                        lagTerms.reg.fur = c(policy_lags, inf_lags_cases), # lags of those instruments
                        include.dum = TRUE, # include time dummies as instrument?
                        dum.diff = TRUE, #  include as differences?
                        dum.lev = FALSE, # include as levels?
                        varname.dum = "week", # variable name of the time dummies
                        w.mat = "iid.err", # type of weighting matrix
                        std.err = "corrected", # type of std.err.
                        estimation = "twostep", # estimation procedure
                        #inst.thresh = 10, # maximum number of instruments that will be used
                        opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_residential) # Results

# Serial test
mtest.fct(mod_residential, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_residential = round(coef(summary(mod_residential)),4) # Estimated coefficients
test_results_mod_residential_slope = c(as.vector(round(summary(mod_residential)$slopef$statistic, 4)),
                                      as.vector(round(summary(mod_residential)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_residential_hansen = c(round(as.vector(summary(mod_residential)$hansenj$statistic),4),
                                       round(as.vector(summary(mod_residential)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_residential_serial_cor = c(round(as.vector(mtest.fct(mod_residential, 2)$statistic), 4),
                                           round(as.vector(mtest.fct(mod_residential, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_residential = rbind(coefs_mod_residential, test_stat_header, test_results_mod_residential_slope,
                               test_results_mod_residential_hansen, test_results_mod_residential_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths", "Total No. Cases")
rownames(results_mod_residential) = c("lag(Workplaces, 7)", paste0("lag(",names_coefs,", 7)"),
                                     paste("Week",row.names(results_mod_residential)[13:43], "Dummy"), "",
                                     c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_residential) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_residential = xtable(results_mod_residential,
                              caption = "Results of two-step GMM estimation of behavior and information on the frequency of staying at residential areas for all countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                              label = "tab_results:residential_full",
                              align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_residential, type = "latex",
             file = "./Tables/Residential_Results_full.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Reduced Form Behavior ####

# Get coefficent tables and round the values
coef_table_retail = round(coef(summary(mod_retail)),4)[1:12,]
coef_table_grocery = round(coef(summary(mod_grocery)),4)[1:12,]
coef_table_parks = round(coef(summary(mod_parks)),4)[1:12,]
coef_table_transit = round(coef(summary(mod_transit)),4)[1:12,]
coef_table_workplaces = round(coef(summary(mod_workplaces)),4)[1:12,]
coef_table_residential = round(coef(summary(mod_residential)),4)[1:12,]

# Add asterixes to coefficents
asterix_fun = function(x, y){
  x_vals = character(length(x))
  
  for(i in 1:length(x)){
    if(y[i] < 0.001){
      x_vals[i] = paste0(c(x[i], "***"), collapse = "")
    }else if(y[i] < 0.01){
      x_vals[i] = paste0(c(x[i], "**"), collapse = "")
    }else if(y[i] < 0.05){
      x_vals[i] = paste0(c(x[i], "*"), collapse = "")
    }else{
      x_vals[i] = paste0(c(x[i],""), collapse = "")
    }
  }
  return(x_vals)
}

estimates_retail = asterix_fun(coef_table_retail[,1], coef_table_retail[, 4])
estimates_grocery = asterix_fun(coef_table_grocery[,1], coef_table_grocery[, 4])
estimates_parks = asterix_fun(coef_table_parks[,1], coef_table_parks[, 4])
estimates_transit = asterix_fun(coef_table_transit[,1], coef_table_transit[, 4])
estimates_workplaces = asterix_fun(coef_table_workplaces[,1], coef_table_workplaces[, 4])
estimates_residential = asterix_fun(coef_table_residential[,1], coef_table_residential[, 4])

# Add parenthesis to std.err
std_err_retail = paste0("(", coef_table_retail[,2], ")")
std_err_grocery = paste0("(", coef_table_grocery[,2], ")")
std_err_parks = paste0("(", coef_table_parks[,2], ")")
std_err_transit = paste0("(", coef_table_transit[,2], ")")
std_err_workplaces = paste0("(", coef_table_workplaces[,2], ")")
std_err_residential = paste0("(", coef_table_residential[,2], ")")

# Create columns
column_1 = paste0(estimates_retail, " ", std_err_retail)
column_2 = paste0(estimates_grocery, " ", std_err_grocery)
column_3 = paste0(estimates_parks, " ", std_err_transit)
column_4 = paste0(estimates_transit, " ", std_err_transit)
column_5 = paste0(estimates_workplaces, " ", std_err_workplaces)
column_6 = paste0(estimates_residential, " ", std_err_residential)

# Create Table
table_results_3 = data.frame(column_1, column_2, column_3, column_4, column_5, column_6)
colnames(table_results_3) = c("Retail", "Grocery", "Parks", "Transit", "Workplaces", "Residential")
names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths", "Total No. Cases")
rownames(table_results_3) = c("lag($Y$, 7)", paste0("lag(",names_coefs,", 7)"))

# Create latex table
table_results_3_red = xtable(table_results_3[,1:3], caption = "Reduced results of two-step GMM estimation of policy and information on behavior, i.e. mobility for all countries. $Y$ is the respective dependent variable.  \\\\ Windmeijer corrected standard errors in parenthesis. \\\\ ***: $p<0.001$; **: $p< 0.01$; *: $p< 0.05$",
                             label = "tab_results:behavior_reduced_1",
                             align = c("l", "c", "c", "c"))
print.xtable(table_results_3_red, type = "latex",
             file = "./Tables/Behavior_Results_reduced_1.tex", floating = T,
             include.rownames = T,
             booktabs = TRUE,
             size = "small")

table_results_4_red = xtable(table_results_3[,4:6], caption = "Reduced results of two-step GMM estimation of policy and information on behavior, i.e. mobility for all countries. $Y$ is the respective dependent variable.  \\\\ Windmeijer corrected standard errors in parenthesis. \\\\ ***: $p<0.001$; **: $p< 0.01$; *: $p< 0.05$",
                             label = "tab_results:behavior_reduced_2",
                             align = c("l", "c", "c", "c"))
print.xtable(table_results_4_red, type = "latex",
             file = "./Tables/Behavior_Results_reduced_2.tex", floating = T,
             include.rownames = T,
             booktabs = TRUE,
             size = "small")

#################################### Case Change \wo Behavior ####
##### Basic Specification Cases ####

# instruments_control = c("population_density", "median_age", "aged_65_older",
#                         "gdp_per_capita", "cardiovasc_death_rate", "hospital_beds_per_thousand",
#                         "life_expectancy", "doc_density")
# control_lags = rep(0, length(instruments_control))

# Behavior affects the dependent variable, therefore those will be included as instruments
instruments_behavior = c()
behavior_lags = c()

# The same can be said for the policy variables
instruments_policy = c("events", "travel", "distance", "public", "lock", "closure", "visit", "mask")
policy_lags = rep(0, length(instruments_policy))

# Lastly we include as information variables the test rate and the total cases/deaths
instrument_inf_cases = c("chg_test", "total_deaths")
inf_lags_cases = c(0,1)#rep(0, length(instrument_inf_cases))

# Estimate Model
mod_1 = pdynmc(trans_data_2, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_cases", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_cases), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_cases), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_1) # Results

# Serial test
mtest.fct(mod_1, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_1 = round(coef(summary(mod_1)),4) # Estimated coefficients
test_results_mod_1_slope = c(as.vector(round(summary(mod_1)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_1)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_1_hansen = c(round(as.vector(summary(mod_1)$hansenj$statistic),4),
                              round(as.vector(summary(mod_1)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_1_serial_cor = c(round(as.vector(mtest.fct(mod_1, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_1, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_1 = rbind(coefs_mod_1, test_stat_header, test_results_mod_1_slope,
                      test_results_mod_1_hansen, test_results_mod_1_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths")
rownames(results_mod_1) = c("lag(% change Cases, 7)", paste0("lag(",names_coefs,", 7)"),
                            "lag(Total No. Deaths, 14)",
                            paste("Week",row.names(results_mod_1)[19:49], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_1) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_1 = xtable(results_mod_1,
                     caption = "Results of two-step GMM estimation of policy and information on % change in COVID-19 cases for all countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:cases_spec_1_full_wo_behavior",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_1, type = "latex",
             file = "./Tables/Spec_1_Cases_Results_full_wo_behavior.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Only European Countries ####

# Create a subset of European countries
eu_names = data_weekly %>% filter(continent == "Europe") %>% select(location) %>% unique() 

numbers_eu = identifier_selector %>% filter(Name %in% eu_names$location) %>% select(Number)

data_eu = trans_data_2 %>% filter(location %in% numbers_eu$Number)

# Estimate Model
mod_2 = pdynmc(data_eu, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_cases", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_cases), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_cases), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_2) # Results

# Serial test
mtest.fct(mod_2, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)

coefs_mod_2 = round(coef(summary(mod_2)),4) # Estimated coefficients
test_results_mod_2_slope = c(as.vector(round(summary(mod_2)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_2)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_2_hansen = c(round(as.vector(summary(mod_2)$hansenj$statistic),4),
                              round(as.vector(summary(mod_2)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_2_serial_cor = c(round(as.vector(mtest.fct(mod_2, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_2, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_2 = rbind(coefs_mod_2, test_stat_header, test_results_mod_2_slope,
                      test_results_mod_2_hansen, test_results_mod_2_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths")
rownames(results_mod_2) = c("lag(% change Cases, 7)", paste0("lag(",names_coefs,", 7)"),
                            "lag(Total No. Deaths, 14)",
                            paste("Week",row.names(results_mod_2)[19:49], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_2) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_2 = xtable(results_mod_2,
                     caption = "Results of two-step GMM estimation of policy and information on % change in COVID-19 cases for all European countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:cases_spec_2_full_wo_behavior",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_2, type = "latex",
             file = "./Tables/Spec_2_Cases_Results_full_wo_behavior.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Only Non-European Countries ####

# Create a subset of non European countries
non_eu_names = data_weekly %>% filter(continent != "Europe") %>% select(location) %>% unique() 

numbers_non_eu = identifier_selector %>% filter(Name %in% non_eu_names$location) %>% select(Number)

data_non_eu = trans_data_2 %>% filter(location %in% numbers_non_eu$Number)


# Estimate Model
mod_3 = pdynmc(data_non_eu, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_cases", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_cases), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_cases), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_3) # Results

# Serial test
mtest.fct(mod_3, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)

coefs_mod_3 = round(coef(summary(mod_3)),4) # Estimated coefficients
test_results_mod_3_slope = c(as.vector(round(summary(mod_3)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_3)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_3_hansen = c(round(as.vector(summary(mod_3)$hansenj$statistic),4),
                              round(as.vector(summary(mod_3)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_3_serial_cor = c(round(as.vector(mtest.fct(mod_3, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_3, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_3 = rbind(coefs_mod_3, test_stat_header, test_results_mod_3_slope,
                      test_results_mod_3_hansen, test_results_mod_3_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths")
rownames(results_mod_3) = c("lag(% change Cases, 7)", paste0("lag(",names_coefs,", 7)"),
                            "lag(Total No. Deaths, 14)",
                            paste("Week",row.names(results_mod_3)[19:49], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_3) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_3 = xtable(results_mod_3,
                     caption = "Results of two-step GMM estimation of policy and information on % change in COVID-19 cases for all non-European countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:cases_spec_3_full_wo_behavior",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_3, type = "latex",
             file = "./Tables/Spec_3_Cases_Results_full_wo_behavior.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")


##### Only Countries that were not heavily imputed ####

# Create a subset of non European countries
imput_names = c("Canada", "Poland", "Spain", "Luxembourg", "Estonia", "Slovenia", "Australia")

numbers_non_imput = identifier_selector %>% filter(!(Name %in% imput_names)) %>%
  select(Number)

data_non_impute = trans_data_2 %>% filter(location %in% numbers_non_imput$Number)


# Estimate Model
mod_4 = pdynmc(data_non_impute, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_cases", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_cases), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_cases), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_4) # Results

# Serial test
mtest.fct(mod_4, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)

coefs_mod_4 = round(coef(summary(mod_4)),4) # Estimated coefficients
test_results_mod_4_slope = c(as.vector(round(summary(mod_4)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_4)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_4_hansen = c(round(as.vector(summary(mod_4)$hansenj$statistic),4),
                              round(as.vector(summary(mod_4)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_4_serial_cor = c(round(as.vector(mtest.fct(mod_4, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_4, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_4 = rbind(coefs_mod_4, test_stat_header, test_results_mod_4_slope,
                      test_results_mod_4_hansen, test_results_mod_4_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths")
rownames(results_mod_4) = c("lag(% change Cases, 7)", paste0("lag(",names_coefs,", 7)"),
                            "lag(Total No. Deaths, 14)",
                            paste("Week",row.names(results_mod_4)[19:49], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_4) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_4 = xtable(results_mod_4,
                     caption = "Results of two-step GMM estimation of policy and information on % change in COVID-19 cases for all countries except: Canada, Poland, Spain, Luxembourg, Estonia, Slovenia, and Australia. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:cases_spec_4_full_wo_behavior",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_4, type = "latex",
             file = "./Tables/Spec_4_Cases_Results_full.tex_wo_behavior", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

#################################### Death Change ####
##### Basic Specification Deaths ####

# Since pdynmc cannot omit lags of length 0:
trans_data_3 = trans_data %>% group_by(location) %>%
  mutate(across(total_cases:distance, ~ dplyr::lag(.x, k = 2))) %>% ungroup()
trans_data_3 = drop_na(trans_data_3)
trans_data_3 = as.data.frame(trans_data_3)

# Behavior affects the dependent variable, therefore those will be included as instruments
instruments_behavior = c()
behavior_lags = c()

# The same can be said for the policy variables
instruments_policy = c("events", "travel", "distance", "public", "lock", "closure", "visit", "mask")
policy_lags = rep(0, length(instruments_policy))

# Lastly we include as information variables the test rate and the total cases/deaths
instrument_inf_deaths = c("chg_test", "total_cases")
inf_lags_deaths = rep(0, length(instrument_inf_deaths))


# Estimate Model
mod_5 = pdynmc(trans_data_3, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_deaths", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_deaths), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_deaths), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_5) # Results

# Serial test
mtest.fct(mod_5, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_5 = round(coef(summary(mod_5)),4) # Estimated coefficients
test_results_mod_5_slope = c(as.vector(round(summary(mod_5)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_5)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_5_hansen = c(round(as.vector(summary(mod_5)$hansenj$statistic),4),
                              round(as.vector(summary(mod_5)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_5_serial_cor = c(round(as.vector(mtest.fct(mod_5, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_5, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_5 = rbind(coefs_mod_5, test_stat_header, test_results_mod_5_slope,
                      test_results_mod_5_hansen, test_results_mod_5_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Cases")
rownames(results_mod_5) = c("lag(% change Deaths, 14)", paste0("lag(",names_coefs,", 14)"),
                            paste("Week",row.names(results_mod_5)[12:42], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_5) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_5 = xtable(results_mod_5,
                     caption = "Results of two-step GMM estimation of policy and information on % change in COVID-19 deaths for all countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:deaths_spec_5_full_wo_behavior",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_5, type = "latex",
             file = "./Tables/Spec_5_Deaths_Results_full_wo_behavior.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Only European Countries ####

# Create a subset of European countries
eu_names = data_weekly %>% filter(continent == "Europe") %>% select(location) %>% unique() 

numbers_eu = identifier_selector %>% filter(Name %in% eu_names$location) %>% select(Number)

data_eu_2 = trans_data_3 %>% filter(location %in% numbers_eu$Number)

# Estimate Model
mod_6 = pdynmc(data_eu_2, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_deaths", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_deaths), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_deaths), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_6) # Results

# Serial test
mtest.fct(mod_6, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_6 = round(coef(summary(mod_6)),4) # Estimated coefficients
test_results_mod_6_slope = c(as.vector(round(summary(mod_6)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_6)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_6_hansen = c(round(as.vector(summary(mod_6)$hansenj$statistic),4),
                              round(as.vector(summary(mod_6)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_6_serial_cor = c(round(as.vector(mtest.fct(mod_6, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_6, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_6 = rbind(coefs_mod_6, test_stat_header, test_results_mod_6_slope,
                      test_results_mod_6_hansen, test_results_mod_6_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Cases")
rownames(results_mod_6) = c("lag(% change Deaths, 14)", paste0("lag(",names_coefs,", 14)"),
                            paste("Week",row.names(results_mod_6)[12:42], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_6) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_6 = xtable(results_mod_6,
                     caption = "Results of two-step GMM estimation of policy and information on % change in COVID-19 deaths for European countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:deaths_spec_6_full_wo_behavior",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_6, type = "latex",
             file = "./Tables/Spec_6_Deaths_Results_full_wo_behavior.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Only Non-European Countries ####

# Create a subset of non European countries
non_eu_names = data_weekly %>% filter(continent != "Europe") %>% select(location) %>% unique() 

numbers_non_eu = identifier_selector %>% filter(Name %in% non_eu_names$location) %>% select(Number)

data_non_eu_2 = trans_data_3 %>% filter(location %in% numbers_non_eu$Number)


# Estimate Model
mod_7 = pdynmc(data_non_eu_2, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_deaths", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_deaths), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_deaths), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_7) # Results

# Serial test
mtest.fct(mod_7, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_7 = round(coef(summary(mod_7)),4) # Estimated coefficients
test_results_mod_7_slope = c(as.vector(round(summary(mod_7)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_7)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_7_hansen = c(round(as.vector(summary(mod_7)$hansenj$statistic),4),
                              round(as.vector(summary(mod_7)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_7_serial_cor = c(round(as.vector(mtest.fct(mod_7, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_7, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_7 = rbind(coefs_mod_7, test_stat_header, test_results_mod_7_slope,
                      test_results_mod_7_hansen, test_results_mod_7_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Cases")
rownames(results_mod_7) = c("lag(% change Deaths, 14)", paste0("lag(",names_coefs,", 14)"),
                            paste("Week",row.names(results_mod_7)[12:42], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_7) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_7 = xtable(results_mod_7,
                     caption = "Results of two-step GMM estimation of policy and information on % change in COVID-19 deaths for non-European countries. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:deaths_spec_7_full_wo_behavior",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_7, type = "latex",
             file = "./Tables/Spec_7_Deaths_Results_full_wo_behavior.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

##### Only Countries that were not heavily imputed ####

# Create a subset of non European countries
imput_names = c("Canada", "Poland", "Spain", "Luxembourg", "Estonia", "Slovenia", "Australia")

numbers_non_imput = identifier_selector %>% filter(!(Name %in% imput_names)) %>%
  select(Number)

data_non_impute_2 = trans_data_3 %>% filter(location %in% numbers_non_imput$Number)


# Estimate Model
mod_8 = pdynmc(data_non_impute_2, # data
               varname.i = "location", # variable name of the group identifier
               varname.t = "week", # variable name of the time identifier
               use.mc.diff = TRUE, # Use differences as instruments?
               use.mc.lev = FALSE, # Use levels as instruments?
               use.mc.nonlin = FALSE, # Use non-linear (e.g. quadratic expansion) as instruments?
               include.y = TRUE, # Include the dependent variable in the formula, i.e. as instrument
               varname.y = "chg_deaths", # variable name of the dependent variable
               lagTerms.y = 1, # number of lags of the dependent variable
               #include.x = TRUE,
               #varname.reg.pre = control_filter,
               #lagTerms.reg.pre = control_lags,
               #include.x.toInstr = TRUE, # include covariates which will not be used as instruments?
               #varname.reg.toInstr = control_filter, # name of those covariates
               fur.con = TRUE, # include further variables that will be used as instruments for which the coef. should be estimated?
               fur.con.diff = TRUE, # Include as differences?
               fur.con.lev = FALSE, # Include as levels?
               varname.reg.fur = c(instruments_behavior, instruments_policy, instrument_inf_deaths), # names of the instruments
               lagTerms.reg.fur = c(behavior_lags, policy_lags, inf_lags_deaths), # lags of those instruments
               include.dum = TRUE, # include time dummies as instrument?
               dum.diff = TRUE, #  include as differences?
               dum.lev = FALSE, # include as levels?
               varname.dum = "week", # variable name of the time dummies
               w.mat = "iid.err", # type of weighting matrix
               std.err = "corrected", # type of std.err.
               estimation = "twostep", # estimation procedure
               #inst.thresh = 10, # maximum number of instruments that will be used
               opt.meth = "none") # Set to none if you want closed form solutions

summary(mod_8) # Results

# Serial test
mtest.fct(mod_8, 2) # method is appropiate of H_0 cannot be rejected

# Create Table of results (FULL)
coefs_mod_8 = round(coef(summary(mod_8)),4) # Estimated coefficients
test_results_mod_8_slope = c(as.vector(round(summary(mod_8)$slopef$statistic, 4)),
                             as.vector(round(summary(mod_8)$slopef$p.value, 4)), rep("",2)) # Chisq statistic of Wald test that all slope coeff. are jointly zero
test_results_mod_8_hansen = c(round(as.vector(summary(mod_8)$hansenj$statistic),4),
                              round(as.vector(summary(mod_8)$hansenj$p.value),4), rep("",2)) # Chisq statistic of Hansen's overidentification test
test_results_mod_8_serial_cor = c(round(as.vector(mtest.fct(mod_8, 2)$statistic), 4),
                                  round(as.vector(mtest.fct(mod_8, 2)$p.value), 4), rep("",2)) # Normal statistic of second order autorcorrelation test
test_stat_header = c("Test statistic", "p-value", "", "")
header_results = c("Coefficent", "Std.Err*", "Value of Z-statistic", "p-value")

results_mod_8 = rbind(coefs_mod_8, test_stat_header, test_results_mod_8_slope,
                      test_results_mod_8_hansen, test_results_mod_8_serial_cor)

names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Cases")
rownames(results_mod_8) = c("lag(% change Deaths, 14)", paste0("lag(",names_coefs,", 14)"),
                            paste("Week",row.names(results_mod_8)[12:42], "Dummy"), "",
                            c("Wald test**", "Hansen's J-Test", "2nd Order Autocor. Test"))
colnames(results_mod_8) = c("Estimate", "Std.Err*", "Z-stat. value", "p-value")

# Create Latex table
table_mod_8 = xtable(results_mod_8,
                     caption = "Results of two-step GMM estimation of policy and information on % change in COVID-19 deaths all countries except: Canada, Poland, Spain, Luxembourg, Estonia, Slovenia, and Australia. *Windmeijer corrected standard errors. ** Wald test that all slope coefficents are jointly zero.",
                     label = "tab_results:deaths_spec_8_full_wo_behavior",
                     align = c("l", "c", "c", "c", "c"))
print.xtable(table_mod_8, type = "latex",
             file = "./Tables/Spec_8_Deaths_Results_full_wo_behavior.tex", floating = F, 
             tabular.environment="longtable",
             include.rownames = T,
             booktabs = TRUE,
             size = "tiny")

################################### Reduced Results tables ####
##### Cases 
# Get coefficent tables and round the values
coef_table_1 = round(coef(summary(mod_1)),4)[1:11,]
coef_table_2 = round(coef(summary(mod_2)),4)[1:11,]
coef_table_3 = round(coef(summary(mod_3)),4)[1:11,]
coef_table_4 = round(coef(summary(mod_4)),4)[1:11,]

# Add asterixes to coefficents
asterix_fun = function(x, y){
  x_vals = character(length(x))
  
  for(i in 1:length(x)){
    if(y[i] < 0.001){
      x_vals[i] = paste0(c(x[i], "***"), collapse = "")
    }else if(y[i] < 0.01){
      x_vals[i] = paste0(c(x[i], "**"), collapse = "")
    }else if(y[i] < 0.05){
      x_vals[i] = paste0(c(x[i], "*"), collapse = "")
    }else{
      x_vals[i] = paste0(c(x[i],""), collapse = "")
    }
  }
  return(x_vals)
}

estimates_1 = asterix_fun(coef_table_1[,1], coef_table_1[, 4])
estimates_2 = asterix_fun(coef_table_2[,1], coef_table_2[, 4])
estimates_3 = asterix_fun(coef_table_3[,1], coef_table_3[, 4])
estimates_4 = asterix_fun(coef_table_4[,1], coef_table_4[, 4])

# Add parenthesis to std.err
std_err_1 = paste0("(", coef_table_1[,2], ")")
std_err_2 = paste0("(", coef_table_2[,2], ")")
std_err_3 = paste0("(", coef_table_3[,2], ")")
std_err_4 = paste0("(", coef_table_4[,2], ")")

# Create columns
column_1 = paste0(estimates_1, " ", std_err_1)
column_2 = paste0(estimates_2, " ", std_err_2)
column_3 = paste0(estimates_3, " ", std_err_3)
column_4 = paste0(estimates_4, " ", std_err_4)

# Create Table
table_results_1 = data.frame(column_1, column_2, column_3, column_4)
colnames(table_results_1) = c("All", "Europe", "Non-Europe", "Reduced")
names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Deaths")
rownames(table_results_1) = c("lag(% change Cases, 7)", paste0("lag(",names_coefs,", 7)"))

# Create latex table
table_results_1_red = xtable(table_results_1, caption = "Reduced results of two-step GMM estimation of policy and information on \\% change in COVID-19 cases. Specifications are: All: All countries in sample; Europe: All European countries in sample; Non-Europe: All non-European countries in sample; Reduced: All countries except Canada, Poland, Spain, Luxembourg, Estonia, Slovenia, and Australia. \\\\ Windmeijer corrected standard errors in parenthesis. \\\\ ***: $p<0.001$; **: $p< 0.01$; *: $p< 0.05$",
                             label = "tab_results:cases_reduced_wo_behavior",
                             align = c("l", "c", "c", "c", "c"))
print.xtable(table_results_1_red, type = "latex",
             file = "./Tables/Cases_Results_reduced_wo_behavior.tex", floating = T,
             include.rownames = T,
             booktabs = TRUE,
             size = "small")

##### Deaths

# Get coefficent tables and round the values
coef_table_5 = round(coef(summary(mod_5)),4)[1:11,]
coef_table_6 = round(coef(summary(mod_6)),4)[1:11,]
coef_table_7 = round(coef(summary(mod_7)),4)[1:11,]
coef_table_8 = round(coef(summary(mod_8)),4)[1:11,]

# Add asterixes to coefficents
asterix_fun = function(x, y){
  x_vals = character(length(x))
  
  for(i in 1:length(x)){
    if(y[i] < 0.001){
      x_vals[i] = paste0(c(x[i], "***"), collapse = "")
    }else if(y[i] < 0.01){
      x_vals[i] = paste0(c(x[i], "**"), collapse = "")
    }else if(y[i] < 0.05){
      x_vals[i] = paste0(c(x[i], "*"), collapse = "")
    }else{
      x_vals[i] = paste0(c(x[i],""), collapse = "")
    }
  }
  return(x_vals)
}

estimates_5 = asterix_fun(coef_table_5[,1], coef_table_5[, 4])
estimates_6 = asterix_fun(coef_table_6[,1], coef_table_6[, 4])
estimates_7 = asterix_fun(coef_table_7[,1], coef_table_7[, 4])
estimates_8 = asterix_fun(coef_table_8[,1], coef_table_8[, 4])

# Add parenthesis to std.err
std_err_5 = paste0("(", coef_table_5[,2], ")")
std_err_6 = paste0("(", coef_table_6[,2], ")")
std_err_7 = paste0("(", coef_table_7[,2], ")")
std_err_8 = paste0("(", coef_table_8[,2], ")")

# Create columns
column_5 = paste0(estimates_5, " ", std_err_5)
column_6 = paste0(estimates_6, " ", std_err_6)
column_7 = paste0(estimates_7, " ", std_err_7)
column_8 = paste0(estimates_8, " ", std_err_8)

# Create Table
table_results_2 = data.frame(column_5, column_6, column_7, column_8)
colnames(table_results_2) = c("All", "Europe", "Non-Europe", "Reduced")
names_coefs = c("Events",
                "Travel", "Distance", "Public", "Lock", "Closure", "Visit", "Mask", "% Change Tests",
                "Total No. Cases")
rownames(table_results_2) = c("lag(% change Deaths, 14)", paste0("lag(",names_coefs,", 14)"))

# Create latex table
table_results_2_red = xtable(table_results_2, caption = "Reduced results of two-step GMM estimation of policy and information on \\% change in COVID-19 deaths Specifications are: All: All countries in sample; Europe: All European countries in sample; Non-Europe: All non-European countries in sample; Reduced: All countries except Canada, Poland, Spain, Luxembourg, Estonia, Slovenia, and Australia. \\\\ Windmeijer corrected standard errors in parenthesis. \\\\ ***: $p<0.001$; **: $p< 0.01$; *: $p< 0.05$",
                             label = "tab_results:deaths_reduced_wo_behavior",
                             align = c("l", "c", "c", "c", "c"))
print.xtable(table_results_2_red, type = "latex",
             file = "./Tables/Deaths_Results_reduced.tex_wo_behavior", floating = T,
             include.rownames = T,
             booktabs = TRUE,
             size = "small")