############ First Look at Data and Creation of Dataset ############################

#######Load Packages####
library(magrittr)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(RColorBrewer)


#####WHO Data ####
columnnames = c("date", "day", "month", "year", "cases", "deaths", "countries",
                "geold", "country_code", "population", "continent", "cum_rel")

data = read.csv("Data/COVID-19_cases_worldwide20201101.csv", sep = ",",
                header = TRUE,
                  colClasses = c(NA, "integer", "integer", "integer",
                                 "integer", "integer", "factor", "factor", "factor",
                                 "integer", "factor", "numeric"),
                col.names = columnnames) %>% as_tibble()

data$date = data$date %>% as.Date(format = "%d/%m/%Y")


country = "France" # Change when you want
lock_start = as.Date("2020-03-17") # Change when you want

lock_1weeks = lock_start + 7
lock_2weeks = lock_start + 14
lock_3weeks = lock_start + 21
lock_4weeks = lock_start + 28
rate_start = data %>% filter(countries == country, date == lock_start) %>%
  select(cum_rel) %>% pull()
rate_1weeks = data %>% filter(countries == country, date == lock_1weeks) %>%
  select(cum_rel) %>% pull()
rate_2weeks = data %>% filter(countries == country, date == lock_2weeks) %>%
  select(cum_rel) %>% pull()
rate_3weeks = data %>% filter(countries == country, date == lock_3weeks) %>%
  select(cum_rel) %>% pull()
rate_4weeks = data %>% filter(countries == country, date == lock_4weeks) %>%
  select(cum_rel) %>% pull()

lockdown_data = data.frame(
  Start_Date = c(lock_start, lock_1weeks, lock_2weeks, lock_3weeks),
  End_Date = c(lock_1weeks, lock_2weeks, lock_3weeks, lock_4weeks),
  Start_Rate = c(rate_start, rate_1weeks, rate_2weeks, rate_3weeks),
  End_Rate = c(rate_1weeks, rate_2weeks, rate_3weeks, rate_4weeks),
  Identifier = c("1 Week", "2 Weeks", "3 Weeks", "4 Weeks"),
  Identifier_2 = factor(x = c(as.character(lock_start), "1st Week", "2nd Week", "3rd Week"),
                        levels = c(as.character(lock_start), "1st Week", "2nd Week", "3rd Week")))

data %>% filter(countries == country,
                date %in% seq.Date(lock_start-14, lock_4weeks+14, by = "day")) %>%
ggplot(aes(x = date, y = cum_rel))+
  geom_point(alpha = 0.6)+
  geom_vline(data = lockdown_data,
             mapping = aes(xintercept = Start_Date,
            linetype = Identifier_2))+
  scale_linetype("Weeks")+
  geom_segment(data = lockdown_data, mapping = aes(x = Start_Date,
                   y = Start_Rate,
                   xend = End_Date,
                   yend = End_Rate,
               color = Identifier), size = 1.5)+
  scale_color_brewer("Trend", palette = "Blues")+
  ggtitle(paste(country, "14 Day Cumulative Infection Rates"))+
  xlab("Dates")+ylab("Cumulative Rate")
ggsave(paste0("Trends/",country,"4weeks.png"), height = 8, width = 13)
