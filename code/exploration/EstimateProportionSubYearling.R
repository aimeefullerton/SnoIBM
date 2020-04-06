
library(dplyr)
library(lubridate)

# load and organize data
chinook.data <- read.csv("SnoqualmieTrap_ChinFKL_12-16.csv", header = T,
                         stringsAsFactors = F) %>%
    select(Date, Fork.Length.mm.) %>% 
    rename(FL = Fork.Length.mm.) %>% 
    mutate(Date = mdy(Date))

# calculate the number of subyearlings and yearlings from a given year

subyearlings.2014 <- chinook.data %>%
    filter(year(Date) == "2014", FL < 65) %>%
    nrow()
yearlings.2014 <- chinook.data %>%
    filter(year(Date) == "2015", Date < "2015-5-01", FL >= 65) %>%
    nrow()

subyearlings.2015 <- chinook.data %>%
    filter(year(Date) == "2015", FL < 65) %>%
    nrow()
yearlings.2015 <- chinook.data %>%
    filter(year(Date) == "2016", Date < "2016-05-01", FL >= 65) %>%
    nrow()

# put number of subyearlings and yearlings in a table and calculate
# corresponding proportions

proportions <- data.frame(Subyearlings = c(subyearlings.2014, 
                                           subyearlings.2015),
                          Yearlings = c(yearlings.2014, 
                                        yearlings.2015)) %>% 
    mutate(PropSubyearling = Subyearlings / (Subyearlings + Yearlings),
           PropYearling = Yearlings / (Subyearlings + Yearlings))

row.names(proportions) <- c("2014", "2015")

# account for catch per unit effort based on Tulalip Tribes 2018 Report

cpue.2014 <- 0.25
cpue.2015 <- 0.08
cpue.2016 <- 0.04

cpue.subyearlings.2014 <- subyearlings.2014 / cpue.2014
cpue.subyearlings.2015 <- subyearlings.2015 / cpue.2015
cpue.yearlings.2014 <- yearlings.2014 / cpue.2015
cpue.yearlings.2015 <- yearlings.2015 / cpue.2016

cpue.proportions <- data.frame(Subyearlings = c(cpue.subyearlings.2014, 
                                                cpue.subyearlings.2015),
                               Yearlings = c(cpue.yearlings.2014, 
                                             cpue.yearlings.2015)) %>% 
    mutate(PropSubyearling = Subyearlings / (Subyearlings + Yearlings),
           PropYearling = Yearlings / (Subyearlings + Yearlings))

row.names(cpue.proportions) <- c("2014", "2015")

# compare original calculations and cpue-adjusted calculations

print(proportions)
print(cpue.proportions)
