if (!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse,
               DBI,
               janitor
)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(gtsummary)

setwd("U:/Core Services/Field Epidemiology Team Bristol/Surveillance/Non-disease specific reports/PHEC Quarterly Reports/J to S 2022/Raw data")
Mumps<- read.csv("U:/Core Services/Field Epidemiology Team Bristol/Surveillance/Non-disease specific reports/PHEC Quarterly Reports/J to S 2022/Raw data/Mumps Raw.csv", header=TRUE)

###Make date entered a date column
clean_names(Mumps)
glimpse(Mumps)

Mumps$Date.entered

class(Mumps$Date.entered)
Mumps$Date.entered<- as.Date(Mumps$Date.entered, format = "%d/%m/%Y")

##check that the class has been changed correctly
Mumps$Date.entered

##Allocate the dates for the two 4 week periods
#Mumps <- Mumps %>%
#  mutate(week=cut(Date.entered, breaks = "week", labels = FALSE))
#Mumps$week
Mumps %>%
  mutate(Date_week = isoweek(Date.entered))%>%
  glimpse()

#Mumps %>%
 # mutate(four_week = cut.Date(Mumps$Date.entered, breaks = "4 weeks", labels = FALSE))%>%
 # glimpse

 # class(Mumps$Date_week)

##Clean_mumps <- Mumps %>%
 ## mutate(Date_week = floor_date(Date.entered, unit = "week", week_start =1)) %>%
  #glimpse()
#Clean_mumps$Date_week

## Table these for Mumps?

Trialdate_tab <- Mumps%>%
  select(Date_week) %>%
  tbl_summary()

trialdate_tab2 <- Mumps%>%
  select(Date.entered) %>%
  tbl_summary(statistic = all_continuous())

##Mumps_count<-Mumps%>%
#  filter(Date.entered >= "25-07-2022" & Date.entered <= "30-08-2022")%>%
  #group_by(Diagnosis)%>%
 # count()%>%
 # filter(Diagnosis == "Mumps")

#Mumps_count
##Mumps$date_range <- cut(as.Date(Mumps$Date.entered),breaks="Day")
##glimpse(Mumps)

#mumps_counts <- Mumps%>%
#  filter(Date.entered >= as.Date("01-06-2022") & Date.entered <=as.Date("01-07-2022")) %>%
#  group_by(Date.entered) %>%
#  summarise(count=n())
#glimpse(mumps_counts)

##clean_names(Mumps)

##glimpse(Mumps)

##subset_df<- subset(Mumps, >="10/07/2022" & $ date.entered) <="10/12/2022"

##table(subset_mumps$Diagnosis)