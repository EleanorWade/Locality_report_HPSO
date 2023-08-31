########HPZONE CASE

# Setup: Read in HPZ data ---------------------------
con.sw <- DBI::dbConnect(odbc::odbc(),
                         Driver = "SQL Server",
                         Server = "SQLCLUSCOLCVD19\\CVD19",
                         Database = "S107_HP_SouthWest_PID",
                         Trusted_Connection = "True")

hpz_sw <- dbGetQuery(con.sw, "'")


##Import case data from HPZone with Locality already attached (Required columns Diagnosis, date entered, infection, confidence )
wd <- setwd("C:\Users\eleanor.wade\Documents\R\Locality report")
data <- "Locality case data"
datafilepath <- file.path(paste0(wd, "/", data, ".csv"))
data <- read.csv(datafilepath, header = TRUE, check.names = FALSE, sep = ",", stringsAsFactors = FALSE)

## HPZone- minimal cleaning required, only remove non confirmed measles and add LA
##If measles keep only confirmed - filter diagnosis = measles confidence =confirmed

data <- data %>%
  Mutate(inclusion_criteria = case_when(diagnosis == “measles” & confidence != “confirmed” ~ “exclude”,
                                        TRUE ~ “include”))  

                                        
##Allocate/specify 4 week period
#Creates week numbers that are used throughout the rest of the script. 

library("ISOweek")
library("tidyverse")



##Allocate/specify previous 4 week period
end_date <- dmy(end_date)
start_date <- end_date(%Y-%m-01)


dates1 <- get_dates(seq.Date(from = start_date, to = end_date, by = 1)) %>% 
  mutate(date = ymd(day)) %>% 
  glimpse()


quarter_list <- dates1 %>% 
  arrange(desc(year.quarter)) %>% 
  distinct(year.quarter) %>% 
  head(12) %>% 
  pull()


dates2 <- dates1 %>% 
  filter(year.quarter %in% quarter_list)

min_date <-  min(dates2$date)

max_date <- end_date

dates <- get_dates(seq.Date(from = min_date, to = max_date, by = 1)) %>% 
  glimpse()

rm(dates1, dates2)


##Count if for each organism for current 4 week period (Measles will need to be confirmed only)


##Count if for each organism for previous 4 week period (Measles will need to be confirmed only)

##produce 

#####HPZone SITUATION
wd <- setwd("C:\Users\eleanor.wade\Documents\R\Locality report")
data <- "Locality situation data"
datafilepath <- file.path(paste0(wd, "/", data, ".csv"))
data <- read.csv(datafilepath, header = TRUE, check.names = FALSE, sep = ",", stringsAsFactors = FALSE)