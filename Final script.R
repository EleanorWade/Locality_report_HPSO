###Setup

if (!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse,
               DBI,
               janitor
)


library("EpiFunc")
library(pivottabler)
library(dplyr)
library(lubridate)


###Import the data using a SHeD query, the dates pulled are within the query and should be updated each time this is run. 

con.sw <- DBI::dbConnect(odbc::odbc(),
                         Driver = "SQL Server",
                         Server = "SQLCLUSCOLCVD19\\CVD19",
                         Database = "S107_HP_SouthWest_PID",
                         Trusted_Connection = "True")

case_data <- dbGetQuery(con.sw, paste("SELECT [Case_identifier],[Diagnosis],[Confidence],[Postcode],[Local_authority],[Date_closed],[date_entered],[pct],[Infection]
FROM [S107_HP_SouthWest_PID].[dbo].[vHPZ_Cases_PID]
WHERE Diagnosis in ('measles','E.coli infection, VTEC O157','Hepatitis A, unspecified','Hepatitis B (with D), acute','Legionnaires Disease','Mumps','Pertussis','Scarlet Fever','E.coli infection, VTEC','Acute Hepatitis A','Hepatitis B (with D), chronic','Meningococcal infection, unspecified','Whooping cough','Hepatitis B, acute','Hepatitis B, chronic','Hepatitis B, carrier','Hepatitis B, unspecified','Meningococcal infection, other')
and Confidence in ('confirmed','probable')
and Date_entered between '11 jun 2023' and '06 Aug 2023 23:59:59.999'
or Infection in ('iGAS (Invasive group a streptococcal) infection','Meningococcal infection')
and Confidence in ('confirmed','probable')
and Date_entered between '11 jun 2023' and '06 Aug 2023 23:59:59.999'"))

##Import population data    ###use data lake in the future
con.sw <- DBI::dbConnect(odbc::odbc(),
                         Driver = "SQL Server",
                         Server = "SQLCLUSCOLCVD19\\CVD19",
                         Database = "S107_HP_SouthWest_PID",
                         Trusted_Connection = "True")

population <- dbGetQuery(con.sw, paste("SELECT pop.[Period]
      ,pop.[GeoName]
      ,pop.[SexDesc]
      ,SUM(pop.[Population]) as Population
  FROM [Populations].[dbo].[vRes_UTLA21_SingleYear] pop
  LEFT JOIN [LookupsShared].[dbo].[vLKP_UTLA21] utla on pop.[GeoName] = utla.[UTLA19NM]
  WHERE utla.RGN09NM = 'South West'
  AND SexDesc = 'Persons'
  and Period = '2020'
  GROUP BY [Period], [GeoName], SexDesc
  ORDER BY Period"))

####Clean data variable names
case_data<- clean_names(case_data)
glimpse(case_data)


##remove mumps and measles if the confidence is not confirmed
##Change mumps and measles to NA if probable
case_data$diagnosis <- ifelse(case_data$diagnosis == "Mumps" & case_data$confidence == "Probable", "NA", case_data$diagnosis)    
case_data$diagnosis[case_data$diagnosis == "NA"] <- NA

case_data %>% filter(diagnosis=="Mumps") %>% tabyl(confidence)

##
case_data$diagnosis <- ifelse(case_data$diagnosis == "Measles" & case_data$confidence == "Probable", "NA", case_data$diagnosis) 

case_data %>% filter(diagnosis=="Measles") %>% tabyl(confidence)

#merge IGAS into one diagnosis based on infection.
case_data$diagnosis <- ifelse(case_data$infection == "iGAS (Invasive group a streptococcal) infection", "IGAS (Invasive group a streptococcal) infection", case_data$diagnosis) 
#Merge meningo into one diagnosis based on infection 
case_data$diagnosis <- ifelse(case_data$infection == "Meningococcal infection", "Meningococcal infection", case_data$diagnosis) 
#merge hep B multiple diagnosis into one diagnosis 
case_data$diagnosis <- ifelse(case_data$diagnosis == "Hepatitis B, acute", "Hepatitis B", case_data$diagnosis) 
case_data$diagnosis <- ifelse(case_data$diagnosis == "Hepatitis B, chronic", "Hepatitis B", case_data$diagnosis) 
case_data$diagnosis <- ifelse(case_data$diagnosis == "Hepatitis B, unspecified", "Hepatitis B", case_data$diagnosis) 

##Merge e.coli into one diagnosis based on diagnosis.
case_data$diagnosis <- ifelse(case_data$diagnosis == "E.coli infection, VTEC", "E.coli infection VTEC", case_data$diagnosis) 
case_data$diagnosis <- ifelse(case_data$diagnosis == "E.coli infection, VTEC O157", "E.coli infection VTEC", case_data$diagnosis) 

##remove NA diagnosis
case_data <- drop_na(case_data, diagnosis)



#make date entered into a date variable
class(case_data$date_entered)
case_data$date_entered<- as.Date(case_data$date_entered, format = "%d/%m/%Y")
class(case_data$date_entered)

##Clean and add UTLA
case_data <- case_data%>%
  rename('LTLA' = 'local_authority') %>%
  mutate_at(vars(LTLA, pct), ~tolower(.))

## Replace LA with PCT if the LA field is blank 

case_data$LTLA <- ifelse(case_data$LTLA == "null", case_data$pct,case_data$LTLA)
case_data$LTLA <- ifelse(case_data$LTLA == "", case_data$pct,case_data$LTLA)

## Assign UTLA using LTLA field 

case_data <- case_data %>%
  mutate(UTLA = case_when(LTLA == "bath and north east somerset" ~ "Bath and North East Somerset",
                          LTLA == "bournemouth"|LTLA == "christchurch district"|LTLA == "poole"|LTLA == "bournemouth, christchurch and poole" ~ "Bournemouth, Christchurch and Poole",
                          LTLA == "city of bristol"|LTLA == "bristol, city of" | LTLA == "bristol" ~ "Bristol",
                          LTLA == "caradon district"| LTLA == "carrick district"| LTLA == "cornwall"| LTLA == "kerrier district"|LTLA == "penwith district"|
                            LTLA == "north cornwall district"|LTLA == "restormel district"|LTLA == "isles of scilly"|LTLA == "cornwall and isles of scilly" ~ "Cornwall and Isles of Scilly",
                          LTLA == "east devon district"|LTLA == "exeter district"|LTLA == "mid devon district"|LTLA == "south hams district"|LTLA == "north devon district"|
                            LTLA == "teignbridge district"|LTLA == "torridge district"|LTLA == "west devon district"|LTLA == "devon" ~ "Devon",
                          LTLA == "east dorset district"|LTLA == "north dorset district"|LTLA == "west dorset district"|LTLA == "purbeck district"|
                            LTLA == "weymouth and portland district"|LTLA == "dorset" ~ "Dorset",
                          LTLA == "cheltenham district"|LTLA == "cotswold district"|LTLA == "forest of dean district"|LTLA == "gloucester district"|
                            LTLA == "stroud district"|LTLA == "tewkesbury district"|LTLA == "gloucestershire" ~ "Gloucestershire",
                          LTLA == "north somerset" ~ "North Somerset",
                          LTLA == "plymouth"|LTLA == "city of plymouth" ~ "Plymouth",
                          LTLA == "mendip district"|LTLA == "sedgemoor district"|LTLA == "south somerset district"|LTLA == "somerset west"|
                            LTLA == "taunton deane district"|LTLA == "somerset west and taunton deane"|LTLA == "west somerset district"|LTLA == "somerset" ~ "Somerset",
                          LTLA == "south gloucestershire" ~ "South Gloucestershire",
                          LTLA == "swindon" ~ "Swindon",
                          LTLA == "torbay" ~ "Torbay",
                          LTLA == "kennet district"| LTLA == "north wiltshire district"|LTLA == "west wiltshire district"|LTLA == "salisbury district"|LTLA == "wiltshire" ~ "Wiltshire"))
###Quick pivot table to ensure all UTLA are changed
qpvt(case_data, "UTLA",, "n()")


###Assign a flag variable depending on which 4 week period they are in, please keep in mind this is backwards, 
#so if it not not within these dates, previous- do not input the wrong dates! 

## TOP LINE DATES SHOULD BE THE CURRENT 4 WEEK PERIOD, SECOND LINE SHOULD BE PREVIOUS!
case_data$date_range <- ifelse(case_data$date_entered <=("2023-07-09") | case_data$date_entered >= "2023-08-06", "Previous 4 week count",
ifelse(case_data$date_entered <=("2023-06-16") & case_data$date_entered >= "2023-07-08", case_data$range,"Current 4 week count"))

qhpvt(case_data, "diagnosis","date_range", "n()")

pt <- PivotTable$new()
pt$addData(case_data)
pt$addRowDataGroups("diagnosis",)
pt$addColumnDataGroups("date_range", totalCaption = "Total 8 week count")
pt$defineCalculation(calculationName="TotalCount",caption="Count", 
                     summariseExpression="n()")
pt$renderPivot()

###can't get this filter to work###pt<- PivotFilter$new(pt, variableName = "UTLA", type = "ALL", values = "Bath and North East Somerset")


                        
                                          
