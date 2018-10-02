#### packages & libraries ####
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(lubridate)

#### reading data set ####
setwd("C:/Users/Lenovo/Desktop/Ubiqum_data/Module_4/task_1")
library(readr)
HPC_II <- read_delim("household_power_consumption.txt", 
                    ";", escape_double = FALSE, trim_ws = TRUE)

#### changing the datatypes ####
HPC_II$Date <- as.Date(HPC_II$Date, "%d/%m/%Y")
HPC_II$Date_Time<-ymd_hms(paste(HPC_II$Date,HPC_II$Time))

HPC_II$Hour<-hour(HPC_II$Time)
HPC_II$month<-month(HPC_II$Date)
HPC_II$day<-day(HPC_II$Date)

####Cahnging KW/H  ####
HPC_II<-HPC_II %>% mutate(Global_Consumption=((HPC_II$Global_active_power*1000)/60))
HPC_II<-HPC_II %>% mutate(Global_Consumption_reactive=((HPC_II$Global_reactive_power*1000)/60))
HPC_II<-HPC_II %>% mutate(Global_Consumption_kwh=(HPC_II$Global_Consumption/1000))
HPC_II<-HPC_II %>% mutate(Global_reactive_kwh=(HPC_II$Global_Consumption_reactive/1000))
HPC_II<-HPC_II %>% mutate(Submetter1_kwh=(HPC_II$Sub_metering_1/1000))
HPC_II<-HPC_II %>% mutate(Submetter2_kwh=(HPC_II$Sub_metering_2/1000))
HPC_II<-HPC_II %>% mutate(Submetter3_kwh=(HPC_II$Sub_metering_3/1000))
#### NA's ####
#preproccesing the data
NAs<-HPC_II %>% filter(is.na(Global_active_power)) %>% mutate(n=(count(Date))) 
HPC_III<-left_join(HPC_II,NAs, by = "Date")
#NAs replaced by last value known
#HPC_3$B<-na.locf(HPC_3$Global_reactive_kwh, na.rm = FALSE, maxgap = 1440)

HPC_III<-mutate(HPC_III, Global_Consumption_kwh_2= ifelse(n >= 1000, 0, 
                                                      replace_na_with_last(Global_Consumption_kwh) ))
HPC_III<-mutate(HPC_III, Global_reactive_kwh_2= ifelse(n >= 1000, 0, 
                                                   replace_na_with_last(Global_reactive_kwh) ))

HPC_III<-mutate(HPC_III, Global_intensity_2= ifelse(n >= 1000, 0, 
                                                rollForward(Global_intensity) ))
HPC_III<-mutate(HPC_III, Voltage_2= ifelse(n >= 1000, 0, 
                                       rollForward(Voltage) ))
HPC_III<-mutate(HPC_III, Submetter1_kwh_2= ifelse(n >= 1000, 0, 
                                              rollForward(Submetter1_kwh) ))
HPC_III<-mutate(HPC_III, Submetter2_kwh_2= ifelse(n >= 1000, 0, 
                                              rollForward(Submetter2_kwh) ))
HPC_III<-mutate(HPC_III, Submetter3_kwh_2= ifelse(n >= 1000, 0, 
                                              rollForward(Submetter3_kwh) ))


####Functions for NA's  ####
replace_na_with_last<-function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}


rollForward <- function(x){
  curr <- 0
  for (i in 1:length(x)){
    if (is.na(x[i])){
      x[i] <- curr
    }
    else{
      curr <- x[i]
    }
  }
  return(x)
}

####Daylight saving  ####
#startdate=as_datetime('2007-03-25 02:00:00')
#enddate=as_datetime('2007-10-29 01:59:00')
HPC_II<-mutate(HPC_II, DateTime_2= 
         ifelse(Date_Time >= as_datetime('2007-03-25 02:00:00') & 
        Date_Time <= as_datetime('2007-10-28 01:59:00'),Date_Time+ hours(1),
        ifelse(Date_Time >= as_datetime('2008-03-30 02:00:00') & 
     Date_Time <= as_datetime('2008-10-26 01:59:00'),Date_Time+ hours(1),
     ifelse (Date_Time >= as_datetime('2009-03-29 02:00:00') & 
       Date_Time <= as_datetime('2009-10-29 01:59:00'),Date_Time+ hours(1),
       ifelse(Date_Time >= as_datetime('2010-03-28 02:00:00') & 
                Date_Time <= as_datetime('2010-10-31 01:59:00'),Date_Time+ hours(1),
                           Date_Time )))))


#testing daylight saving #
HPC_II$DateTime_2<- as_datetime(HPC_II$DateTime_2)
HPC_II$Hour_2<-hour(HPC_II$DateTime_2)

test_ds<-HPC_II%>% select(Hour,Hour_2,Date) %>%
  group_by(Hour,Hour_2,Date)%>%summarise()
write.csv(test_ds,"test_ds.csv") 

#### Adding month's name in order ####
HPC_II<- transform(HPC_II, MonthAbb = month.abb[month])
HPC_II$MonthAbb <-factor(HPC_II$MonthAbb, 
                      levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
#### Adding day's names ####
HPC_II$W_Days <- wday(HPC_II$Date_Time)
HPC_II$W_Days_2 <- wday(HPC_II$Date_Time,label=TRUE)
####Creating a Column with the Names of the Day####
HPC_II$WeekNames <-""
HPC_II$WeekNames [HPC_II$W_Days == "1"] <- "Sun"
HPC_II$WeekNames [HPC_II$W_Days == "2"] <- "Mon"
HPC_II$WeekNames [HPC_II$W_Days == "3"] <- "Tue"
HPC_II$WeekNames [HPC_II$W_Days == "4"] <- "Wed"
HPC_II$WeekNames [HPC_II$W_Days == "5"] <- "Thu"
HPC_II$WeekNames [HPC_II$W_Days == "6"] <- "Fri"
HPC_II$WeekNames [HPC_II$W_Days == "7"] <- "Sat"

HPC_II$WeekNames <-factor(HPC_II$WeekNames, 
          levels = c("Sun","Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ))

summary(HPC_II)
#### checking the outliers ####
install.packages("tsoutliers")
library(tsoutliers)
install.packages("expsmooth")
library(expsmooth)
install.packages("fma")
library(fma)

outlier.HPC_II <- tso(HPC_II,start=c(2006,1),maxit.iloop=10)
outlier.HPC_II
plot(outlier.chicken)