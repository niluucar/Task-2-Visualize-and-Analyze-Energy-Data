install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(lubridate)
setwd("C:/Users/Lenovo/Desktop/Ubiqum_data/Module_4/task_1")
library(readr)
HPC <- read_delim("household_power_consumption.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)
View(HPC)
#####1####
HPC <-cbind(HPC,paste(HPC$Date,HPC$Time), stringsAsFactors=FALSE)
colnames(HPC)[10] <-"DateTime"

HPC <- HPC[,c(ncol(HPC), 1:(ncol(HPC)-1))]
head(HPC)
colnames(HPC)[10] <-"DateTime"
HPC$DateTime <- strptime(HPC$DateTime, "%d/%m/%Y %H:%M:%S")
HPC$Date <- as.Date(HPC$Date, "%d/%m/%Y")
HPC$Month_Yr <- format(as.Date(HPC$Date), "%Y-%m")
HPC$Time_h <- strftime(HPC$Time, format="%H:%00")

str(HPC)
summary(HPC)


HPC_c<-HPC %>% select(Global_reactive_kwh,B,DateTime)
####5####
HPC_2<- na.omit(HPC)
HPC_3<-HPC
HPC<-HPC_3
####plots####
hist(HPC$Global_active_power,
     col="red", 
     main="Global Active Power", 
     xlab="Global Active Power (kilowatts)")


ggplot(data=HPC_my, aes(x=month, y=Global_Consumption_kwh, 
                        group=Year,colour=Year)) +
  geom_line()+theme_bw()+
  geom_point()+facet_wrap(facets = Year ~ .)#, margins = FALSE)


plot(HPC_my$Global_Consumption_kwh,
     col="red", 
     main="Global Active Power", 
     xlab="Global Active Power (kilowatts)")


plot(HPC$DateTime, HPC$Voltage, 
     type="line", 
     xlab="", 
     ylab="Global reActive Power (kilowatts)")

plot(HPC$DateTime, HPC$Sub_metering_2, 
     type="line", 
     xlab="", 
     ylab="Global reActive Power (kilowatts)")

plot(HPC$DateTime, HPC$Sub_metering_1,
     "n",
     xlab = "",
     ylab = "Energy sub metering")

points(HPC$DateTime, HPC$Sub_metering_1, type = "line")

points(HPC$DateTime, HPC$Sub_metering_2, type = "line", col = "red")

points(HPC$DateTime, HPC$Sub_metering_3, type = "line", col = "blue")

legend("topright", 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "red", "blue"),
       lty = c(1, 1, 1))

####2####
### group by filter###

HPC$Year<-year(HPC$Date)
HPC$months<-months(HPC$Date)
HPC$month<-month(HPC$Date)
HPC$Hour<-hour(HPC$Time)


####6####
HPC_my<-HPC_2 %>% select(Month_Yr,Year,month,Global_Consumption,Global_Consumption_reactive,
                         Sub_metering_1,Sub_metering_2,Sub_metering_3,Submetter3_kwh,
                         Submetter2_kwh,Submetter1_kwh,Global_reactive_kwh,Global_Consumption_kwh) %>%
  group_by(Month_Yr,Year,month)%>%  #summarise(Global_Consumption=mean(Global_Consumption),
                                     #Global_Consumption_reactive=sum(Global_Consumption_reactive))
  summarise_at(vars(Global_Consumption,Global_Consumption_reactive,
                    Sub_metering_1,Sub_metering_2,Sub_metering_3,Submetter3_kwh,
                    Submetter2_kwh,Submetter1_kwh,Global_reactive_kwh,Global_Consumption_kwh),
               funs(sum))
####ggggg####
HPC_hour<-HPC_2 %>% select(Year,month,Hour,Time_h,Global_Consumption,Global_Consumption_reactive,
                         Sub_metering_1,Sub_metering_2,Sub_metering_3,Sub_metering_1,Sub_metering_2,Sub_metering_3,Submetter3_kwh,
                         Submetter2_kwh,Submetter1_kwh,Global_reactive_kwh,Global_Consumption_kwh) %>%
  filter(month==6 & Year!=2006) %>%
  group_by(Year,month,Hour,Time_h)%>%  #summarise(Global_Consumption=mean(Global_Consumption),
  #Global_Consumption_reactive=sum(Global_Consumption_reactive))
  summarise_at(vars(Global_Consumption,Global_Consumption_reactive,
                    Sub_metering_1,Sub_metering_2,Sub_metering_3,Sub_metering_1,Sub_metering_2,Sub_metering_3,Submetter3_kwh,
                    Submetter2_kwh,Submetter1_kwh,Global_reactive_kwh,Global_Consumption_kwh),
               funs(sum))
####7####
install.packages("anytime")
library(anytime)
HPC_my$Month_Yr<-anydate(HPC_my$Month_Yr)
####not run####
write.csv(hour_1,"hour.csv")
####NA's####
NAs_2<-HPC_C %>% group_by(Date) %>% summarise(D=sum(new_gap))
  #filter(is.na(Global_active_power)) %>% count(Date) %>%
  #group_by(Date)
NAs_3<-HPC_C2 %>% group_by(Date) %>% summarise(D=sum(new_gap))
#filter(is.na(Global_active_power)) %>% count(Date) %>%
#group_by(Date)

hpc_new<-left_join(HPC,NAs, by = "Date")
hpc_new_2<-left_join(NAs,NAs_2, by = "Date")
hpc_new_3<-left_join(hpc_new_2,NAs_3, by = "Date")
NAs<-NAs %>% group_by(Date) %>% summarise()
 

HPC$DateTime <- as.POSIXct(HPC$DateTime, 
                             format = "%Y-%m-%d %H:%M%:%S", tz = "Europe/Paris")
HPC_2 <-select(HPC,Global_active_power,Global_reactive_power,Date)
HPC_3 <-select(HPC,-datetime4)

deneme<-HPC %>% filter(Date > "2006-12-20" & Date <"2006-12-23")

####Calendar Heat####
install.packages("chron")
library("chron")
source("calendarHeat.R")
calendarHeat2<-calendarHeat
calendarHeat(HPC$Date, HPC$Global_reactive_power, varname="Global_reActive_Power")

summary(HPC)

####3####
###Prepoccess###
HPC<-HPC %>% mutate(Global_Consumption=((HPC$Global_active_power*1000)/60))
HPC<-HPC %>% mutate(Global_Consumption_reactive=((HPC$Global_reactive_power*1000)/60))
HPC<-HPC %>% mutate(Global_Consumption_kwh=(HPC$Global_Consumption/1000))
HPC<-HPC %>% mutate(Global_reactive_kwh=(HPC$Global_Consumption_reactive/1000))
HPC<-HPC %>% mutate(Submetter1_kwh=(HPC$Sub_metering_1/1000))
HPC<-HPC %>% mutate(Submetter2_kwh=(HPC$Sub_metering_2/1000))
HPC<-HPC %>% mutate(Submetter3_kwh=(HPC$Sub_metering_3/1000))

####4####

HPC$DateTime <- as.POSIXct(HPC$DateTime, 
                 format = "%Y-%m-%d %H:%M%:%S", tz = "Europe/Paris")
#not run#
HPC_MY_2<-HPC_my$Month_Yr <- as.POSIXct(HPC_my$Month_Yr, 
                           format = "%Y-%m-%d", tz = "Europe/Paris")

####8####
HPC_my<- transform(HPC_my, MonthAbb = month.abb[month])
HPC<- transform(HPC, MonthAbb = month.abb[month])
####10####
HPC_My2006<- HPC_my %>% filter(Year!=2006)
HPC_MyJune<- HPC_my %>% filter(Year!=2006 & month==6)
HPC_MyAug<- HPC_my %>% filter(Year==2008 & month==8)


HPC_My2006$MonthAbb <-factor(HPC_My2006$MonthAbb, 
                       levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
HPC$MonthAbb <-factor(HPC$MonthAbb, 
                             levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
####9####
ggplot(data=HPC_My2006, aes(HPC_My2006$MonthAbb,group=1))+
  #geom_line(aes(y = HPC_My2006$Submetter1_kwh, color="Kitchen")) + 
  #geom_line(aes(y = HPC_My2006$Submetter2_kwh, color="Laundry Room")) + 
  #geom_line(aes(y = HPC_My2006$Submetter3_kwh, color="Heater")) + 
  geom_line(aes(y = HPC_My2006$Global_Consumption_kwh, color="Active_Power"))+
  geom_line(aes(y = HPC_My2006$Global_reactive_kwh, color="Reactive_Power"))+
  xlab("Year")+
  ylab("KWh")+
  ggtitle("Energy Consumption by Month")+
  #scale_x_discrete(labels=  month.abb) + 
  #scale_x_date(labels = date_format("%b"))+
  #theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  #theme_bw()+
  scale_y_continuous(labels = function(x) format(x, scientific =FALSE))+
 #scale_colour_manual(name='', 
                    #  values=c('Active_Power'="#CC6666"), # Kitchen',
               #'Reactive_Power'="blue"), #Laundry Room'="blue", 
                    #'Heater'="darkgreen"), 
          #guide='legend') +
 facet_wrap( ~ Year )
                   #facet_grid(facets = Year ~ ., margins = FALSE) 

####11####
ggplot(data=HPC_hour, aes(HPC_hour$Time_h,group=1))+
  #geom_line(aes(y = HPC_hour$Submetter1_kwh, color="Kitchen")) + 
  #geom_line(aes(y = HPC_hour$Submetter2_kwh, color="Laundry Room")) + 
  #geom_line(aes(y = HPC_hour$Submetter3_kwh, color="Heater")) + 
  #geom_line(aes(y = HPC_hour$Global_Consumption_kwh, color="Active_Power"))+
  geom_line(aes(y = HPC_hour$Global_reactive_kwh, color="Reactive_Power"))+
  xlab("Year")+
  ylab("KWh")+
  ggtitle("Measurements of Submeters in June by Year")+
  #scale_x_discrete(labels=Hour) + 
  #scale_x_date(labels = date_format("%b"))+
  #theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  theme_bw()+theme(axis.text.x=element_text(angle=90, hjust=1))+
  scale_y_continuous(labels = function(x) format(x, scientific =FALSE))+
 facet_wrap( ~ Year )
#facet_grid(facets = Year ~ ., margins = FALSE)
Sys.timezone()



HPC$W_Days <- wday(HPC$DateTime)
HPC$W_Days_2 <- wday(HPC$DateTime,label=TRUE)
####Creating a Column with the Names of the Day####
HPC$WeekNames <-""
HPC$WeekNames [HPC$W_Days == "1"] <- "Sun"
HPC$WeekNames [HPC$W_Days == "2"] <- "Mon"
HPC$WeekNames [HPC$W_Days == "3"] <- "Tue"
HPC$WeekNames [HPC$W_Days == "4"] <- "Wed"
HPC$WeekNames [HPC$W_Days == "5"] <- "Thu"
HPC$WeekNames [HPC$W_Days == "6"] <- "Fri"
HPC$WeekNames [HPC$W_Days == "7"] <- "Sat"

#####NA's replacing####
install.packages("zoo")
library(zoo)
#NAs replaced by last value known
HPC_3$B<-na.locf(HPC_3$Global_reactive_kwh, na.rm = FALSE, maxgap = 1440)

hpc_new$n[is.na(hpc_new$n)] <- 0

HPC_C<-mutate(HPC_C, new_gap= ifelse(n >= 1000, 0, 
                                     replace_na_with_last(Global_Consumption_kwh) ))


HPC_C2<-mutate(HPC_C, new_gap= ifelse(n >= 1000, 0, 
                                     rollForward(Global_Consumption_kwh) ))

HPC_C<-mutate(HPC_C, D = lag(Global_Consumption_kwh,na.pad=TRUE))

####Functions for NA's####
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

is.na(deneme[5])

#### daylight saving####