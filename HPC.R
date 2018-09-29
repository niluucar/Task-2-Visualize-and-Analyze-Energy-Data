library(lubridate)
setwd("C:/Users/Lenovo/Desktop/Ubiqum_data/Module_4/task_1")
library(readr)
HPC_new <- read_delim("household_power_consumption.txt", 
                  ";")
View(HPC)

HPC_new$Datetime<- dm(paste(HPC_new$Date,HPC_new$Time))

Date <- seq(as.Date("2010/1/1"), as.Date("2014/1/1"), "week")
Y <- rnorm(n=length(Date), mean=100, sd=1)


ggplot(data=HPC_My2006, aes(HPC_My2006$MonthAbb,group=1))+
  geom_line(aes(y = HPC_My2006$Submetter1_kwh, color="Kitchen")) + 
  geom_line(aes(y = HPC_My2006$Submetter2_kwh, color="Laundry Room")) + 
  geom_line(aes(y = HPC_My2006$Submetter3_kwh, color="Heater")) + 
  #geom_line(aes(y = HPC_my$Global_Consumption_kwh))+#, color="Global Consumption_KWh"))+
  #geom_line(aes(y = HPC_my$Global_reactive_kwh))+#, color=="Global_Cons_Reactive_KWh"))+
  xlab("Year")+
  ylab("KWh")+
  #ggtitle("Global Active Power by Time")+
   scale_x_discrete(labels=  month.abb) + 
     #scale_x_date(labels = date_format("%b"))+
     #theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
     theme_bw()+
     scale_y_continuous(labels = function(x) format(x, scientific =FALSE))+
    scale_colour_manual(name='', 
                         values=c('Kitchen'="#CC6666",
                'Laundry Room'="blue", 
                     'Heater'="darkgreen"), 
             guide='legend') + facet_wrap( ~ Year )
