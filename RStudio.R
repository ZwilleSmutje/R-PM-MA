
library(dplyr)
library("lubridate")
library(ggplot2)
library(caTools)
library(directlabels)
library(scales)
library(tidyr)
library(readr)

###########################Compute Time Limit#####################

setSessionTimeLimit(cpu = Inf, elapsed = Inf)

###########################Device C1#####################################


setwd("~/R")
Luftmessung_c1 <- read.delim("~/R/ConstantDC_C1.txt", header = TRUE, sep = "," )
Zeitachse_c1 <- as.POSIXct(Luftmessung_c1$Time, format = "%Y-%m-%d %H:%M:%S ")
#make each time characteristic an own column
Luftmessung_c1$year <- as.factor(year(Zeitachse_c1))
Luftmessung_c1$month <- as.factor(month(Zeitachse_c1))
Luftmessung_c1$day <- as.factor(day(Zeitachse_c1))
Luftmessung_c1$hour <- as.factor(hour(Zeitachse_c1))
Luftmessung_c1$minute <- as.factor(minute(Zeitachse_c1))
#group by these columns
gruppiert_c1 <- group_by(Luftmessung_c1, year, month, day, hour, minute) 
#clever Dyplr summarises each indidual group
data_c1 <- summarise(gruppiert_c1, Humi_DHT=mean(Humi_top), Temp_DHT=mean(Temp_top), Humi_ENV=mean(Humi_ENV), Temp_ENV=mean(Temp_ENV),  press_ENV=mean(press_ENV),    lux=mean(lux),  uva=mean(uva), uvb=mean(uvb),  PM25=mean(PM25), PM10=mean(PM10))
data_c1$Identifier <- as.character('Instrument 1')
#correcting humidity 

###########################Device C2#####################################

Luftmessung_c2 <- read.delim("~/R/ConstantDC_C1.txt", header = TRUE, sep = "," )
Zeitachse_c2 <- as.POSIXct(Luftmessung_c2$Time, format = "%Y-%m-%d %H:%M:%S ")
#make each time characteristic an own factor for grouping
Luftmessung_c2$year <- as.factor(year(Zeitachse_c2))
Luftmessung_c2$month <- as.factor(month(Zeitachse_c2))
Luftmessung_c2$day <- as.factor(day(Zeitachse_c2))
Luftmessung_c2$hour <- as.factor(hour(Zeitachse_c2))
Luftmessung_c2$minute <- as.factor(minute(Zeitachse_c2))
#group by these columns
gruppiert_c2 <- group_by(Luftmessung_c2, year, month, day, hour, minute) 
#clever Dyplr summarises each indidual group
data_c2 <- summarise(gruppiert_c2, Humi_DHT=mean(Humi_top), Temp_DHT=mean(Temp_top), Humi_ENV=mean(Humi_ENV), Temp_ENV=mean(Temp_ENV),  press_ENV=mean(press_ENV),    lux=mean(lux),  uva=mean(uva), uvb=mean(uvb),  PM25=mean(PM25), PM10=mean(PM10))
data_c2$Identifier <- as.character('Instrument 2')

#clever Dyplr paste the rows under each other
summed_data <- bind_rows(data_c1, data_c2)

#make numbers again, so we can combine them back to a time
summed_data$year_int <- as.integer(as.character(summed_data$year))
summed_data$month_int <- as.integer(as.character(summed_data$month))
summed_data$day_int <- as.integer(as.character(summed_data$day))
summed_data$hour_int <- as.integer(as.character(summed_data$hour))
summed_data$minute_int <- as.integer(as.character(summed_data$minute))
#merge the columns and make a time out of it
summed_data$time <- as.POSIXct(paste0(summed_data$year_int, "-", summed_data$month_int, "-", summed_data$day_int, " ", summed_data$hour_int, ":", summed_data$minute_int), format = "%Y-%m-%d %H:%M")
#coeffi for second axis
coeff <- 100/250

########################correcting humidity##########################
#calibrated data combined with DHT and or ENV


########################humidiy correction################################
k <- 0.61 #KAPPA #ifelse(>35, ifelse(>100, rH99, CALC CORRECTION  ), 1 )
summed_data$Humi_DHT_rm <- runmean(summed_data$Humi_DHT, 60)
summed_data$Humi_ENV_rm <- runmean(summed_data$Humi_ENV, 60)
summed_data$corr_DHT <- ifelse(summed_data$Humi_DHT_rm>35, ifelse(summed_data$Humi_DHT_rm>100, (1+k*(99/(100-99))),(1+k*(summed_data$Humi_DHT_rm/(100-summed_data$Humi_DHT_rm)))),1)
summed_data$corr_ENV <- ifelse(summed_data$Humi_ENV_rm>35, ifelse(summed_data$Humi_ENV_rm>100, (1+k*(99/(100-99))),(1+k*(summed_data$Humi_ENV_rm/(100-summed_data$Humi_ENV_rm)))),1)


################################runmean###############################
summed_data$PM10_rm <- runmean(summed_data$PM10, 60)
summed_data$PM10_rm_corr <- runmean(summed_data$PM10/summed_data$corr_ENV, 60)
summed_data$PM25_rm <- runmean(summed_data$PM25, 60)
summed_data$PM25_rm_corr <- runmean(summed_data$PM25/summed_data$corr_ENV, 60)


################################TIDY DATA##########################
#mit PM10&PM25 #selected_data <- summed_data %>% select(time,PM10,PM10_rm,PM10_rm_corr,PM25,PM25_rm,PM25_rm_corr,Humi_DHT,corr_DHT,Humi_ENV,corr_ENV,Temp_DHT,Temp_ENV,press_ENV,lux,uva,uvb,Identifier)
selected_data <- summed_data %>% select(time,PM10_rm,PM10_rm_corr,PM25_rm,PM25_rm_corr,Humi_DHT,Humi_DHT_rm,corr_DHT,Humi_ENV,Humi_ENV_rm,corr_ENV,Temp_DHT,Temp_ENV,press_ENV,lux,uva,uvb,Identifier)
tidy_data <- selected_data %>% pivot_longer(-c(year,month,day,hour,time,Humi_DHT,Humi_DHT_rm,corr_DHT,Humi_ENV,Humi_ENV_rm,corr_ENV,Temp_DHT,Temp_ENV,press_ENV,lux,uva,uvb,Identifier), names_to = "PM", values_to = "concentration")
tidy_data$time <- as.POSIXct(tidy_data$time)




###############################plot####################################
Luftplot <- tidy_data %>% ggplot(aes(time,concentration,group=PM,color=PM)) 
Luftplot <- Luftplot + facet_grid(Identifier~.)
Luftplot <- Luftplot + geom_line()
Luftplot <- Luftplot + geom_line(y=tidy_data$Humi_ENV_rm/ coeff, group=tidy_data$PM,color='#FF0166')
Luftplot <- Luftplot + scale_y_continuous(sec.axis = sec_axis(~.*coeff, name="Humidity [%]"))
Luftplot <- Luftplot + labs(title="test measurement  ", subtitle = "LONGRUN", y="PM10 [g/m³]") 
Luftplot <- Luftplot + scale_x_datetime(date_breaks = "8 hour", date_labels = "%d.%m %H:%M") 
#Luftplot <- Luftplot + geom_label(label=tidy_data$PM)#Funktioniert nicht
#Luftplot <- Luftplot + annotation_logticks(sides = "l", scaled = FALSE)  #is the data already log-scaled? This should be TRUE (default) when the data is already transformed with log10() or when using scale_y_log10. It should be FALSE when using coord_trans(y = "log10").
#Luftplot <- Luftplot + coord_trans(y='log10')  #LOG10 occurs after statistics
#Luftplot <- Luftplot + geom_dl(aes(label=PM), method= "last.polygons")
Luftplot <- Luftplot + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'right',  legend.box = 'vertical')


plot(Luftplot)
