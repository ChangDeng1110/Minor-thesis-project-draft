# This file use to combine 2 different datasets "AC_1.4_1.21" and "AC_G1_G04".
# "AC_1.4_1.21" ~ get roof temperature
# "AC_G1_G04" ~ get room temperature, temperature high set point, temperature low set point, G1 status, AC status

library("ggplot2")
library(lubridate)
library(tidyr)
library(stringr)
ac_1.4 <- read.csv("data/AC_1.4_1.21.csv",header = T,stringsAsFactors = F)
list_roof <- ac_1.4[,"Bld81_RoofOATemp"][0:21072]
ac_G04 <- read.csv("data/AC_G1_G04.csv",header = T,stringsAsFactors = F)
room_temp <- ac_G04[c("Date.Time","MeetingRmG04Temp","AC_G1TempSP.Hi","AC_G1TempSP.Lo")]
room_temp <- room_temp[0:21072,]
room_temp["outside"] = list_roof
room_temp$Date.Time <- as.POSIXct(room_temp[,1], format = "%d/%m/%Y %I:%M:%S %p")
room_temp <- room_temp[-1,]
room_temp <- room_temp[0:21024,]
room_temp$MeetingRmG04Temp = as.numeric(as.character(room_temp$MeetingRmG04Temp))
room_temp$AC_G1TempSP.Hi = as.numeric(as.character(room_temp$AC_G1TempSP.Hi))
room_temp$AC_G1TempSP.Lo = as.numeric(as.character(room_temp$AC_G1TempSP.Lo))
room_temp$outside = as.numeric(as.character(room_temp$outside))
room_temp["day"] <- rep(seq(219),each = 96) #~ get different day (219 total)

per_day_temp <- list()
for (i in 1:219){
  a <- subset(room_temp, day == i)
  per_day_temp[[i]] <- a
}

plot_temp <- list()
for (i in 1:219){
  x <- ggplot(per_day_temp[[i]],aes(x=Date.Time))+
    geom_point(aes(y = outside, colour = "outside"))+
    #geom_point(aes(y = AC_G1TempSP.Hi, colour = "set_point"))+
    #geom_point(aes(y = AC_G1TempSP.Lo, colour = "set_point"))+
    geom_point(aes(y = MeetingRmG04Temp, colour = "MeetingRmG04Temp"))
    plot_temp[[i]] <- x
}


#attach(x)
pdf("temprature.pdf",family="GB1")
for(i in 1:219){
  plot(plot_temp[[i]])
}
dev.off()
#detach(x)

