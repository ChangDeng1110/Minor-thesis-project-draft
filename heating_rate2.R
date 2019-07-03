heating_period <- list()
index <- 1
for (i in 125:219){
  heating_period[[index]] <- per_day_temp[[i]]
  index <-  index + 1
}

detect_heating_start <- function(input_data){
  min_temp <- 0
  min_position <- 0
  finish_position <- 0
  heating_rate <- 0
  heating_rate_outside <- 0
  times = 0
  start = FALSE
  start_index <- 1
  start_list <- list()
  for(i in (((lengths(input_data)-1)[1]-25):1)){
    if (input_data[i,]$MeetingRmG04Temp > input_data[i+1,]$MeetingRmG04Temp){
      start = TRUE
      times = times + 1
    }else{
      start = FALSE
    }
    
    if(start == TRUE & times == 1){
      min_position <- input_data[i+1,]$Date.Time
      start_list[start_index] <- as.character(min_position)
      start_index = start_index + 1
    }
    
    if (start == FALSE){
      times = 0
    }
  }
  
  return (start_list)
}

#x <- detect_heating_start(heating_period[[21]])


heating_stop <- function(input_data,start_point){
  start_time <- c()
  finish_time <- c()
  start_temp <- c()
  finish_temp <- c()
  indoor_rate <- c()
  outside_temp_diff <- c()
  outside_temp <-  c()
  for (j in (1:length(start_point))){
    for(i in (((lengths(input_data)-1)[1]):1)){
      stop_position <- NA
      time_name <- as.character(input_data[i,]$Date.Time)
      #print(time_name)
      #print(start_point[[j]])
      if (time_name == start_point[[j]]){
        #print(j)
        #print(i)
        position <- NA
        for (k in i:2){
          if (input_data[k,]$MeetingRmG04Temp < input_data[k-1,]$MeetingRmG04Temp){
            stop_position <- input_data[k-1,]$Date.Time
            position <- k-1
          }else{
            break
          }
        }
        time_find <- as.numeric(difftime(input_data[position,]$Date.Time,input_data[i,]$Date.Time,units="mins"))
        if (time_find > 15){
          #print(input_data[i,]$Date.Time)
          #print(stop_position)
          start_time[j] <- as.character(input_data[i,]$Date.Time)
          finish_time[j] <- as.character(stop_position)
          start_temp[j] <- input_data[i,]$MeetingRmG04Temp
          finish_temp[j] <- input_data[position,]$MeetingRmG04Temp
          indoor_rate[j] <- (input_data[position,]$MeetingRmG04Temp - input_data[i,]$MeetingRmG04Temp)/
            as.numeric(difftime(input_data[position,]$Date.Time,input_data[i,]$Date.Time,units="mins"))
          outside_temp_diff[j] <- input_data[i,]$MeetingRmG04Temp - input_data[i,]$outside
          outside_temp[j] <- input_data[i,]$outside
          
        }
      }
    }

  }
  
  output_frame <- data.frame("start_time" = start_time,
                             "finish_time" = finish_time,
                             "start_temp" = start_temp,
                             "finish_temp" = finish_temp,
                             "indoor_rate" = indoor_rate,
                             "outside_temp_diff" = outside_temp_diff,
                             "outside_temp" = outside_temp)
  output_frame <- na.omit(output_frame)
  
  return (output_frame)
}


#x <- detect_heating_start(heating_period[[77]])
#heating_stop(heating_period[[77]],x)


pdf("heating2.pdf",family="GB1")
plot_heating <- list()
for(i in 1:95){
  x <- detect_heating_start(heating_period[[i]])
  dataset1 <- heating_period[[i]]
  dataset1["sign"] <- rep(1, times=96)
  if (length(x) > 0){
    heating_point <- heating_stop(heating_period[[i]],x)
    #print(heating_point)
    if(length(heating_point) > 0){
      print(heating_point)
      number <- 2
      for (j in 1:lengths(heating_point)[1]){
        for (m in 1:95){
          if (as.character(dataset1[m,"Date.Time"]) == as.character(heating_point[j,"start_time"]) | 
              as.character(dataset1[m,"Date.Time"]) == as.character(heating_point[j,"finish_time"])){
            dataset1[m,"sign"] <- number
          }
        }
        number = number + 1
      }
      
      plot_plot <- ggplot(dataset1,aes(x=Date.Time))+
        geom_point(aes(y = outside, colour = "outside"))+
        geom_point(aes(y = MeetingRmG04Temp, colour = "MeetingRmG04Temp"))+
        geom_line(data=dataset1[(dataset1$sign!=1)&(dataset1$sign!=2)
                                &(dataset1$sign!=3)&(dataset1$sign!=4)&(dataset1$sign!=5), ],aes(y = MeetingRmG04Temp))+
        geom_line(data=dataset1[(dataset1$sign!=1)&(dataset1$sign!=2)
                                &(dataset1$sign!=3)&(dataset1$sign!=4)&(dataset1$sign!=6), ],aes(y = MeetingRmG04Temp))+
        geom_line(data=dataset1[(dataset1$sign!=1)&(dataset1$sign!=2)
                                &(dataset1$sign!=3)&(dataset1$sign!=5)&(dataset1$sign!=6), ],aes(y = MeetingRmG04Temp))+
        geom_line(data=dataset1[(dataset1$sign!=1)&(dataset1$sign!=2)
                                &(dataset1$sign!=4)&(dataset1$sign!=5)&(dataset1$sign!=6), ],aes(y = MeetingRmG04Temp))+
        geom_line(data=dataset1[(dataset1$sign!=1)&(dataset1$sign!=3)
                                &(dataset1$sign!=4)&(dataset1$sign!=5)&(dataset1$sign!=6), ],aes(y = MeetingRmG04Temp))
      
      plot(plot_plot)
    }
  }
  #print(heating_point)
}
dev.off()

