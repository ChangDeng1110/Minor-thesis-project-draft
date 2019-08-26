list_final_heating <- list_final
list_final_heating

list_simulation_heating <- list()

for (i in 2:117){
  simulation_heating_table <- as.data.frame(matrix(NA, nrow = 10, ncol = 5))
  simulation_heating_table[c(1:lengths(list_final_heating[[i]])[1]),5] <- list_final_heating[[i]][,2]
  names(simulation_heating_table)[1] <- "start_temp"
  names(simulation_heating_table)[2] <- "simu_ave_temp"
  names(simulation_heating_table)[3] <- "simu_high_temp"
  names(simulation_heating_table)[4] <- "simu_low_temp"
  names(simulation_heating_table)[5] <- "outside"
  
  simulation_heating_table[1,1] <- list_final_heating[[i]][1,1]
  simulation_heating_table[1,2] <- list_final_heating[[i]][1,1]
  simulation_heating_table[1,3] <- list_final_heating[[i]][1,1]
  simulation_heating_table[1,4] <- list_final_heating[[i]][1,1]
  
  for(j in 1:lengths(list_final_heating[[i]])[1]){
    simulation_heating_table[j,1] <- list_final_heating[[i]][j,1]
    use_data <- data.frame("start_temp" = simulation_heating_table[j,2], 
                           "outside_temp" = simulation_heating_table[j,5])
    heat <- boot(data=train_rf_heating, statistic=heating_model, 
                 R=5, formula=rate ~ start_temp + outside_temp)
    heat_ave <- mean(heat$t)
    heat_low <- quantile(heat$t,probs=c(0.025,0.975))[1]
    heat_high <- quantile(heat$t,probs=c(0.025,0.975))[2]
    
    simulation_heating_table[j+1,2] <- simulation_heating_table[j,2] + heat_ave*15
    simulation_heating_table[j+1,3] <- simulation_heating_table[j,2] + heat_high*15
    simulation_heating_table[j+1,4] <- simulation_heating_table[j,2] + heat_low*15
  }
  simulation_heating_table <- na.omit(simulation_heating_table)
  list_simulation_heating[[i]] <- simulation_heating_table
  print(i)
}

#backup <- list_simulation_heating

for (i in 2:117){
  list_simulation_heating[[i]]['time'] <- seq(0,(lengths(list_simulation_heating[[i]])[1]-1)*15, by = 15)
}


plot_heating_simulation <- list()
for (i in 2:length(list_simulation_heating)){
  print(i)
  x <- ggplot(list_simulation_heating[[i]],aes(x=time))+
    geom_line(aes(y = start_temp,colour = "start_temp"),size=0.5)+
    geom_point(aes(y = start_temp, colour = "start_temp"),size=0.5)+
    geom_line(aes(y = simu_ave_temp,colour = "simu_ave_temp"),size=0.5)+
    geom_point(aes(y = simu_ave_temp, colour = "simu_ave_temp"),size=0.5)+
    geom_line(aes(y = simu_high_temp,colour = "simu_high_temp"),size=0.5,linetype="dotted")+
    geom_point(aes(y = simu_high_temp, colour = "simu_high_temp"),size=0.5) + 
    geom_line(aes(y = simu_low_temp,colour = "simu_low_temp"),size=0.5,linetype="dotted")+
    geom_point(aes(y = simu_low_temp, colour = "simu_low_temp"),size=0.5)
  
  plot_heating_simulation[[i]] <- x
}

pdf("heating_simulation.pdf",family="GB1")
for(i in 2:length(plot_heating_simulation)[1]){
  plot(plot_heating_simulation[[i]])
}
dev.off()

RMSE_heating <- c()

for (k in 2:117){
  rmse_value <- RMSE(list_simulation_heating[[k]][2:lengths(list_simulation_heating[[k]])[1],'start_temp'],
                     list_simulation_heating[[k]][2:lengths(list_simulation_heating[[k]])[1],'simu_ave_temp'])
  RMSE_heating[k-1] <- rmse_value
}

boxplot(RMSE_heating, ylab="RMSE (temperature)", xlab = "Heating period")




