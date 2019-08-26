list_final_cooling

list_simulation_cooling <- list()

for (i in 2:80){
  simulation_cooling_table <- as.data.frame(matrix(NA, nrow = 50, ncol = 5))
  simulation_cooling_table[c(1:lengths(list_final_cooling[[i]])[1]),5] <- list_final_cooling[[i]][,2]
  names(simulation_cooling_table)[1] <- "start_temp"
  names(simulation_cooling_table)[2] <- "simu_ave_temp"
  names(simulation_cooling_table)[3] <- "simu_high_temp"
  names(simulation_cooling_table)[4] <- "simu_low_temp"
  names(simulation_cooling_table)[5] <- "outside"
  
  simulation_cooling_table[1,1] <- list_final_cooling[[i]][1,1]
  simulation_cooling_table[1,2] <- list_final_cooling[[i]][1,1]
  simulation_cooling_table[1,3] <- list_final_cooling[[i]][1,1]
  simulation_cooling_table[1,4] <- list_final_cooling[[i]][1,1]
  
  for(j in 1:lengths(list_final_cooling[[i]])[1]){
    simulation_cooling_table[j,1] <- list_final_cooling[[i]][j,1]
    use_data_cool <- data.frame("start_temp" = simulation_cooling_table[j,2], 
                           "outside_temp" = simulation_cooling_table[j,5])
    cool <- boot(data=train_rf_cooling, statistic=cooling_model, 
                 R=10, formula=rate~outside_temp + start_temp)
    cool_ave <- mean(cool$t)
    cool_low <- quantile(cool$t,probs=c(0.025,0.975))[1]
    cool_high <- quantile(cool$t,probs=c(0.025,0.975))[2]
    
    simulation_cooling_table[j+1,2] <- simulation_cooling_table[j,2] + cool_ave*15
    simulation_cooling_table[j+1,3] <- simulation_cooling_table[j,2] + cool_high*15
    simulation_cooling_table[j+1,4] <- simulation_cooling_table[j,2] + cool_low*15
  }
  simulation_cooling_table <- na.omit(simulation_cooling_table)
  list_simulation_cooling[[i]] <- simulation_cooling_table
  print(i)
}


for (i in 2:80){
  list_simulation_cooling[[i]]['time'] <- seq(0,(lengths(list_simulation_cooling[[i]])[1]-1)*15, by = 15)
}


plot_cooling_simulation <- list()
for (i in 2:length(list_simulation_cooling)){
  print(i)
  x <- ggplot(list_simulation_cooling[[i]],aes(x=time))+
    geom_line(aes(y = start_temp,colour = "start_temp"),size=0.5)+
    geom_point(aes(y = start_temp, colour = "start_temp"),size=0.5)+
    geom_line(aes(y = simu_ave_temp,colour = "simu_ave_temp"),size=0.5)+
    geom_point(aes(y = simu_ave_temp, colour = "simu_ave_temp"),size=0.5)+
    geom_line(aes(y = simu_high_temp,colour = "simu_high_temp"),size=0.5,linetype="dotted")+
    geom_point(aes(y = simu_high_temp, colour = "simu_high_temp"),size=0.5) + 
    geom_line(aes(y = simu_low_temp,colour = "simu_low_temp"),size=0.5,linetype="dotted")+
    geom_point(aes(y = simu_low_temp, colour = "simu_low_temp"),size=0.5)
  
  plot_cooling_simulation[[i]] <- x
}

pdf("cooling_simulation.pdf",family="GB1")
for(i in 2:length(plot_cooling_simulation)[1]){
  plot(plot_cooling_simulation[[i]])
}
dev.off()

RMSE_cooling <- c()

for (k in 2:80){
  rmse_value <- RMSE(list_simulation_cooling[[k]][2:lengths(list_simulation_cooling[[k]])[1],'start_temp'],
                     list_simulation_cooling[[k]][2:lengths(list_simulation_cooling[[k]])[1],'simu_ave_temp'])
  RMSE_cooling[k-1] <- rmse_value
}

boxplot(RMSE_cooling, ylab="RMSE (temperature)", xlab = "Cooling period")





