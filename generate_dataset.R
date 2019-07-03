library(ggplot2)

generate_data <- list()
number <- 1
for(i in 1:95){
  x <- detect_heating_start(heating_period[[i]])
  if (length(x) > 0){
    heating <- heating_stop(heating_period[[i]],x)
    if(length(heating) > 0){
      generate_data[[i]] <- heating
    }
  }
}

final_dataset <- generate_data[[1]]
for (i in 2:95){
  final_dataset <- rbind(final_dataset,generate_data[[i]])
}
row.names(final_dataset) <- NULL
write.csv(final_dataset, file = "heating_rate.csv")


ggplot(final_dataset, aes(x = final_dataset$start_temp, y = final_dataset$indoor_rate)) + geom_point()
