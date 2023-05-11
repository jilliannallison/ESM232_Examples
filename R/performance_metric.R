flow_5year = function(modeled, observed, wy, end_year){
  start_year = end_year - 4

  df = as.data.frame(cbind(modeled, observed, wy))%>%
    filter(wy %in% c(start_year:end_year)) 
  
  # then do sum model and sum actual and calculate error 
  obs_flow = sum(df$observed)
  mod_flow = sum(df$modeled)
  
  error = 1 - (abs((mod_flow - obs_flow))/obs_flow) 
  
  correlation = cor(df$modeled, df$observed)
  
  combined = 0.5*error + 0.5*correlation 
  
  # paste("Modeled flow = ", mod_flow, "Observed flow = ", obs_flow)
  return(list(correlation = correlation, error = error, combined_metric = combined))
  
}


### GOAL = AGGREGATE OVER X YEAR PERIOD 