#================================ Split data ================================

#' [Test code]
# data = tar_read(raw_data)

split_data = function(data){
  
  # Read in data
  data2 <- data %>% 
    # Create a unqique ID for each infiltration measurement
    mutate(
      index = factor((paste(date, site, test_location, sep = "_")))
    ) %>% 
    as.data.frame()
  
  # Pull unique indexes
  indexs = unique(data2$index)

  # Set output directory
  out_dir <- "_split-measurements"
  
  # Split data into unique dataframes for each index
  for(i in indexs){
    datai = data2 %>% 
      filter(index == i)
    
    # Pull file name from index column
    name = as.character(unique(datai$index))
    
    write.csv(datai,
              file.path(out_dir, paste0(name, ".csv")),
              row.names = FALSE)
    
  }
  
  # List files that were just split
  split_list = list.files(out_dir, full.names = TRUE)
  
  return(split_list)
  
}


#================================ Compute K_sat ================================

#' [Test code]
# path = "_split-measurements/2026-11-12_ARB-JW_1.csv"

compute_Ksat = function(path){
  
  # Read in data
  df = read.csv(path) %>%
    # Convert seconds to hours
    mutate(hours = seconds/3600)
  
  
  
  # Define variables from data
  # Fixed parameters
  r = 5 # Radius of infiltrometer
  L = 5 # Depth infiltrometer is inserted into ground
  beta = 1.23 # a parameter that Philip says “takes account of the hydraulic inefficiency of the flow paths,” (default β = 1.23)
  eta = 0.43 # Soil porosity, assumed??
  
  # Parameters pulled from data
  H_0 = max(df$height) # Initial water depth
  moist_initial = unique(df$initial_moisture_pc / 100) # Initial moisture content (unitless proportion)
  moist_final = unique(df$final_moisture_pc / 100) # Final moisture content (unitless proportion)
  
  # Computed parameters
  delta_moist = moist_final - moist_initial
  d_theta = delta_moist * eta # Change in porosity
  
  
  #' [WIP]
  # We can likely put an if statement here, to ensure we only fit a polynomail
  # to datasets with enough points. Something like:
  
  # ifelse(min(df$height_cm) - max(df$height_cm) > 5, continue, stop)
  
  # In case of fail, we may fit some kind of polynomial and extrapolate out to get a
  # "minumum detectable value"
  
  # We may also look at the R2 of the polynomial fit. If the R2 is poor, may not
  # be applicable to use our method. May also have it drop the last mm.
  
  
  # Fit a second order polynomial to the water drop rate
  fit <- lm(data = df,
            height_cm ~ hours +I(hours^2)
  )
  df$predic = predict(fit) # Generate predictions for plotting 
  R_squared = summary(fit)$r.squared # Pull R2
  
  # Pull the coefficients from the fit
  a2 = tidy(fit)[[3, "estimate"]]
  a1 = tidy(fit)[[2, "estimate"]]
  a0 = tidy(fit)[[1, "estimate"]]
  
  
  # Define a function to compute d_R
  # R being the distance from the source of the sphere to the wetting front
  d_R_fn <- function(R_initial) {
    R_calc <- r * sqrt((H_0 - (H_0 / 2)) / (d_theta * ((2 * R_initial / 3) + L)))
    d_R = abs(R_initial - R_calc)
    return(d_R)
  }
  
  # Run an optimizer to solve for R_final
  opt <- optim(par = 23, # Initial value, something reasonable
               fn = d_R_fn,
               method = "Brent", # Method
               lower = 1e-6, # Lower bound, small value prevents negative solutions
               upper = 100, # Upper bound, just in case
  )
  R_final <- opt$par
  
  # opt$convergence # Check convergence, should be 0
  
  # Compute f(R)
  f_R = log(( r /(2 * R_final))*((R_final + L)/((r / 2)+ L)))
  
  # Compute Ksat (cm/hr)
  K_sat = -(2 * a2 / a1)*(L - ((beta *(r^2)*f_R)/(2 * L)))
  
  # Compute C (cm) wetting front capillary suction head, a negative value
  C = (a1 / K_sat)*((L -((beta * (r^2) * f_R)/(2 * L)))) + a0 + L
  
  
  # Grab file name
  file_name = tools::file_path_sans_ext(basename(path))
  
  
  # Export file name, data with predictions, Ksat, and C in a list
  list_out = list(file_name, df, K_sat, C, R_squared)
  
  return(list_out)
  
}


#================================ Plotting ================================

#' [Test code]
# input = tar_read(Ksat_list_18cccad9d41adcce)

plotting = function(input){
  
  # Plot
  plot = ggplot(data = input[[2]]) +
    geom_point(mapping = aes(x = hours, y = height_cm)) +
    geom_line(mapping = aes(x = hours, y = predic)) +
    
    labs(
      title = input[[1]],
      x = "Time (hrs)",
      y = "Height (cm)",
      subtitle = paste0("K_sat: ", round(input[[3]], 2),
                        "cm/hr | C: ", round(input[[4]], 2),
                        "cm | R^2: ", round(input[[5]], 4))
    )
  
  out_path = paste0("_plot-outputs/", input[[1]], ".png")
  ggsave(out_path, plot, width = 4, height = 4)
  
  return(out_path)
  
}


#================================ Plotting ================================





