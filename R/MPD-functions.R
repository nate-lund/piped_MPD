#================================ Split data ================================

#' [Test code]
# path = "_inputs/_measurements/test.xlsx"

split_data = function(path){
  
  # Read in data
  data <- read_excel("_inputs/_measurements/test.xlsx") %>% 
    # Create a unqique ID for each infiltration measurement
    mutate(
      index = factor((paste(date, site, test_location, sep = "_")))
    ) %>% 
    as.data.frame()
  
  # Pull unique indexes
  indexs = unique(data$index)

  data_list = list(NA)
  
  # Set output directory
  out_dir <- "_split-measurements"
  
  # Split data into unique dataframes for each index
  for(i in indexs){
    datai = data %>% 
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
  
  
  
  # Fit a second order polynomial to the water drop rate
  fit <- lm(data = df,
            height_cm ~ hours +I(hours^2)
  )
  df$predic = predict(fit)
  
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
  
  
  
}

