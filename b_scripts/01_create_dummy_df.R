#########################################
#
# Author: Braeden Klaver
# Date: 2023-06-27
# Version: 4.1.1
#
#######################################



set.seed(666)


date_randomizer <- function(n, min, max){
  
  dates <- seq.Date(from = as.Date(min), to = as.Date(max), by = "day")
  
  date_probs <- abs(rnorm(length(dates), mean = 50, sd = 50))
  
  date_random <- sample(dates, n, replace = T, prob = date_probs)
  
  return(date_random)
  
}




### create data for the cases

n <- 10000

cases <- data.frame(
  id = 1:n,
  outcome = 1,
  age = abs(round(rnorm(n, mean = 47, sd = 20), 0)),
  sex = rbinom(n, size = 1, prob = 0.59611),
  surv_ha = (rbinom(n, size = 4, prob = 0.45))+1,
  surv_date = date_randomizer(n = n, min = "2015-01-01", max = Sys.Date())
)


### CREATE DATA FOR CONTROLS 

n_2 <- 9990000

controls <- data.frame(
  id = 1:n_2,
  outcome = 0,
  age = abs(round(rnorm(n_2, mean = 40, sd = 20), 0)),
  sex = rbinom(n_2, size = 1, prob = 0.50611),
  surv_ha = (rbinom(n_2, size = 4, prob = 0.45542))+1,
  surv_date = date_randomizer(n = n_2, min = "2015-01-01", max = Sys.Date())
)


### HA CROSSWALK 

ha_xwalk <- tibble(surv_ha = c(1:5),
                   surv_name = c("IHA", "FHA", "VCHA", "VIHA", "NHA"))



### CREATE DX VARIABLES 


new_vars <- function(n_cases, n_controls, case_df, control_df, how_many) {
  
  # n_cases = 965
  # n_controls = 10000
  # case_df = cases
  # control_df = controls
  # how_many = 5
  
  
  for (x in 1:how_many) {
    var_prob_cases <- runif(1, min = 0.001, max = 0.35)
    
    if ((x %% 2) == 0) {
      add_prob_controls <- runif(1, min = 0.005, max = 0.01)
      var_prob_controls <- var_prob_cases + add_prob_controls
      df_temp_controls <- data.frame(var = rbinom(n_controls, size = 1, prob = var_prob_controls))
    }
    
    if ((x %% 2) == 1) {
      add_prob_controls <- runif(1, min = 0.005, max = 0.0075)
      var_prob_controls <- var_prob_cases - add_prob_controls
      if (var_prob_controls < 0) {
        var_prob_controls <- 0
      }
      df_temp_controls <- data.frame(var = rbinom(n_controls, size = 1, prob = var_prob_controls))
    }
    
    
    var_name <- paste0("dx_", x)
    
    df_temp_cases <- data.frame(var = rbinom(n_cases, size = 1, prob = var_prob_cases))
    
    names(df_temp_cases)[names(df_temp_cases) == "var"] <- var_name
    
    names(df_temp_controls)[names(df_temp_controls) == "var"] <- var_name
    
    case_df <- cbind(case_df, df_temp_cases)
    
    control_df <- cbind(control_df, df_temp_controls)
  }
  
  combined_df <- rbind(case_df, control_df)
  
  return(combined_df)
}


df <- new_vars(n, n_2, cases, controls, 25)


rm(cases, controls)

fwrite(df, here("data", "dummy_df.csv"))

