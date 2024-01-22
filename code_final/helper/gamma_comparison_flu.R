rm(list = ls())

# run first code.R
library(rstudioapi)
library(dplyr)
library(rlang)
dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))

country_list <- c("BE")
gamma_seq <- c(5, 10)
N = 5000
gamma <- round(1/gamma_seq, 2)
nboot = 3000

if(!file.exists("../rds/POLYMOD/attack_rates/attack_rate_physical_temporal_00367_BE_gamma_005.rds")){
  for(i in 1:length(country_list)){
    for(k in 1:length(gamma_seq)){
    gamma_num <- gamma_seq[k]
    attack_rate_baseline <- list()
    attack_rate_temporal <- list()
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    
    temp_foi <- read.csv(sprintf("../../data/vsc_polymod_input/input_forVSC_physical_%s_gamma_%04d.csv", country_input, gamma_num))
    foi <- unique(temp_foi$foi)*10000
    
    population_data <- readRDS(sprintf("../rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
    for(j in foi){
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){
        temp_env <- env()
        print(sprintf("doing country %s for foi %s and %05d boot", country_list[i], j, b))
        load(sprintf("../rds/POLYMOD/output_physical_%s_gamma/full_epidemic_simulation_CM_%04d_%03d_%05d.RData", 
                     country_input, as.integer(j), as.numeric(gamma_num), as.numeric(b)), temp_env)
        
        temp_sum_baseline <- sum(temp_env$output.simulate.baseline.vsc$incidenceMat)
        temp_sum_temporal <- sum(temp_env$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_baseline > 0.1*N){
          infected_baseline <- data.frame(population_data, incidence = rowSums(temp_env$output.simulate.baseline.vsc$incidenceMat))
          temp_ar_baseline <- aggregate(infected_baseline$incidence,
                                        by = list(infected_baseline$part_age_cat),
                                        mean)
          colnames(temp_ar_baseline) <- c("part_age_cat", "attack_rates")
          temp_ar_baseline <- data.frame(temp_ar_baseline, boot = b)
          attack_rate_baseline[[count_base]] <-  temp_ar_baseline
          count_base = count_base + 1
        }
        
        if(temp_sum_temporal > 0.1*N){
          infected_temporal <- data.frame(population_data, incidence = rowSums(temp_env$output.simulate.vsc$incidenceMat))
          temp_ar_temporal <- aggregate(infected_temporal$incidence,
                                        by = list(infected_temporal$part_age_cat),
                                        mean)
          colnames(temp_ar_temporal) <- c("part_age_cat", "attack_rates")
          temp_ar_temporal <- data.frame(temp_ar_temporal, boot = b)
          attack_rate_temporal[[count_temp]] <-  temp_ar_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      attack_rate_baseline_dataframe <- do.call("rbind", attack_rate_baseline)
      attack_rate_temporal_dataframe <- do.call("rbind", attack_rate_temporal)
      
      saveRDS(attack_rate_baseline_dataframe, 
              sprintf("../rds/POLYMOD/attack_rates/attack_rate_physical_baseline_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
      saveRDS(attack_rate_temporal_dataframe, 
              sprintf("../rds/POLYMOD/attack_rates/attack_rate_physical_temporal_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
      }
    }
  }
}

foi_gamma <- data.frame(country = rep("BE", 6),
                        recovery_rate = c(5, 5, 7, 7, 10, 10),
                        scenario = rep(c("Flu", "COVID-19"), 3),
                        foi = c(0.0367, 0.0931, 0.0262, 0.0665,0.0183,0.0466))

#----- calculate attack rate

calculate_ar_Gammacomparison_with_ci <- function(ar_baseline_flu, 
                                                 ar_temporal_flu){
  
  age_cat <- c("Children", "Teens", "Adult", "Elderly")
  nboot = 2000
  output <- data.frame()
  count = 1
    for(a in age_cat){
      temp_baseline_boot <- ar_baseline_flu[ar_baseline_flu$part_age_cat %in% a, ]
      temp_temporal_boot <- ar_temporal_flu[ar_temporal_flu$part_age_cat %in% a, ]
      
      length_baseline_boot <- nrow(temp_baseline_boot)
      length_temporal_boot <- nrow(temp_temporal_boot)
      ar_ratio <- c()
      for(b in 1:nboot){
        boot_baseline <- mean(sample(temp_baseline_boot$attack_rates, length_baseline_boot, replace = T))
        boot_temporal <- mean(sample(temp_temporal_boot$attack_rates, length_temporal_boot, replace = T))
        
        ar_ratio[b] <- boot_temporal/boot_baseline
      }
      output[count, "age_cat"] <- a
      output[count, "lower"] <- round(quantile(ar_ratio, probs = 0.025), 3)
      output[count, "attack_rates"] <- round(mean(ar_ratio), 3)
      output[count, "upper"] <- round(quantile(ar_ratio, probs = 0.975), 3)
      count = count + 1
      print(count)
      
    }
  return(output)
}

# Set the path to your directory
directory_path <- "../rds/POLYMOD/attack_rates/"
list_output <- list()

for(i in 1:nrow(foi_gamma)){
  print(paste0("i = ", i))
  foi_input <- as.integer(foi_gamma$foi[i]*10000)

  data_temporal <- readRDS(paste0(directory_path, list.files(directory_path, pattern = sprintf("attack_rate_physical_temporal_.*%04d.*_BE.*", foi_input))))
  data_baseline <- readRDS(paste0(directory_path, list.files(directory_path, pattern = sprintf("attack_rate_physical_baseline_.*%04d.*_BE.*", foi_input))))
  
  list_output[[i]] <- data.frame(calculate_ar_Gammacomparison_with_ci(data_baseline,
                                       data_temporal), 
                                 foi = foi_gamma$foi[i])
}

