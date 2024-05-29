rm(list = ls())

# run first code.R
library(rstudioapi)
library(dplyr)
library(rlang)
dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))

calculate_q_cm <- function(R0_input, initial_top, initial_bottom, c.crit = 0.001){
  
  print(paste0("calculating q..."))
  qran <- (initial_top + initial_bottom)/2
  
  R0_temp <- max(eigen((S %*% B) * qran/gamma)$values)

  while (abs((R0_input - R0_temp)) > c.crit) {
    if (R0_temp > R0_input) {
      initial_top <- qran
    } else {
      initial_bottom <- qran
    }
    qran <- (initial_top + initial_bottom)/2
    R0_temp <- max(Re(eigen((S %*% B) * qran/gamma)$values))
  }
  return(round(qran, 4))
}

calculate_q_naive_cm <- function(R0_input, rec_rate = gamma, cm_daily = cm$daily, cm_others = cm$all - cm$daily,
                                 initial_top, initial_bottom, c.crit = 0.001){
  print(paste0("calculating q..."))
  qran <- (initial_top + initial_bottom)/2
  
  r0_agestr <- function(foi = 0.03, 
                        gamma = rec_rate,
                        cm_daily = cm_daily,
                        cm_others = cm_others){
    
    # r0_others = foi * cm_others/gamma
    r0_others = foi * cm_others/(1/gamma)
    r0_daily = cm_daily * (1-(((1/gamma)*(1-foi))/(1-(1-(1/gamma))*(1-foi))))
    
    r0 = (r0_others + r0_daily)
    
    return(max(Re(eigen(r0)$values)))
  }
  
  R0_temp <- r0_agestr(foi = qran, gamma = rec_rate, cm_daily = cm_daily, cm_others = cm_others)
  
  while (abs((R0_input - R0_temp)) > c.crit) {
    if (R0_temp > R0_input) {
      initial_top <- qran
    } else {
      initial_bottom <- qran
    }
    qran <- (initial_top + initial_bottom)/2
    R0_temp <- r0_agestr(foi = qran, gamma = rec_rate, cm_daily = cm_daily, cm_others = cm_others)
  }
  return(qran)
}

country_list <- c("BE")
gamma_seq <- c(1, 2, 5)
N = 5000
gamma <- round(1/gamma_seq, 2)
nboot = 3000
R0 <- c(1.3 , 3.3)

temp_env_baseline <- env()
temp_env_frequency <- env()

if(!file.exists("../rds/POLYMOD/attack_rates/samefoi/attack_rate_physical_temporal_00001_BE_gamma_001.rds")){
  for(i in 1:length(country_list)){
    for(k in 1:length(gamma_seq)){
    gamma_num <- gamma_seq[k]
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    
    population_data <- readRDS(sprintf("../rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      attack_rate_baseline <- list()
      attack_rate_temporal <- list()
      
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){
        temp_env_baseline <- environment()
        temp_env_frequency <- environment()
        print(sprintf("doing country %s for foi %s, gamma %s and %05d boot", i, j, k, b))
        load(sprintf("../rds/POLYMOD/output_physical_%s_gamma_%03d/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                     country_input, gamma_num, j, b), temp_env_baseline)
        load(sprintf("../rds/POLYMOD/output_physical_%s_gamma_%03d/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                     country_input, gamma_num, j, b), temp_env_frequency)
        
        temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
        temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_baseline > 0.1*N){
          infected_baseline <- data.frame(population_data, incidence = rowSums(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat))
          temp_ar_baseline <- aggregate(infected_baseline$incidence,
                                        by = list(infected_baseline$part_age_cat),
                                        mean)
          colnames(temp_ar_baseline) <- c("part_age_cat", "attack_rates")
          temp_ar_baseline <- data.frame(temp_ar_baseline, boot = b)
          attack_rate_baseline[[count_base]] <-  temp_ar_baseline
          count_base = count_base + 1
        }
        
        if(temp_sum_temporal > 0.1*N){
          infected_temporal <- data.frame(population_data, incidence = rowSums(temp_env_frequency$output.simulate.vsc$incidenceMat))
          temp_ar_temporal <- aggregate(infected_temporal$incidence,
                                        by = list(infected_temporal$part_age_cat),
                                        mean)
          colnames(temp_ar_temporal) <- c("part_age_cat", "attack_rates")
          temp_ar_temporal <- data.frame(temp_ar_temporal, boot = b)
          attack_rate_temporal[[count_temp]] <- temp_ar_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      attack_rate_baseline_dataframe <- do.call("rbind", attack_rate_baseline)
      attack_rate_temporal_dataframe <- do.call("rbind", attack_rate_temporal)
      
      saveRDS(attack_rate_baseline_dataframe, 
              sprintf("../rds/POLYMOD/attack_rates/samefoi/attack_rate_physical_baseline_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
      saveRDS(attack_rate_temporal_dataframe, 
              sprintf("../rds/POLYMOD/attack_rates/samefoi/attack_rate_physical_temporal_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
      }
    }
  }
}

#----- calculate attack rate
calculate_ar_Gammacomparison_with_ci <- function(ar_baseline_flu, 
                                                 ar_temporal_flu){
  
  age_cat <- c("Children", "Teens", "Adult", "Elderly")
  nboot = 3000
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
directory_path <- "../rds/POLYMOD/attack_rates/samefoi/"
list_output <- list()

count_input = 1
gamma_seq <- c(1, 2)
for(i in 1:length(R0)){
  print(paste0("i = ", R0[i]))
  for(j in 1:length(gamma_seq)){
    input_gamma = gamma_seq[j]

    data_temporal <- readRDS(paste0(directory_path, list.files(directory_path, pattern = sprintf("attack_rate_physical_temporal_.*%05d.*_BE_gamma_%03d.*", i, input_gamma))))
    data_baseline <- readRDS(paste0(directory_path, list.files(directory_path, pattern = sprintf("attack_rate_physical_baseline_.*%05d.*_BE_gamma_%03d.*", i, input_gamma))))
    
    list_output[[count_input]] <- data.frame(
      calculate_ar_Gammacomparison_with_ci(data_baseline,
                                           data_temporal), 
                                   R0 = R0[i],
                                   gamma = input_gamma)
    count_input = count_input + 1
  }
}

#----- calculate final size
if(!file.exists("../rds/POLYMOD/final_size_gamma/final_size_temporal_00001_BE_gamma_005.rds")){
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    gamma_seq <- c(5, 10)
    for(k in 1:length(gamma_seq)){
      gamma_num <- gamma_seq[k]
      attack_rate_baseline <- list()
      attack_rate_temporal <- list()
      country_input = country_list[i]
      print(sprintf("doing %s for physical contacts", country_input))
      
      population_data <- readRDS(sprintf("../rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
      for(j in 1:length(R0)){
        final_size_baseline <- NULL
        final_size_temporal <- NULL

        temp_foi <- R0[j]
        
        count_base = 1
        count_temp = 1
        for(b in 1:nboot){
          temp_env_baseline <- env()
          temp_env_frequency <- env()
          print(sprintf("doing country %s for foi %s, gamma %s and %05d boot", i, j, k, b))
          load(sprintf("../rds/POLYMOD/output_physical_%s_gamma_%03d/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                        country_input, gamma_num, j, b), temp_env_baseline)
          load(sprintf("../rds/POLYMOD/output_physical_%s_gamma_%03d/full_epidemic_simulation_CM_frequency_%03d_%05d.RData",
                       country_input, gamma_num, j, b), temp_env_frequency)

          temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
          temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
          
          if(temp_sum_baseline > 0.1*N){
             final_size_baseline[count_base] <- temp_env_baseline$output.simulate.baseline.vsc$final_size
             count_base = count_base + 1
          }
          
          if(temp_sum_temporal > 0.1*N){
            final_size_temporal[count_temp] <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
            count_temp = count_temp + 1

          }
          
        }

        saveRDS(final_size_baseline, 
                 sprintf("../rds/POLYMOD/final_size_gamma/final_size_baseline_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
        saveRDS(final_size_temporal, 
                sprintf("../rds/POLYMOD/final_size_gamma/final_size_temporal_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
      }
    }
  }
  
  gamma_num <- 7
  attack_rate_baseline <- list()
  attack_rate_temporal <- list()
  country_input = country_list[i]
  print(sprintf("doing %s for physical contacts", country_input))
  
  population_data <- readRDS(sprintf("../rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
  for(j in 1:length(R0)){
    final_size_baseline <- NULL
    final_size_temporal <- NULL
    
    temp_foi <- R0[j]
    
    count_base = 1
    count_temp = 1
    for(b in 1:nboot){
      temp_env_baseline <- env()
      temp_env_frequency <- env()
      
      print(paste0("print ", b))
      load(sprintf("../rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                    country_input, j, b), temp_env_baseline)
      load(sprintf("../rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                   country_input, j, b), temp_env_frequency)
      
      temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
      temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
      if(temp_sum_baseline > 0.1*N){
        final_size_baseline[count_base] <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
        count_base = count_base + 1
      }
      
      if(temp_sum_temporal > 0.1*N){
         final_size_temporal[count_temp] <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
         count_temp = count_temp + 1
         
      }
      
    }
    
    saveRDS(final_size_baseline, 
            sprintf("../rds/POLYMOD/final_size_gamma/final_size_baseline_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
    saveRDS(final_size_temporal, 
            sprintf("../rds/POLYMOD/final_size_gamma/final_size_temporal_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
  }  
} else {
  fs_baseline_flu <- list()
  fs_temporal_flu <- list()
  fs_baseline_covid <- list()
  fs_temporal_covid <- list()
  gamma_seq <- c(5, 7, 10)
  
  path <- "../rds/POLYMOD/final_size_gamma"
  for(i in 1:length(gamma_seq)){
    country_input = "BE"
    print(sprintf("loading country %s", country_input))
    
    for(j in 1:length(R0)){
      if (j == 1){
        base_flu <- readRDS(sprintf("%s/final_size_baseline_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        temp_flu <- readRDS(sprintf("%s/final_size_temporal_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        fs_baseline_flu[[i]] <- base_flu
        fs_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- readRDS(sprintf("%s/final_size_baseline_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        temp_covid <- readRDS(sprintf("%s/final_size_temporal_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        fs_baseline_covid[[i]] <- base_covid
        fs_temporal_covid[[i]] <- temp_covid
      }
    }
  }
}

boxplot(fs_temporal_flu[[1]], fs_baseline_flu[[1]],
        fs_temporal_flu[[2]], fs_baseline_flu[[2]],
        fs_temporal_flu[[3]], fs_baseline_flu[[3]],
        names = c("Frequency-based (5 days)", "Naive (5 days)", 
                  "Frequency-based (7 days)", "Naive (7 days)", 
                  "Frequency-based (10 days)", "Naive (10 days)"),
        main = "Final size")
dev.off()

calculate_Gammacomparison_with_ci <- function(fs_baseline_flu, 
                                                 fs_temporal_flu){
  
  nboot = 3000
  output <- data.frame()
  count = 1
  gamma_seq <- c(5, 7, 10)
  for(i in 1:length(fs_baseline_flu)){
    temp_baseline_boot <- fs_baseline_flu[[i]]
    temp_temporal_boot <- fs_temporal_flu[[i]]
    
    length_baseline_boot <- length(temp_baseline_boot)
    length_temporal_boot <- length(temp_temporal_boot)
    fs_ratio <- c()
    for(b in 1:nboot){
      boot_baseline <- mean(sample(temp_baseline_boot, length_baseline_boot, replace = T))
      boot_temporal <- mean(sample(temp_temporal_boot, length_temporal_boot, replace = T))
      
      fs_ratio[b] <- boot_temporal/boot_baseline
    }
    output[count, "gama"] <- gamma_seq[i]
    output[count, "lower"] <- round(quantile(fs_ratio, probs = 0.025), 3)
    output[count, "value"] <- round(mean(fs_ratio), 3)
    output[count, "upper"] <- round(quantile(fs_ratio, probs = 0.975), 3)
    count = count + 1
    print(count)
    
  }
  return(output)
}

calculate_Gammacomparison_with_ci(fs_baseline_flu,
                                     fs_temporal_flu)

calculate_Gammacomparison_with_ci(fs_baseline_covid,
                                     fs_temporal_covid)

# plot final size
# Combine lists into a data frame
data_flu_gamma_05 <- data.frame(value = c(fs_baseline_flu[[1]], fs_temporal_flu[[1]]),
                                group = c(rep("Baseline", length(fs_baseline_flu[[1]])),
                                          rep("Temporal", length(fs_temporal_flu[[1]]))))

# Plot histogram
ggplot(data_flu_gamma_05, aes(x = value, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.7) +
  labs(title = "Gamma = 5 days",
       x = "Value",
       y = "Frequency") +
  ylim(0, 200) +
  xlim(500, 3000) +
  scale_fill_manual(values = c("Baseline" = "blue", "Temporal" = "red"))

data_flu_gamma_07 <- data.frame(value = c(fs_baseline_flu[[2]], fs_temporal_flu[[2]]),
                                group = c(rep("Baseline", length(fs_baseline_flu[[2]])),
                                          rep("Temporal", length(fs_temporal_flu[[2]]))))

# Plot histogram
ggplot(data_flu_gamma_07, aes(x = value, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.7) +
  labs(title = "Gamma = 7 days",
       x = "Value",
       y = "Frequency") +
  ylim(0, 200) +
  xlim(500, 3000) +
  scale_fill_manual(values = c("Baseline" = "blue", "Temporal" = "red"))

data_flu_gamma_10 <- data.frame(value = c(fs_baseline_flu[[3]], fs_temporal_flu[[3]]),
                                group = c(rep("Baseline", length(fs_baseline_flu[[3]])),
                                          rep("Temporal", length(fs_temporal_flu[[3]]))))

# Plot histogram
ggplot(data_flu_gamma_10, aes(x = value, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.7) +
  labs(title = "Gamma = 10 days",
       x = "Value",
       y = "Frequency") +
  ylim(0, 200) +
  xlim(500, 3000) +
  scale_fill_manual(values = c("Baseline" = "blue", "Temporal" = "red"))

#---- epidemic duration
if(!file.exists("../rds/POLYMOD/epidemic_duration/epidemic_duration_physical_temporal_00001_BE_gamma_005.rds")){
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    gamma_seq <- c(5, 10)
    for(k in 1:length(gamma_seq)){
      gamma_num <- gamma_seq[k]
      country_input = country_list[i]
      print(sprintf("doing %s for physical contacts", country_input))
      
      population_data <- readRDS(sprintf("../rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
      for(j in 1:length(R0)){
        timestep_baseline <- NULL
        timestep_temporal <- NULL
        count_base = 1
        count_temp = 1
        for(b in 1:nboot){
          temp_env_baseline <- env()
          temp_env_frequency <- env()
          print(sprintf("doing country %s for foi %s, gamma %s and %05d boot", i, j, k, b))
          load(sprintf("../rds/POLYMOD/output_physical_%s_gamma_%03d/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                       country_input, gamma_num, j, b), temp_env_baseline)
          load(sprintf("../rds/POLYMOD/output_physical_%s_gamma_%03d/full_epidemic_simulation_CM_frequency_%03d_%05d.RData",
                       country_input, gamma_num, j, b), temp_env_frequency)
          
          temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
          temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
          
          if(temp_sum_baseline > 0.1*N){
            timestep_baseline[count_base] <- temp_env_baseline$output.simulate.baseline.vsc$time_steps
            count_base = count_base + 1
          }
          
          if(temp_sum_temporal > 0.1*N){
            timestep_temporal[count_temp] <- temp_env_frequency$output.simulate.vsc$time_steps
            count_temp = count_temp + 1
            
          }
          
        }
        
        saveRDS(timestep_baseline, 
                sprintf("../rds/POLYMOD/epidemic_duration/samefoi/epidemic_duration_physical_baseline_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
        saveRDS(timestep_temporal, 
                sprintf("../rds/POLYMOD/epidemic_duration/samefoi/epidemic_duration_physical_temporal_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
      }
    }
  }
  
  gamma_num <- 7
  country_input = country_list[i]
  print(sprintf("doing %s for physical contacts", country_input))
  
  population_data <- readRDS(sprintf("../rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
  for(j in 1:length(R0)){
    timestep_baseline <- NULL
    timestep_temporal <- NULL

    count_base = 1
    count_temp = 1
    for(b in 1:nboot){
      
      temp_env_baseline <- env()
      temp_env_frequency <- env()
      print(sprintf("doing country %s for foi %s, gamma %s and %05d boot", i, j, k, b))
      load(sprintf("../rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                   country_input, j, b), temp_env_baseline)
      load(sprintf("../rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                   country_input, j, b), temp_env_frequency)
      
      temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
      temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
      if(temp_sum_baseline > 0.1*N){
        timestep_baseline[count_base] <- temp_env_baseline$output.simulate.baseline.vsc$time_steps
        count_base = count_base + 1
      }
      
      if(temp_sum_temporal > 0.1*N){
        timestep_temporal[count_temp] <- temp_env_frequency$output.simulate.vsc$time_steps
        count_temp = count_temp + 1
        
      }
      
    }
    
    saveRDS(timestep_baseline, 
            sprintf("../rds/POLYMOD/epidemic_duration/samefoi/epidemic_duration_physical_baseline_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
    saveRDS(timestep_temporal, 
            sprintf("../rds/POLYMOD/epidemic_duration/samefoi/epidemic_duration_physical_temporal_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
    }  
} else {
  ed_baseline_flu <- list()
  ed_temporal_flu <- list()
  ed_baseline_covid <- list()
  ed_temporal_covid <- list()
  gamma_seq <- c(5, 7, 10)
  
  path <- "../rds/POLYMOD/epidemic_duration/samefoi"
  for(i in 1:length(gamma_seq)){
    country_input = "BE"
    print(sprintf("loading country %s", country_input))
    
    for(j in 1:length(R0)){
      if (j == 1){
        base_flu <- readRDS(sprintf("%s/epidemic_duration_physical_baseline_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        temp_flu <- readRDS(sprintf("%s/epidemic_duration_physical_temporal_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        ed_baseline_flu[[i]] <- base_flu
        ed_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- readRDS(sprintf("%s/epidemic_duration_physical_baseline_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        temp_covid <- readRDS(sprintf("%s/epidemic_duration_physical_temporal_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        ed_baseline_covid[[i]] <- base_covid
        ed_temporal_covid[[i]] <- temp_covid
      }
    }
  }
}

boxplot(ed_temporal_flu[[1]], ed_baseline_flu[[1]],
        ed_temporal_flu[[2]], ed_baseline_flu[[2]],
        ed_temporal_flu[[3]], ed_baseline_flu[[3]],
        names = c("Frequency-based (5 days)", "Naive (5 days)", 
                  "Frequency-based (7 days)", "Naive (7 days)", 
                  "Frequency-based (10 days)", "Naive (10 days)"),
        main = "Epidemic duration")

calculate_Gammacomparison_with_ci(ed_baseline_flu,
                                     ed_temporal_flu)

calculate_Gammacomparison_with_ci(ed_baseline_covid,
                                     ed_temporal_covid)

#---- time to peak
if(!file.exists("../rds/POLYMOD/timetopeak/timetopeak_physical_temporal_00001_BE_gamma_005.rds")){
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    gamma_seq <- c(5, 10)
    for(k in 1:length(gamma_seq)){
      gamma_num <- gamma_seq[k]
      country_input = country_list[i]
      print(sprintf("doing %s for physical contacts", country_input))
      
      population_data <- readRDS(sprintf("../rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
      for(j in 1:length(R0)){
        ttp_baseline <- NULL
        ttp_temporal <- NULL
        count_base = 1
        count_temp = 1
        for(b in 1:nboot){
          temp_env_baseline <- env()
          temp_env_frequency <- env()
          print(sprintf("doing country %s for foi %s, gamma %s and %05d boot", i, j, k, b))
          load(sprintf("../rds/POLYMOD/output_physical_%s_gamma_%03d/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                       country_input, gamma_num, j, b), temp_env_baseline)
          load(sprintf("../rds/POLYMOD/output_physical_%s_gamma_%03d/full_epidemic_simulation_CM_frequency_%03d_%05d.RData",
                       country_input, gamma_num, j, b), temp_env_frequency)
          
          temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
          temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
          
          if(temp_sum_baseline > 0.1*N){
            ttp_baseline[count_base] <- which.max(temp_env_baseline$output.simulate.baseline.vsc$infected_daily)
            count_base = count_base + 1
          }
          
          if(temp_sum_temporal > 0.1*N){
            ttp_temporal[count_temp] <- which.max(temp_env_frequency$output.simulate.vsc$infected.daily)
            count_temp = count_temp + 1
            
          }
          
        }
        
        saveRDS(ttp_baseline, 
                sprintf("../rds/POLYMOD/timetopeak/samefoi/timetopeak_physical_baseline_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
        saveRDS(ttp_temporal, 
                sprintf("../rds/POLYMOD/timetopeak/samefoi/timetopeak_physical_temporal_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
      }
    }
  }
  
  gamma_num <- 7
  country_input = "BE"
  print(sprintf("doing %s for physical contacts", country_input))
  
  population_data <- readRDS(sprintf("../rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
  for(j in 1:length(R0)){
    ttp_baseline <- NULL
    ttp_temporal <- NULL
    
    count_base = 1
    count_temp = 1
    for(b in 1:nboot){
      temp_env_baseline <- env()
      temp_env_frequency <- env()
      print(sprintf("doing country %s for foi %s, gamma %s and %05d boot", i, j, k, b))
      load(sprintf("../rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                   country_input, j, b), temp_env_baseline)
      load(sprintf("../rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                   country_input, j, b), temp_env_frequency)
      
      temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
      temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
      if(temp_sum_baseline > 0.1*N){
        ttp_baseline[count_base] <- which.max(temp_env_baseline$output.simulate.baseline.vsc$infected_daily)
        count_base = count_base + 1
      }
      
      if(temp_sum_temporal > 0.1*N){
        ttp_temporal[count_temp] <- which.max(temp_env_frequency$output.simulate.vsc$infected.daily)
        count_temp = count_temp + 1
        
      }
      
    }
    
    saveRDS(ttp_baseline, 
            sprintf("../rds/POLYMOD/timetopeak/samefoi/timetopeak_physical_baseline_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
    saveRDS(ttp_temporal, 
            sprintf("../rds/POLYMOD/timetopeak/samefoi/timetopeak_physical_temporal_%05d_%s_gamma_%03d.rds", as.integer(j), country_input, gamma_num))
  }  
} else {
  ttp_baseline_flu <- list()
  ttp_temporal_flu <- list()
  ttp_baseline_covid <- list()
  ttp_temporal_covid <- list()
  gamma_seq <- c(5, 7, 10)
  
  path <- "../rds/POLYMOD/timetopeak/samefoi"
  for(i in 1:length(gamma_seq)){
    country_input = "BE"
    print(sprintf("loading country %s", country_input))
    
    for(j in 1:length(R0)){
      if (j == 1){
        base_flu <- readRDS(sprintf("%s/timetopeak_physical_baseline_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        temp_flu <- readRDS(sprintf("%s/timetopeak_physical_temporal_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        ttp_baseline_flu[[i]] <- base_flu
        ttp_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- readRDS(sprintf("%s/timetopeak_physical_baseline_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        temp_covid <- readRDS(sprintf("%s/timetopeak_physical_temporal_%05d_BE_gamma_%03d.rds", path, as.integer(j), gamma_seq[i]))
        ttp_baseline_covid[[i]] <- base_covid
        ttp_temporal_covid[[i]] <- temp_covid
      }
    }
  }
}

boxplot(ttp_temporal_flu[[1]], ttp_baseline_flu[[1]],
        ttp_temporal_flu[[2]], ttp_baseline_flu[[2]],
        ttp_temporal_flu[[3]], ttp_baseline_flu[[3]],
        names = c("Frequency-based (5 days)", "Naive (5 days)", 
                  "Frequency-based (7 days)", "Naive (7 days)", 
                  "Frequency-based (10 days)", "Naive (10 days)"),
        main = "Time to peak")

calculate_Gammacomparison_with_ci(ttp_baseline_flu,
                                     ttp_temporal_flu)

calculate_Gammacomparison_with_ci(ttp_baseline_covid,
                                     ttp_temporal_covid)

#------ comparison of q....
# input vsc different gamma
R0 = c(1.3, 3.3)
gamma_list = seq(1, 14, by = 1)
country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
count = 1
output = data.frame()

for(i in 1:length(country_list)){
  country_code = country_list[i]
  for(g in 1:length(gamma_list)){
    gamma = round(1/gamma_list[g], 4)
    foi <- list()
    title_phys <- "_physical"
    
    load(sprintf("../../data/data_input/POLYMOD/input_polymod%s_%s.RData", title_phys, country_code))
    load(sprintf("../../data/data_input/POLYMOD/social_matrices%s_%s.RData", title_phys, country_code))
      
    data_temp <- readRDS(sprintf("../rds/POLYMOD/data_temp_5000_pop_%s.rds", country_code))
      
    S = diag(table(data_temp$dataset.res$part_age_cat))
    A = diag(1, nrow = 4, ncol = 4)
    C = diag(1, nrow = 4, ncol = 4)
    B = t(cm$all)/as.vector(table(data_temp$dataset.res$part_age_cat))
    
    foi <- sapply(R0, function(x) calculate_q_cm(x, 1, 0.001))
    foi_freq <- sapply(R0, function(x) calculate_q_naive_cm(x, 
                                                       cm_daily = cm$daily,
                                                       cm_others = cm$all - cm$daily,
                                                       rec_rate = gamma_list[g],
                                                       initial_top = 1,
                                                       initial_bottom = 0.001))
    
    print(paste0(i, " : ", foi))
    foi_length = length(foi)
      
    output[count, "country"] <- country_code
    output[count, "recovery_day"] <- gamma_list[g]
    output[count, "q_naive_flu"] <- foi[1]
    output[count, "q_freq_flu"] <- foi_freq[1]
    output[count, "q_naive_covid"] <- foi[2]
    output[count, "q_freq_covid"] <- foi_freq[2]
    
    count = count + 1
  }
}

output <- output %>% 
  mutate(q_comparison_flu = q_freq_flu/q_naive_flu,
         q_comparison_covid = q_freq_covid/q_naive_covid)

country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")

png(filename = "../results/POLYMOD/gamma_q_comparison.png", width = 800, height = 500, units = "px")
par(mfrow = c(1, 2))
plot(gamma_list, output[output$country == "BE",]$q_comparison_flu, type = "l", 
     col = 1, ylim = range(c(1, 1.6)), 
     xlab = "Infectious period",
     ylab = "Frequency over naive-based probability of transmission per contact",
     lwd = 2, main = "Influenza-like",
     cex.lab=1.25, cex.axis=1.25, cex.main=1.25, cex.sub=1.25)
lines(gamma_list, output[output$country == "DE",]$q_comparison_flu, col = 2, lwd = 2)
lines(gamma_list, output[output$country == "FI",]$q_comparison_flu, col = 3, lwd = 2)
lines(gamma_list, output[output$country == "IT",]$q_comparison_flu, col = 4, lwd = 2)
lines(gamma_list, output[output$country == "LU",]$q_comparison_flu, col = 5, lwd = 2)
lines(gamma_list, output[output$country == "NL",]$q_comparison_flu, col = 6, lwd = 2)
lines(gamma_list, output[output$country == "PL",]$q_comparison_flu, col = 7, lwd = 2)
lines(gamma_list, output[output$country == "GB",]$q_comparison_flu, col = 8, lwd = 2)
legend("topleft",
       legend = country_list,
       col = seq(1, 8),
       ncol = 4,
       lwd = c(rep(2, length(country_list))))

plot(gamma_list, output[output$country == "BE",]$q_comparison_covid, type = "l", 
     col = 1, ylim = range(c(1, 1.6)), 
     xlab = "Infectious period",
     ylab = "",
     lwd = 2, main = "COVID-19-like",
     cex.lab=1.25, cex.axis=1.25, cex.main=1.25, cex.sub=1.25, lty = 2)
lines(gamma_list, output[output$country == "DE",]$q_comparison_covid, col = 2, lwd = 2, lty = 2)
lines(gamma_list, output[output$country == "FI",]$q_comparison_covid, col = 3, lwd = 2, lty = 2)
lines(gamma_list, output[output$country == "IT",]$q_comparison_covid, col = 4, lwd = 2, lty = 2)
lines(gamma_list, output[output$country == "LU",]$q_comparison_covid, col = 5, lwd = 2, lty = 2)
lines(gamma_list, output[output$country == "NL",]$q_comparison_covid, col = 6, lwd = 2, lty = 2)
lines(gamma_list, output[output$country == "PL",]$q_comparison_covid, col = 7, lwd = 2, lty = 2)
lines(gamma_list, output[output$country == "GB",]$q_comparison_covid, col = 8, lwd = 2, lty = 2)
legend("topleft",
       legend = country_list,
       col = seq(1, 8),
       ncol = 4,
       lwd = c(rep(2, length(country_list))))
dev.off()
