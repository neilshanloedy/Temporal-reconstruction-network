# Here, the temporal weekly simulated does not take any age structure
# meaning, it only uses the the mean of daily, weekly, ...
# also, the simulation does not take any age structure,
# hence, only overall mean (mean = 11.9)
rm(list = ls())

# run first code.R
library(rstudioapi)
library(ggplot2)
dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
country <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
is_physical <- c("physical", "all_contacts")

# Here is the code to run GP dist of the
# temporal reconstructed network
#----------------------------------------
for(i in country){
  for(cnt in is_physical){
    source('helper/simulate_weekly_temporal.R')
    source('helper/simulate_epidemic_CM_fullepidemics.R')
    source('helper/functions.R')

    print(paste0("doing ", i, " for ", cnt, " contacts"))
    title_phys <- ""
    
    if(cnt %in% "physical"){
      title_phys <- "_physical"
    }
    
    load(sprintf("../data/data_input/POLYMOD/input_polymod%s_%s.RData", title_phys, i))
    load(sprintf("../data/data_input/POLYMOD/social_matrices%s_%s.RData", title_phys, i))
    
    # here we simulate the epidemic
    N = 5000
    
    if(!file.exists(sprintf("rds/POLYMOD/data_temp_5000_pop%s_%s.rds", title_phys, i))){
      data_temp <- simulate_weekly_contact_gp(n_sample = N,
                                              number.contacts = number.contact.input,
                                              proportion.week = prop.week,
                                              proportion.age = prop.age.partdata)
      saveRDS(data_temp, sprintf("rds/POLYMOD/data_temp_5000_pop%s_%s.rds", title_phys, i))
    } else {
      data_temp <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop%s_%s.rds", title_phys, i))
    }
  }
}

# create an overall rds for
# the ratio of the reconstructed per country
#----------------------------------------

country <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
is_physical <- c("physical", "all_contacts")

storage_list <- list()
count = 1

if(!file.exists("rds/POLYMOD/data_ratio_polymod.rds")){
  for(i in country){
    for(cnt in is_physical){
      print(paste0("doing ", i, " for ", cnt, " contacts"))
      title_phys <- ""
      
      if(cnt %in% "physical"){
        title_phys <- "_physical"
      }
      
      N = 5000
      
      temp_env <- env()
      data_temp <- readRDS(sprintf("rds/POLYMOD/data_temp_gp_5000_pop%s_%s.rds", title_phys, i))
      
      storage_list[[count]] <- cbind(cbind(calculate_ratio_temporal(data_temp), country = i), cnt_type = cnt)
      count = count + 1
    }
  }
  saveRDS(storage_list, "rds/POLYMOD/data_ratio_polymod.rds")
} else {
  storage_list <- readRDS("rds/POLYMOD/data_ratio_polymod.rds")
}

# boxplot and violin plot
#----------------------------------------
temporary <- environment()
output_to_plot <- list()
count = 1

for(i in country){
  for(cnt in is_physical){
    print(paste0("doing ", i, " for ", cnt, " contacts"))
    title_phys <- ""
    
    if(cnt %in% "physical"){
      title_phys <- "_physical"
    }
    
    load(sprintf("../data/data_input/POLYMOD/input_polymod%s_%s.RData", title_phys, i), envir = temporary)
    
    temporary_data <- temporary$temp_dat
    temporary_data$country <- i
    temporary_data$contact_type <- cnt
    output_to_plot[[count]] <- temporary_data
    count = count + 1
  }
}

# code for dummy population to calculate R0

# for(i in country){
#   for(cnt in is_physical){
#     source('helper/simulate_weekly_temporal.R')
#     source('helper/simulate_epidemic_CM_fullepidemics.R')
#     source('helper/functions.R')
#     
#     print(paste0("doing ", i, " for ", cnt, " contacts"))
#     title_phys <- ""
#     
#     if(cnt %in% "physical"){
#       title_phys <- "_physical"
#     }
#     
#     load(sprintf("../data/data_input/POLYMOD/input_polymod%s_%s.RData", title_phys, i))
#     load(sprintf("../data/data_input/POLYMOD/social_matrices%s_%s.RData", title_phys, i))
#     
#     # here we simulate the epidemic
#     N = 1000
#     
#     if(!file.exists(sprintf("rds/POLYMOD/reproduction_number_pop/data_temp_1000_pop%s_%s.rds", title_phys, i))){
#       data_temp <- simulate_weekly_contact_gp(n_sample = N,
#                                               number.contacts = number.contact.input,
#                                               proportion.week = prop.week,
#                                               proportion.age = prop.age.partdata)
#       saveRDS(data_temp, sprintf("rds/POLYMOD/reproduction_number_pop/data_temp_1000_pop%s_%s.rds", title_phys, i))
#     } else {
#       data_temp <- readRDS(sprintf("rds/POLYMOD/reproduction_number_pop/data_temp_1000_pop%s_%s.rds", title_phys, i))
#     }
#   }
# }
