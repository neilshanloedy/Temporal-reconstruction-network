rm(list = ls())
library(rstudioapi)
dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
country <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
is_physical <- c("physical", "all_contacts")

calculate_q_cm <- function(R0_input, initial.guess){
  
  qran <- initial.guess
  R0_temp <- max(eigen((S %*% B) * qran/gamma)$values)
  qran_prev <- initial.guess
  while (abs((R0_input - R0_temp)) > 0.0001) {
    if (R0_temp > R0_input) {
      qran <- runif(1, 0, qran_prev)
    } else {
      qran <- runif(1, qran_prev, 1)
    }
    qran_prev <- qran
    R0_temp <- max(eigen((S %*% B) * qran/gamma)$values)
  }
  return(round(qran, 4))
}

# R0 flu: Maital, S., & Barzani, E. (2020). The global economic impact of COVID-19: A summary of research. 
# R0 COVID: Achaiah, N. C., Subbarajasetty, S. B., & Shetty, R. M. (2020). R0 and re of COVID-19: can we predict when the pandemic outbreak will be contained?. 
R0 = c(1.3, 3.3)
gamma = 1/7
nboot = 3000

for(i in country){
  for(j in is_physical){
  foi <- list()
  
  print(paste0("doing ", i, " for ", j, " contacts"))
  title_phys <- ""
  
  if(j %in% "physical"){
    title_phys <- "_physical"
  }
  
  load(sprintf("../../data/data_input/POLYMOD/input_polymod%s_%s.RData", title_phys, i))
  load(sprintf("../../data/data_input/POLYMOD/social_matrices%s_%s.RData", title_phys, i))
  
  data_temp <- readRDS(sprintf("../rds/POLYMOD/data_temp_5000_pop_%s.rds", i))
  
  S = diag(table(data_temp$dataset.res$part_age_cat))
  A = diag(1, nrow = 4, ncol = 4)
  C = diag(1, nrow = 4, ncol = 4)
  B = t(cm$all)/as.vector(table(data_temp$dataset.res$part_age_cat))
  
  foi <- sapply(R0, function(x) calculate_q_cm(x, 0.02))
  print(paste0(i, " : ", foi))
  foi_length = length(foi)
  
  output <- data.frame(id = rep(seq(1, nboot), foi_length),
                       foi = rep(foi, each = nboot),
                       country = rep(which(country %in% i), nboot))
  
  filename = sprintf("../../data/vsc_polymod_input/input_forVSC%s_%s.csv", title_phys, i)
  write.csv(output, filename, quote = F, row.names = F)
  }
}

# input vsc different gamma
country_code <- "BE"
is_physical <- c("physical", "all_contacts")

R0 = c(1.3, 3.3)
gamma_list = c(5, 7, 10)
nboot = 3000

for(g in gamma_list){
  for(j in is_physical){
    gamma = round(1/gamma_list[g], 4)
    foi <- list()
    
    print(paste0("doing ", i, " for ", j, " contacts"))
    title_phys <- ""
    
    if(j %in% "physical"){
      title_phys <- "_physical"
    }
    
    load(sprintf("../../data/data_input/POLYMOD/input_polymod%s_%s.RData", title_phys, country_code))
    load(sprintf("../../data/data_input/POLYMOD/social_matrices%s_%s.RData", title_phys, country_code))
    
    data_temp <- readRDS(sprintf("../rds/POLYMOD/data_temp_5000_pop_%s.rds", country_code))
    
    S = diag(table(data_temp$dataset.res$part_age_cat))
    A = diag(1, nrow = 4, ncol = 4)
    C = diag(1, nrow = 4, ncol = 4)
    B = t(cm$all)/as.vector(table(data_temp$dataset.res$part_age_cat))
    
    foi <- sapply(R0, function(x) calculate_q_cm(x, 0.02))
    print(paste0(i, " : ", foi))
    foi_length = length(foi)
    
    output <- data.frame(id = rep(seq(1, nboot), foi_length),
                         foi = rep(foi, each = nboot),
                         gamma_num = rep(g, nboot),
                         gamma = rep(gamma, nboot))
    filename = sprintf("../../data/vsc_polymod_input/input_forVSC%s_%s_gamma_%04d.csv", title_phys, country_code, g)
    write.csv(output, filename, quote = F, row.names = F)
  }
}

