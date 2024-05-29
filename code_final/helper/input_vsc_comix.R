rm(list = ls())
library(rstudioapi)
dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
wave <- c(19, 27, 43)
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
    
    return(max(eigen(r0)$values))
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

# R0 flu: Maital, S., & Barzani, E. (2020). The global economic impact of COVID-19: A summary of research. 
# R0 COVID: Achaiah, N. C., Subbarajasetty, S. B., & Shetty, R. M. (2020). R0 and re of COVID-19: can we predict when the pandemic outbreak will be contained?. 
R0 = c(1.3, 3.3)
gamma = 1/7
nboot = 3000

for(i in wave){
  for(j in is_physical){
    foi <- list()
    
    print(paste0("doing wave ", i))
    title_phys <- ""
    
    if(j %in% "physical"){
      title_phys <- "_physical"
    }
    
    load(sprintf("../../data/data_input/CoMix/input_comix%s_%04d.RData", title_phys, i))
    load(sprintf("../../data/data_input/CoMix/social_matrices%s_%04d.RData", title_phys, i))
    
    data_temp <- readRDS(sprintf("../rds/CoMix/data_temp_CoMix%s_5000pop_%04d.rds", title_phys, i))
    
    S = diag(table(data_temp$dataset.res$part_age_cat))
    A = diag(1, nrow = 4, ncol = 4)
    C = diag(1, nrow = 4, ncol = 4)
    B = t(cm$all)/as.vector(table(data_temp$dataset.res$part_age_cat))
    
    foi <- sapply(R0, function(x) calculate_q_cm(x, 0.02))
    print(paste0(i, " : ", foi))
    foi_length = length(foi)
    
    output <- data.frame(id = c(seq(1, nboot), seq(1, nboot)),
                         foi = c(rep(foi[1], each = nboot), rep(foi[2], each = nboot)),
                         wave = rep(i, (nboot + nboot)))
    
    filename = sprintf("../../data/vsc_comix_input/input_forVSC%s_%04d.csv", title_phys, i)
    write.csv(output, filename, quote = F, row.names = F)
  }
}

# for the frequency-based approach
is_physical <- c("physical")
for(i in wave){
  for(j in is_physical){
    j = is_physical
    foi <- list()
    
    print(paste0("doing ", i, " for ", j, " contacts"))
    title_phys <- ""
    
    if(j %in% "physical"){
      title_phys <- "_physical"
    }
    
    # load(sprintf("../../data/data_input/CoMix/input_comix%s_%04d.RData", title_phys, i))
    load(sprintf("../../data/data_input/CoMix/social_matrices%s_%04d.RData", title_phys, i))
    
    foi <- sapply(R0, function(x) calculate_q_naive_cm(x, 
                                                       cm_daily = cm$daily,
                                                       cm_others = cm$all - cm$daily,
                                                       rec_rate = (1/gamma),
                                                       initial_top = 1,
                                                       initial_bottom = 0.001))
    print(paste0(i, " : ", foi))
    foi_length = length(foi)
    
    output <- data.frame(id = rep(seq(1, nboot), foi_length),
                         foi = rep(foi, each = nboot),
                         country = rep(i, nboot))
    
    filename = sprintf("../../data/vsc_comix_input/naive_input_forVSC%s_%s.csv", title_phys, i)
    write.csv(output, filename, quote = F, row.names = F)
  }
}
