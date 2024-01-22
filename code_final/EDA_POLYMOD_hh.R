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
country <- c("BE", "DE", "FI", "GB", "IT", "LU", "NL", "PL")
type_of_contacts <- c("contacts_at_home", "contacts_not_at_home")
source('helper/functions.R')

# Here is the code to run either NBI or PO dist of
# the temporal reconstructed network
#--------------------------------------------------
for(i in country){
  for(cnt in type_of_contacts){
    
    source('helper/simulate_weekly_temporal.R')
    source('helper/simulate_weekly_temporal_poisson.R')
    source('helper/simulate_epidemic_CM_fullepidemics.R')
    source('helper/functions.R')
    source('helper/ggplot_functions.R')
    
    print(paste0("doing ", i, " for ", cnt, " contacts"))
    title_phys <- ""
    
    simulate <- simulate_weekly_contact_rnbinom
    
    load(sprintf("../data/data_input/POLYMOD/%s/input_polymod%s_%s_%s.RData", cnt, title_phys, cnt, i))
    
    if(any(number.contact.input$number.contact.general$var - number.contact.input$number.contact.general$mean < 0)){
      simulate <- simulate_weekly_contact_poisson
    }
    
    # here we simulate the epidemic
    N = 5000
    
    if(!file.exists(sprintf("rds/POLYMOD/%s/data_temp_5000_pop%s_%s_%s.rds", cnt, title_phys, cnt, i))){
      data_temp <- simulate(n_sample = N,
                            number.contacts = number.contact.input,
                            proportion.week = prop.week,
                            proportion.age = prop.age.partdata)
      saveRDS(data_temp, sprintf("rds/POLYMOD/%s/data_temp_5000_pop%s_%s_%s.rds", cnt, title_phys, cnt, i))
    } else {
      data_temp <- readRDS(sprintf("rds/POLYMOD/%s/data_temp_5000_pop%s_%s_%s.rds", cnt, title_phys, cnt, i))
    }
    
    plot_agecat <- f_plot_agecat()
    ggsave(plot_agecat, file = sprintf("results/EDA_POLYMOD/%s/plot_agecat%s_%s_%s.png", cnt, title_phys, cnt, i),
           width = plot_width*1.75,
           height = plot_height*1.75,
           units = c("mm"))
    
    plot_boxplot <- fplot_boxplot_result(dataset = data_temp,
                                         observed_data = temp_dat)
    ggsave(plot_boxplot, file = sprintf("results/EDA_POLYMOD/%s/plot_results%s_%s_%s.png", cnt, title_phys, cnt, i),
           width = plot_width*2.75,
           height = plot_height*2.5,
           units = c("mm"))
  }
}

# Here is the code to run GP dist of the
# temporal reconstructed network
#----------------------------------------

for(i in country){
  for(cnt in type_of_contacts){

    source('helper/simulate_weekly_temporal.R')
    source('helper/simulate_epidemic_CM_fullepidemics.R')
    source('helper/functions.R')

    print(paste0("doing ", i, " for ", cnt, " contacts"))
    title_phys <- ""
    
    load(sprintf("../data/data_input/POLYMOD/%s/input_polymod%s_%s_%s.RData", cnt, title_phys, cnt, i))
    
    # here we simulate the epidemic
    N = 5000
    
    if(!file.exists(sprintf("rds/POLYMOD/data_temp_gp_5000_pop%s_%s_%s.rds", cnt, title_phys, cnt, i))){
      data_temp <- simulate_weekly_contact_gp(n_sample = N,
                            number.contacts = number.contact.input,
                            proportion.week = prop.week,
                            proportion.age = prop.age.partdata)
      saveRDS(data_temp, sprintf("rds/POLYMOD/data_temp_gp_5000_pop%s_%s_%s.rds", title_phys, cnt, i))
    } else {
      data_temp <- readRDS(sprintf("rds/POLYMOD/data_temp_gp_5000_pop%s_%s_%s.rds", title_phys, cnt, i))
    }
  }
}

# -- contacts at home
#----------------------
storage_list_at_home <- list()
storage_list_not_at_home <- list()

for(cnt in type_of_contacts){
  storage_list <- list()
  if(!file.exists(sprintf("rds/POLYMOD/%s/data_temp_%s_all.rds", cnt, cnt))){
    for(i in 1:length(country)){
      country_input = country[i]
      print(sprintf("doing %s for %s", country_input, cnt))
      temp_env <- env()
      temp <- readRDS(sprintf("rds/POLYMOD/%s/data_temp_5000_pop_%s_%s.rds", cnt, cnt, country_input))
      
      storage_list[[i]] <- cbind(calculate_ratio_temporal(temp), country = country_input)
    }
    saveRDS(storage_list, sprintf("rds/POLYMOD/%s/data_temp_%s_all.rds", cnt, cnt))
  } else {
    storage_list_at_home <- readRDS(sprintf("rds/POLYMOD/%s/data_temp_%s_all.rds", type_of_contacts[1], type_of_contacts[1]))
    storage_list_not_at_home <- readRDS(sprintf("rds/POLYMOD/%s/data_temp_%s_all.rds", type_of_contacts[2], type_of_contacts[2]))
  }
}

storage_data_at_home <- do.call(rbind, storage_list_at_home)
storage_data_not_at_home <- do.call(rbind, storage_list_not_at_home)
storage_list <- readRDS("rds/POLYMOD/data_ratio_polymod.rds")
data_ratio_to_plot <- do.call("rbind", storage_list)
data_ratio_to_plot <- data_ratio_to_plot[data_ratio_to_plot$cnt_type == "all_contacts",]

png(filename = "results/POLYMOD/ratio_allcontacts_home_results.png", width = 600*1.5, height = 600*1.2)
par(mfrow = c(2,2))
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home[storage_data_not_at_home$part_age_cat == "Children",]$ratio,
     ylim= c(0,1), type = "n", xlab = "Country", ylab = "", main = "Children", xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home[storage_data_not_at_home$part_age_cat == "Children",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 22, cex = 1, main = "Children", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_at_home[storage_data_at_home$part_age_cat == "Children",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 23, cex = 1, main = "Children", bg = "green",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 21, cex = 1, main = "Children", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
legend("bottomright", legend = c("All contacts", "Contacts outside home",
                                 "Contacts at home"),
       pch = c(21, 22, 23), pt.bg = c("red", "blue", "green"))
axis(1, at = 1:length(country), labels=country)
mtext(side = 2, line = 3, "Temporal over weekly", cex = 1.2)
mtext(side = 2, line = 2, "observed distinct contact ratio", cex = 1.2)
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home[storage_data_not_at_home$part_age_cat == "Teens",]$ratio,
     ylim= c(0,1), type = "n", xlab = "Country", ylab = "", main = "Teens", xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home[storage_data_not_at_home$part_age_cat == "Teens",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 22, cex = 1, main = "Teens", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_at_home[storage_data_at_home$part_age_cat == "Teens",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 23, cex = 1, main = "Teens", bg = "green",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 21, cex = 1, main = "Teens", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
legend("bottomright", legend = c("All contacts", "Contacts outside home",
                                 "Contacts at home"),
       pch = c(21, 22, 23), pt.bg = c("red", "blue", "green"))
axis(1, at = 1:length(country), labels=country)
mtext(side = 2, line = 3, "Temporal over weekly", cex = 1.2)
mtext(side = 2, line = 2, "observed distinct contact ratio", cex = 1.2)
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home[storage_data_not_at_home$part_age_cat == "Adult",]$ratio,
     ylim= c(0,1), type = "n", xlab = "Country", ylab = "", main = "Adult", xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home[storage_data_not_at_home$part_age_cat == "Adult",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 22, cex = 1, main = "Adult", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_at_home[storage_data_at_home$part_age_cat == "Adult",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 23, cex = 1, main = "Adult", bg = "green",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 21, cex = 1, main = "Adult", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
legend("bottomright", legend = c("All contacts", "Contacts outside home",
                                 "Contacts at home"),
       pch = c(21, 22, 23), pt.bg = c("red", "blue", "green"))
axis(1, at = 1:length(country), labels=country)
mtext(side = 2, line = 3, "Temporal over weekly", cex = 1.2)
mtext(side = 2, line = 2, "observed distinct contact ratio", cex = 1.2)
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home[storage_data_not_at_home$part_age_cat == "Elderly",]$ratio,
     ylim= c(0,1), type = "n", xlab = "Country", ylab = "", main = "Elderly", xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home[storage_data_not_at_home$part_age_cat == "Elderly",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 22, cex = 1, main = "Elderly", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_at_home[storage_data_at_home$part_age_cat == "Elderly",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 23, cex = 1, main = "Elderly", bg = "green",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$ratio,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 21, cex = 1, main = "Elderly", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
legend("bottomright", legend = c("All contacts", "Contacts outside home",
                                 "Contacts at home"),
       pch = c(21, 22, 23), pt.bg = c("red", "blue", "green"))
axis(1, at = 1:length(country), labels=country)
mtext(side = 2, line = 3, "Temporal over weekly", cex = 1.2)
mtext(side = 2, line = 2, "observed distinct contact ratio", cex = 1.2)
dev.off()

#---------------- plot with CIs

for(cnt in type_of_contacts){
  storage_list <- list()
  if(!file.exists(sprintf("rds/POLYMOD/%s/data_temp_%s_all_withci.rds", cnt, cnt))){
    for(i in 1:length(country)){
      country_input = country[i]
      print(sprintf("doing %s for %s", country_input, cnt))
      temp_env <- env()
      temp <- readRDS(sprintf("rds/POLYMOD/%s/data_temp_5000_pop_%s_%s.rds", cnt, cnt, country_input))
      
      storage_list[[i]] <- cbind(calculate_ratio_temporal_withci(temp), country = country_input)
    }
    saveRDS(storage_list, sprintf("rds/POLYMOD/%s/data_temp_%s_all_withci.rds", cnt, cnt))
  } else {
    storage_list_at_home_withci <- readRDS(sprintf("rds/POLYMOD/%s/data_temp_%s_all_withci.rds", type_of_contacts[1], type_of_contacts[1]))
    storage_list_not_at_home_withci <- readRDS(sprintf("rds/POLYMOD/%s/data_temp_%s_all_withci.rds", type_of_contacts[2], type_of_contacts[2]))
  }
}

storage_data_at_home_withci <- do.call(rbind, storage_list_at_home_withci)
storage_data_not_at_home_withci <- do.call(rbind, storage_list_not_at_home_withci)

country <- c("BE", "DE", "FI", "GB", "IT", "LU", "NL", "PL")
is_physical <- c("physical", "all_contacts")
count = 1 

if(!file.exists("rds/POLYMOD/data_ratio_polymod_withci.rds")){
  for(i in country){
    for(cnt in is_physical){
      print(paste0("doing ", i, " for ", cnt, " contacts"))
      title_phys <- ""
      
      if(cnt %in% "physical"){
        title_phys <- "_physical"
      }
      
      N = 5000
      
      temp_env <- env()
      data_temp <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop%s_%s.rds", title_phys, i))
      
      storage_list[[count]] <- cbind(cbind(calculate_ratio_temporal_withci(data_temp), country = i), cnt_type = cnt)
      count = count + 1
    }
  }
  saveRDS(storage_list, "rds/POLYMOD/data_ratio_polymod_withci.rds")
} else {
  storage_list <- readRDS("rds/POLYMOD/data_ratio_polymod_withci.rds")
}

storage_list <- readRDS("rds/POLYMOD/data_ratio_polymod_withci.rds")
data_ratio_to_plot <- do.call("rbind", storage_list)
data_ratio_to_plot <- data_ratio_to_plot[data_ratio_to_plot$cnt_type == "all_contacts",]

country <- c("BE", "DE", "FI", "GB", "IT", "LU", "NL", "PL")

png(filename = "results/POLYMOD/ratio_allcontacts_home_results_withci.png", width = 800*1.2, height = 800)
par(oma = c(3,1,0.75,1), mfrow = c(2, 2), mar = c(3, 4, 3, 1))
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$mean,
     ylim= c(0,1), type = "n", xlab = "Country", ylab = "", main = "Children", xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1, main = "Children", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 23, cex = 1, main = "Children", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$lower,
       x1 = seq(1, length(country)),
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 21, cex = 1, main = "Children", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country)
mtext(side = 2, line = 3, "Temporal over weekly", cex = 1.2)
mtext(side = 2, line = 2, "observed distinct contact ratio", cex = 1.2)
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$mean,
     ylim= c(0,1), type = "n", xlab = "Country", ylab = "", main = "Teens", xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1, main = "Teens", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 23, cex = 1, main = "Teens", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$lower,
       x1 = seq(1, length(country)),
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 21, cex = 1, main = "Teens", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country)
mtext(side = 2, line = 3, "Temporal over weekly", cex = 1.2)
mtext(side = 2, line = 2, "observed distinct contact ratio", cex = 1.2)
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$mean,
     ylim= c(0,1), type = "n", xlab = "Country", ylab = "", main = "Adult", xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1, main = "Adult", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 23, cex = 1, main = "Adult", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$lower,
       x1 = seq(1, length(country)),
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 21, cex = 1, main = "Adult", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country)
mtext(side = 2, line = 3, "Temporal over weekly", cex = 1.2)
mtext(side = 2, line = 2, "observed distinct contact ratio", cex = 1.2)
mtext(side = 1, line = 2.5, "Country", cex = 1.2)
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$mean,
     ylim= c(0,1), type = "n", xlab = "Country", ylab = "", main = "Elderly", xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1, main = "Elderly", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 23, cex = 1, main = "Elderly", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$lower,
       x1 = seq(1, length(country)),
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 21, cex = 1, main = "Elderly", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country)
mtext(side = 2, line = 3, "Temporal over weekly", cex = 1.2)
mtext(side = 2, line = 2, "observed distinct contact ratio", cex = 1.2)
mtext(side = 1, line = 2.5, "Country", cex = 1.2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = "bottom",
       legend = c("All contacts", "Contacts outside home", "Contacts at home"), 
       pt.bg= c("red", "blue", "darkgreen"), 
       pch = c(21, 22, 23), cex=1.35, horiz = TRUE, bty = "n")
dev.off()
