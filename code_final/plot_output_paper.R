rm(list = ls())
set.seed(123)
# run first code.R
library(rstudioapi)
library(dplyr)
library(RColorBrewer)
dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
is_physical <- c("physical", "all_contacts")
wave_list <- seq(9, 43)
foi <- c(1.3, 3.3)
source('helper/functions.R')
N = 5000
color_polymod <- brewer.pal(8, "Set2")

#------- plot boxplot for the number of contacts (POLYMOD)
temporary <- environment()
output_to_plot <- list()
count = 1

for(i in country_list){
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

data_to_plot <- do.call("rbind", output_to_plot)
data_to_plot$part_age_cat <- factor(data_to_plot$part_age_cat,
                                    labels = c("Children (0-12 years)", "Teens (13-18 years)",
                                               "Adult (19-65 years)", "Elderly (66+ years)"))

boxplot_polymod_aggregate_physical <- fplot_boxplot_daily_nondaily(dataset = data_to_plot[data_to_plot$contact_type == "physical",],
                                                                   breaks_input = c(0, 5, 15, 30, 60))
ggsave("results/POLYMOD/boxplot_physical_contacts_aggregate.png",
       width = 2200*1.25, height = 1800*1.25, units = "px")

#------- plot boxplot for the number of contacts (CoMix)

data_to_plot_comix <- read.csv("../data/CoMix_Fatigue/redistribute_physical.csv")
data_to_plot_comix$part_age_cat <- factor(data_to_plot_comix$part_age_cat,
                                          labels = c("Children (0-12 years)", "Teens (13-18 years)",
                                                     "Adult (19-65 years)", "Elderly (66+ years)"))
boxplot_comix_aggregate_physical <- fplot_boxplot_daily_nondaily_comix(dataset = data_to_plot_comix,
                                                                       breaks_input = c(0, 5, 10, 20))
ggsave("results/CoMix/boxplot_physical_contacts_aggregate.png",
       width = 2200*1.35, height = 1800*1.25, units = "px")

#------- plot the number of crude numbers (POLYMOD) - physical/ all contacts

storage_list <- list()
count = 1

if(!file.exists("rds/POLYMOD/data_ratio_polymod.rds")){
  for(i in country_list){
    for(cnt in is_physical){
      print(paste0("doing ", i, " for ", cnt, " contacts"))
      title_phys <- ""
      
      if(cnt %in% "physical"){
        title_phys <- "_physical"
      }
      
      N = 5000
      
      temp_env <- env()
      data_temp <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop%s_%s.rds", title_phys, i))
      
      storage_list[[count]] <- cbind(cbind(calculate_ratio_temporal(data_temp), country = i), cnt_type = cnt)
      count = count + 1
    }
  }
  saveRDS(storage_list, "rds/POLYMOD/data_ratio_polymod.rds")
} else {
  storage_list <- readRDS("rds/POLYMOD/data_ratio_polymod.rds")
}

data_ratio_to_plot <- do.call("rbind", storage_list)

data_ratio_to_plot_allcnt <- data_ratio_to_plot[data_ratio_to_plot$cnt_type == "all_contacts",]
data_ratio_to_plot_allcnt$country <- as.factor(data_ratio_to_plot_allcnt$country)
data_ratio_to_plot_physical <- data_ratio_to_plot[data_ratio_to_plot$cnt_type == "physical",]

png(filename = "results/POLYMOD/crude_allcontacts_results.png", width = 600*1.5, height = 600*1.3)
par(oma = c(3,1,0.85,1), mfrow = c(2, 2), mar = c(4.75, 4, 3, 1))
plot(x = seq(1, length(country_list)), 
     y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Children",]$sum_naive,
     ylim= c(0,200), type = "n", xlab = "Country", ylab = "", main = "Children (0-12 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Children",]$sum_naive,
       ylim= c(0,200),
       lty = 2, pch = 21, cex = 1.2, main = "Children (0-12 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Children",]$sum_temp,
       ylim= c(0,200),
       lty = 2, pch = 21, cex = 1.2, main = "Children (0-12 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Children",]$sum_naive,
       ylim= c(0,200),
       lty = 2, pch = 24, cex = 1.2, main = "Children (0-12 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Children",]$sum_temp,
       ylim= c(0,200),
       lty = 2, pch = 24, cex = 1.2, main = "Children (0-12 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(country_list)), 
     y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Teens",]$sum_naive,
     ylim= c(0,200), type = "n", xlab = "Country", ylab = "", main = "Teens (13-18 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Teens",]$sum_naive,
       ylim= c(0,200),
       lty = 2, pch = 21, cex = 1.2, main = "Teens (13-18 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Teens",]$sum_temp,
       ylim= c(0,200),
       lty = 2, pch = 21, cex = 1.2, main = "Teens (13-18 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Teens",]$sum_naive,
       ylim= c(0,200),
       lty = 2, pch = 24, cex = 1.2, main = "Teens (13-18 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Teens",]$sum_temp,
       ylim= c(0,200),
       lty = 2, pch = 24, cex = 1.2, main = "Teens (13-18 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(country_list)), 
     y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Adult",]$sum_naive,
     ylim= c(0,200), type = "n", xlab = "Country", ylab = "", main = "Adults (19-65 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Adult",]$sum_naive,
       ylim= c(0,200),
       lty = 2, pch = 21, cex = 1.2, main = "Adults (19-65 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Adult",]$sum_temp,
       ylim= c(0,200),
       lty = 2, pch = 21, cex = 1.2, main = "Adults (19-65 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Adult",]$sum_naive,
       ylim= c(0,200),
       lty = 2, pch = 24, cex = 1.2, main = "Adults (19-65 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Adult",]$sum_temp,
       ylim= c(0,200),
       lty = 2, pch = 24, cex = 1.2, main = "Adults (19-65 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(country_list)), 
     y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Elderly",]$sum_naive,
     ylim= c(0,200), type = "n", xlab = "Country", ylab = "", main = "Elderly (66+ years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Elderly",]$sum_naive,
       ylim= c(0,200),
       lty = 2, pch = 21, cex = 1.2, main = "Elderly (66+ years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Elderly",]$sum_temp,
       ylim= c(0,200),
       lty = 2, pch = 21, cex = 1.2, main = "Elderly (66+ years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Elderly",]$sum_naive,
       ylim= c(0,200),
       lty = 2, pch = 24, cex = 1.2, main = "Elderly (66+ years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Elderly",]$sum_temp,
       ylim= c(0,200),
       lty = 2, pch = 24, cex = 1.2, main = "Elderly (66+ years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0.5, 0), mar = c(0, 0, 0.5, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = "bottom",
       legend = c("All contacts (Naive)", "All contacts (Frequency-based)",
                  "Physical contacts (Naive)", "Physical contacts (Frequency-based)"),
       pch = c(21, 21, 24, 24), pt.bg = c("#c1121f", "#669bbc", "#c1121f", "#669bbc"),
       cex=1.35, bty = "n", ncol = 2)
dev.off()

#------- plot the number of crude numbers (CoMix) - physical/ all contacts

storage_list_comix <- list()
count = 1

if(!file.exists("rds/CoMix/data_ratio_comix.rds")){
  for(i in wave_list){
    for(cnt in is_physical){
      print(paste0("doing ", i, " for ", cnt, " contacts"))
      title_phys <- ""
      
      if(cnt %in% "physical"){
        title_phys <- "_physical"
      } else {
        title_phys <- "_allcontacts"
      }
      
      N = 5000
      
      temp_env <- env()
      data_temp <- readRDS(sprintf("rds/CoMix/data_temp_CoMix%s_5000pop_%04d.rds", title_phys, i))
      
      storage_list_comix[[count]] <- cbind(cbind(calculate_ratio_temporal(data_temp), wave = i), cnt_type = cnt)
      count = count + 1
    }
  }
  saveRDS(storage_list_comix, "rds/CoMix/data_ratio_comix.rds")
} else {
  storage_list_comix <- readRDS("rds/CoMix/data_ratio_comix.rds")
}

data_ratio_to_plot_comix <- do.call("rbind", storage_list_comix)

data_ratio_to_plot_allcnt <- data_ratio_to_plot_comix[data_ratio_to_plot_comix$cnt_type == "all_contacts",]
data_ratio_to_plot_physical <- data_ratio_to_plot_comix[data_ratio_to_plot_comix$cnt_type == "physical",]

png(filename = "results/CoMix/crude_allcontacts_results.png", width = 600*1.5, height = 600*1.2)
par(oma = c(3,1,0.85,1), mfrow = c(2, 2), mar = c(4.75, 4, 3, 1))
plot(x = seq(1, length(wave_list)), 
     y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Children",]$sum_naive,
     ylim= c(0,40), type = "n", xlab = "Wave", ylab = "", main = "Children (0-12 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Children",]$sum_naive,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 21, cex = 1.2, main = "Children (0-12 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Children",]$sum_temp,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 21, cex = 1.2, main = "Children (0-12 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Children",]$sum_naive,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Children (0-12 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Children",]$sum_temp,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Children (0-12 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(wave_list)), 
     y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Teens",]$sum_naive,
     ylim= c(0,40), type = "n", xlab = "Wave", ylab = "", main = "Teens (13-18 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Teens",]$sum_naive,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 21, cex = 1.2, main = "Teens (13-18 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Teens",]$sum_temp,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 21, cex = 1.2, main = "Teens (13-18 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Teens",]$sum_naive,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Teens (13-18 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Teens",]$sum_temp,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Teens (13-18 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(wave_list)), 
     y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Adult",]$sum_naive,
     ylim= c(0,40), type = "n", xlab = "Wave", ylab = "", main = "Adults (19-65 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Adult",]$sum_naive,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 21, cex = 1, main = "Adults (19-65 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Adult",]$sum_temp,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 21, cex = 1, main = "Adults (19-65 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Adult",]$sum_naive,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 24, cex = 1, main = "Adults (19-65 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Adult",]$sum_temp,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 24, cex = 1, main = "Adults (19-65 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(wave_list)), 
     y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Elderly",]$sum_naive,
     ylim= c(0,40), type = "n", xlab = "Wave", ylab = "", main = "Elderly (66+ years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Elderly",]$sum_naive,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 21, cex = 1, main = "Elderly (66+ years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_allcnt[data_ratio_to_plot_allcnt$part_age_cat == "Elderly",]$sum_temp,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 21, cex = 1, main = "Elderly (66+ years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Elderly",]$sum_naive,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 24, cex = 1, main = "Elderly (66+ years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = data_ratio_to_plot_physical[data_ratio_to_plot_physical$part_age_cat == "Elderly",]$sum_temp,
       ylim= c(0,40), type = "b",
       lty = 2, pch = 24, cex = 1, main = "Elderly (66+ years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0.5, 0), mar = c(0, 0, 0.5, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = "bottom",
       legend = c("All contacts (Naive)", "All contacts (Frequency-based)",
                  "Physical contacts (Naive)", "Physical contacts (Frequency-based)"),
       pch = c(21, 21, 24, 24), pt.bg = c("#c1121f", "#669bbc", "#c1121f", "#669bbc"),
       cex=1.35, bty = "n", ncol = 2)
dev.off()


#------- plot the ratio over temporal and observed weekly distinct contacts for physical (POLYMOD)
storage_list_physical <- list()
if(!file.exists("rds/POLYMOD/data_temp_physical_all_withci.rds")){
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    temp_env <- env()
    temp <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))
    
    storage_list_physical[[i]] <- cbind(calculate_ratio_temporal_withci(temp), country = country_input)
  }
  saveRDS(storage_list_physical, sprintf("rds/POLYMOD/data_temp_physical_all_withci.rds"))
} else {
  storage_list_physical <- readRDS(sprintf("rds/POLYMOD/data_temp_physical_all_withci.rds"))
}

storage_data_physical_withci <- do.call(rbind, storage_list_physical)
storage_data_physical_withci$part_age_cat <- factor(storage_data_physical_withci$part_age_cat,
                                                    levels = c("Children", "Teens", "Adult", "Elderly"),
                                                    labels = c("Children (0-12 years)", "Teens (13-18 years)",
                                                               "Adult (19-65 years)", "Elderly (66+ years)"))

mean(storage_data_physical_withci$mean)
mean(storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Children (0-12 years)",]$mean)
mean(storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Teens (13-18 years)",]$mean)
mean(storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Adult (19-65 years)",]$mean)
mean(storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Elderly (66+ years)",]$mean)

temp <- ggplot(data = storage_data_physical_withci) +
  geom_point(aes(country, mean, color = country),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(x = country, ymin=lower, ymax=upper, width=0.5,
                    color = country),
                position = position_dodge(width = 0.5),
                linewidth = 1) +
  facet_wrap(~ part_age_cat, nrow = 1) +
  ylab("Ratio of frequency-based to naive \n total number of weekly distinct contacts") +
  xlab("Country") +
  scale_color_manual(values = color_polymod) +
  theme_bw() +
  theme(legend.position = "top",
        legend.title=element_blank(),
        text = element_text(size = 20),
        strip.background = element_rect(fill = "#edf2f4"),
        strip.text = element_text(face = "bold")) +
  guides(colour = guide_legend(nrow = 1))

ggsave("results/POLYMOD/ggplot_ratio_onlyphysical_results_withci_temp.png",
       width = 1350*3, height = 675*3, units = "px")

#------- plot the ratio over temporal and observed weekly distinct contacts for physical (CoMix)
storage_list_physical <- list()
if(!file.exists("rds/CoMix/data_temp_physical_all_withci.rds")){
  for(i in 1:length(wave_list)){
    wave_input = wave_list[i]
    print(sprintf("doing wave %s for physical contacts", wave_input))
    temp_env <- env()
    temp <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_physical_5000pop_%04d.rds", as.integer(wave_input)))
    
    storage_list_physical[[i]] <- cbind(calculate_ratio_temporal_withci(temp), wave = wave_input)
  }
  saveRDS(storage_list_physical, sprintf("rds/CoMix/data_temp_physical_all_withci.rds"))
} else {
  storage_list_physical <- readRDS(sprintf("rds/CoMix/data_temp_physical_all_withci.rds"))
}

storage_data_physical_withci <- do.call(rbind, storage_list_physical)

png(filename = "results/CoMix/ratio_allcontacts_onlyphysical_results_withci.png", width = 600*1.3, height = 700)
par(oma = c(3,1,0.85,1), mfrow = c(2, 2), mar = c(4.75, 4, 3, 1))
plot(x = seq(1, length(wave_list)), 
     y = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Children",]$mean,
     ylim= c(0.1,0.55), type = "n", xlab = "Wave", ylab = "", main = "Children (0-12 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_list)),
       y0 = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Children",]$lower,
       x1 = seq(1, length(wave_list)),
       y1 = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "darkslateblue", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_list)), 
       y = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Children",]$mean,
       ylim= c(0.1,0.55), type = "b",
       lty = 2, pch = 22, cex = 1, main = "Children (0-12 years)", bg = "darkslateblue",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(wave_list)), 
     y = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Teens",]$mean,
     ylim= c(0.1,0.55), type = "n", xlab = "Wave", ylab = "", main = "Teens (13-18 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_list)),
       y0 = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Teens",]$lower,
       x1 = seq(1, length(wave_list)),
       y1 = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "darkslateblue", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_list)), 
       y = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Teens",]$mean,
       ylim= c(0.1,0.55), type = "b",
       lty = 2, pch = 22, cex = 1, main = "Teens (13-18 years)", bg = "darkslateblue",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(wave_list)), 
     y = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Adult",]$mean,
     ylim= c(0.1,0.55), type = "n", xlab = "Wave", ylab = "", main = "Adults (19-65 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_list)),
       y0 = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Adult",]$lower,
       x1 = seq(1, length(wave_list)),
       y1 = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "darkslateblue", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_list)), 
       y = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Adult",]$mean,
       ylim= c(0.1,0.55), type = "b",
       lty = 2, pch = 22, cex = 1, main = "Adults (19-65 years)", bg = "darkslateblue",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n",
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(wave_list)), 
     y = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Elderly",]$mean,
     ylim= c(0.1,0.55), type = "n", xlab = "Wave", ylab = "", main = "Elderly (66+ years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_list)),
       y0 = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Elderly",]$lower,
       x1 = seq(1, length(wave_list)),
       y1 = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "darkslateblue", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_list)), 
       y = storage_data_physical_withci[storage_data_physical_withci$part_age_cat == "Elderly",]$mean,
       ylim= c(0.1,0.55), type = "b",
       lty = 2, pch = 22, cex = 1, main = "Elderly (66+ years)", bg = "darkslateblue",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
dev.off()

#------- plot the ratio over temporal and observed weekly distinct contacts all contacts (POLYMOD)
storage_list_allcontacts <- list()
if(!file.exists("rds/POLYMOD/data_temp_allcontacts_all_withci.rds")){
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("doing %s for all contacts", country_input))
    temp_env <- env()
    temp <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop_%s.rds", country_input))
    
    storage_list_allcontacts[[i]] <- cbind(calculate_ratio_temporal_withci(temp), country = country_input)
  }
  saveRDS(storage_list_allcontacts, sprintf("rds/POLYMOD/data_temp_allcontacts_all_withci.rds"))
} else {
  storage_list_allcontacts <- readRDS(sprintf("rds/POLYMOD/data_temp_allcontacts_all_withci.rds"))
}

storage_data_allcontacts_withci <- do.call(rbind, storage_list_allcontacts)
storage_data_allcontacts_withci$part_age_cat <- factor(storage_data_allcontacts_withci$part_age_cat,
                                                    levels = c("Children", "Teens", "Adult", "Elderly"),
                                                    labels = c("Children (0-12 years)", "Teens (13-18 years)",
                                                               "Adult (19-65 years)", "Elderly (66+ years)"))

mean(storage_data_allcontacts_withci$mean)
mean(storage_data_allcontacts_withci[storage_data_allcontacts_withci$part_age_cat == "Children (0-12 years)",]$mean)
mean(storage_data_allcontacts_withci[storage_data_allcontacts_withci$part_age_cat == "Teens (13-18 years)",]$mean)
mean(storage_data_allcontacts_withci[storage_data_allcontacts_withci$part_age_cat == "Adult (19-65 years)",]$mean)
mean(storage_data_allcontacts_withci[storage_data_allcontacts_withci$part_age_cat == "Elderly (66+ years)",]$mean)

temp_allcontacts <- ggplot(data = storage_data_allcontacts_withci) +
  geom_point(aes(country, mean, color = country),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(x = country, ymin=lower, ymax=upper, width=0.5,
                    color = country),
                position = position_dodge(width = 0.5),
                linewidth = 1) +
  facet_wrap(~ part_age_cat, nrow = 1) +
  ylab("Ratio of frequency-based to naive \n total number of weekly distinct contacts") +
  xlab("Country") +
  scale_color_manual(values = color_polymod) +
  theme_bw() +
  theme(legend.position = "top",
        legend.title=element_blank(),
        text = element_text(size = 18),
        strip.background = element_rect(fill = "#edf2f4"),
        strip.text = element_text(face = "bold")) +
  guides(colour = guide_legend(nrow = 1))

ggsave("results/POLYMOD/ggplot_ratio_allcontacts_results_withci_temp.png",
       width = 1350*3, height = 675*3, units = "px")

#------- attack rates for physical contacts (POLYMOD) ---> For the same q
temp_env_baseline <- environment()
temp_env_frequency <- environment()

nboot = 3000

# if the summary of the outbreak characteristics is not present, then summarise. Otherwise,
# use the summarise outbreak characteristics.

R0 <- c(1.3 , 3.3)
if(!file.exists("rds/POLYMOD/attack_rates/samefoi/attack_rate_physical_baseline_00001_BE.rds")){
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    
    population_data <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
    for(j in 1:length(R0)){
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }
      
      attack_rate_baseline <- list()
      attack_rate_temporal <- list()
      
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){
        print(sprintf("doing country %s for foi %s and %05d boot", i, j, b))
        load(sprintf("rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                     country_input, j, b), temp_env_baseline)
        load(sprintf("rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                     country_input, j, b), temp_env_frequency)
        
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
          attack_rate_temporal[[count_temp]] <-  temp_ar_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      attack_rate_baseline_dataframe <- do.call("rbind", attack_rate_baseline)
      attack_rate_temporal_dataframe <- do.call("rbind", attack_rate_temporal)
      
      saveRDS(attack_rate_baseline_dataframe, 
              sprintf("rds/POLYMOD/attack_rates/samefoi/attack_rate_physical_baseline_%05d_%s.rds", as.integer(j), country_input))
      saveRDS(attack_rate_temporal_dataframe, 
              sprintf("rds/POLYMOD/attack_rates/samefoi/attack_rate_physical_temporal_%05d_%s.rds", as.integer(j), country_input))
    }
  }
} else {
  
  ar_baseline_flu <- list()
  ar_temporal_flu <- list()
  ar_baseline_covid <- list()
  ar_temporal_covid <- list()
  
  path <- "rds/POLYMOD/attack_rates/samefoi"
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("loading %s", country_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/attack_rate_physical_baseline_%05d_%s.rds", path, as.integer(j), country_input)))
        temp_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/attack_rate_physical_temporal_%05d_%s.rds", path, as.integer(j), country_input)))
        ar_baseline_flu[[i]] <- base_flu
        ar_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/attack_rate_physical_baseline_%05d_%s.rds", path, as.integer(j), country_input)))
        temp_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/attack_rate_physical_temporal_%05d_%s.rds", path, as.integer(j), country_input)))
        ar_baseline_covid[[i]] <- base_covid
        ar_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_ar_comparison_with_ci <- function(ar_baseline_flu, 
                                            ar_temporal_flu){
  
  country_length <- length(ar_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  age_cat <- c("Children", "Teens", "Adult", "Elderly")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    ar_baseline_sub <- ar_baseline_flu[[i]]
    ar_temporal_sub <- ar_temporal_flu[[i]]
    for(a in age_cat){
      temp_baseline_boot <- ar_baseline_sub[ar_baseline_sub$part_age_cat %in% a, ]
      temp_temporal_boot <- ar_temporal_sub[ar_temporal_sub$part_age_cat %in% a, ]
      
      length_baseline_boot <- nrow(temp_baseline_boot)
      length_temporal_boot <- nrow(temp_temporal_boot)
      ar_ratio <- c()
      for(b in 1:nboot){
        boot_baseline <- mean(sample(temp_baseline_boot$attack_rates, length_baseline_boot, replace = T))
        boot_temporal <- mean(sample(temp_temporal_boot$attack_rates, length_temporal_boot, replace = T))
        
        ar_ratio[b] <- boot_temporal/boot_baseline
      }
      output[count, "country"] <- country_list[i]
      output[count, "age_cat"] <- a
      output[count, "lower"] <- quantile(ar_ratio, probs = 0.025)
      output[count, "attack_rates"] <- mean(ar_ratio)
      output[count, "upper"] <- quantile(ar_ratio, probs = 0.975)
      
      count = count + 1
      print(count)
    }
  }
  return(output)
}

output_flu <- calculate_ar_comparison_with_ci(ar_baseline_flu,
                                              ar_temporal_flu)

output_covid <- calculate_ar_comparison_with_ci(ar_baseline_covid,
                                                ar_temporal_covid)

calculate_correction_of_ar_comparison_with_ci <- function(ar_baseline_flu, 
                                                          ar_temporal_flu){
  
  country_length <- length(ar_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  age_cat <- c("Children", "Teens", "Adult", "Elderly")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    ar_baseline_sub <- ar_baseline_flu[[i]]
    ar_temporal_sub <- ar_temporal_flu[[i]]
    for(a in age_cat){
      temp_baseline_boot <- ar_baseline_sub[ar_baseline_sub$part_age_cat %in% a, ]
      temp_temporal_boot <- ar_temporal_sub[ar_temporal_sub$part_age_cat %in% a, ]
      
      length_baseline_boot <- nrow(temp_baseline_boot)
      length_temporal_boot <- nrow(temp_temporal_boot)
      ar_ratio <- c()
      for(b in 1:nboot){
        boot_baseline <- mean(sample(temp_baseline_boot$attack_rates, length_baseline_boot, replace = T))
        boot_temporal <- mean(sample(temp_temporal_boot$attack_rates, length_temporal_boot, replace = T))
        
        ar_ratio[b] <- boot_baseline/boot_temporal
      }
      output[count, "country"] <- country_list[i]
      output[count, "age_cat"] <- a
      output[count, "lower"] <- quantile(ar_ratio, probs = 0.025)
      output[count, "correction"] <- mean(ar_ratio)
      output[count, "upper"] <- quantile(ar_ratio, probs = 0.975)
      
      count = count + 1
      print(count)
    }
  }
  return(output)
}

correction_flu <- calculate_correction_of_ar_comparison_with_ci(ar_baseline_flu,
                                                                ar_temporal_flu)

correction_covid <- calculate_correction_of_ar_comparison_with_ci(ar_baseline_covid,
                                                                  ar_temporal_covid)

output_correction_polymod <- rbind(cbind(correction_flu, scenario = "Influenza-like"),
                                   cbind(correction_covid, scenario = "COVID-19-like"))

output_correction_polymod$scenario <- factor(output_correction_polymod$scenario,
                                             levels = c("Influenza-like", "COVID-19-like"))

output_correction_polymod$age_cat <- factor(output_correction_polymod$age_cat,
                                            levels = c("Children", "Teens", "Adult", "Elderly"),
                                            labels = c("Children (0-12 years)", "Teens (13-18 years)",
                                                       "Adult (19-65 years)", "Elderly (66+ years"))

output_correction_polymod %>% 
  group_by(scenario) %>% 
  summarise(mean = mean(correction))

output_correction_polymod_influenza <- ggplot(data = output_correction_polymod[output_correction_polymod$scenario == "Influenza-like",]) +
  geom_point(aes(country, correction, col = country)) +
  geom_errorbar(aes(x = country, ymin=lower, ymax=upper, width=0.3, col = country), linewidth = 1) +
  facet_wrap(~ age_cat, ncol = 4) +
  ylab("Changes in attack rates") +
  xlab("Country") +
  theme_bw() +
  scale_color_manual(values = color_polymod) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 20),
        strip.background = element_rect(fill = "#edf2f4"),
        strip.text = element_text(face = "bold")) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("results/POLYMOD/ggplot_correction_onlyphysical_results_withci_influenza.png",
       width = 35, height = 16, units = "cm")

output_correction_polymod_covid <- ggplot(data = output_correction_polymod[output_correction_polymod$scenario == "COVID-19-like",]) +
  geom_point(aes(country, correction, col = country)) +
  geom_errorbar(aes(x = country, ymin=lower, ymax=upper, width=0.3, col = country), linewidth = 1) +
  facet_wrap(~ age_cat, ncol = 4) +
  ylab("Changes in attack rates") +
  xlab("Country") +
  theme_bw() +
  scale_color_manual(values = color_polymod) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 20),
        strip.background = element_rect(fill = "#edf2f4"),
        strip.text = element_text(face = "bold")) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("results/POLYMOD/ggplot_correction_onlyphysical_results_withci_covid.png",
       width = 35, height = 16, units = "cm")

#------- attack rates for physical contacts (CoMix)
nboot = 3000
wave_epi <- c(19, 27, 43)

temp_env_baseline <- environment()
temp_env_frequency <- environment()
if(!file.exists("rds/CoMix/attack_rates/attack_rate_physical_baseline_00001_0019.rds")){
  for(i in 1:length(wave_epi)){
    wave_epi_input = wave_epi[i]
    print(sprintf("doing wave %s for physical contacts", wave_epi_input))
    
    population_data <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_physical_5000pop_%04d.rds", wave_epi_input))$dataset.res
    
    for(j in 1:length(R0)){
      
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }
      
      attack_rate_baseline <- list()
      attack_rate_temporal <- list()
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){
        print(sprintf("doing wave %03d for foi %05d and %05d boot", as.integer(wave_epi_input), as.integer(j), as.integer(b)))
        
        load(sprintf("rds/CoMix/output_CoMix_wave_physical_%04d/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                     wave_epi_input, j, b), temp_env_baseline)
        load(sprintf("rds/CoMix/output_CoMix_wave_physical_%04d/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                     wave_epi_input, j, b), temp_env_frequency)
        
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
          attack_rate_temporal[[count_temp]] <-  temp_ar_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      attack_rate_baseline_dataframe <- do.call("rbind", attack_rate_baseline)
      attack_rate_temporal_dataframe <- do.call("rbind", attack_rate_temporal)
      
      saveRDS(attack_rate_baseline_dataframe, 
              sprintf("rds/CoMix/attack_rates/attack_rate_physical_baseline_%05d_%04d.rds", as.integer(j), wave_epi_input))
      saveRDS(attack_rate_temporal_dataframe, 
              sprintf("rds/CoMix/attack_rates/attack_rate_physical_temporal_%05d_%04d.rds", as.integer(j), wave_epi_input))
    }
  }
} else {
  
  ar_baseline_flu <- list()
  ar_temporal_flu <- list()
  ar_baseline_covid <- list()
  ar_temporal_covid <- list()
  
  path <- "rds/CoMix/attack_rates"
  for(i in 1:length(wave_epi)){
    wave_epi_input = wave_epi[i]
    print(sprintf("loading wave %s", wave_epi_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(wave = wave_epi_input,
                               readRDS(sprintf("%s/attack_rate_physical_baseline_%05d_%04d.rds", path, as.integer(j), wave_epi_input)))
        temp_flu <- data.frame(wave = wave_epi_input,
                               readRDS(sprintf("%s/attack_rate_physical_temporal_%05d_%04d.rds", path, as.integer(j), wave_epi_input)))
        ar_baseline_flu[[i]] <- base_flu
        ar_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(wave = wave_epi_input,
                                 readRDS(sprintf("%s/attack_rate_physical_baseline_%05d_%04d.rds", path, as.integer(j), wave_epi_input)))
        temp_covid <- data.frame(wave = wave_epi_input,
                                 readRDS(sprintf("%s/attack_rate_physical_temporal_%05d_%04d.rds", path, as.integer(j), wave_epi_input)))
        ar_baseline_covid[[i]] <- base_covid
        ar_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_ar_comix_comparison_with_ci <- function(ar_baseline_flu, 
                                                  ar_temporal_flu){
  
  wave_length <- length(ar_baseline_flu)
  wave_list <- c("19", "27", "43")
  age_cat <- c("Children", "Teens", "Adult", "Elderly")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:wave_length){
    ar_baseline_sub <- ar_baseline_flu[[i]]
    ar_temporal_sub <- ar_temporal_flu[[i]]
    for(a in age_cat){
      temp_baseline_boot <- ar_baseline_sub[ar_baseline_sub$part_age_cat %in% a, ]
      temp_temporal_boot <- ar_temporal_sub[ar_temporal_sub$part_age_cat %in% a, ]
      
      length_baseline_boot <- nrow(temp_baseline_boot)
      length_temporal_boot <- nrow(temp_temporal_boot)
      ar_ratio <- c()
      for(b in 1:nboot){
        boot_baseline <- mean(sample(temp_baseline_boot$attack_rates, length_baseline_boot, replace = T))
        boot_temporal <- mean(sample(temp_temporal_boot$attack_rates, length_temporal_boot, replace = T))
        
        ar_ratio[b] <- boot_temporal/boot_baseline
      }
      output[count, "wave"] <- wave_list[i]
      output[count, "age_cat"] <- a
      output[count, "lower"] <- quantile(ar_ratio, probs = 0.025)
      output[count, "attack_rates"] <- mean(ar_ratio)
      output[count, "upper"] <- quantile(ar_ratio, probs = 0.975)
      
      count = count + 1
      print(count)
    }
  }
  return(output)
}

output_flu <- calculate_ar_comix_comparison_with_ci(ar_baseline_flu,
                                                    ar_temporal_flu)

output_covid <- calculate_ar_comix_comparison_with_ci(ar_baseline_covid,
                                                      ar_temporal_covid)

png(filename = "results/CoMix/ratio_onlyphysical_attackrates_flu_withci.png", width = 650*1.2, height = 600)
par(oma = c(3,1,0.85,1), mfrow = c(2, 2), mar = c(4.75, 4, 3, 1))
plot(x = seq(1, length(wave_epi)), 
     y = output_flu[output_flu$age_cat == "Children",]$attack_rates,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Children (0-12 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_flu[output_flu$age_cat == "Children",]$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_flu[output_flu$age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_epi)), 
       y = output_flu[output_flu$age_cat == "Children",]$attack_rates,
       ylim= c(0,1.2),
       lty = 2, pch = 22, cex = 1, main = "Children (0-12 years)", bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "attack rates", cex = 1.2)
mtext(side = 1, line = 2.5, "Wave", cex = 1.2)
plot(x = seq(1, length(wave_epi)), 
     y = output_flu[output_flu$age_cat == "Teens",]$attack_rates,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Teens (13-18 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_flu[output_flu$age_cat == "Teens",]$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_flu[output_flu$age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_epi)), 
       y = output_flu[output_flu$age_cat == "Teens",]$attack_rates,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1, main = "Teens (13-18 years)", bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "attack rates", cex = 1.2)
mtext(side = 1, line = 2.5, "Wave", cex = 1.2)
plot(x = seq(1, length(wave_epi)), 
     y = output_flu[output_flu$age_cat == "Adult",]$attack_rates,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Adults (19-65 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_flu[output_flu$age_cat == "Adult",]$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_flu[output_flu$age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_epi)), 
       y = output_flu[output_flu$age_cat == "Adult",]$attack_rates,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1, main = "Adults (19-65 years)", bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "attack rates", cex = 1.2)
mtext(side = 1, line = 2.5, "Wave", cex = 1.2)
plot(x = seq(1, length(wave_epi)), 
     y = output_flu[output_flu$age_cat == "Elderly",]$attack_rates,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Elderly (66+ years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_flu[output_flu$age_cat == "Elderly",]$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_flu[output_flu$age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_epi)), 
       y = output_flu[output_flu$age_cat == "Elderly",]$attack_rates,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1, main = "Elderly (66+ years)", bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "attack rates", cex = 1.2)
mtext(side = 1, line = 2.5, "Wave", cex = 1.2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
dev.off()

png(filename = "results/CoMix/ratio_onlyphysical_attackrates_covid_withci.png", width = 650*1.2, height = 600)
par(oma = c(3,1,0.85,1), mfrow = c(2, 2), mar = c(4.75, 4, 3, 1))
plot(x = seq(1, length(wave_epi)), 
     y = output_covid[output_covid$age_cat == "Children",]$attack_rates,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Children (0-12 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_covid[output_covid$age_cat == "Children",]$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_covid[output_covid$age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_epi)), 
       y = output_covid[output_covid$age_cat == "Children",]$attack_rates,
       ylim= c(0,1.2),
       lty = 2, pch = 22, cex = 1, main = "Children (0-12 years)", bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "attack rates", cex = 1.2)
mtext(side = 1, line = 2.5, "Wave", cex = 1.2)
plot(x = seq(1, length(wave_epi)), 
     y = output_covid[output_covid$age_cat == "Teens",]$attack_rates,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Teens (13-18 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_covid[output_covid$age_cat == "Teens",]$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_covid[output_covid$age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_epi)), 
       y = output_covid[output_covid$age_cat == "Teens",]$attack_rates,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1, main = "Teens (13-18 years)", bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "attack rates", cex = 1.2)
mtext(side = 1, line = 2.5, "Wave", cex = 1.2)
plot(x = seq(1, length(wave_epi)), 
     y = output_covid[output_covid$age_cat == "Adult",]$attack_rates,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Adults (19-65 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_covid[output_covid$age_cat == "Adult",]$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_covid[output_covid$age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_epi)), 
       y = output_covid[output_covid$age_cat == "Adult",]$attack_rates,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1, main = "Adults (19-65 years)", bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "attack rates", cex = 1.2)
mtext(side = 1, line = 2.5, "Wave", cex = 1.2)
plot(x = seq(1, length(wave_epi)), 
     y = output_covid[output_covid$age_cat == "Elderly",]$attack_rates,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Elderly (66+ years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_covid[output_covid$age_cat == "Elderly",]$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_covid[output_covid$age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 1.5)
points(x = seq(1, length(wave_epi)), 
       y = output_covid[output_covid$age_cat == "Elderly",]$attack_rates,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1, main = "Elderly (66+ years)", bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Ratio of frequency-based to naive", cex = 1.2)
mtext(side = 2, line = 2.5, "attack rates", cex = 1.2)
mtext(side = 1, line = 2.5, "Wave", cex = 1.2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
dev.off()

#------- peak incidence for physical contacts (POLYMOD)
nboot = 3000

if(!file.exists("rds/POLYMOD/peak_incidence/samefoi/peak_incidence_physical_baseline_00001_BE.rds")){
  for(i in 1:length(country_list)){
    peak_incidence_baseline <- list()
    peak_incidence_temporal <- list()
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    population_data <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
    for(j in 1:length(R0)){
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){ 
        temp_env_baseline <- environment()
        temp_env_frequency <- environment()
        print(sprintf("doing country %s for foi %s and %05d boot", i, j, b))
        load(sprintf("rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                     country_input, j, b), temp_env_baseline)
        load(sprintf("rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_frequency_%03d_%05d.RData",
                     country_input, j, b), temp_env_frequency)
        
        temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
        temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_baseline > 0.1*N){
          pi_baseline <- max(temp_env_baseline$output.simulate.baseline.vsc$infected_daily)
          temp_pi_baseline <- data.frame(pi_baseline, boot = b)
          peak_incidence_baseline[[count_base]] <-  temp_pi_baseline
          count_base = count_base + 1
        }
        
        if(temp_sum_temporal > 0.1*N){
          pi_temporal <- max(temp_env_frequency$output.simulate.vsc$infected.daily)
          temp_pi_temporal <- data.frame(pi_temporal, boot = b)
          peak_incidence_temporal[[count_temp]] <-  temp_pi_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      peak_incidence_baseline_dataframe <- do.call("rbind", peak_incidence_baseline)
      peak_incidence_temporal_dataframe <- do.call("rbind", peak_incidence_temporal)
      
      saveRDS(peak_incidence_baseline_dataframe, 
              sprintf("rds/POLYMOD/peak_incidence/samefoi/peak_incidence_physical_baseline_%05d_%s.rds", as.integer(j), country_input))
      saveRDS(peak_incidence_temporal_dataframe,
              sprintf("rds/POLYMOD/peak_incidence/samefoi/peak_incidence_physical_temporal_%05d_%s.rds", as.integer(j), country_input))
    }
  }
} else {
  
  pi_baseline_flu <- list()
  pi_temporal_flu <- list()
  pi_baseline_covid <- list()
  pi_temporal_covid <- list()
  
  path <- "rds/POLYMOD/peak_incidence/samefoi"
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("loading %s", country_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/peak_incidence_physical_baseline_%05d_%s.rds", path, as.integer(j), country_input)))
        temp_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/peak_incidence_physical_temporal_%05d_%s.rds", path, as.integer(j), country_input)))
        pi_baseline_flu[[i]] <- base_flu
        pi_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/peak_incidence_physical_baseline_%05d_%s.rds", path, as.integer(j), country_input)))
        temp_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/peak_incidence_physical_temporal_%05d_%s.rds", path, as.integer(j), country_input)))
        pi_baseline_covid[[i]] <- base_covid
        pi_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_pi_comparison_with_ci <- function(pi_baseline_flu, 
                                            pi_temporal_flu){
  
  country_length <- length(pi_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    pi_baseline_sub <- pi_baseline_flu[[i]]
    pi_temporal_sub <- pi_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(pi_baseline_sub)
    length_temporal_boot <- nrow(pi_temporal_sub)
    pi_ratio <- c()
      for(b in 1:nboot){
        boot_baseline <- mean(sample(pi_baseline_sub$pi_baseline, length_baseline_boot, replace = T))
        boot_temporal <- mean(sample(pi_temporal_sub$pi_temporal, length_temporal_boot, replace = T))
        
        pi_ratio[b] <- boot_temporal/boot_baseline
      }
      output[count, "country"] <- country_list[i]
      output[count, "lower"] <- quantile(pi_ratio, probs = 0.025)
      output[count, "peak_incidence"] <- mean(pi_ratio)
      output[count, "upper"] <- quantile(pi_ratio, probs = 0.975)
      
      count = count + 1
      print(count)
  }
  return(output)
}

output_pi_flu <- calculate_pi_comparison_with_ci(pi_baseline_flu,
                                                 pi_temporal_flu)

output_pi_covid <- calculate_pi_comparison_with_ci(pi_baseline_covid,
                                                pi_temporal_covid)

png(filename = "results/POLYMOD/ratio_onlyphysical_peakincidence_withci.png", width = 650*1.3, height = 450)
par(oma = c(2,1,0.75,1), mfrow = c(1, 2), mar = c(4.5, 4, 4, 1.5))
plot(x = seq(1, length(country_list)), 
     y = output_pi_flu$peak_incidence,
     ylim= c(0,1.25), type = "n", xlab = "Country", ylab = "", main = "Influenza-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_pi_flu$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_pi_flu$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_pi_flu$peak_incidence,
       ylim= c(0,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n", yaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "peak incidence", cex = 1.5)
plot(x = seq(1, length(country_list)), 
     y = output_pi_covid$peak_incidence,
     ylim= c(0,1.25), type = "n", xlab = "Country", ylab = "", main = "COVID-19-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_pi_covid$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_pi_covid$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_pi_covid$peak_incidence,
       ylim= c(0,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n", yaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "peak incidence", cex = 1.5)
dev.off()

#------- peak incidence for physical contacts (CoMix)
nboot = 3000

if(!file.exists("rds/CoMix/peak_incidence/peak_incidence_physical_baseline_00001_0019.rds")){
  for(i in 1:length(wave_epi)){
    peak_incidence_baseline <- list()
    peak_incidence_temporal <- list()
    wave_input = wave_epi[i]
    print(sprintf("doing %s for physical contacts", wave_input))
    
    population_data <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_physical_5000pop_%04d.rds", wave_input))$dataset.res
    for(j in 1:length(R0)){
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }
      
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){ 
        temp_env_baseline <- environment()
        temp_env_frequency <- environment()
        
        print(sprintf("doing wave %s for foi %s and %05d boot", wave_input, j, b))
        load(sprintf("rds/CoMix/output_CoMix_wave_physical_%04d/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                     wave_epi_input, j, b), temp_env_baseline)
        load(sprintf("rds/CoMix/output_CoMix_wave_physical_%04d/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                     wave_epi_input, j, b), temp_env_frequency)
        
        temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
        temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_baseline > 0.1*N){
          pi_baseline <- max(temp_env_baseline$output.simulate.baseline.vsc$infected_daily)
          temp_pi_baseline <- data.frame(pi_baseline, boot = b)
          peak_incidence_baseline[[count_base]] <-  temp_pi_baseline
          count_base = count_base + 1
        }
        
        if(temp_sum_temporal > 0.1*N){
          pi_temporal <- max(temp_env_frequency$output.simulate.vsc$infected.daily)
          temp_pi_temporal <- data.frame(pi_temporal, boot = b)
          peak_incidence_temporal[[count_temp]] <-  temp_pi_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      peak_incidence_baseline_dataframe <- do.call("rbind", peak_incidence_baseline)
      peak_incidence_temporal_dataframe <- do.call("rbind", peak_incidence_temporal)
      
      saveRDS(peak_incidence_baseline_dataframe, 
              sprintf("rds/CoMix/peak_incidence/peak_incidence_physical_baseline_%05d_%04d.rds", as.integer(j), as.integer(wave_input)))
      saveRDS(peak_incidence_temporal_dataframe, 
              sprintf("rds/CoMix/peak_incidence/peak_incidence_physical_temporal_%05d_%04d.rds", as.integer(j), as.integer(wave_input)))
    }
  }
} else {
  
  pi_baseline_flu <- list()
  pi_temporal_flu <- list()
  pi_baseline_covid <- list()
  pi_temporal_covid <- list()
  
  path <- "rds/CoMix/peak_incidence"
  for(i in 1:length(wave_epi)){
    wave_input = wave_epi[i]
    print(sprintf("loading %s", wave_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(wave = wave_input,
                               readRDS(sprintf("%s/peak_incidence_physical_baseline_%05d_%04d.rds", path, as.integer(j), wave_input)))
        temp_flu <- data.frame(wave = wave_input,
                               readRDS(sprintf("%s/peak_incidence_physical_temporal_%05d_%04d.rds", path, as.integer(j), wave_input)))
        pi_baseline_flu[[i]] <- base_flu
        pi_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(wave = wave_input,
                                 readRDS(sprintf("%s/peak_incidence_physical_baseline_%05d_%04d.rds", path, as.integer(j), wave_input)))
        temp_covid <- data.frame(wave = wave_input,
                                 readRDS(sprintf("%s/peak_incidence_physical_temporal_%05d_%04d.rds", path, as.integer(j), wave_input)))
        pi_baseline_covid[[i]] <- base_covid
        pi_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_pi_comix_comparison_with_ci <- function(pi_baseline_flu, 
                                                  pi_temporal_flu){
  
  wave_length <- length(pi_baseline_flu)
  wave_list <- c("19", "27", "43")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:wave_length){
    pi_baseline_sub <- pi_baseline_flu[[i]]
    pi_temporal_sub <- pi_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(pi_baseline_sub)
    length_temporal_boot <- nrow(pi_temporal_sub)
    pi_ratio <- c()
    for(b in 1:nboot){
      boot_baseline <- mean(sample(pi_baseline_sub$pi_baseline, length_baseline_boot, replace = T))
      boot_temporal <- mean(sample(pi_temporal_sub$pi_temporal, length_temporal_boot, replace = T))
      
      pi_ratio[b] <- boot_temporal/boot_baseline
    }
    output[count, "wave"] <- wave_list[i]
    output[count, "lower"] <- quantile(pi_ratio, probs = 0.025)
    output[count, "peak_incidence"] <- mean(pi_ratio)
    output[count, "upper"] <- quantile(pi_ratio, probs = 0.975)
    
    count = count + 1
    print(count)
  }
  return(output)
}

output_pi_flu <- calculate_pi_comix_comparison_with_ci(pi_baseline_flu,
                                                       pi_temporal_flu)

output_pi_covid <- calculate_pi_comix_comparison_with_ci(pi_baseline_covid,
                                                         pi_temporal_covid)

png(filename = "results/CoMix/ratio_onlyphysical_peakincidence_withci.png", width = 650*1.3, height = 450)
par(oma = c(2,1,0.75,1), mfrow = c(1, 2), mar = c(4.5, 4, 4, 1.5))
plot(x = seq(1, length(wave_epi)), 
     y = output_pi_flu$peak_incidence,
     ylim= c(0,1), type = "n", xlab = "Wave", ylab = "", main = "Influenza-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_pi_flu$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_pi_flu$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(wave_epi)), 
       y = output_pi_flu$peak_incidence,
       ylim= c(0,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "peak incidence", cex = 1.5)
plot(x = seq(1, length(wave_epi)), 
     y = output_pi_covid$peak_incidence,
     ylim= c(0,1), type = "n", xlab = "Wave", ylab = "", main = "COVID-19", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_pi_covid$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_pi_covid$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(wave_epi)), 
       y = output_pi_covid$peak_incidence,
       ylim= c(0,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "peak incidence", cex = 1.5)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
dev.off()

#------- epidemic duration for physical contacts (POLYMOD)
nboot = 3000
if(!file.exists("rds/POLYMOD/epidemic_duration/samefoi/epidemic_duration_physical_baseline_00001_BE.rds")){
  for(i in 1:length(country_list)){
    epidemic_duration_baseline <- list()
    epidemic_duration_temporal <- list()
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    
    population_data <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
    for(j in 1:length(R0)){
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){
        print(sprintf("doing country %s for foi %s and %05d boot", i, j, b))
        load(sprintf("rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                     country_input, j, b), temp_env_baseline)
        load(sprintf("rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                     country_input, j, b), temp_env_frequency)
        
        temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
        temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_baseline > 0.1*N){
          ed_baseline <- length(temp_env_baseline$output.simulate.baseline.vsc$infected_daily)
          temp_ed_baseline <- data.frame(ed_baseline, boot = b)
          epidemic_duration_baseline[[count_base]] <-  temp_ed_baseline
          count_base = count_base + 1
        }
        
        if(temp_sum_temporal > 0.1*N){
          ed_temporal <- length(temp_env_frequency$output.simulate.vsc$infected.daily)
          temp_ed_temporal <- data.frame(ed_temporal, boot = b)
          epidemic_duration_temporal[[count_temp]] <-  temp_ed_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      epidemic_duration_baseline_dataframe <- do.call("rbind", epidemic_duration_baseline)
      epidemic_duration_temporal_dataframe <- do.call("rbind", epidemic_duration_temporal)
      
      saveRDS(epidemic_duration_baseline_dataframe, 
              sprintf("rds/POLYMOD/epidemic_duration/samefoi/epidemic_duration_physical_baseline_%05d_%s.rds", as.integer(j), country_input))
      saveRDS(epidemic_duration_temporal_dataframe, 
              sprintf("rds/POLYMOD/epidemic_duration/samefoi/epidemic_duration_physical_temporal_%05d_%s.rds", as.integer(j), country_input))
    }
  }
} else {
  
  ed_baseline_flu <- list()
  ed_temporal_flu <- list()
  ed_baseline_covid <- list()
  ed_temporal_covid <- list()
  
  path <- "rds/POLYMOD/epidemic_duration/samefoi/"
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("loading %s", country_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/epidemic_duration_physical_baseline_%05d_%s.rds", path, as.integer(j), country_input)))
        temp_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/epidemic_duration_physical_temporal_%05d_%s.rds", path, as.integer(j), country_input)))
        ed_baseline_flu[[i]] <- base_flu
        ed_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/epidemic_duration_physical_baseline_%05d_%s.rds", path, as.integer(j), country_input)))
        temp_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/epidemic_duration_physical_temporal_%05d_%s.rds", path, as.integer(j), country_input)))
        ed_baseline_covid[[i]] <- base_covid
        ed_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_ed_comparison_with_ci <- function(ed_baseline_flu, 
                                            ed_temporal_flu){
  
  country_length <- length(ed_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    ed_baseline_sub <- ed_baseline_flu[[i]]
    ed_temporal_sub <- ed_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(ed_baseline_sub)
    length_temporal_boot <- nrow(ed_temporal_sub)
    ed_ratio <- c()
    for(b in 1:nboot){
      boot_baseline <- mean(sample(ed_baseline_sub$ed_baseline, length_baseline_boot, replace = T))
      boot_temporal <- mean(sample(ed_temporal_sub$ed_temporal, length_temporal_boot, replace = T))
      
      ed_ratio[b] <- boot_temporal/boot_baseline
    }
    output[count, "country"] <- country_list[i]
    output[count, "lower"] <- quantile(ed_ratio, probs = 0.025)
    output[count, "epidemic_duration"] <- mean(ed_ratio)
    output[count, "upper"] <- quantile(ed_ratio, probs = 0.975)
    
    count = count + 1
    print(count)
  }
  return(output)
}

output_ed_flu <- calculate_ed_comparison_with_ci(ed_baseline_flu,
                                                 ed_temporal_flu)

output_ed_covid <- calculate_ed_comparison_with_ci(ed_baseline_covid,
                                                   ed_temporal_covid)

png(filename = "results/POLYMOD/ratio_onlyphysical_epidur_withci.png", width = 650*1.3, height = 450)
par(oma = c(2,1,0.75,1), mfrow = c(1, 2), mar = c(4.5, 4, 4, 1.5))
plot(x = seq(1, length(country_list)), 
     y = output_ed_flu$epidemic_duration,
     ylim= c(0,1.25), type = "n", xlab = "Country", ylab = "", main = "Influenza-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_ed_flu$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_ed_flu$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_ed_flu$epidemic_duration,
       ylim= c(0,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "epidemic duration", cex = 1.5)
plot(x = seq(1, length(country_list)), 
     y = output_ed_covid$epidemic_duration,
     ylim= c(0,1.25), type = "n", xlab = "Country", ylab = "", main = "COVID-19-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_ed_covid$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_ed_covid$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_ed_covid$epidemic_duration,
       ylim= c(0,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "epidemic duration", cex = 1.5)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
dev.off()

#------- epidemic duration for physical contacts (CoMix)
nboot = 3000

if(!file.exists("rds/CoMix/epidemic_duration/epidemic_duration_physical_baseline_00001_0019.rds")){
  for(i in 1:length(wave_epi)){
    epidemic_duration_baseline <- list()
    epidemic_duration_temporal <- list()
    wave_input = wave_epi[i]
    print(sprintf("doing %s for physical contacts", wave_input))
    
    population_data <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_physical_5000pop_%04d.rds", wave_input))$dataset.res
    for(j in 1:length(R0)){
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }
      
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){ 
        print(sprintf("doing wave %04d for foi %s and %05d boot", wave_input, j, b))
        load(sprintf("rds/CoMix/output_CoMix_wave_physical_%04d/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                     wave_epi_input, j, b), temp_env_baseline)
        load(sprintf("rds/CoMix/output_CoMix_wave_physical_%04d/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                     wave_epi_input, j, b), temp_env_frequency)
        
        temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
        temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_baseline > 0.1*N){
          ed_baseline <- length(temp_env_baseline$output.simulate.baseline.vsc$infected_daily)
          temp_ed_baseline <- data.frame(ed_baseline, boot = b)
          epidemic_duration_baseline[[count_base]] <-  temp_ed_baseline
          count_base = count_base + 1
        }
        
        if(temp_sum_temporal > 0.1*N){
          ed_temporal <- length(temp_env_frequency$output.simulate.vsc$infected.daily)
          temp_ed_temporal <- data.frame(ed_temporal, boot = b)
          epidemic_duration_temporal[[count_temp]] <-  temp_ed_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      epidemic_duration_baseline_dataframe <- do.call("rbind", epidemic_duration_baseline)
      epidemic_duration_temporal_dataframe <- do.call("rbind", epidemic_duration_temporal)
      
      saveRDS(epidemic_duration_baseline_dataframe, 
              sprintf("rds/CoMix/epidemic_duration/epidemic_duration_physical_baseline_%05d_%04d.rds", as.integer(j), as.integer(wave_input)))
      saveRDS(epidemic_duration_temporal_dataframe, 
              sprintf("rds/CoMix/epidemic_duration/epidemic_duration_physical_temporal_%05d_%04d.rds", as.integer(j), as.integer(wave_input)))
    }
  }
} else {
  
  ed_baseline_flu <- list()
  ed_temporal_flu <- list()
  ed_baseline_covid <- list()
  ed_temporal_covid <- list()
  
  path <- "rds/CoMix/epidemic_duration"
  for(i in 1:length(wave_epi)){
    wave_epi_input = wave_epi[i]
    print(sprintf("loading wave %s", wave_epi_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(wave = wave_input,
                               readRDS(sprintf("%s/epidemic_duration_physical_baseline_%05d_%04d.rds", path, as.integer(j), as.integer(wave_input))))
        temp_flu <- data.frame(wave = wave_input,
                               readRDS(sprintf("%s/epidemic_duration_physical_temporal_%05d_%04d.rds", path, as.integer(j), as.integer(wave_input))))
        ed_baseline_flu[[i]] <- base_flu
        ed_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(wave = wave_input,
                                 readRDS(sprintf("%s/epidemic_duration_physical_baseline_%05d_%04d.rds", path, as.integer(j), as.integer(wave_input))))
        temp_covid <- data.frame(wave = wave_input,
                                 readRDS(sprintf("%s/epidemic_duration_physical_temporal_%05d_%04d.rds", path, as.integer(j), as.integer(wave_input))))
        ed_baseline_covid[[i]] <- base_covid
        ed_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_ed_comparison_with_ci <- function(ed_baseline_flu, 
                                            ed_temporal_flu){
  
  wave_length <- length(ed_baseline_flu)
  wave_list <- c("19", "27", "43")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:wave_length){
    ed_baseline_sub <- ed_baseline_flu[[i]]
    ed_temporal_sub <- ed_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(ed_baseline_sub)
    length_temporal_boot <- nrow(ed_temporal_sub)
    ed_ratio <- c()
    for(b in 1:nboot){
      boot_baseline <- mean(sample(ed_baseline_sub$ed_baseline, length_baseline_boot, replace = T))
      boot_temporal <- mean(sample(ed_temporal_sub$ed_temporal, length_temporal_boot, replace = T))
      
      ed_ratio[b] <- boot_temporal/boot_baseline
    }
    output[count, "wave"] <- wave_list[i]
    output[count, "lower"] <- quantile(ed_ratio, probs = 0.025)
    output[count, "epidemic_duration"] <- mean(ed_ratio)
    output[count, "upper"] <- quantile(ed_ratio, probs = 0.975)
    
    count = count + 1
    print(count)
  }
  return(output)
}

output_ed_flu <- calculate_ed_comparison_with_ci(ed_baseline_flu,
                                                 ed_temporal_flu)

output_ed_covid <- calculate_ed_comparison_with_ci(ed_baseline_covid,
                                                   ed_temporal_covid)

png(filename = "results/CoMix/ratio_onlyphysical_epidur_withci.png", width = 650*1.3, height = 450)
par(oma = c(2,1,0.75,1), mfrow = c(1, 2), mar = c(4.5, 4, 4, 1.5))
plot(x = seq(1, length(wave_epi)), 
     y = output_ed_flu$epidemic_duration,
     ylim= c(0,1.5), type = "n", xlab = "Wave", ylab = "", main = "Influenza-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_ed_flu$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_ed_flu$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(wave_epi)), 
       y = output_ed_flu$epidemic_duration,
       ylim= c(0,1.5),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "epidemic duration", cex = 1.5)
plot(x = seq(1, length(wave_epi)), 
     y = output_ed_covid$epidemic_duration,
     ylim= c(0,1.5), type = "n", xlab = "Wave", ylab = "", main = "COVID-19-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_ed_covid$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_ed_covid$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(wave_epi)), 
       y = output_ed_covid$epidemic_duration,
       ylim= c(0,1.5),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "epidemic duration", cex = 1.5)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
dev.off()

#------- time to peak for physical contacts (POLYMOD)
nboot = 3000

if(!file.exists("rds/POLYMOD/timetopeak/samefoi/timetopeak_physical_baseline_00001_BE.rds")){
  for(i in 1:length(country_list)){
    timetopeak_baseline <- list()
    timetopeak_temporal <- list()
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    
    population_data <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
    for(j in 1:length(R0)){
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }  
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){ 
        print(sprintf("doing country %s for foi %s and %05d boot", i, j, b))
        load(sprintf("rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                     country_input, j, b), temp_env_baseline)
        load(sprintf("rds/POLYMOD/output_physical_%s_samefoi/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                     country_input, j, b), temp_env_frequency)
        
        temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
        temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_baseline > 0.1*N){
          ttp_baseline <- which.max(temp_env_baseline$output.simulate.baseline.vsc$infected_daily)
          temp_ttp_baseline <- data.frame(ttp_baseline, boot = b)
          timetopeak_baseline[[count_base]] <-  temp_ttp_baseline
          count_base = count_base + 1
        }
        
        if(temp_sum_temporal > 0.1*N){
          ttp_temporal <- which.max(temp_env_frequency$output.simulate.vsc$infected.daily)
          temp_ttp_temporal <- data.frame(ttp_temporal, boot = b)
          timetopeak_temporal[[count_temp]] <-  temp_ttp_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      timetopeak_baseline_dataframe <- do.call("rbind", timetopeak_baseline)
      timetopeak_temporal_dataframe <- do.call("rbind", timetopeak_temporal)
      
      saveRDS(timetopeak_baseline_dataframe, 
              sprintf("rds/POLYMOD/timetopeak/timetopeak_physical_baseline_%05d_%s.rds", as.integer(j), country_input))
      saveRDS(timetopeak_temporal_dataframe, 
              sprintf("rds/POLYMOD/timetopeak/timetopeak_physical_temporal_%05d_%s.rds", as.integer(j), country_input))
    }
  }
} else {
  
  ttp_baseline_flu <- list()
  ttp_temporal_flu <- list()
  ttp_baseline_covid <- list()
  ttp_temporal_covid <- list()
  
  path <- "rds/POLYMOD/timetopeak/samefoi"
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("loading %s", country_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/timetopeak_physical_baseline_%05d_%s.rds", path, as.integer(j), country_input)))
        temp_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/timetopeak_physical_temporal_%05d_%s.rds", path, as.integer(j), country_input)))
        ttp_baseline_flu[[i]] <- base_flu
        ttp_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/timetopeak_physical_baseline_%05d_%s.rds", path, as.integer(j), country_input)))
        temp_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/timetopeak_physical_temporal_%05d_%s.rds", path, as.integer(j), country_input)))
        ttp_baseline_covid[[i]] <- base_covid
        ttp_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_ttp_comparison_with_ci <- function(ttp_baseline_flu, 
                                             ttp_temporal_flu){
  
  country_length <- length(ttp_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    ttp_baseline_sub <- ttp_baseline_flu[[i]]
    ttp_temporal_sub <- ttp_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(ttp_baseline_sub)
    length_temporal_boot <- nrow(ttp_temporal_sub)
    ttp_ratio <- c()
    for(b in 1:nboot){
      boot_baseline <- mean(sample(ttp_baseline_sub$ttp_baseline, length_baseline_boot, replace = T))
      boot_temporal <- mean(sample(ttp_temporal_sub$ttp_temporal, length_temporal_boot, replace = T))
      
      ttp_ratio[b] <- boot_temporal/boot_baseline
    }
    output[count, "country"] <- country_list[i]
    output[count, "lower"] <- quantile(ttp_ratio, probs = 0.025)
    output[count, "timetopeak"] <- mean(ttp_ratio)
    output[count, "upper"] <- quantile(ttp_ratio, probs = 0.975)
    
    count = count + 1
    print(count)
  }
  return(output)
}

output_ttp_flu <- calculate_ttp_comparison_with_ci(ttp_baseline_flu,
                                                   ttp_temporal_flu)

output_ttp_covid <- calculate_ttp_comparison_with_ci(ttp_baseline_covid,
                                                     ttp_temporal_covid)

png(filename = "results/POLYMOD/ratio_onlyphysical_timetopeak_withci.png", width = 650*1.3, height = 450)
par(oma = c(2,1,0.75,1), mfrow = c(1, 2), mar = c(4.5, 4, 4, 1.5))
plot(x = seq(1, length(country_list)), 
     y = output_ttp_flu$timetopeak,
     ylim= c(0.5,1.25), type = "n", xlab = "Country", ylab = "", main = "Influenza-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_ttp_flu$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_ttp_flu$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_ttp_flu$timetopeak,
       ylim= c(0.5,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "time to peak of the epidemic", cex = 1.5)
plot(x = seq(1, length(country_list)), 
     y = output_ttp_covid$timetopeak,
     ylim= c(0.5,1.25), type = "n", xlab = "Country", ylab = "", main = "COVID-19-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_ttp_covid$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_ttp_covid$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_ttp_covid$timetopeak,
       ylim= c(0.5,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "time to peak of the epidemic", cex = 1.5)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
dev.off()

#------- time to peak for physical contacts (CoMix)
nboot = 3000

if(!file.exists("rds/CoMix/timetopeak/timetopeak_physical_temporal_00001_0019.rds")){
  for(i in 1:length(wave_epi)){
    timetopeak_baseline <- list()
    timetopeak_temporal <- list()
    wave_epi_input = wave_epi[i]
    print(sprintf("doing wave %s for physical contacts", wave_epi_input))
    
    population_data <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_physical_5000pop_%04d.rds", wave_epi_input))$dataset.res
    
    for(j in 1:length(R0)){
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }
      
      attack_rate_baseline <- list()
      attack_rate_temporal <- list()
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){
        print(sprintf("doing wave %03d for foi %05d and %05d boot", as.integer(wave_epi_input), as.integer(j), as.integer(b)))
        
        load(sprintf("rds/CoMix/output_CoMix_wave_physical_%04d/full_epidemic_simulation_CM_baseline_%03d_%05d.RData", 
                     wave_epi_input, j, b), temp_env_baseline)
        load(sprintf("rds/CoMix/output_CoMix_wave_physical_%04d/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                     wave_epi_input, j, b), temp_env_frequency)
        
        temp_sum_baseline <- sum(temp_env_baseline$output.simulate.baseline.vsc$incidenceMat)
        temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_baseline > 0.1*N){
          ttp_baseline <- which.max(temp_env_baseline$output.simulate.baseline.vsc$infected_daily)
          temp_ttp_baseline <- data.frame(ttp_baseline, boot = b)
          timetopeak_baseline[[count_base]] <-  temp_ttp_baseline
          count_base = count_base + 1
        }
        
        if(temp_sum_temporal > 0.1*N){
          ttp_temporal <- which.max(temp_env_frequency$output.simulate.vsc$infected.daily)
          temp_ttp_temporal <- data.frame(ttp_temporal, boot = b)
          timetopeak_temporal[[count_temp]] <-  temp_ttp_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      timetopeak_baseline_dataframe <- do.call("rbind", timetopeak_baseline)
      timetopeak_temporal_dataframe <- do.call("rbind", timetopeak_temporal)
      
      saveRDS(timetopeak_baseline_dataframe, 
              sprintf("rds/CoMix/timetopeak/timetopeak_physical_baseline_%05d_%04d.rds", as.integer(j), wave_epi_input))
      saveRDS(timetopeak_temporal_dataframe, 
              sprintf("rds/CoMix/timetopeak/timetopeak_physical_temporal_%05d_%04d.rds", as.integer(j), wave_epi_input))
    }
  }
} else {
  
  ttp_baseline_flu <- list()
  ttp_temporal_flu <- list()
  ttp_baseline_covid <- list()
  ttp_temporal_covid <- list()
  
  path <- "rds/CoMix/timetopeak"
  for(i in 1:length(wave_epi)){
    wave_epi_input = wave_epi[i]
    print(sprintf("loading wave %s", wave_epi_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(wave = wave_epi_input,
                               readRDS(sprintf("%s/timetopeak_physical_baseline_%05d_%04d.rds", path, as.integer(j), wave_epi_input)))
        temp_flu <- data.frame(wave = wave_epi_input,
                               readRDS(sprintf("%s/timetopeak_physical_temporal_%05d_%04d.rds", path, as.integer(j), wave_epi_input)))
        ttp_baseline_flu[[i]] <- base_flu
        ttp_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(wave = wave_epi_input,
                                 readRDS(sprintf("%s/timetopeak_physical_baseline_%05d_%04d.rds", path, as.integer(j), wave_epi_input)))
        temp_covid <- data.frame(wave = wave_epi_input,
                                 readRDS(sprintf("%s/timetopeak_physical_temporal_%05d_%04d.rds", path, as.integer(j), wave_epi_input)))
        ttp_baseline_covid[[i]] <- base_covid
        ttp_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_ttp_comparison_with_ci <- function(ttp_baseline_flu, 
                                             ttp_temporal_flu){
  
  wave_length <- length(ttp_baseline_flu)
  wave_epi <- c("19", "27", "43")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:wave_length){
    ttp_baseline_sub <- ttp_baseline_flu[[i]]
    ttp_temporal_sub <- ttp_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(ttp_baseline_sub)
    length_temporal_boot <- nrow(ttp_temporal_sub)
    ttp_ratio <- c()
    for(b in 1:nboot){
      boot_baseline <- mean(sample(ttp_baseline_sub$ttp_baseline, length_baseline_boot, replace = T))
      boot_temporal <- mean(sample(ttp_temporal_sub$ttp_temporal, length_temporal_boot, replace = T))
      
      ttp_ratio[b] <- boot_temporal/boot_baseline
    }
    output[count, "wave"] <- wave_epi[i]
    output[count, "lower"] <- quantile(ttp_ratio, probs = 0.025)
    output[count, "timetopeak"] <- mean(ttp_ratio)
    output[count, "upper"] <- quantile(ttp_ratio, probs = 0.975)
    
    count = count + 1
    print(count)
  }
  return(output)
}

output_ttp_flu <- calculate_ttp_comparison_with_ci(ttp_baseline_flu,
                                                   ttp_temporal_flu)

output_ttp_covid <- calculate_ttp_comparison_with_ci(ttp_baseline_covid,
                                                     ttp_temporal_covid)

png(filename = "results/CoMix/ratio_onlyphysical_timetopeak_withci.png", width = 650*1.3, height = 450)
par(oma = c(2,1,0.75,1), mfrow = c(1, 2), mar = c(4.5, 4, 4, 1.5))
plot(x = seq(1, length(wave_epi)), 
     y = output_ttp_flu$timetopeak,
     ylim= c(0.5,1.8), type = "n", xlab = "Wave", ylab = "", main = "Influenza-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_ttp_flu$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_ttp_flu$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(wave_epi)), 
       y = output_ttp_flu$timetopeak,
       ylim= c(0.5,1.8),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "time to peak of the epidemic", cex = 1.5)
plot(x = seq(1, length(wave_epi)), 
     y = output_ttp_covid$timetopeak,
     ylim= c(0.5,1.8), type = "n", xlab = "Wave", ylab = "", main = "COVID-19-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(wave_epi)),
       y0 = output_ttp_covid$lower,
       x1 = seq(1, length(wave_epi)),
       y1 = output_ttp_covid$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(wave_epi)), 
       y = output_ttp_covid$timetopeak,
       ylim= c(0.5,1.8),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(wave_epi), labels=wave_epi,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "time to peak of the epidemic", cex = 1.5)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
dev.off()

#------- extinction rate for physical contacts (POLYMOD)
nboot = 3000

calculate_extinctionrate <- function(ttp_baseline_flu, 
                                     ttp_temporal_flu, nboot = 3000){
  
  country_length <- length(ttp_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    er_baseline_sub <- ttp_baseline_flu[[i]]
    er_temporal_sub <- ttp_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(er_baseline_sub)
    length_temporal_boot <- nrow(er_temporal_sub)
    
    boot_baseline <- 1-(length_baseline_boot/nboot)
    boot_temporal <- 1-(length_temporal_boot/nboot)
    
    output[count, "country"] <- country_list[i]
    output[count, "extinction_rate_baseline"] <- round(boot_baseline,3)
    output[count, "extinction_rate_temporal"] <- round(boot_temporal,3)
    count = count + 1
    print(count)
  }
  return(output)
}

output_extinctionrate_flu <- calculate_extinctionrate(ttp_baseline_flu,
                                                      ttp_temporal_flu)

output_extinctionrate_covid <- calculate_extinctionrate(ttp_baseline_covid,
                                                        ttp_temporal_covid)

#------- extinction rate for physical contacts (CoMix)
nboot = 3000

calculate_extinctionrate_comix <- function(ttp_baseline_flu, 
                                           ttp_temporal_flu, 
                                           nboot = 3000){
  
  epi_length <- length(ttp_baseline_flu)
  wave_list <- c("19", "27", "43")
  output <- data.frame()
  count = 1
  for(i in 1:epi_length){
    er_baseline_sub <- ttp_baseline_flu[[i]]
    er_temporal_sub <- ttp_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(er_baseline_sub)
    length_temporal_boot <- nrow(er_temporal_sub)
    
    boot_baseline <- 1-(length_baseline_boot/nboot)
    boot_temporal <- 1-(length_temporal_boot/nboot)
    
    output[count, "wave"] <- wave_list[i]
    output[count, "extinction_rate_baseline"] <- round(boot_baseline,3)
    output[count, "extinction_rate_temporal"] <- round(boot_temporal,3)
    count = count + 1
    print(count)
  }
  return(output)
}

output_extinctionrate_flu <- calculate_extinctionrate_comix(ttp_baseline_flu,
                                                            ttp_temporal_flu)

output_extinctionrate_covid <- calculate_extinctionrate_comix(ttp_baseline_covid,
                                                              ttp_temporal_covid)

# Contacts at home/ in home ratio and crude numbers (CoMix)
count = 1
cnt_location <- c("inshome", "outhome")

for(cnt in cnt_location){
  storage_list <- list()
  if(!file.exists(sprintf("rds/CoMix/data_temp_CoMix_%s_all_withci.rds", cnt))){
    for(i in 1:length(wave_list)){
      wave = wave_list[i]
      print(sprintf("doing %s for %s", wave, cnt))
      temp_env <- env()
      temp <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_%s_5000pop_%04d.rds", cnt, wave))
      
      storage_list[[i]] <- cbind(calculate_ratio_temporal_withci(temp), wave = wave)
    }
    saveRDS(storage_list, sprintf("rds/CoMix/data_temp_CoMix_%s_all_withci.rds", cnt))
  } else {
    storage_list_at_home_withci <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_%s_all_withci.rds", cnt_location[1]))
    storage_list_not_at_home_withci <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_%s_all_withci.rds", cnt_location[2]))
  }
}

storage_data_at_home_withci <- do.call(rbind, storage_list_at_home_withci)
storage_data_not_at_home_withci <- do.call(rbind, storage_list_not_at_home_withci)

if(!file.exists("rds/CoMix/data_temp_CoMix_withci.rds")){
  storage_list_comix <- list()
  for(i in 1:length(wave_list)){
    wave = wave_list[i]
    
    temp_env <- env()
    temp <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_allcontacts_5000pop_%04d.rds", wave))
    
    storage_list_comix[[i]] <- cbind(calculate_ratio_temporal_withci(temp), wave = wave)
  }
  saveRDS(storage_list_comix, "rds/CoMix/data_temp_CoMix_withci.rds")
} else {
  storage_list_comix <- readRDS("rds/CoMix/data_temp_CoMix_withci.rds")
}

data_ratio_to_plot <- do.call("rbind", storage_list_comix)

png(filename = "results/CoMix/ratio_allcontacts_home_results_withci.png", width = 800*1.2, height = 800)
par(oma = c(3,1,0.75,1), mfrow = c(2, 2), mar = c(3, 4.5, 3, 1))
plot(x = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$wave, 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$mean,
     ylim= c(0,1), type = "n", xlab = "Wave", ylab = "", main = "Children (0-12 years)",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$wave,
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$lower,
       x1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$wave,
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 2)
points(x = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$wave, 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Children (0-12 years)", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$wave,
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$lower,
       x1 = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$wave,
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 2)
points(x = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$wave, 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 23, cex = 1.2, main = "Children (0-12 years)", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$wave,
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$lower,
       x1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$wave,
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 2)
points(x = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$wave, 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 21, cex = 1.2, main = "Children (0-12 years)", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
mtext(side = 2, line = 3.75, "Ratio of frequency-based to naive", cex = 1.5)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.5)
plot(x = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$wave, 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$mean,
     ylim= c(0,1), type = "n", xlab = "Wave", ylab = "", main = "Teens (13-18 years)",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$wave,
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$lower,
       x1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$wave,
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 2)
points(x = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$wave, 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Teens (13-18 years)", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$wave,
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$lower,
       x1 = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$wave,
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 2)
points(x = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$wave, 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 23, cex = 1.2, main = "Teens (13-18 years)", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$wave,
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$lower,
       x1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$wave,
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 2)
points(x = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$wave, 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 21, cex = 1.2, main = "Teens (13-18 years)", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
mtext(side = 2, line = 3.75, "Ratio of frequency-based to naive", cex = 1.5)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.5)
plot(x = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$wave, 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$mean,
     ylim= c(0,1), type = "n", xlab = "Wave", ylab = "", main = "Adult (13-65 years)",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$wave,
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$lower,
       x1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$wave,
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 2)
points(x = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$wave, 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Adult (13-65 years)", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$wave,
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$lower,
       x1 = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$wave,
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 2)
points(x = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$wave, 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 23, cex = 1.2, main = "Adult (13-65 years)", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$wave,
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$lower,
       x1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$wave,
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 2)
points(x = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$wave, 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 21, cex = 1.2, main = "Adult (13-65 years)", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
mtext(side = 2, line = 3.75, "Ratio of frequency-based to naive", cex = 1.5)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.5)
mtext(side = 1, line = 2.5, "Wave", cex = 1.2)
plot(x = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$wave, 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$mean,
     ylim= c(0,1), type = "n", xlab = "Wave", ylab = "", main = "Elderly (66+ years)",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$wave,
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$lower,
       x1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$wave,
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 2)
points(x = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$wave, 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Elderly (66+ years)", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$wave,
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$lower,
       x1 = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$wave,
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 2)
points(x = storage_data_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$wave, 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 23, cex = 1.2, main = "Elderly (66+ years)", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$wave,
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$lower,
       x1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$wave,
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 2)
points(x = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$wave, 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$mean,
       ylim= c(0,1), type = "b",
       lty = 2, pch = 21, cex = 1, main = "Elderly (66+ years)", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
mtext(side = 2, line = 3.75, "Ratio of frequency-based to naive", cex = 1.5)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.5)
mtext(side = 1, line = 2.5, "Wave", cex = 1.2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = "bottom",
       legend = c("All contacts", "Contacts outside home", "Contacts at home"), 
       pt.bg= c("red", "blue", "darkgreen"), 
       pch = c(21, 22, 23), cex=1.5, horiz = TRUE, bty = "n")
dev.off()

if(!file.exists("rds/CoMix/data_ratio_comix_home.rds")){
  for(i in wave_list){
    for(cnt in cnt_location){
      print(paste0("doing ", i, " for ", cnt, " contacts"))
      N = 5000
      temp_env <- env()
      data_temp <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_%s_5000pop_%04d.rds", cnt, i))
      
      storage_list[[count]] <- cbind(cbind(calculate_ratio_temporal(data_temp), country = i), cnt_type = cnt)
      count = count + 1
    }
  }
  saveRDS(storage_list, "rds/CoMix/data_ratio_comix_home.rds")
  
  
} else {
  storage_list <- readRDS("rds/CoMix/data_ratio_comix_home.rds")
}

data_ratio_to_plot_home <- do.call("rbind", storage_list)
storage_data_at_home_withci <- data_ratio_to_plot_home[data_ratio_to_plot_home$cnt_type == "inshome",]
storage_data_not_at_home_withci <- data_ratio_to_plot_home[data_ratio_to_plot_home$cnt_type == "outhome",]

storage_list_comix <- readRDS("rds/CoMix/data_ratio_comix.rds")
data_ratio_to_plot <- do.call("rbind", storage_list_comix)
data_ratio_to_plot <- data_ratio_to_plot[data_ratio_to_plot$cnt_type == "all_contacts",]

png(filename = "results/CoMix/crude_allcontacts_results_home.png", width = 600*1.5, height = 600*1.2)
par(oma = c(3,1,0.85,1), mfrow = c(2, 2), mar = c(4.75, 4, 3, 1))
plot(x = seq(1, length(wave_list)), 
     y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$sum_naive,
     ylim= c(0,50), type = "n", xlab = "Wave", ylab = "", main = "Children (0-12 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(wave_list)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$sum_naive,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Children (0-12 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$sum_temp,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Children (0-12 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$sum_naive,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Children (0-12 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$sum_temp,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Children (0-12 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 2.5, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(wave_list)), 
     y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$sum_naive,
     ylim= c(0,50), type = "n", xlab = "Wave", ylab = "", main = "Teens (13-18 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(wave_list)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$sum_naive,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Teens (13-18 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$sum_temp,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Teens (13-18 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$sum_naive,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Teens (13-18 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$sum_temp,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Teens (13-18 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 2.5, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(wave_list)), 
     y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$sum_naive,
     ylim= c(0,50), type = "n", xlab = "Wave", ylab = "", main = "Adult (13-65 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(wave_list)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$sum_naive,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Adult (13-65 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$sum_temp,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Adult (13-65 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$sum_naive,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Adult (13-65 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$sum_temp,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Adult (13-65 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 2.5, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(wave_list)), 
     y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$sum_naive,
     ylim= c(0,50), type = "n", xlab = "Wave", ylab = "", main = "Elderly (66+ years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(wave_list)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$sum_naive,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Elderly (66+ years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$sum_temp,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 24, cex = 1.2, main = "Elderly (66+ years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$sum_naive,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Elderly (66+ years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(wave_list)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$sum_temp,
       ylim= c(0,50), type = "b",
       lty = 2, pch = 22, cex = 1.2, main = "Elderly (66+ years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(wave_list), labels=wave_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 2.5, "Total number of weekly distinct contacts", cex = 1.2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0.5, 0), mar = c(0, 0, 0.5, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = "bottom",
       legend = c("Inside home contacts (Naive)", "Inside home contacts (Frequency-based)",
                  "Outside home contacts (Naive)", "Outside home contacts (Frequency-based)"),
       pch = c(21, 21, 24, 24), pt.bg = c("#c1121f", "#669bbc", "#c1121f", "#669bbc"),
       cex=1.35, bty = "n", ncol = 2)
dev.off()

# Contacts at home/ in home ratio and crude numbers (POLYMOD)
count = 1
cnt_location <- c("inshome", "outhome")

for(cnt in cnt_location){
  storage_list <- list()
  if(!file.exists(sprintf("rds/POLYMOD/data_temp_%s_all_withci.rds", cnt))){
    for(i in 1:length(country)){
      country_i = country[i]
      print(sprintf("doing %s for %s", country_i, cnt))
      temp_env <- env()
      
      if(cnt %in% "inshome"){
        title <- "contacts_at_home"
      } else {
        title <- "contacts_not_at_home"
      }
      
      temp <- readRDS(sprintf("rds/POLYMOD/data_temp_gp_5000_pop_%s_%s.rds", title, country_i))
      storage_list[[i]] <- cbind(calculate_ratio_temporal_withci(temp), country = country_i)
    }
    saveRDS(storage_list, sprintf("rds/POLYMOD/data_temp_%s_all_withci.rds", cnt))
  } else {
    storage_list_at_home_withci <- readRDS(sprintf("rds/POLYMOD/data_temp_%s_all_withci.rds", cnt_location[1]))
    storage_list_not_at_home_withci <- readRDS(sprintf("rds/POLYMOD/data_temp_%s_all_withci.rds", cnt_location[2]))
  }
}

storage_data_at_home_withci <- do.call(rbind, storage_list_at_home_withci) 
storage_data_not_at_home_withci <- do.call(rbind, storage_list_not_at_home_withci)

if(!file.exists("rds/POLYMOD/data_temp_withci.rds")){
  storage_list_comix <- list()
  for(i in 1:length(country)){
    country_i = country[i]
    print(sprintf("doing for %s", country_i))
    temp_env <- env()
    
    temp <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop_%s.rds", country_i))
    storage_list[[i]] <- cbind(calculate_ratio_temporal_withci(temp), country = country_i)
  }
  saveRDS(storage_list, "rds/POLYMOD/data_temp_withci.rds")
} else {
  storage_list <- readRDS("rds/POLYMOD/data_temp_withci.rds")
}

data_ratio_to_plot <- do.call("rbind", storage_list)

country <- c("BE", "DE", "FI", "GB", "IT", "LU", "NL", "PL")

png(filename = "results/POLYMOD/ratio_allcontacts_home_results_withci.png", width = 800*1.2, height = 800)
par(oma = c(3,1,0.75,1), mfrow = c(2, 2), mar = c(3, 4.5, 3, 1))
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$mean,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Children (0-12 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 2)
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1.2, main = "Children (0-12 years)", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 2)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 23, cex = 1.2, main = "Children (0-12 years)", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$lower,
       x1 = seq(1, length(country)),
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 2)
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 21, cex = 1.2, main = "Children (0-12 years)", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.75, "Ratio of frequency-based to naive", cex = 1.5)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.5)
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$mean,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Teens (13-18 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 2)
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1.2, main = "Teens (13-18 years)", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 2)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 23, cex = 1.2, main = "Teens (13-18 years)", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$lower,
       x1 = seq(1, length(country)),
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 2)
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 21, cex = 1.2, main = "Teens (13-18 years)", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.75, "Ratio of frequency-based to naive", cex = 1.5)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.5)
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$mean,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Adult (13-65 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 2)
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1.2, main = "Adult (13-65 years)", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 2)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 23, cex = 1.2, main = "Adult (13-65 years)", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$lower,
       x1 = seq(1, length(country)),
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 2)
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 21, cex = 1.2, main = "Adult (13-65 years)", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.75, "Ratio of frequency-based to naive", cex = 1.5)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.5)
mtext(side = 1, line = 2.5, "Country", cex = 1.2)
plot(x = seq(1, length(country)), 
     y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$mean,
     ylim= c(0,1), type = "n", xlab = "", ylab = "", main = "Elderly (66+ years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "blue", length = 0.05, lwd = 1.5)
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 22, cex = 1.2, main = "Elderly (66+ years)", bg = "blue",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$lower,
       x1 = seq(1, length(country)),
       y1 = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "darkgreen", length = 0.05, lwd = 2)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 23, cex = 1.2, main = "Elderly (66+ years)", bg = "darkgreen",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
arrows(x0 = seq(1, length(country)),
       y0 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$lower,
       x1 = seq(1, length(country)),
       y1 = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$upper,
       angle = 90, code = 3, col = "red", length = 0.05, lwd = 2)
points(x = seq(1, length(country)), 
       y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$mean,
       ylim= c(0,1),
       lty = 2, pch = 21, cex = 1.2, main = "Elderly (66+ years)", bg = "red",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.75, "Ratio of frequency-based to naive", cex = 1.5)
mtext(side = 2, line = 2.5, "total number of weekly distinct contacts", cex = 1.5)
mtext(side = 1, line = 2.5, "Country", cex = 1.2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = "bottom",
       legend = c("All contacts", "Contacts outside home", "Contacts at home"), 
       pt.bg= c("red", "blue", "darkgreen"), 
       pch = c(21, 22, 23), cex=1.35, horiz = TRUE, bty = "n")
dev.off()

if(!file.exists("rds/POLYMOD/data_ratio_home.rds")){
  for(i in country){
    for(cnt in cnt_location){
      print(paste0("doing ", i, " for ", cnt, " contacts"))
      N = 5000
      temp_env <- env()
      
      if(cnt %in% "inshome"){
        title <- "contacts_at_home"
      } else {
        title <- "contacts_not_at_home"
      }
      
      data_temp <- readRDS(sprintf("rds/POLYMOD/data_temp_gp_5000_pop_%s_%s.rds", title, i))
      
      storage_list[[count]] <- cbind(cbind(calculate_ratio_temporal(data_temp), country = i), cnt_type = cnt)
      count = count + 1
    }
  }
  saveRDS(storage_list, "rds/POLYMOD/data_ratio_home.rds")
} else {
  storage_list <- readRDS("rds/POLYMOD/data_ratio_home.rds")
}

data_ratio_to_plot_home <- do.call("rbind", storage_list)
storage_data_at_home_withci <- data_ratio_to_plot_home[data_ratio_to_plot_home$cnt_type == "inshome",]
storage_data_not_at_home_withci <- data_ratio_to_plot_home[data_ratio_to_plot_home$cnt_type == "outhome",]

storage_list_polymod <- readRDS("rds/POLYMOD/data_ratio_polymod.rds")
data_ratio_to_plot <- do.call("rbind", storage_list_polymod)
data_ratio_to_plot <- data_ratio_to_plot[data_ratio_to_plot$cnt_type == "all_contacts",]

png(filename = "results/POLYMOD/crude_allcontacts_results_home.png", width = 600*1.5, height = 600*1.2)
par(oma = c(3,1,0.85,1), mfrow = c(2, 2), mar = c(4.75, 4, 3, 1))
plot(x = seq(1, length(country)), 
     y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Children",]$sum_naive,
     ylim= c(0,200), type = "n", xlab = "Country", ylab = "", main = "Children (0-12 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$sum_naive,
       ylim= c(0,200), 
       lty = 2, pch = 24, cex = 1.2, main = "Children (0-12 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Children",]$sum_temp,
       ylim= c(0,200), 
       lty = 2, pch = 24, cex = 1.2, main = "Children (0-12 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$sum_naive,
       ylim= c(0,200), 
       lty = 2, pch = 22, cex = 1.2, main = "Children (0-12 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Children",]$sum_temp,
       ylim= c(0,200), 
       lty = 2, pch = 22, cex = 1.2, main = "Children (0-12 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(country)), 
     y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Teens",]$sum_naive,
     ylim= c(0,200), type = "n", xlab = "Country", ylab = "", main = "Teens (13-18 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$sum_naive,
       ylim= c(0,200), 
       lty = 2, pch = 24, cex = 1.2, main = "Teens (13-18 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Teens",]$sum_temp,
       ylim= c(0,200), 
       lty = 2, pch = 24, cex = 1.2, main = "Teens (13-18 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$sum_naive,
       ylim= c(0,200), 
       lty = 2, pch = 22, cex = 1.2, main = "Teens (13-18 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Teens",]$sum_temp,
       ylim= c(0,200), 
       lty = 2, pch = 22, cex = 1.2, main = "Teens (13-18 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(country)), 
     y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Adult",]$sum_naive,
     ylim= c(0,200), type = "n", xlab = "Country", ylab = "", main = "Adult (13-65 years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$sum_naive,
       ylim= c(0,200), 
       lty = 2, pch = 24, cex = 1.2, main = "Adult (13-65 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Adult",]$sum_temp,
       ylim= c(0,200), 
       lty = 2, pch = 24, cex = 1.2, main = "Adult (13-65 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$sum_naive,
       ylim= c(0,200), 
       lty = 2, pch = 22, cex = 1.2, main = "Adult (13-65 years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Adult",]$sum_temp,
       ylim= c(0,200), 
       lty = 2, pch = 22, cex = 1.2, main = "Adult (13-65 years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
plot(x = seq(1, length(country)), 
     y = data_ratio_to_plot[data_ratio_to_plot$part_age_cat == "Elderly",]$sum_naive,
     ylim= c(0,200), type = "n", xlab = "Country", ylab = "", main = "Elderly (66+ years)", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$sum_naive,
       ylim= c(0,200), 
       lty = 2, pch = 24, cex = 1.2, main = "Elderly (66+ years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_at_home_withci[storage_data_at_home_withci$part_age_cat == "Elderly",]$sum_temp,
       ylim= c(0,200), 
       lty = 2, pch = 24, cex = 1.2, main = "Elderly (66+ years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$sum_naive,
       ylim= c(0,200), 
       lty = 2, pch = 22, cex = 1.2, main = "Elderly (66+ years)", bg = "#c1121f",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
points(x = seq(1, length(country)), 
       y = storage_data_not_at_home_withci[storage_data_not_at_home_withci$part_age_cat == "Elderly",]$sum_temp,
       ylim= c(0,200), 
       lty = 2, pch = 22, cex = 1.2, main = "Elderly (66+ years)", bg = "#669bbc",
       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(1, at = 1:length(country), labels=country,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3, "Total number of weekly distinct contacts", cex = 1.2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0.5, 0), mar = c(0, 0, 0.5, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = "bottom",
       legend = c("Inside home contacts (Naive)", "Inside home contacts (Frequency-based)",
                  "Outside home contacts (Naive)", "Outside home contacts (Frequency-based)"),
       pch = c(21, 21, 24, 24), pt.bg = c("#c1121f", "#669bbc", "#c1121f", "#669bbc"),
       cex=1.35, bty = "n", ncol = 2)
dev.off()

# Analysis for attack rates for epidemic simulations using the Same R0
#------- attack rates for physical contacts (POLYMOD) ---> For the same R0
temp_env_frequency <- environment()
nboot = 3000

# if the summary of the outbreak characteristics is not present, then summarise. Otherwise,
# use the summarise outbreak characteristics.
# we do not disentangle the baseline, since it is already disentangled previously.

R0 <- c(1.3 , 3.3)
if(!file.exists("rds/POLYMOD/attack_rates/sameR0/attack_rate_physical_temporal_00001_BE.rds")){
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    
    population_data <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
    for(j in 1:length(R0)){
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }
      
      attack_rate_temporal <- list()
      
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){
        print(sprintf("doing country %s for foi %s and %05d boot", i, j, b))
        load(sprintf("rds/POLYMOD/output_physical_%s_sameR0/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                     country_input, j, b), temp_env_frequency)
        
        temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_temporal > 0.1*N){
          infected_temporal <- data.frame(population_data, incidence = rowSums(temp_env_frequency$output.simulate.vsc$incidenceMat))
          temp_ar_temporal <- aggregate(infected_temporal$incidence,
                                        by = list(infected_temporal$part_age_cat),
                                        mean)
          colnames(temp_ar_temporal) <- c("part_age_cat", "attack_rates")
          temp_ar_temporal <- data.frame(temp_ar_temporal, boot = b)
          attack_rate_temporal[[count_temp]] <-  temp_ar_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      attack_rate_temporal_dataframe <- do.call("rbind", attack_rate_temporal)
      
      saveRDS(attack_rate_temporal_dataframe, 
              sprintf("rds/POLYMOD/attack_rates/sameR0/attack_rate_physical_temporal_%05d_%s.rds", as.integer(j), country_input))
    }
  }
} else {
  
  ar_baseline_flu <- list()
  ar_baseline_covid <- list()
  ar_temporal_flu <- list()
  ar_temporal_covid <- list()
  
  path_r0 <- "rds/POLYMOD/attack_rates/sameR0"
  path_foi <- "rds/POLYMOD/attack_rates/samefoi"
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("loading %s", country_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/attack_rate_physical_baseline_%05d_%s.rds", path_foi, as.integer(j), country_input)))
        temp_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/attack_rate_physical_temporal_%05d_%s.rds", path_r0, as.integer(j), country_input)))
        ar_baseline_flu[[i]] <- base_flu
        ar_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/attack_rate_physical_baseline_%05d_%s.rds", path_foi, as.integer(j), country_input)))
        temp_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/attack_rate_physical_temporal_%05d_%s.rds", path_r0, as.integer(j), country_input)))
        ar_baseline_covid[[i]] <- base_covid
        ar_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_ar_comparison_with_ci <- function(ar_baseline_flu, 
                                            ar_temporal_flu){
  
  country_length <- length(ar_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  age_cat <- c("Children", "Teens", "Adult", "Elderly")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    ar_baseline_sub <- ar_baseline_flu[[i]]
    ar_temporal_sub <- ar_temporal_flu[[i]]
    for(a in age_cat){
      temp_baseline_boot <- ar_baseline_sub[ar_baseline_sub$part_age_cat %in% a, ]
      temp_temporal_boot <- ar_temporal_sub[ar_temporal_sub$part_age_cat %in% a, ]
      
      length_baseline_boot <- nrow(temp_baseline_boot)
      length_temporal_boot <- nrow(temp_temporal_boot)
      ar_ratio <- c()
      for(b in 1:nboot){
        boot_baseline <- mean(sample(temp_baseline_boot$attack_rates, length_baseline_boot, replace = T))
        boot_temporal <- mean(sample(temp_temporal_boot$attack_rates, length_temporal_boot, replace = T))
        
        ar_ratio[b] <- boot_temporal/boot_baseline
      }
      output[count, "country"] <- country_list[i]
      output[count, "age_cat"] <- a
      output[count, "lower"] <- quantile(ar_ratio, probs = 0.025)
      output[count, "attack_rates"] <- mean(ar_ratio)
      output[count, "upper"] <- quantile(ar_ratio, probs = 0.975)
      
      count = count + 1
      print(count)
    }
  }
  return(output)
}

output_flu <- calculate_ar_comparison_with_ci(ar_baseline_flu,
                                              ar_temporal_flu)

output_covid <- calculate_ar_comparison_with_ci(ar_baseline_covid,
                                                ar_temporal_covid)

calculate_correction_of_ar_comparison_with_ci <- function(ar_baseline_flu, 
                                                          ar_temporal_flu){
  
  country_length <- length(ar_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  age_cat <- c("Children", "Teens", "Adult", "Elderly")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    ar_baseline_sub <- ar_baseline_flu[[i]]
    ar_temporal_sub <- ar_temporal_flu[[i]]
    for(a in age_cat){
      temp_baseline_boot <- ar_baseline_sub[ar_baseline_sub$part_age_cat %in% a, ]
      temp_temporal_boot <- ar_temporal_sub[ar_temporal_sub$part_age_cat %in% a, ]
      
      length_baseline_boot <- nrow(temp_baseline_boot)
      length_temporal_boot <- nrow(temp_temporal_boot)
      ar_ratio <- c()
      for(b in 1:nboot){
        boot_baseline <- mean(sample(temp_baseline_boot$attack_rates, length_baseline_boot, replace = T))
        boot_temporal <- mean(sample(temp_temporal_boot$attack_rates, length_temporal_boot, replace = T))
        
        ar_ratio[b] <- boot_baseline/boot_temporal
      }
      output[count, "country"] <- country_list[i]
      output[count, "age_cat"] <- a
      output[count, "lower"] <- quantile(ar_ratio, probs = 0.025)
      output[count, "correction"] <- mean(ar_ratio)
      output[count, "upper"] <- quantile(ar_ratio, probs = 0.975)
      
      count = count + 1
      print(count)
    }
  }
  return(output)
}

correction_flu <- calculate_correction_of_ar_comparison_with_ci(ar_baseline_flu,
                                                                ar_temporal_flu)

correction_covid <- calculate_correction_of_ar_comparison_with_ci(ar_baseline_covid,
                                                                  ar_temporal_covid)

output_correction_polymod <- rbind(cbind(correction_flu, scenario = "Influenza-like"),
                                   cbind(correction_covid, scenario = "COVID-19-like"))

output_correction_polymod$scenario <- factor(output_correction_polymod$scenario,
                                             levels = c("Influenza-like", "COVID-19-like"))

output_correction_polymod$age_cat <- factor(output_correction_polymod$age_cat,
                                            levels = c("Children", "Teens", "Adult", "Elderly"),
                                            labels = c("Children (0-12 years)", "Teens (13-18 years)",
                                                       "Adult (19-65 years)", "Elderly (66+ years)"))

output_correction_polymod_influenza <- ggplot(data = output_correction_polymod[output_correction_polymod$scenario == "Influenza-like",]) +
  geom_point(aes(country, correction, col = country)) +
  geom_errorbar(aes(x = country, ymin=lower, ymax=upper, width=0.3, col = country), linewidth = 1) +
  facet_wrap(~ age_cat, ncol = 4) +
  ylim(0, 1.2) +
  ylab("Changes in attack rates (ILI-like)") +
  xlab("Country") +
  theme_bw() +
  scale_color_manual(values = color_polymod) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 20),
        strip.background = element_rect(fill = "#edf2f4"),
        strip.text = element_text(face = "bold")) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("results/POLYMOD/ggplot_correction_onlyphysical_results_withci_influenza_sameR0.png",
        width = 35, height = 16, units = "cm")

output_correction_polymod_covid <- ggplot(data = output_correction_polymod[output_correction_polymod$scenario == "COVID-19-like",]) +
  geom_point(aes(country, correction, col = country)) +
  geom_errorbar(aes(x = country, ymin=lower, ymax=upper, width=0.3, col = country), linewidth = 1) +
  facet_wrap(~ age_cat, ncol = 4) +
  ylim(0, 1.2) +
  ylab("Changes in attack rates (COVID-19-like)") +
  xlab("Country") +
  theme_bw() +
  scale_color_manual(values = color_polymod) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 20),
        strip.background = element_rect(fill = "#edf2f4"),
        strip.text = element_text(face = "bold")) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("results/POLYMOD/ggplot_correction_onlyphysical_results_withci_covid_sameR0.png",
       width = 35, height = 16, units = "cm")

#------- peak incidence
if(!file.exists("rds/POLYMOD/peak_incidence/sameR0/peak_incidence_physical_temporal_00001_BE.rds")){
  for(i in 1:length(country_list)){
    peak_incidence_temporal <- list()
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    population_data <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
    for(j in 1:length(R0)){
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }
      
      count_temp = 1
      for(b in 1:nboot){ 
        temp_env_frequency <- environment()
        print(sprintf("doing country %s for foi %s and %05d boot", i, j, b))
        load(sprintf("rds/POLYMOD/output_physical_%s_sameR0/full_epidemic_simulation_CM_frequency_%03d_%05d.RData",
                     country_input, j, b), temp_env_frequency)
        
        temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_temporal > 0.1*N){
          pi_temporal <- max(temp_env_frequency$output.simulate.vsc$infected.daily)
          temp_pi_temporal <- data.frame(pi_temporal, boot = b)
          peak_incidence_temporal[[count_temp]] <-  temp_pi_temporal
          count_temp = count_temp + 1
        }

      }
      
      peak_incidence_temporal_dataframe <- do.call("rbind", peak_incidence_temporal)
      
      saveRDS(peak_incidence_temporal_dataframe,
              sprintf("rds/POLYMOD/peak_incidence/sameR0/peak_incidence_physical_temporal_%05d_%s.rds", as.integer(j), country_input))
    }
  }
} else {
  
  pi_baseline_flu <- list()
  pi_temporal_flu <- list()
  pi_baseline_covid <- list()
  pi_temporal_covid <- list()
  
  path_r0 <- "rds/POLYMOD/peak_incidence/sameR0"
  path_foi <- "rds/POLYMOD/peak_incidence/samefoi"
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("loading %s", country_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/peak_incidence_physical_baseline_%05d_%s.rds", path_foi, as.integer(j), country_input)))
        temp_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/peak_incidence_physical_temporal_%05d_%s.rds", path_r0, as.integer(j), country_input)))
        pi_baseline_flu[[i]] <- base_flu
        pi_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/peak_incidence_physical_baseline_%05d_%s.rds", path_foi, as.integer(j), country_input)))
        temp_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/peak_incidence_physical_temporal_%05d_%s.rds", path_r0, as.integer(j), country_input)))
        pi_baseline_covid[[i]] <- base_covid
        pi_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_pi_comparison_with_ci <- function(pi_baseline_flu, 
                                            pi_temporal_flu){
  
  country_length <- length(pi_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    pi_baseline_sub <- pi_baseline_flu[[i]]
    pi_temporal_sub <- pi_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(pi_baseline_sub)
    length_temporal_boot <- nrow(pi_temporal_sub)
    pi_ratio <- c()
    for(b in 1:nboot){
      boot_baseline <- mean(sample(pi_baseline_sub$pi_baseline, length_baseline_boot, replace = T))
      boot_temporal <- mean(sample(pi_temporal_sub$pi_temporal, length_temporal_boot, replace = T))
      
      pi_ratio[b] <- boot_temporal/boot_baseline
    }
    output[count, "country"] <- country_list[i]
    output[count, "lower"] <- quantile(pi_ratio, probs = 0.025)
    output[count, "peak_incidence"] <- mean(pi_ratio)
    output[count, "upper"] <- quantile(pi_ratio, probs = 0.975)
    
    count = count + 1
    print(count)
  }
  return(output)
}

output_pi_flu <- calculate_pi_comparison_with_ci(pi_baseline_flu,
                                                 pi_temporal_flu)

output_pi_covid <- calculate_pi_comparison_with_ci(pi_baseline_covid,
                                                   pi_temporal_covid)

png(filename = "results/POLYMOD/ratio_onlyphysical_peakincidence_withci_sameR0.png", width = 650*1.3, height = 450)
par(oma = c(2,1,0.75,1), mfrow = c(1, 2), mar = c(4.5, 4, 4, 1.5))
plot(x = seq(1, length(country_list)), 
     y = output_pi_flu$peak_incidence,
     ylim= c(0,1.5), type = "n", xlab = "Country", ylab = "", main = "Influenza-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_pi_flu$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_pi_flu$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_pi_flu$peak_incidence,
       ylim= c(0,1.5),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n", yaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "peak incidence", cex = 1.5)
plot(x = seq(1, length(country_list)), 
     y = output_pi_covid$peak_incidence,
     ylim= c(0,1.5), type = "n", xlab = "Country", ylab = "", main = "COVID-19-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_pi_covid$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_pi_covid$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_pi_covid$peak_incidence,
       ylim= c(0,1.5),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n", yaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "peak incidence", cex = 1.5)
dev.off()

#-------- epidemic duration
nboot = 3000
if(!file.exists("rds/POLYMOD/epidemic_duration/sameR0/epidemic_duration_physical_temporal_00001_BE.rds")){
  for(i in 1:length(country_list)){
    epidemic_duration_baseline <- list()
    epidemic_duration_temporal <- list()
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    
    population_data <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
    for(j in 1:length(R0)){
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){
        print(sprintf("doing country %s for foi %s and %05d boot", i, j, b))
        load(sprintf("rds/POLYMOD/output_physical_%s_sameR0/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                     country_input, j, b), temp_env_frequency)
        
        temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_temporal > 0.1*N){
          ed_temporal <- length(temp_env_frequency$output.simulate.vsc$infected.daily)
          temp_ed_temporal <- data.frame(ed_temporal, boot = b)
          epidemic_duration_temporal[[count_temp]] <-  temp_ed_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      epidemic_duration_temporal_dataframe <- do.call("rbind", epidemic_duration_temporal)
      
      saveRDS(epidemic_duration_temporal_dataframe, 
              sprintf("rds/POLYMOD/epidemic_duration/sameR0/epidemic_duration_physical_temporal_%05d_%s.rds", as.integer(j), country_input))
    }
  }
} else {
  
  ed_baseline_flu <- list()
  ed_temporal_flu <- list()
  ed_baseline_covid <- list()
  ed_temporal_covid <- list()
  
  path_r0 <- "rds/POLYMOD/epidemic_duration/sameR0"
  path_foi <- "rds/POLYMOD/epidemic_duration/samefoi"
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("loading %s", country_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/epidemic_duration_physical_baseline_%05d_%s.rds", path_foi, as.integer(j), country_input)))
        temp_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/epidemic_duration_physical_temporal_%05d_%s.rds", path_r0, as.integer(j), country_input)))
        ed_baseline_flu[[i]] <- base_flu
        ed_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/epidemic_duration_physical_baseline_%05d_%s.rds", path_foi, as.integer(j), country_input)))
        temp_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/epidemic_duration_physical_temporal_%05d_%s.rds", path_r0, as.integer(j), country_input)))
        ed_baseline_covid[[i]] <- base_covid
        ed_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_ed_comparison_with_ci <- function(ed_baseline_flu, 
                                            ed_temporal_flu){
  
  country_length <- length(ed_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    ed_baseline_sub <- ed_baseline_flu[[i]]
    ed_temporal_sub <- ed_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(ed_baseline_sub)
    length_temporal_boot <- nrow(ed_temporal_sub)
    ed_ratio <- c()
    for(b in 1:nboot){
      boot_baseline <- mean(sample(ed_baseline_sub$ed_baseline, length_baseline_boot, replace = T))
      boot_temporal <- mean(sample(ed_temporal_sub$ed_temporal, length_temporal_boot, replace = T))
      
      ed_ratio[b] <- boot_temporal/boot_baseline
    }
    output[count, "country"] <- country_list[i]
    output[count, "lower"] <- quantile(ed_ratio, probs = 0.025)
    output[count, "epidemic_duration"] <- mean(ed_ratio)
    output[count, "upper"] <- quantile(ed_ratio, probs = 0.975)
    
    count = count + 1
    print(count)
  }
  return(output)
}

output_ed_flu <- calculate_ed_comparison_with_ci(ed_baseline_flu,
                                                 ed_temporal_flu)

output_ed_covid <- calculate_ed_comparison_with_ci(ed_baseline_covid,
                                                   ed_temporal_covid)

png(filename = "results/POLYMOD/ratio_onlyphysical_epidur_withci_sameR0.png", width = 650*1.3, height = 450)
par(oma = c(2,1,0.75,1), mfrow = c(1, 2), mar = c(4.5, 4, 4, 1.5))
plot(x = seq(1, length(country_list)), 
     y = output_ed_flu$epidemic_duration,
     ylim= c(0,1.25), type = "n", xlab = "Country", ylab = "", main = "Influenza-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_ed_flu$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_ed_flu$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_ed_flu$epidemic_duration,
       ylim= c(0,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "epidemic duration", cex = 1.5)
plot(x = seq(1, length(country_list)), 
     y = output_ed_covid$epidemic_duration,
     ylim= c(0,1.25), type = "n", xlab = "Country", ylab = "", main = "COVID-19-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_ed_covid$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_ed_covid$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_ed_covid$epidemic_duration,
       ylim= c(0,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "epidemic duration", cex = 1.5)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
dev.off()

#------- time to peak  (POLYMOD)
nboot = 3000

if(!file.exists("rds/POLYMOD/timetopeak/sameR0/timetopeak_physical_temporal_00001_BE.rds")){
  for(i in 1:length(country_list)){
    timetopeak_baseline <- list()
    timetopeak_temporal <- list()
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    
    country_input = country_list[i]
    print(sprintf("doing %s for physical contacts", country_input))
    
    population_data <- readRDS(sprintf("rds/POLYMOD/data_temp_5000_pop_physical_%s.rds", country_input))$dataset.res
    for(j in 1:length(R0)){
      if(R0[j] == "1.3"){
        foi = 1
      } else {
        foi = 2
      }  
      count_base = 1
      count_temp = 1
      for(b in 1:nboot){ 
        print(sprintf("doing country %s for foi %s and %05d boot", i, j, b))
        load(sprintf("rds/POLYMOD/output_physical_%s_sameR0/full_epidemic_simulation_CM_frequency_%03d_%05d.RData", 
                     country_input, j, b), temp_env_frequency)
        
        temp_sum_temporal <- sum(temp_env_frequency$output.simulate.vsc$incidenceMat)
        
        if(temp_sum_temporal > 0.1*N){
          ttp_temporal <- which.max(temp_env_frequency$output.simulate.vsc$infected.daily)
          temp_ttp_temporal <- data.frame(ttp_temporal, boot = b)
          timetopeak_temporal[[count_temp]] <-  temp_ttp_temporal
          count_temp = count_temp + 1
        }
        
      }
      
      timetopeak_temporal_dataframe <- do.call("rbind", timetopeak_temporal)
      
      saveRDS(timetopeak_temporal_dataframe, 
              sprintf("rds/POLYMOD/timetopeak/sameR0/timetopeak_physical_temporal_%05d_%s.rds", as.integer(j), country_input))
    }
  }
} else {
  
  ttp_baseline_flu <- list()
  ttp_temporal_flu <- list()
  ttp_baseline_covid <- list()
  ttp_temporal_covid <- list()
  
  path <- "rds/POLYMOD/timetopeak/samefoi"
  for(i in 1:length(country_list)){
    country_input = country_list[i]
    print(sprintf("loading %s", country_input))
    
    for(j in 1:length(R0)){
      temp_foi <- R0[j]
      if (j == 1){
        base_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/timetopeak_physical_baseline_%05d_%s.rds", path, as.integer(j), country_input)))
        temp_flu <- data.frame(country = country_input,
                               readRDS(sprintf("%s/timetopeak_physical_temporal_%05d_%s.rds", path, as.integer(j), country_input)))
        ttp_baseline_flu[[i]] <- base_flu
        ttp_temporal_flu[[i]] <- temp_flu
      } else {
        base_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/timetopeak_physical_baseline_%05d_%s.rds", path, as.integer(j), country_input)))
        temp_covid <- data.frame(country = country_input,
                                 readRDS(sprintf("%s/timetopeak_physical_temporal_%05d_%s.rds", path, as.integer(j), country_input)))
        ttp_baseline_covid[[i]] <- base_covid
        ttp_temporal_covid[[i]] <- temp_covid
      }
    }
  }
  
}

calculate_ttp_comparison_with_ci <- function(ttp_baseline_flu, 
                                             ttp_temporal_flu){
  
  country_length <- length(ttp_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  nboot = 1000
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    ttp_baseline_sub <- ttp_baseline_flu[[i]]
    ttp_temporal_sub <- ttp_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(ttp_baseline_sub)
    length_temporal_boot <- nrow(ttp_temporal_sub)
    ttp_ratio <- c()
    for(b in 1:nboot){
      boot_baseline <- mean(sample(ttp_baseline_sub$ttp_baseline, length_baseline_boot, replace = T))
      boot_temporal <- mean(sample(ttp_temporal_sub$ttp_temporal, length_temporal_boot, replace = T))
      
      ttp_ratio[b] <- boot_temporal/boot_baseline
    }
    output[count, "country"] <- country_list[i]
    output[count, "lower"] <- quantile(ttp_ratio, probs = 0.025)
    output[count, "timetopeak"] <- mean(ttp_ratio)
    output[count, "upper"] <- quantile(ttp_ratio, probs = 0.975)
    
    count = count + 1
    print(count)
  }
  return(output)
}

output_ttp_flu <- calculate_ttp_comparison_with_ci(ttp_baseline_flu,
                                                   ttp_temporal_flu)

output_ttp_covid <- calculate_ttp_comparison_with_ci(ttp_baseline_covid,
                                                     ttp_temporal_covid)

png(filename = "results/POLYMOD/ratio_onlyphysical_timetopeak_withci_sameR0.png", width = 650*1.3, height = 450)
par(oma = c(2,1,0.75,1), mfrow = c(1, 2), mar = c(4.5, 4, 4, 1.5))
plot(x = seq(1, length(country_list)), 
     y = output_ttp_flu$timetopeak,
     ylim= c(0,1.25), type = "n", xlab = "Country", ylab = "", main = "Influenza-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_ttp_flu$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_ttp_flu$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_ttp_flu$timetopeak,
       ylim= c(0,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "time to peak of the epidemic", cex = 1.5)
plot(x = seq(1, length(country_list)), 
     y = output_ttp_covid$timetopeak,
     ylim= c(0,1.25), type = "n", xlab = "Country", ylab = "", main = "COVID-19-like", xaxt = "n",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
arrows(x0 = seq(1, length(country_list)),
       y0 = output_ttp_covid$lower,
       x1 = seq(1, length(country_list)),
       y1 = output_ttp_covid$upper,
       angle = 90, code = 3, col = "darkolivegreen4", length = 0.05, lwd = 2,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xaxt = "n", yaxt = "n")
points(x = seq(1, length(country_list)), 
       y = output_ttp_covid$timetopeak,
       ylim= c(0,1.25),
       lty = 2, pch = 22, cex = 1, bg = "darkolivegreen4",
       cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4, xaxt = "n")
axis(1, at = 1:length(country_list), labels=country_list,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mtext(side = 2, line = 3.5, "Frequency-based over naive", cex = 1.5)
mtext(side = 2, line = 2.5, "time to peak of the epidemic", cex = 1.5)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
dev.off()

#-------- extinction rate
nboot = 3000

calculate_extinctionrate <- function(ttp_baseline_flu, 
                                     ttp_temporal_flu, nboot = 3000){
  
  country_length <- length(ttp_baseline_flu)
  country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
  output <- data.frame()
  count = 1
  for(i in 1:country_length){
    er_baseline_sub <- ttp_baseline_flu[[i]]
    er_temporal_sub <- ttp_temporal_flu[[i]]
    
    length_baseline_boot <- nrow(er_baseline_sub)
    length_temporal_boot <- nrow(er_temporal_sub)
    
    boot_baseline <- 1-(length_baseline_boot/nboot)
    boot_temporal <- 1-(length_temporal_boot/nboot)
    
    output[count, "country"] <- country_list[i]
    output[count, "extinction_rate_baseline"] <- round(boot_baseline,3)
    output[count, "extinction_rate_temporal"] <- round(boot_temporal,3)
    count = count + 1
    print(count)
  }
  return(output)
}

output_extinctionrate_flu <- calculate_extinctionrate(ttp_baseline_flu,
                                                      ttp_temporal_flu)

range(output_extinctionrate_flu$extinction_rate_temporal/output_extinctionrate_flu$extinction_rate_baseline)

#-----------
# plot different q 

data_output <- data.frame()
for(i in 1:length(country_list)){
  country_input <- country_list[i]
  
  temp_naive <- unique(read.csv(sprintf("../data/vsc_polymod_input/input_forVSC_physical_%s.csv", country_input))$foi)
  temp_freq <- unique(read.csv(sprintf("../data/vsc_polymod_input/freq_input_forVSC_physical_%s.csv", country_input))$foi)
  
  data_output[i, "country"] <- country_input
  data_output[i, "Influenza-like"] <- temp_freq[1]/temp_naive[1]
  data_output[i, "COVID-19-like"] <- temp_freq[2]/temp_naive[2]
}

data_output <- pivot_longer(data_output, 
                        cols = -country)
data_output$name <- factor(data_output$name, levels = c("Influenza-like", "COVID-19-like"))

output_q <- ggplot(data = data_output) +
  geom_point(aes(x = country, y = value, col = name, shape = name), stroke = 1.5) +
  scale_shape_manual(values = c(4, 5)) +
  ylim(1, 1.5) +
  ylab("Ratio of the frequency-based over naive \n for the probability of transmission per contact") +
  xlab("Country") +
  facet_wrap(~ name, ncol = 4) +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 20),
        strip.background = element_rect(fill = "#edf2f4"),
        strip.text = element_text(face = "bold")) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("results/POLYMOD/ggplot_ratio_q_sameR0.png",
       width = 1350*3, height = 675*3, units = "px")
