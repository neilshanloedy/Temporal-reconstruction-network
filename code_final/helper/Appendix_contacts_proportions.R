rm(list = ls())
library(rstudioapi)
library(ggpubr)
library(data.table)
library(scales)

dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
source("functions.R")
part_common <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_participant_common.csv")
country <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")

for(i in country){
  prop.age.partdata <- read.csv("../../data/population_data_proportion.csv")[,-1] %>% filter(geo %in% i)
  
  # cross-sectional survey
  dataset <- input_data(country = i)
  dataset <- data_cleaning(dataset)
  dataset <- data_to_agecat(dataset)
  
  dataset$frequency_multi <- factor(dataset$frequency_multi,
                                    levels=c('1','2','3','4','5'),
                                    labels=c('Daily','Weekly','Monthly',
                                             'A few times a year','First time'))
  
  dataset$dayofweek <- factor(dataset$dayofweek,
                              levels=c('2','3','4','5','6','0','1'),
                              labels=c('Mon','Tue','Wed','Thu',
                                       'Fri','Sat','Sun'))
  
  dataset <- dataset[!is.na(dataset$frequency_multi),]
  temp_dat <- data_prepare_frequency(dataset)
  temp_dat$part_age_cat <- factor(temp_dat$part_age_cat,
                                  levels = c("Children", "Teens", "Adult", "Elderly"))
  temp_dat$cnt_home <- factor(temp_dat$cnt_home, levels = c(F,T),
                              labels = c("Contacts not at home", 
                                         "Contacts at home"))
  temp_dat$phys_contact <- factor(temp_dat$phys_contact, levels = c("2","1"),
                              labels = c("Non-physical contacts", "Physical contacts"))
  
  plot_home <- f_plot_agecat_home()
  plot_phys <- f_plot_agecat_phys()
  
  plot_output <- ggarrange(plot_home,
                           plot_phys,
                           ncol = 1,
                           common.legend = T)
  ggsave(sprintf("../results/EDA_POLYMOD/contacts_comparison_home_physical_%s.png", i),
         width = 25, height = 25, units = "cm")
}

#---------

rm(list = ls())

dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
source("functions.R")
part_common <- read.csv("../../data/CoMix_be_zenodo/CoMix_BE_participant_common.csv")
wave_list = seq(9,43)

output_home <- list()
output_phys <- list()

# cross-sectional survey
for(i in 1:length(wave_list)){
  wave = wave_list[i]
  print(sprintf("doing %s", wave))
  
  dataset <- input_data_comix(wave = wave)
  dataset <- data_cleaning(dataset)
  dataset <- data_to_agecat_comix(dataset)
  
  if(wave %in% c(9,10,11)){
    dataset$frequency_multi <- factor(dataset$frequency_multi,
                                      levels=c('1','2','3','4','5'),
                                      labels=c('daily','weekly','monthly',
                                               'a few times a year','first time'))
  } else {
    dataset$frequency_multi <- factor(dataset$frequency_multi,
                                      levels=c('2','3','4','5','1'),
                                      labels=c('daily','weekly','monthly',
                                               'a few times a year','first time'))
  }
  
  dataset$dayofweek <- factor(dataset$dayofweek,
                              levels=c('2','3','4','5','6','0','1'),
                              labels=c('Mon','Tue','Wed','Thu',
                                       'Fri','Sat','Sun'))
  
  dataset <- dataset[!is.na(dataset$frequency_multi),]
  # differentiate between weekend and weekday contacts for rpois.
  temp_dat <- data_prepare_frequency(dataset)
  
  temp_dat$part_age_cat <- factor(temp_dat$part_age_cat,
                                  levels = c("Children", "Teens", "Adult", "Elderly"))
  temp_dat$cnt_home <- factor(temp_dat$cnt_home, levels = c(F,T),
                              labels = c("Contacts not at home", 
                                         "Contacts at home"))
  temp_dat$phys_contact <- factor(temp_dat$phys_contact, levels = c("2","1"),
                                  labels = c("Non-physical contacts", "Physical contacts"))
  
  data_home <- temp_dat %>% 
    na.omit() %>% 
    group_by(frequency_multi, part_age_cat, cnt_home) %>% 
    summarise(n = sum(n)) %>% 
    group_by(cnt_home, part_age_cat) %>% 
    mutate(pctg = round(n/sum(n), 2))
  
  output_home[[i]] <- xtabs(pctg ~ part_age_cat + frequency_multi + cnt_home, data = data_home)
  
  data_phys <- temp_dat %>% 
    na.omit() %>% 
    group_by(frequency_multi, part_age_cat, phys_contact) %>% 
    summarise(n = sum(n)) %>% 
    group_by(phys_contact, part_age_cat) %>% 
    mutate(pctg = round(n/sum(n), 2)) 
  
  output_phys[[i]] <- xtabs(pctg ~ part_age_cat + frequency_multi + phys_contact, data = data_phys)

}

temporary_not_home <- c()
temporary_home <- c()
temporary_physical <- c()
temporary_nonphys <- c()

for(i in 1:length(output_home)){
  temporary_not_home <- rbind(temporary_not_home, setDT(data.frame(cbind(output_home[[i]][,,1], wave = wave_list[i])), keep.rownames = "age_cat"))
  temporary_home <- rbind(temporary_home, setDT(data.frame(cbind(output_home[[i]][,,2], wave = wave_list[i])), keep.rownames = "age_cat"))
  
  temporary_nonphys <- rbind(temporary_nonphys, setDT(data.frame(cbind(output_phys[[i]][,,1], wave = wave_list[i])), keep.rownames = "age_cat"))
  temporary_physical <- rbind(temporary_physical, setDT(data.frame(cbind(output_phys[[i]][,,2], wave = wave_list[i])), keep.rownames = "age_cat"))
}

temporary_not_home <- melt(temporary_not_home, value.name = "percentage", 
                           variable.name = "frequency", id = c("age_cat", "wave"))
temporary_home <- melt(temporary_home, value.name = "percentage", 
                           variable.name = "frequency", id = c("age_cat", "wave"))
temporary_physical <- melt(temporary_physical, value.name = "percentage", 
                           variable.name = "frequency", id = c("age_cat", "wave"))
temporary_nonphys <- melt(temporary_nonphys, value.name = "percentage", 
                           variable.name = "frequency", id = c("age_cat", "wave"))

#--------

levels_age <- unique(temporary_home$age_cat)

png(filename = "../results/CoMix/appendix_paper_contacts_home_proportions.png", width = 900, height = 700)
par(oma = c(3,1,0.75,1), mfrow = c(2, 2), mar = c(3, 4, 3, 1))
for(i in levels_age){
  plot(temporary_home[temporary_home$age_cat == i & temporary_home$frequency == "daily",]$wave, 
       temporary_home[temporary_home$age_cat == i & temporary_home$frequency == "daily",]$percentage,
       type = "b", ylim = c(0, 1), xaxt = "n", pch = 21, bg = "#F8766D",
       main = i, xlab = "", ylab = "",
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  points(temporary_home[temporary_home$age_cat == i & temporary_home$frequency == "weekly",]$wave, 
       temporary_home[temporary_home$age_cat == i & temporary_home$frequency == "weekly",]$percentage,
       type = "b", yaxt = "n", xaxt = "n", pch = 22, bg = "#A3A500", cex = 1.5)
  points(temporary_home[temporary_home$age_cat == i & temporary_home$frequency == "monthly",]$wave, 
         temporary_home[temporary_home$age_cat == i & temporary_home$frequency == "monthly",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 23, bg = "#00BF7D", cex = 1.5)
  points(temporary_home[temporary_home$age_cat == i & temporary_home$frequency == "a.few.times.a.year",]$wave, 
         temporary_home[temporary_home$age_cat == i & temporary_home$frequency == "a.few.times.a.year",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 24, bg = "#00B0F6", cex = 1.5)
  points(temporary_home[temporary_home$age_cat == i & temporary_home$frequency == "first.time",]$wave, 
         temporary_home[temporary_home$age_cat == i & temporary_home$frequency == "first.time",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 25, bg = "#E76BF3", cex = 1.5)
  axis(1, at = seq(9, 43, by = 2), las = 1,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  mtext(side = 2, line = 3, "Percentage of contacts", cex = 1.5)
  if(i %in% c("Adult", "Elderly")){
    mtext(side = 1, line = 2.5, "Survey wave", cex = 1.5)
  }
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n', cex = 1.5)
legend(x = "bottom", inset = 0,
       legend = c("Daily", "Weekly", "Monthly", "A few times a year", "First time"), 
       pt.bg= c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"), 
       pch = c(21,22,23,24,25),
       cex=1.5, horiz = TRUE, bty = "n")
dev.off()

png(filename = "../results/CoMix/appendix_paper_contacts_nothome_proportions.png", width = 900, height = 700)
par(oma = c(3,1,0.75,1), mfrow = c(2, 2), mar = c(3, 4, 3, 1))
for(i in levels_age){
  plot(temporary_not_home[temporary_not_home$age_cat == i & temporary_not_home$frequency == "daily",]$wave, 
       temporary_not_home[temporary_not_home$age_cat == i & temporary_not_home$frequency == "daily",]$percentage,
       type = "b", ylim = c(0, 1), xaxt = "n", pch = 21, bg = "#F8766D",
       main = i, xlab = "", ylab = "",
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  points(temporary_not_home[temporary_not_home$age_cat == i & temporary_not_home$frequency == "weekly",]$wave, 
         temporary_not_home[temporary_not_home$age_cat == i & temporary_not_home$frequency == "weekly",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 22, bg = "#A3A500", cex = 1.5)
  points(temporary_not_home[temporary_not_home$age_cat == i & temporary_not_home$frequency == "monthly",]$wave, 
         temporary_not_home[temporary_not_home$age_cat == i & temporary_not_home$frequency == "monthly",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 23, bg = "#00BF7D", cex = 1.5)
  points(temporary_not_home[temporary_not_home$age_cat == i & temporary_not_home$frequency == "a.few.times.a.year",]$wave, 
         temporary_not_home[temporary_not_home$age_cat == i & temporary_not_home$frequency == "a.few.times.a.year",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 24, bg = "#00B0F6", cex = 1.5)
  points(temporary_not_home[temporary_not_home$age_cat == i & temporary_not_home$frequency == "first.time",]$wave, 
         temporary_not_home[temporary_not_home$age_cat == i & temporary_not_home$frequency == "first.time",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 25, bg = "#E76BF3", cex = 1.5)
  axis(1, at = seq(9, 43, by = 2), las = 1,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  mtext(side = 2, line = 3, "Percentage of contacts", cex = 1.5)
  if(i %in% c("Adult", "Elderly")){
    mtext(side = 1, line = 2.5, "Survey wave", cex = 1.5)
  }
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = "bottom", inset = 0,
       legend = c("Daily", "Weekly", "Monthly", "A few times a year", "First time"), 
       pt.bg= c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"), 
       pch = c(21,22,23,24,25),
       cex=1.5, horiz = TRUE, bty = "n")
dev.off()

png(filename = "../results/CoMix/appendix_paper_contacts_physical_proportions.png", width = 900, height = 700)
par(oma = c(3,1,0.75,1), mfrow = c(2, 2), mar = c(3, 4, 3, 1))
for(i in levels_age){
  plot(temporary_physical[temporary_physical$age_cat == i & temporary_physical$frequency == "daily",]$wave, 
       temporary_physical[temporary_physical$age_cat == i & temporary_physical$frequency == "daily",]$percentage,
       type = "b", ylim = c(0, 1), xaxt = "n", pch = 21, bg = "#F8766D",
       main = i, xlab = "", ylab = "",
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  points(temporary_physical[temporary_physical$age_cat == i & temporary_physical$frequency == "weekly",]$wave, 
         temporary_physical[temporary_physical$age_cat == i & temporary_physical$frequency == "weekly",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 22, bg = "#A3A500", cex = 1.5)
  points(temporary_physical[temporary_physical$age_cat == i & temporary_physical$frequency == "monthly",]$wave, 
         temporary_physical[temporary_physical$age_cat == i & temporary_physical$frequency == "monthly",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 23, bg = "#00BF7D", cex = 1.5)
  points(temporary_physical[temporary_physical$age_cat == i & temporary_physical$frequency == "a.few.times.a.year",]$wave, 
         temporary_physical[temporary_physical$age_cat == i & temporary_physical$frequency == "a.few.times.a.year",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 24, bg = "#00B0F6", cex = 1.5)
  points(temporary_physical[temporary_physical$age_cat == i & temporary_physical$frequency == "first.time",]$wave, 
         temporary_physical[temporary_physical$age_cat == i & temporary_physical$frequency == "first.time",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 25, bg = "#E76BF3", cex = 1.5)
  axis(1, at = seq(9, 43, by = 2), las = 1,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  mtext(side = 2, line = 3, "Percentage of contacts", cex = 1.5)
  if(i %in% c("Adult", "Elderly")){
    mtext(side = 1, line = 2, "Survey wave", cex = 1.5)
  }
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = "bottom", inset = 0,
       legend = c("Daily", "Weekly", "Monthly", "A few times a year", "First time"), 
       pt.bg= c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"), 
       pch = c(21,22,23,24,25),
       cex=1.5, horiz = TRUE, bty = "n")
dev.off()

png(filename = "../results/CoMix/appendix_paper_contacts_nonphysical_proportions.png", width = 900, height = 700)
par(oma = c(3,1,0.75,1), mfrow = c(2, 2), mar = c(3, 4, 3, 1))
for(i in levels_age){
  plot(temporary_nonphys[temporary_nonphys$age_cat == i & temporary_nonphys$frequency == "daily",]$wave, 
       temporary_nonphys[temporary_nonphys$age_cat == i & temporary_nonphys$frequency == "daily",]$percentage,
       type = "b", ylim = c(0, 1), xaxt = "n", pch = 21, bg = "#F8766D",
       main = i, xlab = "", ylab = "",
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  points(temporary_nonphys[temporary_nonphys$age_cat == i & temporary_nonphys$frequency == "weekly",]$wave, 
         temporary_nonphys[temporary_nonphys$age_cat == i & temporary_nonphys$frequency == "weekly",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 22, bg = "#A3A500", cex = 1.5)
  points(temporary_nonphys[temporary_nonphys$age_cat == i & temporary_nonphys$frequency == "monthly",]$wave, 
         temporary_nonphys[temporary_nonphys$age_cat == i & temporary_nonphys$frequency == "monthly",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 23, bg = "#00BF7D", cex = 1.5)
  points(temporary_nonphys[temporary_nonphys$age_cat == i & temporary_nonphys$frequency == "a.few.times.a.year",]$wave, 
         temporary_nonphys[temporary_nonphys$age_cat == i & temporary_nonphys$frequency == "a.few.times.a.year",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 24, bg = "#00B0F6", cex = 1.5)
  points(temporary_nonphys[temporary_nonphys$age_cat == i & temporary_nonphys$frequency == "first.time",]$wave, 
         temporary_nonphys[temporary_nonphys$age_cat == i & temporary_nonphys$frequency == "first.time",]$percentage,
         type = "b", yaxt = "n", xaxt = "n", pch = 25, bg = "#E76BF3", cex = 1.5)
  axis(1, at = seq(9, 43, by = 2), las = 1,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  mtext(side = 2, line = 3, "Percentage of contacts", cex = 1.5)
  if(i %in% c("Adult", "Elderly")){
    mtext(side = 1, line = 2, "Survey wave", cex = 1.5)
  }
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = "bottom", inset = 0,
       legend = c("Daily", "Weekly", "Monthly", "A few times a year", "First time"), 
       pt.bg= c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"), 
       pch = c(21,22,23,24,25),
       cex=1.5, horiz = TRUE, bty = "n")
dev.off()

