# This script is used to correct 
# the fatigue effect in the CoMix data
#
# Created by : Neilshan Loedy
# Date : 11/01/2023
#------------------------
rm(list = ls())

# run first code.R
library(rstudioapi)
library(readr)
library(gamlss)
library(gdata)
library(doParallel)
dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
source("helper/functions.R")
source("helper/functions_fatigue.R")
suppressPackageStartupMessages(source('socrates_rshiny-master/socrates_rshiny-master/R/socrates_main.R'))

# load the data
#----------------
merged_dataset <- read.csv("../data/CoMix_fatigue/comix_dataset_allcontacts.csv")[,-1]
merged_dataset_physical <- read.csv("../data/CoMix_fatigue/comix_dataset_physical.csv")[,-1]
merged_dataset_outhome <- read.csv("../data/CoMix_fatigue/comix_dataset_outhome.csv")[,-1]
merged_dataset_inshome <- read.csv("../data/CoMix_fatigue/comix_dataset_inshome.csv")[,-1]

merged_dataset <- prepare_data_tomodel(merged_dataset)
merged_dataset_physical <- prepare_data_tomodel(merged_dataset_physical)
merged_dataset_outhome <- prepare_data_tomodel(merged_dataset_outhome)
merged_dataset_inshome <- prepare_data_tomodel(merged_dataset_inshome)

# load the aggregated data
#--------------------------
merged_dataset_sum <- read.csv("../data/CoMix_fatigue/comix_dataset_allcontacts_sum.csv")[,-1]
merged_dataset_physical_sum <- read.csv("../data/CoMix_fatigue/comix_dataset_physical_sum.csv")[,-1]
merged_dataset_outhome_sum <- read.csv("../data/CoMix_fatigue/comix_dataset_outhome_sum.csv")[,-1]
merged_dataset_inshome_sum <- read.csv("../data/CoMix_fatigue/comix_dataset_inshome_sum.csv")[,-1]

merged_dataset_sum <- prepare_sumdata_tomodel(merged_dataset_sum)
merged_dataset_physical_sum <- prepare_sumdata_tomodel(merged_dataset_physical_sum)
merged_dataset_outhome_sum <- prepare_sumdata_tomodel(merged_dataset_outhome_sum)
merged_dataset_inshome_sum <- prepare_sumdata_tomodel(merged_dataset_inshome_sum)

model_allcontacts_sum <- readRDS("rds/CoMix_fatigue_gamlss/Gamlss_fatigue_allcontacts_sum.rds")
model_physical_sum <- readRDS("rds/CoMix_fatigue_gamlss/Gamlss_fatigue_physical_sum.rds")
model_outhome_sum <- readRDS("rds/CoMix_fatigue_gamlss/Gamlss_fatigue_outhome_sum.rds")
model_inshome_sum <- readRDS("rds/CoMix_fatigue_gamlss/Gamlss_fatigue_inshome_sum.rds")

cnt_model_marg_allcontacts_sum <- marginal_dataframe(merged_dataset_sum, model_allcontacts_sum)
cnt_model_marg_physical_sum <- marginal_dataframe(merged_dataset_physical_sum, model_physical_sum)
cnt_model_marg_outhome_sum <- marginal_dataframe(merged_dataset_outhome_sum, model_outhome_sum)
cnt_model_marg_inshome_sum <- marginal_dataframe(merged_dataset_inshome_sum, model_inshome_sum)

list_result <- c("cnt_model_marg_allcontacts_sum", "cnt_model_marg_physical_sum",
                 "cnt_model_marg_outhome_sum", "cnt_model_marg_inshome_sum")

list_output_sum <- list()
for(i in 1:length(list_result)){
  boot <- c()
  boot_predict <- c()
  boot_predictf <- c()
  
  temp_data <- get(list_result[i]) %>% 
    dplyr::select(part_id, part_uid, wave, n, predict, predict_f) %>% 
    group_by(part_id, part_uid, wave) %>% 
    summarise(n = sum(n),
              n_predict = sum(predict),
              n_predict_f = sum(predict_f))
  
  temporary <- rbind(
    temp_data %>%
      group_by(wave) %>%
      summarise(mean_contacts = mean(n, na.rm = TRUE),
                sd_n = sd(n, na.rm = TRUE),
                n_length = n()) %>%
      mutate(se_n = sd_n / sqrt(n_length),
             lower = mean_contacts - 1.96 * se_n,
             upper = mean_contacts + 1.96 * se_n,
             source = "Observed") %>% 
      dplyr::select(wave, lower, mean_contacts, upper, source),
    temp_data %>%
      group_by(wave) %>%
      summarise(mean_contacts = mean(n_predict, na.rm = T),
                sd_npredict = sd(n_predict, na.rm = TRUE),
                n_length = n()) %>%
      mutate(se_predict = sd_npredict / sqrt(n_length),
             lower = mean_contacts - 1.96 * se_predict,
             upper = mean_contacts + 1.96 * se_predict,
             source = "Model-based") %>% 
      dplyr::select(wave, lower, mean_contacts, upper, source),
    temp_data %>%
      group_by(wave) %>%
      summarise(mean_contacts = mean(n_predict_f, na.rm = T),
                sd_npredict_f = sd(n_predict_f, na.rm = TRUE),
                n_length = n()) %>%
      mutate(se_predict_f = sd_npredict_f / sqrt(n_length),
             lower = mean_contacts - 1.96 * se_predict_f,
             upper = mean_contacts + 1.96 * se_predict_f,
             source = "Model-based with fatigue correction") %>% 
      dplyr::select(wave, lower, mean_contacts, upper, source))
  
  list_output_sum[[i]] <- cbind(temporary, scenario = sub("^cnt_model_marg_", "", list_result[i]))
}

output_comix_final_sum <- do.call("rbind", list_output_sum)

redistribute_allcontacts <- redistribute_prediction(cnt_model_marg_allcontacts_sum,
                                                    merged_dataset)

redistribute_allcontacts <- redistribute_allcontacts %>% 
  dplyr::select(-c(predict, n)) %>% 
  rename(n = predict_f)

redistribute_physical <- redistribute_prediction(cnt_model_marg_physical_sum,
                                                 merged_dataset_physical)

redistribute_physical <- redistribute_physical %>% 
  dplyr::select(-c(predict, n)) %>% 
  rename(n = predict_f)

redistribute_outhome <- redistribute_prediction(cnt_model_marg_outhome_sum,
                                                merged_dataset_outhome)

redistribute_outhome <- redistribute_outhome %>% 
  dplyr::select(-c(predict, n)) %>% 
  rename(n = predict_f)

redistribute_inshome <- redistribute_prediction(cnt_model_marg_inshome_sum,
                                                merged_dataset_inshome)

redistribute_inshome <- redistribute_inshome %>% 
  dplyr::select(-c(predict, n)) %>% 
  rename(n = predict_f)

write.csv(redistribute_allcontacts, "../data/CoMix_Fatigue/redistribute_allcontacts.csv")
write.csv(redistribute_physical, "../data/CoMix_Fatigue/redistribute_physical.csv")
write.csv(redistribute_outhome, "../data/CoMix_Fatigue/redistribute_outhome.csv")
write.csv(redistribute_inshome, "../data/CoMix_Fatigue/redistribute_inshome.csv")

# plot output
#----------------
output_ggplot <- ggplot(data = output_comix_final_sum) +
  geom_point(aes(x = wave, y = mean_contacts, color = source)) +
  geom_errorbar(aes(x = wave, ymin = lower, ymax = upper, color = source, width = 0.2)) +
  facet_wrap(~ scenario, labeller = labeller(scenario = 
                                               c("allcontacts_sum" = "All contacts",
                                                 "inshome_sum" = "Inside home contacts",
                                                 "outhome_sum" = "Outside home contacts",
                                                 "physical_sum" = "Physical contacts")
  )) +
  ylim(0,5) +
  xlab("Wave") +
  ylab("Mean of contacts") +
  theme_classic() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 18))

ggsave("results/CoMix/ggplot_gamlss_model_comparisons.png",
       width = 25, height = 18, units = "cm")

#-----------

output_comix_final_sum %>% 
  group_by(source, scenario) %>% 
  summarise(mean = mean(mean_contacts)) %>% 
  group_by(scenario) %>% 
  mutate(mean_ratio = mean/mean[source == "Observed"]) 

#--------------
# create new dataset for CM
#--------------

data_temp <- read.csv("../data/CoMix_Fatigue/comix_dataset_physical_sum.csv")[,-1]
data_temp_freq <- read.csv("../data/CoMix_Fatigue/comix_dataset_physical.csv")[,-1]
comix_data <- readRDS("../../data/CoMix/survey_belgium2020_comix_up_to_wave_43.rds")

data_be <- merge(data_temp, comix_data$contacts, by = c("part_id"))

data_be$cnt_age_mean <- mapply(function(x, y)
  if(is.na(x) == F & is.na(y) == F){
    # sample an age based on the min and max age they reported
    resample(seq(x, y), 1)
  } else {
    # if no age was reported, sample between 0-120
    resample(seq(0,100), 1)
  }, data_be$cnt_age_est_min, data_be$cnt_age_est_max)

data_be <- data_be %>% 
  mutate(cnt_age_cat = ifelse(cnt_age_mean < 13, "Children", ifelse(cnt_age_mean < 19, "Teens",
                       ifelse(cnt_age_mean < 71, "Adult", "Elderly"))))

result_newcontacts <- prepare_data_physical_fatigue(data_temp_freq,
                                                     comix_data)

write.csv(result_newcontacts, "../data/CoMix_fatigue/newcontacts_physical_fatiguecorrected.csv")

# calculate scm --------

wave = c(19, 27, 43)

for(i in wave){
  comix_data_participants <- comix_data$participants
  survey_temp2 <- survey(comix_data_participants, result_newcontacts)
  
  cm <- list()
  cm_capita <- list()
  frequency.label <- c(1:5)
  frequency.name <- c("daily", "weekly", "monthly", "few", "first")
  names <- c("Children", "Teens", "Adult", "Elderly")
  age_breaks<- c(0,13,19,66)
  
  for(freq in frequency.label){
    cm[[frequency.name[freq]]] <- socialmixr::contact_matrix(survey=survey_temp2, age.limits = age_breaks,
                                                             estimated.contact.age="sample", weigh.age=T, weigh.dayofweek=T,
                                                             weight.threshold = 3, filter = list(frequency_multi= frequency.name[freq],
                                                                                                 wave = i,
                                                                                                 phys_contact=1))$matrix
    
    
    cm_capita[[frequency.name[freq]]] <- socialmixr::contact_matrix(survey=survey_temp2, age.limits = age_breaks,
                                                                    estimated.contact.age="sample", weigh.age=T, weigh.dayofweek=T,
                                                                    weight.threshold = 3, filter = list(frequency_multi= frequency.name[freq],
                                                                                                        wave = i,
                                                                                                        phys_contact=1))$matrix.per.capita
    
  }
  
  cm[["all"]] <- Reduce('+', cm)
  cm_capita[["all"]] <- Reduce('+', cm_capita)
  
  # Function to change colnames and rownames for a matrix
  change_names <- function(mat) {
    colnames(mat) <- paste0(names)
    rownames(mat) <- paste0(names)
    return(mat)
  }
  
  cm <- lapply(cm, change_names)
  cm_capita <- lapply(cm_capita, change_names)
  
  save(list = c("frequency.name", "names", "cm",
                "cm_capita"), file = sprintf("../data/data_input/CoMix_fatigue/social_matrices_physical_%04d.RData", i))
}

# calculate foi --------
R0 = c(1.3, 3.3)
gamma = 1/7
nboot = 3000

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

# 19 --> the wave is without weight since with weight --> complex problem
for(i in wave){
    foi <- list()
    
    print(paste0("doing wave ", i))
    title_phys <- "_physical"
    load(sprintf("../data/data_input/CoMix_fatigue/input_comix%s_%04d.RData", title_phys, i))
    load(sprintf("../data/data_input/CoMix_fatigue/social_matrices%s_%04d.RData", title_phys, i))
    data_temp <- readRDS(sprintf("rds/CoMix/data_temp_CoMix%s_5000pop_%04d.rds", title_phys, i))
    
    S = diag(table(data_temp$dataset.res$part_age_cat))
    A = diag(1, nrow = 4, ncol = 4)
    C = diag(1, nrow = 4, ncol = 4)
    B = (t(cm$all)/as.vector(table(data_temp$dataset.res$part_age_cat))) 
    
    foi <- sapply(R0, function(x) calculate_q_cm(x, 0.02))
    print(paste0(i, " : ", foi))
    foi_length = length(foi)
    
    output <- data.frame(id = c(seq(1, nboot), seq(1, nboot)),
                         foi = c(rep(foi[1], each = nboot), rep(foi[2], each = nboot)),
                         R0 = rep(c(1,2), each = nboot),
                         wave = rep(i, (nboot + nboot)))
    
    filename = sprintf("../data/vsc_comix_input/input_forVSC%s_%04d.csv", title_phys, i)
    write.csv(output, filename, quote = F, row.names = F)
}

