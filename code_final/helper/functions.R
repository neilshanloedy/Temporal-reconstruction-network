library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(purrr)
library(xtable)
# library(PupillometryR)
# library(ggpubr)
library(scales)
library(rlang)
library(dplyr)
library(stringr)
library(matrixStats)
library(gridExtra)

if (!require('HMMpa')) install.packages('HMMpa'); library('HMMpa')

set.seed(123)
plot_height <- 105
plot_width <- 148.5

input_data <- function(country = "BE"){
  contact_common <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_contact_common.csv")
  part_common <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_participant_common.csv")
  part_extra <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_participant_extra.csv")
  hh_common <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_hh_common.csv")
  hh_extra <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_hh_extra.csv")
  sday <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_sday.csv")
  
  data <- merge(hh_common, merge(part_common, 
                                 merge(contact_common, sday,
                                       by = c("part_id")), by = c("part_id")), by = c("hh_id"))
  
  data <-   data[data$country == country,]
  
  return(data)
}

input_data_with_children <- function(country = "BE"){
  contact_common <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_contact_common.csv")
  part_common <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_participant_common.csv")
  part_extra <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_participant_extra.csv")
  hh_common <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_hh_common.csv")
  hh_extra <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_hh_extra.csv")
  sday <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_sday.csv")
  
  data <- merge(hh_extra, merge(hh_common, merge(part_common, 
                                 merge(contact_common, sday,
                                       by = c("part_id")), by = c("part_id")), by = c("hh_id")),
                by = c("hh_id"))
  
  data <-   data[data$country == country,]
  
  return(data)
}

input_data_comix <- function(wave = c(19, 43),
                             country_code = "be"){
  contact_common <- read.csv(sprintf("../../data/CoMix_%s_zenodo/CoMix_%s_contact_common.csv", country_code, country_code))
  part_common <- read.csv(sprintf("../../data/CoMix_%s_zenodo/CoMix_%s_participant_common.csv", country_code, country_code))
  hh_common <- read.csv(sprintf("../../data/CoMix_%s_zenodo/CoMix_%s_hh_common.csv", country_code, country_code))
  sday <- read.csv(sprintf("../../data/CoMix_%s_zenodo/CoMix_%s_sday.csv", country_code, country_code))
  
  data <- merge(hh_common, merge(part_common, 
                                 merge(contact_common, sday,
                                       by = c("part_id")), by = c("part_id")), by = c("hh_id"))
  
  data <-   data[data$wave %in% wave,]
  
  return(data)
}

input_data_comix_UK <- function(wave = c(19, 43)){
  contact_common <- read.csv("../../data/CoMix_UK_Zenodo/CoMix_uk_contact_common.csv")
  part_common <- read.csv("../../data/CoMix_UK_Zenodo/CoMix_uk_participant_common.csv")
  hh_common <- read.csv("../../data/CoMix_UK_Zenodo/CoMix_uk_hh_common.csv")
  sday <- read.csv("../../data/CoMix_UK_Zenodo/CoMix_uk_sday.csv")
  
  data <- merge(hh_common, merge(part_common, 
                                 merge(contact_common, sday,
                                       by = c("part_id")), by = c("part_id")), by = c("hh_id"))
  
  data <-   data[data$wave %in% wave,]
  
  return(data)
}

input_data_comix_CH <- function(wave = c(19, 43)){
  contact_common <- read.csv("../../data/CoMix_CH_Zenodo/CoMix_ch_contact_common.csv")
  part_common <- read.csv("../../data/CoMix_CH_Zenodo/CoMix_ch_participant_common.csv")
  hh_common <- read.csv("../../data/CoMix_CH_Zenodo/CoMix_ch_hh_common.csv")
  sday <- read.csv("../../data/CoMix_CH_Zenodo/CoMix_ch_sday.csv")
  
  data <- merge(hh_common, merge(part_common, 
                                 merge(contact_common, sday,
                                       by = c("part_id")), by = c("part_id")), by = c("hh_id"))
  
  data <-   data[data$wave %in% wave,]
  
  return(data)
}

data_population_2022 <- function(country = "BE"){
  if (country == "BE"){
  data.pop.2022 <- readxl::read_excel("../../data/population_2022.xlsx")
  data.pop.2022 <- data.pop.2022 %>% 
    dplyr::select(CD_AGE, CD_SEX, MS_POPULATION) %>% 
    rename(age = CD_AGE,
           sex = CD_SEX,
           pop = MS_POPULATION) %>% 
    mutate(part_age_cat = ifelse(age %in% seq(0,12), "Children",
                              ifelse(age %in% seq(13,18), "Teens",
                                     ifelse(age %in% seq(19,65), "Adult", "Elderly"))))
  } else if (country == "IT"){
    data.pop.2022 <- read.csv("../../data/population_2022_italy.csv")
    data.pop.2022 <- data.pop.2022[data.pop.2022$Territory == "Italy",]
    data.pop.2022 <- data.pop.2022[data.pop.2022$Age != "total",]
    data.pop.2022 <- data.pop.2022[data.pop.2022$Gender != "total",]
    data.pop.2022 <- data.pop.2022 %>% 
      select(Age, Gender, Value)
    data.pop.2022$Age <- as.numeric(gsub("([0-9]+).*$", "\\1", data.pop.2022$Age))
    data.pop.2022 <- data.pop.2022 %>% 
      rename(age = Age,
             sex = Gender,
             pop = Value) %>% 
      mutate(sex = ifelse(sex == "males", "M", "F"),
             part_age_cat = ifelse(age %in% seq(0,12), "Children",
                                   ifelse(age %in% seq(13,18), "Teens",
                                          ifelse(age %in% seq(19,65), "Adult", "Elderly"))))
    
  }
  return(data.pop.2022)
}

data_cleaning <- function(dataset){
  dataset <- dataset %>% 
    mutate(contact_day = ifelse(dayofweek %in% c("0","1"), "weekend", "weekday")) %>% 
    as.data.frame()
  factor.col <- c("part_id", "cont_id",
                  "frequency_multi", "phys_contact",
                  "duration_multi", "contact_day")
  dataset$part_id <- as.character(dataset$part_id)
  dataset[factor.col] <- lapply(dataset[factor.col], factor)
  dataset <- dataset %>% 
    mutate_if(is.character,as.factor)
  
  dataset$sday_id <- as.Date(as.character(dataset$sday_id), "%Y%m%d")
  return(dataset)
}

data_cleaning_comix <- function(dataset){
  dataset <- dataset %>% 
    as.data.frame()
  factor.col <- c("part_id",
                  "frequency_multi", "contact_day")
  dataset$part_id <- as.character(dataset$part_id)
  dataset[factor.col] <- lapply(dataset[factor.col], factor)
  dataset <- dataset %>% 
    mutate_if(is.character,as.factor)
  
  dataset$sday_id <- as.Date(as.character(dataset$sday_id), "%Y-%m-%d")
  return(dataset)
}

data_cleaning_fatigue <- function(dataset){
  factor.col <- c("part_id", "cont_id",
                  "frequency_multi")
  dataset[factor.col] <- lapply(dataset[factor.col], factor)
  dataset <- dataset %>% 
    mutate_if(is.character,as.factor)
  
  dataset$sday_id <- as.Date(as.character(dataset$sday_id), "%Y%m%d")
  return(dataset)
}

data_sampling <- function(dataset){
  # sampling contact age
  dataset$cnt_age_exact <- mapply(function(x, y)
    if(is.na(x) == F & is.na(y) == F){
      # sample an age based on the min and max age they reported
      sample(seq(x, y), 1)
    } else {
      # if no age was reported, sample between 0-120
      sample(seq(0,100), 1)
    }, dataset$cnt_age_est_min, dataset$cnt_age_est_max)
  
  return(dataset)
}

data_to_agecat <- function(dataset){
  dataset <- dataset %>% 
    mutate(
      part_age_cat = ifelse(
        part_age %in% seq(0,12), "Children",
        ifelse(part_age %in% seq(13,18), "Teens",
               ifelse(part_age %in% seq(19, 65), "Adult", "Elderly"))),
      cnt_age_cat = ifelse(
        cnt_age_exact %in% seq(0,12), "Children",
        ifelse(cnt_age_exact %in% seq(13,18), "Teens",
               ifelse(cnt_age_exact %in% seq(19, 65), "Adult", "Elderly"))
      ))
  
  dataset$part_age_cat <- factor(dataset$part_age_cat,
                                 levels = c("Children", "Teens", "Adult",
                                            "Elderly"))
  return(dataset)
}

data_to_agecat_with_children <- function(dataset){
  
  dataset %>% 
    mutate(part_age_cat = ifelse(part_age_cat %in% "Adult", any(.[, grepl("hh_age_", names(.))] <= 12), 1, 0))
  
  dataset <- dataset %>%
    rowwise() %>% 
    mutate(part_age_cat = case_when(
      part_age_cat == "Adult" & any(c_across(starts_with("hh_age_")) <= 12) ~ "Adult with Children",
      TRUE ~ as.character(part_age_cat)
    ))
  
  dataset <- dataset[, !grepl("^hh_age_", names(dataset))]
  return(dataset)
}

data_to_agecat_comix <- function(dataset){
  dataset <- dataset %>% 
    mutate(
      part_age_cat = ifelse(
        part_age %in% c("[0,1)", "[1,6)","[6,12)"), "Children",
        ifelse(part_age %in% c("[12,18)") ,"Teens",
               ifelse(part_age %in% c("[18,30)", "[30,40)", "[40,50)",
                      "[50,60)"), "Adult", "Elderly"))),
      cnt_age_cat = ifelse(
        cnt_age_exact %in% c("[0,1)", "[1,6)","[6,12)"), "Children",
        ifelse(cnt_age_exact %in% c("[12,18)"), "Teens",
               ifelse(cnt_age_exact %in% c("[18,30)", "[30,40)", "[40,50)",
                      "[50,60)"), "Adult", "Elderly"))
      ))
  
  dataset$part_age_cat <- factor(dataset$part_age_cat,
                                 levels = c("Children", "Teens", "Adult",
                                            "Elderly"))
  return(dataset)
}

data_prepare <- function(data = data) {
  weekend <- c('Sat', 'Sun')
  weekday <- c('Mon','Tue','Wed','Thu',
               'Fri')
  
  temp_dat <- data %>%
    group_by(part_id, part_age_cat, frequency_multi, contact_day) %>%
    summarise(n = n()) %>% 
    na.omit() %>% 
    data.frame()
  
  temp_dat <- temp_dat %>% 
    tidyr::complete(part_id, nesting(frequency_multi)) %>%
    mutate(n = replace_na(n, 0)) %>% 
    group_by(part_id) %>% 
    fill(part_age_cat, .direction = "downup") %>% 
    fill(contact_day, .direction = "downup") %>% 
    inner_join(distinct(data[,c("part_id", "dayofweek")]))
  
  temp_dat$contact_day <- ifelse(is.na(as.character(temp_dat$contact_day)) != T, as.character(temp_dat$contact_day),
                                 ifelse(as.character(temp_dat$dayofweek) %in% weekday, "weekday", "weekend"))
  
  temp_dat$part_age_cat <- ifelse(is.na(temp_dat$part_age_cat), as.character(data$part_age_cat[match(temp_dat$part_id, data$part_id)]),
              as.character(temp_dat$part_age_cat))
  
  
  return(temp_dat)
}

data_prepare_frequency <- function(data = data){
  weekend <- c('Sat', 'Sun')
  weekday <- c('Mon','Tue','Wed','Thu',
               'Fri')
  
  temp_dat <- data %>%
    group_by(part_id, part_age_cat, frequency_multi, contact_day, cnt_home, phys_contact) %>%
    summarise(n = n()) %>% 
    na.omit() %>% 
    data.frame()
  
  temp_dat <- temp_dat %>% 
    tidyr::complete(part_id, nesting(frequency_multi)) %>%
    mutate(n = replace_na(n, 0)) %>% 
    group_by(part_id) %>% 
    fill(part_age_cat, .direction = "downup") %>% 
    fill(contact_day, .direction = "downup") %>% 
    inner_join(distinct(data[,c("part_id", "dayofweek")]))
  
  temp_dat$contact_day <- ifelse(is.na(as.character(temp_dat$contact_day)) != T, as.character(temp_dat$contact_day),
                                 ifelse(as.character(temp_dat$dayofweek) %in% weekday, "weekday", "weekend"))
  
  temp_dat$part_age_cat <- ifelse(is.na(temp_dat$part_age_cat), as.character(data$part_age_cat[match(temp_dat$part_id, data$part_id)]),
                                  as.character(temp_dat$part_age_cat))
  
  
  return(temp_dat)
}

input_binomial <- function(data = data){
  
  number.contact <- data %>% 
    group_by(frequency_multi) %>% 
    summarise(mean = mean(n),
              var = var(n)) %>% 
    na.omit()
  
  number.contact.general <- data %>% 
    group_by(part_age_cat, frequency_multi) %>% 
    summarise(mean = mean(n),
              var = var(n)) %>% 
    na.omit()
  
  number.contact.input <- list(number.contact = number.contact,
                               number.contact.general = number.contact.general)
  
  return(number.contact.input)
}

input_binomial_fatigue <- function(data = data, fatigue_correction = F){
  
  if(fatigue_correction == F){
    number.contact <- data %>% 
      group_by(frequency_multi) %>% 
      summarise(mean = mean(predict),
                var = var(predict)) %>% 
      na.omit()
    
    number.contact.general <- data %>% 
      group_by(part_age_cat, frequency_multi) %>% 
      summarise(mean = mean(predict),
                var = var(predict)) %>% 
      na.omit()
    
    number.contact.input <- list(number.contact = number.contact,
                                 number.contact.general = number.contact.general)
  } else {
    number.contact <- data %>% 
      group_by(frequency_multi) %>% 
      summarise(mean = mean(predict_f),
                var = var(predict_f)) %>% 
      na.omit()
    
    number.contact.general <- data %>% 
      group_by(part_age_cat, frequency_multi) %>% 
      summarise(mean = mean(predict_f),
                var = var(predict_f)) %>% 
      na.omit()
    
    number.contact.input <- list(number.contact = number.contact,
                                 number.contact.general = number.contact.general)
  }
  
  return(number.contact.input)
}

input_zibinomial <- function(data = data){
  prob.zero <- temp_dat %>% 
    mutate(is.zero = ifelse(n == 0, 1, 0)) %>% 
    group_by(part_age_cat, frequency_multi, contact_day) %>% 
    summarise(sum.zero = sum(is.zero),
              total.data = n(),
              prob.zero = sum.zero/total.data)
  
  prob.zero.noweekend <- temp_dat %>% 
    mutate(is.zero = ifelse(n == 0, 1, 0)) %>% 
    group_by(part_age_cat, frequency_multi) %>% 
    summarise(sum.zero = sum(is.zero),
              total.data = n(),
              prob.zero = sum.zero/total.data)
  
  number.contact.zi <- temp_dat %>% 
    group_by(part_age_cat, frequency_multi, contact_day) %>% 
    summarise(mean = mean(n),
              var = var(n)) %>% 
    na.omit()
  
  number.contact.noweekend <- temp_dat %>% 
    group_by(part_age_cat, frequency_multi) %>% 
    summarise(mean = mean(n),
              var = var(n)) %>% 
    na.omit()
  number.contact.input.zi <- list(number.contact = number.contact.zi,
                                  prob.zero = prob.zero,
                                  number.contact.general = number.contact.noweekend,
                                  prob.zero.general = prob.zero.noweekend)
  
  return(number.contact.input.zi)
}

input.zirnbinom.dens <- function(dataset, prob.data){
  mean <- dataset[dataset$frequency_multi == period,]$mean
  var <- dataset[dataset$frequency_multi == period,]$var
  size <- size(mean, var)
  prob <- prob(size, mean)
  
  prob.zero <- prob.data[prob.data$frequency_multi == period,]$prob.zero
  
  return(list(mean = mean, var = var,
              size = size, prob = prob,
              prob.zero = prob.zero))
}

rnzibinom <- function(n, size, prob, prob.zero){
  # Generate binary values indicating excess zeros
  excess_zeros <- rnbinom(n, 1, prob.zero)
  
  # Combine excess zeros and non-zero counts
  zinb_data <- sapply(excess_zeros, function(x) ifelse(x == 1, 0, rnbinom(1, size, prob)))
  return(zinb_data)
}

proportion.week.prep <- function(data){
  proportion.week <- data %>% 
    group_by(part_id, part_age_cat, dayofweek) %>% 
    summarise(n = sum(n)) %>% 
    group_by(part_age_cat, dayofweek) %>%
    summarise(n = mean(n)) %>%
    na.omit() %>%
    mutate(week = ifelse(dayofweek %in% c("Sat", "Sun"), "weekend", "weekday")) %>% 
    group_by(part_age_cat, week) %>%
    mutate(pctg = n/sum(n))
  
  return(proportion.week)
}

proportion.week.prep.fatigue <- function(data, fatigue_correction = F){
  
  if(fatigue_correction == F){
    proportion.week <- data %>% 
      group_by(part_id, part_age_cat, dayofweek) %>% 
      summarise(n = sum(predict)) %>% 
      group_by(part_age_cat, dayofweek) %>%
      summarise(n = mean(n)) %>%
      na.omit() %>%
      mutate(week = ifelse(dayofweek %in% c("Sat", "Sun"), "weekend", "weekday")) %>% 
      group_by(part_age_cat, week) %>%
      mutate(pctg = n/sum(n))
  } else {
    proportion.week <- data %>% 
      group_by(part_id, part_age_cat, dayofweek) %>% 
      summarise(n = sum(predict_f)) %>% 
      group_by(part_age_cat, dayofweek) %>%
      summarise(n = mean(n)) %>%
      na.omit() %>%
      mutate(week = ifelse(dayofweek %in% c("Sat", "Sun"), "weekend", "weekday")) %>% 
      group_by(part_age_cat, week) %>%
      mutate(pctg = n/sum(n))
  }
  return(proportion.week)
}

proportion.week.noweekend <- function(data){
  
}

size <- function(mean_val, var_val){
  mean_val^2 / (var_val - mean_val)
}

prob <- function(size, mean_val){
  size / (size + mean_val)
}

choose.df <- function(x){
  out <- as.vector(as.matrix(x))
  return(out)
}

distangle.function <- function(data,
                               day = NULL,
                               id = NULL){
  
  if(!is.null(day) & is.null(id)){
    return(data[[day]])
  }
  
  if(!is.null(day) & !is.null(id)){
    return(data[[day]][[id]])
  }
  
}

# used
plot_two_lists_and_means <- function(list1, list2, main_title = "q = 0.035", epi_baseline = 0, tot_pop = N,
                                     n_sample = 30) {
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_NAs, list1)
  list_two <- do.call(rbind_fill_NAs, list2)
  
  list_one_baseline <- list_one[rowSums(list_one, na.rm = T) > epi_baseline, ]
  list_two_baseline <- list_two[rowSums(list_two, na.rm = T) > epi_baseline, ]
  
  sample_plot_one <- sample(nrow(list_one_baseline), n_sample)
  sample_plot_two <- sample(nrow(list_two_baseline), n_sample)
  
  mean_list1 <- list_one_baseline[sample_plot_one, ]
  mean_list2 <- list_two_baseline[sample_plot_two, ]
  
  # mean_list1 <- colMeans(list_one_baseline, na.rm = T)/tot_pop
  # mean_list2 <- colMeans(list_two_baseline, na.rm = T)/tot_pop
  
  # lower_list1 <- apply(list_one_baseline, 2, function(x) quantile(x, 0.025, na.rm = T))
  # upper_list1 <- apply(list_one_baseline, 2, function(x) quantile(x, 0.975, na.rm = T))
  
  # lower_list2 <- apply(list_two_baseline, 2, function(x) quantile(x, 0.025, na.rm = T))
  # upper_list2 <- apply(list_two_baseline, 2, function(x) quantile(x, 0.975, na.rm = T))
  
  name_list <- c("list1", "list2")
  color <- c("blue", "red")
  
  plot(mean_list1, type = "n", ylim = range(c(list_one_baseline, list_two_baseline), na.rm = T)/tot_pop,
       xlim = c(0, max(ncol(mean_list1), ncol(mean_list2), na.rm = T)),
       xlab = "Timestep", ylab = "Number of infected individuals")
  for(i in 1:n_sample){
    lines(mean_list1[i,]/tot_pop, lwd = 1, col = adjustcolor(col = "blue", alpha = 0.5))
    lines(mean_list2[i,]/tot_pop, lwd = 1, col = adjustcolor(col = "red", alpha = 0.5))
  }
  legend("topright", legend = c("Reconstructed approach", "Naive approach"),
         col = c("blue", "red"), lty = c(1,1))
  title(main = paste0(main_title))
  
  # plot(mean_list1, col = "blue", lwd = 1, ylim = range(c(list_one_baseline, list_two_baseline), na.rm = T)/tot_pop,
  #      xlim = c(0, max(length(mean_list1), length(mean_list2), na.rm = T)),
  #      xlab = "Timestep", ylab = "Number of infected individuals")
  # lines(lower_list1, col = "blue", lwd = 1)
  # lines(upper_list1, col = "blue", lwd = 1)
  
  # points(mean_list2, col = "red", pch = 2)
  # lines(lower_list2, col = "red", lwd = 1)
  # lines(upper_list2, col = "red", lwd = 1)
  # col = 1
  # for(list_name in name_list){
  #   temp <- get(list_name)
  #   for(row in 1:length(temp)){
  #     lines(unlist(temp[row])/tot_pop, col = adjustcolor(color[col], alpha = 0.025))
  #   }
  #   col = col + 1
  # }
  
}

# used
plot_lists_and_means <- function(list1, list2, list3, list4, 
                                 main_title = "q = 0.035", epi_baseline = 0, tot_pop = N,
                                 n_sample = 30) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_NAs, list1)
  list_two <- do.call(rbind_fill_NAs, list2)
  list_thr <- do.call(rbind_fill_NAs, list3)
  list_fou <- do.call(rbind_fill_NAs, list4)
  
  list_one_baseline <- list_one[rowSums(list_one, na.rm = T) > epi_baseline, ]
  list_two_baseline <- list_two[rowSums(list_two, na.rm = T) > epi_baseline, ]
  list_thr_baseline <- list_thr[rowSums(list_thr, na.rm = T) > epi_baseline, ]
  list_fou_baseline <- list_fou[rowSums(list_fou, na.rm = T) > epi_baseline, ]
  
  sample_plot_one <- sample(nrow(list_one_baseline), n_sample)
  sample_plot_two <- sample(nrow(list_two_baseline), n_sample)
  sample_plot_thr <- sample(nrow(list_thr_baseline), n_sample)
  sample_plot_fou <- sample(nrow(list_fou_baseline), n_sample)
  
  mean_list1 <- list_one_baseline[sample_plot_one, ]
  mean_list2 <- list_two_baseline[sample_plot_two, ]
  mean_list3 <- list_thr_baseline[sample_plot_thr, ]
  mean_list4 <- list_fou_baseline[sample_plot_fou, ]
  
  name_list <- c("list1", "list2", "list3", "list4")
  color <- c("blue", "red")
  
  plot(mean_list1, type = "n", ylim = range(c(list_one_baseline, list_two_baseline,
                                              list_thr_baseline, list_fou_baseline), na.rm = T)/tot_pop,
       xlim = c(0, max(ncol(mean_list1), ncol(mean_list2), ncol(mean_list3),
                       ncol(mean_list4), na.rm = T)),
       xlab = "Timestep", ylab = "Number of infected individuals")
  for(i in 1:n_sample){
    lines(mean_list1[i,]/tot_pop, lwd = 2, col = adjustcolor(col = "blue", alpha = 0.5))
    lines(mean_list2[i,]/tot_pop, lwd = 2, col = adjustcolor(col = "red", alpha = 0.5))
    lines(mean_list3[i,]/tot_pop, lwd = 2, col = adjustcolor(col = "blue", alpha = 0.5), lty = 3)
    lines(mean_list4[i,]/tot_pop, lwd = 2, col = adjustcolor(col = "red", alpha = 0.5), lty = 3)
  }
  legend("topright", legend = c("Reconstructed approach", "Naive approach",
                                "Reconstructed approach (Physical)",
                                "Naive approach (Physical)"),
         col = c("blue", "red"), lty = c(1,1,3,3))
  title(main = paste0(main_title))
}

# used
plot_two_lists_length <- function(list1, list2, main_title = "Time steps", epi_baseline = 500,
                                  final_size_one = simulate.hist.ppt,
                                  final_size_two = simulate.hist.base) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_NAs, list1)
  list_two <- do.call(rbind_fill_NAs, list2)
  
  list_one_baseline <- list_one[unlist(final_size_one) > epi_baseline, ]
  list_two_baseline <- list_two[unlist(final_size_two) > epi_baseline, ]
  
  list_one_timesteps <- apply(list_one_baseline, 1, function(x) length(x[!is.na(x)]))
  list_two_timesteps <- apply(list_two_baseline, 1, function(x) length(x[!is.na(x)]))
  
  boxplot(list_one_timesteps,
          list_two_timesteps, border = c("blue", "red"),
          col = c("white", "white"),
          names = c("Reconstructed approach", "Naive approach"),
          ylab = "Time",
          lwd = 1.5)
  points(1, mean(list_one_timesteps), col = "blue", cex = 1.5, pch = 18)
  points(2, mean(list_two_timesteps), col = "red", cex = 1.5, pch = 18)
  title(main = paste0(main_title))
}

plot_lists_length <- function(list1, list2, list3, list4,
                              main_title = "Time steps", epi_baseline = 500,
                              final_size_one = simulate.temporal,
                              final_size_two = simulate.baseline,
                              final_size_thr = simulate.temporal.phys,
                              final_size_fou = simulate.baseline.phys) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_NAs, list1)
  list_two <- do.call(rbind_fill_NAs, list2)
  list_thr <- do.call(rbind_fill_NAs, list3)
  list_fou <- do.call(rbind_fill_NAs, list4)
  
  list_one_baseline <- list_one[unlist(final_size_one) > epi_baseline, ]
  list_two_baseline <- list_two[unlist(final_size_two) > epi_baseline, ]
  list_thr_baseline <- list_thr[unlist(final_size_thr) > epi_baseline, ]
  list_fou_baseline <- list_fou[unlist(final_size_fou) > epi_baseline, ]
  
  list_one_timesteps <- apply(list_one_baseline, 1, function(x) length(x[!is.na(x)]))
  list_two_timesteps <- apply(list_two_baseline, 1, function(x) length(x[!is.na(x)]))
  list_thr_timesteps <- apply(list_thr_baseline, 1, function(x) length(x[!is.na(x)]))
  list_fou_timesteps <- apply(list_fou_baseline, 1, function(x) length(x[!is.na(x)]))
  
  boxplot(list_one_timesteps,
          list_two_timesteps, 
          list_thr_timesteps,
          list_fou_timesteps, 
          border = c("blue", "red", "blue", "red"),
          lty = c(1,1,6,6),
          col = c("white", "white", "white", "white"),
          names = c("Reconstructed approach", "Naive approach",
                    "Reconstructed approach (Physical)", 
                    "Naive approach (Physical)"),
          ylab = "Time",
          lwd = 1.5)
  points(1, mean(list_one_timesteps), col = "blue", cex = 1.5, pch = 18)
  points(2, mean(list_two_timesteps), col = "red", cex = 1.5, pch = 18)
  points(3, mean(list_thr_timesteps), col = "blue", cex = 1.5, pch = 18)
  points(4, mean(list_fou_timesteps), col = "red", cex = 1.5, pch = 18)
  title(main = paste0(main_title))
}

# used
plot_two_lists_extinction_prob <- function(list1, list2, main_title = "Extinction Probability", epi_baseline = 50,
                                           final_size_one = simulate.hist.ppt,
                                           final_size_two = simulate.hist.base) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_NAs, list1)
  list_two <- do.call(rbind_fill_NAs, list2)
  
  nrow_one <- nrow(list_one)
  nrow_two <- nrow(list_two)
  
  list_one_baseline <- list_one[unlist(final_size_one) > epi_baseline, ]
  list_two_baseline <- list_two[unlist(final_size_two) > epi_baseline, ]
  
  ep_one <- 1-(nrow(list_one_baseline)/nrow_one)
  ep_two <- 1-(nrow(list_two_baseline)/nrow_two)
  
  data_plot <- data.frame(approach = c("Reconstruction approach", "Naive approach"),
                          val = c(ep_one, ep_two))
  
  plot(c(1,2), data_plot$val, 
       xlab = "",
       ylab = "Probability",
       type = "b", lty = 2, lwd = 1.5,
       ylim = c(0,1), xlim = c(0.75,2.25), xaxt = 'n')
  axis(1, at = c(1,2), labels = c("Reconstruction approach", "Naive approach"))
  title(main = paste0(main_title))
}    

plot_lists_extinction_prob <- function(list1, list2, list3, list4,
                                       main_title = "Extinction Probability", epi_baseline = 50,
                                       final_size_one = simulate.temporal,
                                       final_size_two = simulate.baseline,
                                       final_size_thr = simulate.temporal.phys,
                                       final_size_fou = simulate.baseline.phys) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_NAs, list1)
  list_two <- do.call(rbind_fill_NAs, list2)
  list_thr <- do.call(rbind_fill_NAs, list3)
  list_fou <- do.call(rbind_fill_NAs, list4)
  
  nrow_one <- nrow(list_one)
  nrow_two <- nrow(list_two)
  nrow_thr <- nrow(list_thr)
  nrow_fou <- nrow(list_fou)
  
  list_one_baseline <- list_one[unlist(final_size_one) > epi_baseline, ]
  list_two_baseline <- list_two[unlist(final_size_two) > epi_baseline, ]
  list_thr_baseline <- list_thr[unlist(final_size_thr) > epi_baseline, ]
  list_fou_baseline <- list_fou[unlist(final_size_fou) > epi_baseline, ]
  
  ep_one <- 1-(nrow(list_one_baseline)/nrow_one)
  ep_two <- 1-(nrow(list_two_baseline)/nrow_two)
  ep_thr <- 1-(nrow(list_thr_baseline)/nrow_thr)
  ep_fou <- 1-(nrow(list_fou_baseline)/nrow_fou)
  
  data_plot <- data.frame(approach = c("Reconstruction approach", "Naive approach",
                                       "Reconstruction approach (Physical)", "Naive approach (Physical)"),
                          val = c(ep_one, ep_two, ep_thr, ep_fou))
  
  plot(c(1,2), data_plot[1:2,]$val, 
       xlab = "",
       ylab = "Probability",
       type = "b", lty = 2, lwd = 1.5,
       ylim = c(0,1), xlim = c(0.75,4.25), xaxt = 'n')
  lines(c(3,4), data_plot[3:4,]$val, type = "b",
        lty = 2, lwd = 1.5)
  axis(1, at = c(1,2,3,4), labels = c("Reconstruction approach", "Naive approach", 
                                      "Reconstruction approach (Physical)", "Naive approach (Physical)"))
  title(main = paste0(main_title))
} 

# used
plot_two_lists_peakcases <- function(list1, list2, main_title = "Peak number of cases", epi_baseline = 50, tot_pop = N,
                                     final_size_one = simulate.hist.ppt,
                                     final_size_two = simulate.hist.base) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_NAs, list1)
  list_two <- do.call(rbind_fill_NAs, list2)
  
  list_one_baseline <- list_one[unlist(final_size_one) > epi_baseline, ]
  list_two_baseline <- list_two[unlist(final_size_two) > epi_baseline, ]
  
  list_one_max <- apply(list_one_baseline, 1, function(x) max(x, na.rm = T))
  list_two_max <- apply(list_two_baseline, 1, function(x) max(x, na.rm = T))
  
  name_list <- c("list1", "list2")
  color <- c("blue", "red")
  
  boxplot(list_one_max,
          list_two_max, border = c("blue", "red"),
          col = c("white", "white"),
          names = c("Reconstructed approach", "Naive approach"),
          ylab = "Peak Incidence",
          lwd = 1.5)
  points(1, mean(list_one_max), col = "blue", cex = 1.5, pch = 18)
  points(2, mean(list_two_max), col = "red", cex = 1.5, pch = 18)
  title(main = paste0(main_title))
  
}

plot_lists_peakcases <- function(list1, list2, list3, list4,
                                 main_title = "Peak number of cases", epi_baseline = 50, tot_pop = N,
                                 final_size_one = simulate.temporal,
                                 final_size_two = simulate.baseline,
                                 final_size_thr = simulate.temporal.phys,
                                 final_size_fou = simulate.baseline.phys) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_NAs, list1)
  list_two <- do.call(rbind_fill_NAs, list2)
  list_thr <- do.call(rbind_fill_NAs, list3)
  list_fou <- do.call(rbind_fill_NAs, list4)
  
  list_one_baseline <- list_one[unlist(final_size_one) > epi_baseline, ]
  list_two_baseline <- list_two[unlist(final_size_two) > epi_baseline, ]
  list_thr_baseline <- list_thr[unlist(final_size_thr) > epi_baseline, ]
  list_fou_baseline <- list_fou[unlist(final_size_fou) > epi_baseline, ]
  
  list_one_max <- apply(list_one_baseline, 1, function(x) max(x, na.rm = T))
  list_two_max <- apply(list_two_baseline, 1, function(x) max(x, na.rm = T))
  list_thr_max <- apply(list_thr_baseline, 1, function(x) max(x, na.rm = T))
  list_fou_max <- apply(list_fou_baseline, 1, function(x) max(x, na.rm = T))
  
  name_list <- c("list1", "list2", "list3", "list4")
  color <- c("blue", "red","blue","red")
  
  boxplot(list_one_max,
          list_two_max, 
          list_thr_max,
          list_fou_max,
          border = c("blue", "red","blue","red"),
          lty = c(1,1,6,6),
          col = c("white", "white","white", "white"),
          names = c("Reconstructed approach", "Naive approach",
                    "Reconstructed approach (Physical)", "Naive approach (Physical)"),
          ylab = "Peak Incidence",
          lwd = 1.5)
  points(1, mean(list_one_max), col = "blue", cex = 1.5, pch = 18)
  points(2, mean(list_two_max), col = "red", cex = 1.5, pch = 18)
  points(3, mean(list_thr_max), col = "blue", cex = 1.5, pch = 18)
  points(4, mean(list_fou_max), col = "red", cex = 1.5, pch = 18)
  title(main = paste0(main_title))
  
}

# used
plot_two_lists_attackrate <- function(list1, list2, main_title = "Peak number of cases", epi_baseline = 50, tot_pop = N,
                                      final_size_one = simulate.hist.ppt,
                                      final_size_two = simulate.hist.base) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_NAs, list1)
  list_two <- do.call(rbind_fill_NAs, list2)
  
  list_one_baseline <- which(unlist(final_size_one) > epi_baseline)
  list_two_baseline <- which(unlist(final_size_two) > epi_baseline)
  
  list_plot_one <- unlist(final_size_one)[list_one_baseline]/tot_pop
  list_plot_two <- unlist(final_size_two)[list_two_baseline]/tot_pop
  
  name_list <- c("list1", "list2")
  color <- c("blue", "red")
  
  boxplot(list_plot_one,
          list_plot_two, border = c("blue", "red"),
          col = c("white", "white"),
          names = c("Reconstructed approach", "Naive approach"),
          ylab = "Attack rate",
          lwd = 1.5)
  points(1, mean(list_plot_one), col = "blue", cex = 1.5, pch = 18)
  points(2, mean(list_plot_two), col = "red", cex = 1.5, pch = 18)
  
  title(main = paste0(main_title))
  
}

plot_lists_attackrate <- function(list1, list2, list3, list4,
                                  main_title = "Peak number of cases", 
                                  epi_baseline = 50, tot_pop = N,
                                  final_size_one = simulate.temporal,
                                  final_size_two = simulate.baseline,
                                  final_size_thr = simulate.temporal.phys,
                                  final_size_fou = simulate.baseline.phys) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_NAs, list1)
  list_two <- do.call(rbind_fill_NAs, list2)
  list_thr <- do.call(rbind_fill_NAs, list3)
  list_fou <- do.call(rbind_fill_NAs, list4)
  
  list_one_baseline <- which(unlist(final_size_one) > epi_baseline)
  list_two_baseline <- which(unlist(final_size_two) > epi_baseline)
  list_thr_baseline <- which(unlist(final_size_thr) > epi_baseline)
  list_fou_baseline <- which(unlist(final_size_fou) > epi_baseline)
  
  list_plot_one <- unlist(final_size_one)[list_one_baseline]/tot_pop
  list_plot_two <- unlist(final_size_two)[list_two_baseline]/tot_pop
  list_plot_thr <- unlist(final_size_thr)[list_thr_baseline]/tot_pop
  list_plot_fou <- unlist(final_size_fou)[list_fou_baseline]/tot_pop
  
  name_list <- c("list1", "list2","list3","list4")
  color <- c("blue", "red","blue","red")
  
  boxplot(list_plot_one,
          list_plot_two, 
          list_plot_thr,
          list_plot_fou,
          border = c("blue", "red","blue","red"),
          col = c("white", "white","white","white"),
          lty = c(1,1,6,6),
          names = c("Reconstructed approach", "Naive approach",
                    "Reconstructed approach (Physical)", "Naive approach (Physical)"),
          ylab = "Attack rate",
          lwd = 1.5)
  points(1, mean(list_plot_one), col = "blue", cex = 1.5, pch = 18)
  points(2, mean(list_plot_two), col = "red", cex = 1.5, pch = 18)
  points(3, mean(list_plot_thr), col = "blue", cex = 1.5, pch = 18)
  points(4, mean(list_plot_fou), col = "red", cex = 1.5, pch = 18)
  
  title(main = paste0(main_title))
  
}


# used 
plot_two_lists_timetopeak <- function(list1, list2, main_title = "Time steps", epi_baseline = 500,
                                      final_size_one = simulate.hist.ppt,
                                      final_size_two = simulate.hist.base) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_zeros, list1)
  list_two <- do.call(rbind_fill_zeros, list2)
  
  list_one_baseline <- list_one[unlist(final_size_one) > epi_baseline, ]
  list_two_baseline <- list_two[unlist(final_size_two) > epi_baseline, ]
  
  list_one_timesteps <- apply(list_one_baseline, 1, which.max)
  list_two_timesteps <- apply(list_two_baseline, 1, which.max)
  
  boxplot(list_one_timesteps,
          list_two_timesteps, border = c("blue", "red"),
          col = c("white", "white"),
          names = c("Reconstructed approach", "Naive approach"),
          ylab = "Time to peak incidence",
          lwd = 1.5)
  points(1, mean(list_one_timesteps), col = "blue", cex = 1.5, pch = 18)
  points(2, mean(list_two_timesteps), col = "red", cex = 1.5, pch = 18)
  title(main = paste0(main_title))
}

plot_lists_timetopeak <- function(list1, list2, list3, list4,
                                  main_title = "Time steps", epi_baseline = 500,
                                  final_size_one = simulate.temporal,
                                  final_size_two = simulate.baseline,
                                  final_size_thr = simulate.temporal.phys,
                                  final_size_fou = simulate.baseline.phys) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  rbind_fill_NAs <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_NAs <- max_rows - length(x)
        c(x, rep(NA, pad_NAs))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_zeros, list1)
  list_two <- do.call(rbind_fill_zeros, list2)
  list_thr <- do.call(rbind_fill_zeros, list3)
  list_fou <- do.call(rbind_fill_zeros, list4)
  
  list_one_baseline <- list_one[unlist(final_size_one) > epi_baseline, ]
  list_two_baseline <- list_two[unlist(final_size_two) > epi_baseline, ]
  list_thr_baseline <- list_thr[unlist(final_size_thr) > epi_baseline, ]
  list_fou_baseline <- list_fou[unlist(final_size_fou) > epi_baseline, ]
  
  list_one_timesteps <- apply(list_one_baseline, 1, which.max)
  list_two_timesteps <- apply(list_two_baseline, 1, which.max)
  list_thr_timesteps <- apply(list_thr_baseline, 1, which.max)
  list_fou_timesteps <- apply(list_fou_baseline, 1, which.max)
  
  boxplot(list_one_timesteps,
          list_two_timesteps,
          list_thr_timesteps,
          list_fou_timesteps,
          border = c("blue", "red","blue", "red"),
          col = c("white", "white","white", "white"),
          names = c("Reconstructed approach", "Naive approach",
                    "Reconstructed approach (Physical)", "Naive approach (Physical)"),
          ylab = "Time to peak incidence",
          lty = c(1,1,6,6),
          lwd = 1.5)
  points(1, mean(list_one_timesteps), col = "blue", cex = 1.5, pch = 18)
  points(2, mean(list_two_timesteps), col = "red", cex = 1.5, pch = 18)
  points(3, mean(list_thr_timesteps), col = "blue", cex = 1.5, pch = 18)
  points(4, mean(list_fou_timesteps), col = "red", cex = 1.5, pch = 18)
  title(main = paste0(main_title))
}


# used
plot_two_lists_and_medians <- function(list1, list2) {
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_zeros, list1)
  list_two <- do.call(rbind_fill_zeros, list2)
  
  median_list1 <- colMedians(list_one)
  median_list2 <- colMedians(list_two)
  
  name_list <- c("list1", "list2")
  color <- c("blue", "red")
  
  plot(median_list1, col = "blue", lwd = 1, ylim = range(c(list_one, list_two)),
       xlim = c(0, max(length(median_list1), length(median_list2))),
       xlab = "Timestep", ylab = "Number of infected individuals")
  points(median_list2, col = "red", pch = 2)
  col = 1
  for(list_name in name_list){
    temp <- get(list_name)
    for(row in 1:length(temp)){
      lines(unlist(temp[row]), col = adjustcolor(color[col], alpha = 0.15))
    }
    col = col + 1
  }
  
  legend("topright", legend = c("Reconstructed approach", "Baseline approach"),
         col = c("blue", "red"), pch = c(1, 2))
}

plot_lists_and_means_agecat <- function(list1, list2, list3,
                                        data_reconstruct_gender = simulate_result_rnbinom$dataset.res,
                                        data_reconstruct_gender_weekend = simulate_result_rnbinom_weekend$dataset.res,
                                        agecat = "Children") {
  
  # list1 = simulate.result
  # list2 = simulate.result.weekend 
  # list3 = simulate.baseline.result
  
  rbind_fill_zeros <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(0, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list1 <- lapply(simulate.result, function(x) cbind(data_reconstruct_gender, x$infectedMat))
  list2 <- lapply(simulate.result.weekend, function(x) cbind(data_reconstruct_gender_weekend, x$infectedMat))
  list3 <- lapply(simulate.baseline.result, function(x) cbind(data_reconstruct_gender, x$infectedMat))
  
  list1 <- lapply(list1, function(x) x[x$part_age_cat == agecat, -c(1,2)])
  list2 <- lapply(list2, function(x) x[x$part_age_cat == agecat, -c(1,2)])
  list3 <- lapply(list3, function(x) x[x$part_age_cat == agecat, -c(1,2)])
  
  list1 <- lapply(list1, function(x) colSums(x, na.rm = T))
  list2 <- lapply(list2, function(x) colSums(x, na.rm = T))
  list3 <- lapply(list3, function(x) colSums(x, na.rm = T))
  
  list_one <- do.call(rbind_fill_zeros, list1)/nrow(data_reconstruct_gender[data_reconstruct_gender$part_age_cat == agecat,])
  list_two <- do.call(rbind_fill_zeros, list2)/nrow(data_reconstruct_gender_weekend[data_reconstruct_gender_weekend$part_age_cat == agecat,])
  list_three <- do.call(rbind_fill_zeros, list3)/nrow(data_reconstruct_gender[data_reconstruct_gender$part_age_cat == agecat,])
  
  mean_list1 <- colMeans(list_one, na.rm = T)
  mean_list2 <- colMeans(list_two, na.rm = T)
  mean_list3 <- colMeans(list_three, na.rm = T)
  
  name_list <- c("list_one", "list_two", "list_three")
  color <- c("black", "blue", "red")
  
  plot(mean_list1, col = "black", lwd = 1, ylim = range(c(list_one, list_two, list_three)),
       xlim = c(0, max(length(mean_list1), length(mean_list2), length(mean_list3))),
       xlab = "Timestep", ylab = paste0("Number of infected individuals (", agecat, ")"))
  points(mean_list2, col = "blue", pch = 2)
  points(mean_list3, col = "red", pch = 3)
  col = 1
  for(list_name in name_list){
    temp <- get(list_name)
    for(row in 1:nrow(temp)){
      lines(temp[row,], col = adjustcolor(color[col], alpha = 0.15))
    }
    col = col + 1
  }
  
  legend("topright", legend = c("Reconstructed approach", "Reconstructed approach (weekend)", "Baseline approach"),
         col = c("black", "blue", "red"), pch = c(1, 2, 3))
}

plot_two_rt <- function(list1, list2) {
  
  rbind_fill_na <- function(...) {
    lists <- list(...)
    max_rows <- max(sapply(lists, length))
    result <- lapply(lists, function(x) {
      if (length(x) < max_rows) {
        pad_zeros <- max_rows - length(x)
        c(x, rep(NA, pad_zeros))
      } else {
        x
      }
    })
    do.call(rbind, result)
  }
  
  list_one <- do.call(rbind_fill_na, list1)
  list_two <- do.call(rbind_fill_na, list2)
  
  mean_list1 <- colMeans(list_one, na.rm = T)
  mean_list2 <- colMeans(list_two, na.rm = T)
  
  name_list <- c("list1", "list2")
  color <- c("black", "red")
  
  plot(mean_list1, col = "black", lwd = 1, ylim = range(c(list_one, list_two), na.rm = T),
       xlim = c(0, max(length(mean_list1), length(mean_list2))),
       xlab = "Timestep", ylab = "Number of infected individuals")
  points(mean_list2, col = "red", pch = 2)
  col = 1
  for(list_name in name_list){
    temp <- get(list_name)
    for(row in 1:length(temp)){
      lines(unlist(temp[row]), col = adjustcolor(color[col], alpha = 0.15))
    }
    col = col + 1
  }
  
  legend("topright", legend = c("Reconstructed approach", "Baseline approach"),
         col = c("black", "red"), pch = c(1, 2))
}

summarise_simulation <- function(data.input, n_sample = n_sample){
  
  for(i in 1:length(data.input$result)){
    data.input$dataset.res[i,"daily"] <- as.vector(rowMeans(data.input$result[[i]][data.input$result[[i]]$cnt_type == "daily",week]))
    data.input$dataset.res[i,"weekly"] <- as.vector(rowMeans(data.input$result[[i]][data.input$result[[i]]$cnt_type == "weekly",week]))
    data.input$dataset.res[i,"monthly"] <- as.vector(rowMeans(data.input$result[[i]][data.input$result[[i]]$cnt_type == "monthly",week]))
    data.input$dataset.res[i,"few"] <- as.vector(rowMeans(data.input$result[[i]][data.input$result[[i]]$cnt_type == "few",week]))
    data.input$dataset.res[i,"first"] <- as.vector(rowMeans(data.input$result[[i]][data.input$result[[i]]$cnt_type == "first",week]))
  }
  
  distribution.out <- data.input$dataset.res %>% 
    pivot_longer(c(daily, weekly, monthly, few, first),
                 names_to = "frequency",
                 values_to = "contact")
  
  distribution.out <- distribution.out %>% 
    mutate(frequency = ifelse(frequency == "few", "a few times a year", ifelse(frequency == "first", "first time", frequency)))
  
  return(distribution.out)
}

summarise_simulation.update <- function(data.input, n_sample = n_sample){
  week <- c('Sun','Mon','Tue','Wed','Thu',
            'Fri','Sat')
  for(i in 1:length(data.input$result)){
    data.input$dataset.res[i,"daily"] <- as.vector(rowMeans(data.input$result[[i]][data.input$result[[i]]$cnt_type == "daily",week]))
    data.input$dataset.res[i,"weekly"] <- as.vector(rowSums(data.input$result[[i]][data.input$result[[i]]$cnt_type == "weekly",week]))
    data.input$dataset.res[i,"monthly"] <- as.vector(rowSums(data.input$result[[i]][data.input$result[[i]]$cnt_type == "monthly",week]))
    data.input$dataset.res[i,"few"] <- as.vector(rowSums(data.input$result[[i]][data.input$result[[i]]$cnt_type == "few",week]))
    data.input$dataset.res[i,"first"] <- as.vector(rowSums(data.input$result[[i]][data.input$result[[i]]$cnt_type == "first",week]))
  }
  
  distribution.out <- data.input$dataset.res %>% 
    pivot_longer(c(daily, weekly, monthly, few, first),
                 names_to = "frequency",
                 values_to = "contact")
  
  distribution.out <- distribution.out %>% 
    mutate(frequency = ifelse(frequency == "few", "a few times a year", ifelse(frequency == "first", "first time", frequency)))
  
  return(distribution.out)
}

summarise_simulation.sum <- function(data.input, n_sample = n_sample){
  
  for(i in 1:length(data.input$result)){
    data.input$dataset.res[i,"daily"] <- as.vector(rowSums(data.input$result[[i]][data.input$result[[i]]$cnt_type == "daily",week]))
    data.input$dataset.res[i,"weekly"] <- as.vector(rowSums(data.input$result[[i]][data.input$result[[i]]$cnt_type == "weekly",week]))
    data.input$dataset.res[i,"monthly"] <- as.vector(rowSums(data.input$result[[i]][data.input$result[[i]]$cnt_type == "monthly",week]))
    data.input$dataset.res[i,"few"] <- as.vector(rowSums(data.input$result[[i]][data.input$result[[i]]$cnt_type == "few",week]))
    data.input$dataset.res[i,"first"] <- as.vector(rowSums(data.input$result[[i]][data.input$result[[i]]$cnt_type == "first",week]))
  }
  
  distribution.out <- data.input$dataset.res %>% 
    pivot_longer(c(daily, weekly, monthly, few, first),
                 names_to = "frequency",
                 values_to = "contact")
  
  distribution.out <- distribution.out %>% 
    mutate(frequency = ifelse(frequency == "few", "a few times a year", ifelse(frequency == "first", "first time", frequency)))
  
  return(distribution.out)
}

distangle_incidence <- function(data = data, nboot = 100){
  output <- list()
  for(b in 1:nboot){
    print(paste("do ", b))
    temp_data <- data[[b]]
    for(i in 1:length(temp_data$full.diary)){
      temp <- temp_data$full.diary[[i]]
      names <- names(temp)[-(length(names(temp)))]
      
      for(j in 1:length(names)){
        id = names[j]
        incidence = as.vector(sapply(temp_data$full.diary[[i]][j]$id$incidence.diary,
                           function(x) ifelse(is_empty(x), NA,
                           x)))
        if(is_empty(incidence) == T){
          temp <- data.frame(id = names[j],
                             incidence = NA, day = i, boot = b)
        } else {
        temp <- data.frame(id = names[j],
                           incidence = as.vector(sapply(incidence, function(x) x)),
                           day = i, boot = b)
        }
        
        if(exists("storage")){
          storage <- data.frame(rbind(storage, temp))
        } else {
          storage <- data.frame(temp)
        }
      }
      if(exists("output.storage")){
        output.storage <- data.frame(rbind(output.storage, storage))
      } else {
        output.storage <- storage
      }
      rm(storage)
    }
    output[[b]] <- output.storage
    rm(output.storage)
  }
  return(output)
}

secondary_case_function <- function(data = data,
                                    data_age = data_age,
                                    age_cat = F){
  if(age_cat == F){
    output <- data %>% 
      mutate(case = ifelse(is.na(incidence), 0, 1)) %>% 
      group_by(id, boot) %>% 
      summarise(n = sum(case)) %>% 
      group_by(boot) %>% 
      summarise(mean = mean(n))
  } else {
    output <- merge(data, data_age$dataset.res,
                    by.x = "id", by.y = "part_id") %>% 
      mutate(case = ifelse(is.na(incidence), 0, 1)) %>% 
      group_by(id, boot, part_age_cat) %>% 
      summarise(n = sum(case)) %>% 
      group_by(boot, part_age_cat) %>% 
      summarise(mean = mean(n))
  }
  
  return(output)
}

# used
calculate_r0_function <- function(dataset = incidence_list_baseline){
  data.incidence <- dataset
  
  incidence_temp <- do.call(rbind, data.incidence)
  first_infected <- sapply(data.incidence, function(x) x[1,"id"])
  first_infected <- data.frame(boot = 1:length(first_infected), id = first_infected)
  
  data_first_infected <- merge(incidence_temp, first_infected) %>% 
    mutate(n = ifelse(is.na(incidence), 0, 1)) %>% 
    arrange(boot, day)
  
  calculate_r0 <- data_first_infected %>% 
    group_by(boot) %>% 
    summarise(sum = sum(n)) %>% 
    summarise(mean_r0 = mean(sum),
              low_quantile = mean_r0 - (1.96*sqrt(var(sum))/sqrt(length(unique(data_first_infected$id)))),
              high_quantile = mean_r0 + (1.96*sqrt(var(sum))/sqrt(length(unique(data_first_infected$id)))))
  return(calculate_r0)
  
}

plot_r0 <- function(dataset = input){
  data.incidence <- dataset
  
  incidence_temp <- do.call(rbind, data.incidence)
  first_infected <- sapply(data.incidence, function(x) x[1,"id"])
  first_infected <- data.frame(boot = 1:length(first_infected), id = first_infected)
  
  data_first_infected <- merge(incidence_temp, first_infected) %>% 
    mutate(n = ifelse(is.na(incidence), 0, 1)) %>% 
    arrange(boot, day)
  
  calculate_r0 <- data_first_infected %>% 
    group_by(boot) %>% 
    summarise(sum = sum(n))
  
  calculate_running_mean <- function(df) {
    n <- nrow(df)
    result <- numeric(n)
    for (i in 1:n) {
      result[i] <- mean(df$sum[1:i])
    }
    return(result)
  }
  
  running_means <- calculate_running_mean(calculate_r0)
  
  return(running_means)
  
}

subset_list_by_length <- function(lst, x) {
  subset <- lapply(lst, function(vec) {
    if (length(vec) > x) {
      return(vec)
    }
  })
  subset <- subset[!sapply(subset, is.null)]
  return(subset)
}

effective_secondary_case_function <- function(data = distange_dataframe_aug){
  
  nboot <- max(data$boot)
  data_temp <- list()
  
  for(i in 1:nboot){
    temp <- data[data$boot == i, ]
    data_time <- temp[!duplicated(temp$id),] %>% 
      select(-incidence)
    
    data_cases <- temp %>% 
      mutate(case = ifelse(is.na(incidence), 0, 1)) %>% 
      group_by(id, boot) %>% 
      summarise(n = sum(case),
                .groups = 'drop')
    
    data_output <- merge(data_time, data_cases, by = c('id', 'boot')) %>% 
      group_by(boot, day) %>% 
      summarise(mean = mean(n),
                .groups = 'drop')
    
    data_temp[[i]] <- data_output
  }
  
  output <- do.call(rbind, data_temp) %>% 
    group_by(day) %>% 
    summarise(mean_rt = mean(mean),
              low_quantile = mean_rt - (1.96*sqrt(var(mean))/sqrt(n())),
              high_quantile = mean_rt + (1.96*sqrt(var(mean))/sqrt(n())))
  
  return(output)
}

# R0 formula
r0_noage <- function(foi, mean_daily, other_daily){
  r0_unclust = (N-1-mean_daily)/N * foi/gamma * other_daily
  r0_clust = (mean_daily) * (1-(gamma*(1-foi))/(1-(1-gamma)*(1-foi)))
  
  r0 = r0_clust + r0_unclust
  return(r0)
}

r0_age <- function(foi, mean_data, prop.age.partdata, age_level, frequency_level){
  
  mean_input = mean_data
  other_input = mean_input[mean_input$frequency_multi != "daily", ]
  daily_input = mean_input[mean_input$frequency_multi == "daily", ]
  
  other_temp <- list()
  r0_unclust_temp <- list()
  r0_clust_temp <- list()
  daily_temp <- list()
  r0_temp <- list()
  for(i in age_level){
    other_temp[[i]] <- other_input[other_input$part_age_cat %in% i, ]
    daily_temp[[i]] <- daily_input[daily_input$part_age_cat %in% i, ]
    r0_unclust_temp[[i]] <- (N-1-daily_temp[[i]]$mean)/N * as.vector(other_temp[[i]]$mean)*foi/gamma
    r0_clust_temp[[i]] <- (daily_temp[[i]]$mean) * (1-(gamma*(1-foi))/(1-(1-gamma)*(1-foi)))
    r0_temp[[i]] <- sum(c(r0_clust_temp[[i]], r0_unclust_temp[[i]]))
  }
  
  r0 <- weighted.mean(as.vector(unlist(r0_temp)), prop.age.partdata$prop)
  return(r0)
}

# calculate q
calculate_q_noagestr <- function(R0_input, initial.guess, mean = mean_daily, other = other_daily){
  
  qran <- runif(1, 0, 1)
  R0_temp <- r0_noage(qran, mean, other)
  qran_prev <- initial.guess
  while (abs((R0_input - R0_temp)) > 0.0001) {
    if (R0_temp > R0_input) {
      qran <- runif(1, 0, qran_prev)
    } else {
      qran <- runif(1, qran_prev, 1)
    }
    qran_prev <- qran
    R0_temp <- r0_noage(qran, mean, other)
  }
  return(qran)
}

calculate_ratio_temporal <- function(dataset = temp){
  week <- c('Sun','Mon','Tue','Wed','Thu',
            'Fri','Sat')
  coo <- data.frame(do.call(rbind, Map(cbind, part_id = names(dataset$result), dataset$result)))
  rownames(coo) <- NULL
  coo[week] <- lapply(coo[week], as.numeric)
  
  result_weight <- coo %>%
    rowwise() %>% 
    group_by(cnt_type, part_id) %>%
    mutate(sum_weight = ifelse(cnt_type == "daily", mean(c(Mon, Tue, Wed, Thu, Fri, Sat, Sun)),
                               ifelse(cnt_type == "weekly", sum(c(Sun, Mon, Tue, Wed, Thu, Fri, Sat)),
                                      ifelse(cnt_type == "monthly", sum(c(Sun, Mon, Tue, Wed, Thu, Fri, Sat)),
                                             sum(c(Sun, Mon, Tue, Wed, Thu, Fri, Sat)))))) %>% 
    group_by(part_id) %>% 
    summarise(n = sum(sum_weight)) %>% 
    merge(., dataset$dataset.res) %>% 
    group_by(part_age_cat) %>% 
    summarise(sum_temp = mean(n))
  
  result_weight$part_age_cat <- factor(result_weight$part_age_cat,
                                       levels = c("Children", "Teens", "Adult", "Elderly"))
  
  result_naive <- dataset$number.contact %>% group_by(part_age_cat) %>% 
    summarise(sum_naive = sum(mean) * 7)
  
  output <- merge(result_naive, result_weight, by = c("part_age_cat")) %>% 
    mutate(ratio = sum_temp/sum_naive) %>% data.frame()
  
  output$part_age_cat <- factor(output$part_age_cat,
                                levels = c("Children", "Teens", "Adult", "Elderly"))
  
  output <- output %>% 
    arrange(part_age_cat)
  
  return(output)
}

calculate_ratio_temporal_withci <- function(dataset = temp){
  week <- c('Sun','Mon','Tue','Wed','Thu',
            'Fri','Sat')
  coo <- data.frame(do.call(rbind, Map(cbind, part_id = names(dataset$result), dataset$result)))
  rownames(coo) <- NULL
  coo[week] <- lapply(coo[week], as.numeric)
  
  result_weight <- coo %>%
    rowwise() %>% 
    group_by(cnt_type, part_id) %>%
    mutate(sum_weight = ifelse(cnt_type == "daily", mean(c(Mon, Tue, Wed, Thu, Fri, Sat, Sun)),
                               ifelse(cnt_type == "weekly", sum(c(Sun, Mon, Tue, Wed, Thu, Fri, Sat)),
                                      ifelse(cnt_type == "monthly", sum(c(Sun, Mon, Tue, Wed, Thu, Fri, Sat)),
                                             sum(c(Sun, Mon, Tue, Wed, Thu, Fri, Sat)))))) %>% 
    group_by(part_id) %>% 
    summarise(n = sum(sum_weight)) %>% 
    merge(., dataset$dataset.res)

  result_weight$part_age_cat <- factor(result_weight$part_age_cat,
                                       levels = c("Children", "Teens", "Adult", "Elderly"))
  
  result_naive <- dataset$number.contact %>% group_by(part_age_cat) %>% 
    summarise(sum_naive = sum(mean) * 7)
  
  nboot = 1000
  output <- data.frame()
  for(i in 1:length(levels(result_weight$part_age_cat))){
    levels_temp <- levels(result_weight$part_age_cat)[i]
    boot <- c()
    for(b in 1:nboot){
      result_temp <- result_weight[result_weight$part_age_cat %in% levels_temp, ]
      length_result_temp <- nrow(result_temp)
      boot[b] <- mean(sample(result_temp$n, length_result_temp, replace = T))/result_naive[result_naive$part_age_cat %in% levels_temp,]$sum_naive
    }
    output[i, "part_age_cat"] <- levels_temp
    output[i, "lower"] <- quantile(boot, 0.025)
    output[i, "mean"] <- mean(boot)
    output[i, "upper"] <- quantile(boot, 0.975)
  }
  
  output$part_age_cat <- factor(output$part_age_cat,
                                levels = c("Children", "Teens", "Adult", "Elderly"))
  
  output <- output %>% 
    arrange(part_age_cat)
  
  return(output)
}

fplot_boxplot_daily_nondaily_comix <- function(dataset = data_to_plot, breaks_input){
  
  dataset <- dataset %>% 
    mutate(freq_agg = ifelse(frequency_multi %in% "daily", "daily", "non_daily"))
  
  dataset$wave <- as.factor(dataset$wave)
  
  dataset <- dataset %>% 
    group_by(part_id, part_age_cat, wave, freq_agg) %>% 
    summarise(sum = sum(n))
  
  minimum <- min(dataset$sum, na.rm = T)
  maximum <- max(dataset$sum, na.rm = T)
  
  plot <- ggplot(dataset, aes(x = wave, y = log(sum+1), fill = freq_agg)) +
    geom_boxplot() + 
    facet_wrap(~ part_age_cat, nrow = 4) +
    scale_y_continuous(
      name = expression("Number of contacts (log-scale)"),
      sec.axis = sec_axis(~ exp(.)-1,
                          name = "Number of contacts",
                          breaks = breaks_input)) +
    theme_classic() +
    ylab("Number of contacts (log-scale)") +
    xlab("Wave") +
    scale_fill_manual(labels = c("Daily", "Non-daily"),
                      values = c("#219ebc", "#ffb703")) +
    theme(legend.position = "top",
          text = element_text(size = 16),
          legend.title = element_blank())
  
  return(plot)
  
}

fplot_boxplot_daily_nondaily <- function(dataset = data_to_plot, breaks_input){
  
  dataset <- dataset %>% 
    mutate(freq_agg = ifelse(frequency_multi %in% "daily", "daily", "non_daily"))
  
  dataset <- dataset %>% 
    group_by(part_id, part_age_cat, country, freq_agg) %>% 
    summarise(sum = sum(n))
  
  minimum <- min(dataset$sum, na.rm = T)
  maximum <- max(dataset$sum, na.rm = T)
  
  plot <- ggplot(dataset, aes(x = country, y = log(sum+1), fill = freq_agg)) +
    geom_boxplot() + 
    facet_wrap(~ part_age_cat) +
    scale_y_continuous(
      name = expression("Number of contacts (log-scale)"),
      sec.axis = sec_axis(~ exp(.)-1,
                          name = "Number of contacts",
                          breaks = breaks_input)) +
    theme_classic() +
    ylab("Number of contacts (log-scale)") +
    xlab("Country") +
    scale_fill_manual(labels = c("Daily", "Non-daily"),
                      values = c("#219ebc", "#ffb703")) +
    theme(legend.position = "top",
          text = element_text(size = 18),
          legend.title = element_blank()) +
    facet_wrap(~ part_age_cat, nrow = 4) 
  
  return(plot)
}

