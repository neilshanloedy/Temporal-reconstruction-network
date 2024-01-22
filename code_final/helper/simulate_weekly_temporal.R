##################################################################################
## This script provides a function to simulate a weekly temporal contacts
## using a negative binomial distribution
## 
## Author: Neilshan Loedy
## last update: 01/09/2024
##
## 01/09/2024 : add a new function to account for underdispersion, 
##              with the distribution of Generalised Poisson Distribution
##
##################################################################################

#' @param n_sample : Number of samples whose contacts will be reconstructed weekly
#' @param number.contacts : A list of average and variance of the number of contacts for each frequencies per age categories
#' @param prop.week : Proportion of contacts throughout the week per age categories
#' @param prop.age.partdata : Proportion of age categories in the population
#' @param is.diffweekend : A mark on whether contacts are different in the weekend or not, the default is 'F'

simulate_weekly_contact_rnbinom <- function(n_sample = 500,
                                            number.contacts = number.contact.input,
                                            proportion.week = prop.week,
                                            proportion.age = prop.age.partdata){
  
  data_partid = data.frame(part_id = paste0("id_", seq(1:n_sample)),
                           part_age_cat = (sample(proportion.age$part_age_cat, size = n_sample, replace = T,
                                                  prob = proportion.age$prop)))
  week <- c('Mon','Tue','Wed','Thu',
            'Fri','Sat','Sun')
  weekday <- c('Mon','Tue','Wed','Thu','Fri')
  weekend <- c('Sat', 'Sun')
  
  input.rnbinom <- function(dataset, period){
    mean <- dataset[dataset$frequency_multi == period,]$mean
    var <- dataset[dataset$frequency_multi == period,]$var
    size <- size(mean, var)
    prob <- prob(size, mean)
    
    return(list(mean = mean, var = var,
                size = size, prob = prob))
  }
  
  number.contacts <- number.contacts$number.contact.general

  list_id <- list()
  temp <- data.frame()
  for(id in data_partid$part_id){
    age_cat_loop <- data_partid[data_partid$part_id == id,]$part_age_cat
    
      number.cnt <- number.contacts[number.contacts$part_age_cat == age_cat_loop, ]
      input.daily <- input.rnbinom(number.cnt, "daily")
      input.weekly <- input.rnbinom(number.cnt, "weekly")
      input.monthly <- input.rnbinom(number.cnt, "monthly")
      input.few <- input.rnbinom(number.cnt, "a few times a year")
      input.first <- input.rnbinom(number.cnt, "first time")
      
      cnt.daily <- rnbinom(1, input.daily$size, input.daily$prob)
      cnt.weekly <- rnbinom(7, input.weekly$size, input.weekly$prob)
      cnt.monthly <- rnbinom(7, input.monthly$size, input.monthly$prob)
      cnt.few <- rnbinom(7, input.few$size, input.few$prob)
      cnt.first <- rnbinom(7, input.first$size, input.first$prob)
      
      cnt_type <- c("daily", "weekly", "monthly", "few", "first")
      cnt_dist <- NULL
      cnt <- NULL
      
      if(nrow(proportion.week) != length(unique(proportion.week$part_age_cat))*length(week)){
        proportion.week = data.frame(part_age_cat = rep(unique(proportion.week$part_age_cat), each = 7),
                                     dayofweek = rep(week, length(unique(proportion.week$part_age_cat))),
                                     pctg = 1/length(week))
      }
      
      for(freq in cnt_type){
        if(freq == "daily"){
          cnt <- c(freq , as.vector(c(rep(cnt.daily, length(week)))))
        } else {
          pr <- as.vector(proportion.week[proportion.week$part_age_cat == age_cat_loop,]$pctg)
          cnt <- c(freq, as.vector(rmultinom(1, sum(get(paste0("cnt.", freq))), prob = pr)))
        }
        
        if(!is.null(cnt_dist)){
          cnt_dist <- rbind(cnt_dist, cnt)
        } else {
          cnt_dist <- cnt
        } # end of if(exists...)
        
      } # end of for loop
      
      colnames(cnt_dist) <- c("cnt_type", levels(prop.week$dayofweek))
      rownames(cnt_dist) <- NULL
      cnt_dist <- data.frame(cnt_dist)
      cnt_dist <- cnt_dist %>% mutate_at(week, as.numeric)
      list_id[[id]] <- cnt_dist
  } # end of for(id in ...)
  return(list(result = list_id, sample.size = nrow(data_partid),
              number.contact = number.contacts, prop.week = proportion.week,
              dataset.res = data_partid))
}

simulate_weekly_contact_gp <- function(n_sample = 500,
                                       number.contacts = number.contact.input,
                                       proportion.week = prop.week,
                                       proportion.age = prop.age.partdata){
  
  data_partid = data.frame(part_id = paste0("id_", seq(1:n_sample)),
                           part_age_cat = (sample(proportion.age$part_age_cat, size = n_sample, replace = T,
                                                  prob = proportion.age$prop)))
  week <- c('Mon','Tue','Wed','Thu',
            'Fri','Sat','Sun')
  weekday <- c('Mon','Tue','Wed','Thu','Fri')
  weekend <- c('Sat', 'Sun')
  
  input.distribution <- function(dataset, period){
    mean <- dataset[dataset$frequency_multi == period,]$mean
    var <- dataset[dataset$frequency_multi == period,]$var
    
    if(mean == 0){
      mean <- 1e-07
    }
    
    if(var == 0){
      var <- 1e-07
    }
    
    lambda_1 <- mean * sqrt(mean/var)
    lambda_2 <- (1- sqrt(mean/var))
    
    return(list(mean = mean, var = var,
                lambda_1 = lambda_1, lambda_2 = lambda_2))
  }
  
  number.contacts <- number.contacts$number.contact.general
  
  list_id <- list()
  temp <- data.frame()
  for(id in data_partid$part_id){
    
    age_cat_loop <- data_partid[data_partid$part_id == id,]$part_age_cat
    
    number.cnt <- number.contacts[number.contacts$part_age_cat == age_cat_loop, ]
    input.daily <- input.distribution(number.cnt, "daily")
    input.weekly <- input.distribution(number.cnt, "weekly")
    input.monthly <- input.distribution(number.cnt, "monthly")
    input.few <- input.distribution(number.cnt, "a few times a year")
    input.first <- input.distribution(number.cnt, "first time")
    
    cnt.daily <- rgenpois(1, input.daily$lambda_1, input.daily$lambda_2)
    cnt.weekly <- rgenpois(7, input.weekly$lambda_1, input.weekly$lambda_2)
    cnt.monthly <- rgenpois(7, input.monthly$lambda_1, input.monthly$lambda_2)
    cnt.few <- rgenpois(7, input.few$lambda_1, input.few$lambda_2)
    cnt.first <- rgenpois(7, input.first$lambda_1, input.first$lambda_2)
    
    cnt_type <- c("daily", "weekly", "monthly", "few", "first")
    cnt_dist <- NULL
    cnt <- NULL
    
    if(nrow(proportion.week) != length(unique(proportion.week$part_age_cat))*length(week)){
      proportion.week = data.frame(part_age_cat = rep(unique(proportion.week$part_age_cat), each = 7),
                                   dayofweek = rep(week, length(unique(proportion.week$part_age_cat))),
                                   pctg = 1/length(week))
    }
    
    for(freq in cnt_type){
      if(freq == "daily"){
        cnt <- c(freq , as.vector(c(rep(cnt.daily, length(week)))))
      } else {
        pr <- as.vector(proportion.week[proportion.week$part_age_cat == age_cat_loop,]$pctg)
        cnt <- c(freq, as.vector(rmultinom(1, sum(get(paste0("cnt.", freq))), prob = pr)))
      }
      
      if(!is.null(cnt_dist)){
        cnt_dist <- rbind(cnt_dist, cnt)
      } else {
        cnt_dist <- cnt
      } # end of if(exists...)
      
    } # end of for loop
    
    colnames(cnt_dist) <- c("cnt_type", levels(prop.week$dayofweek))
    rownames(cnt_dist) <- NULL
    cnt_dist <- data.frame(cnt_dist)
    cnt_dist <- cnt_dist %>% mutate_at(week, as.numeric)
    list_id[[id]] <- cnt_dist
  } # end of for(id in ...)
  return(list(result = list_id, sample.size = nrow(data_partid),
              number.contact = number.contacts, prop.week = proportion.week,
              dataset.res = data_partid))
}
