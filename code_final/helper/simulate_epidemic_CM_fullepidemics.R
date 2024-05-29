##################################################################################-
## This script provides a function to simulate an epidemic on 
## a weekly temporal contacts using a negative binomial distribution (simulate.epidemic)
## 
## The age structure here will not be taken into account at all
## This simulation will runs until there is no infected individuals in the population
##
## Author: Neilshan Loedy
## last update: 18/10/2023
## 
## 18/10/2023: simulate baseline CM poisson is added.
## 10/01/2024: simulate baseline CM is adjusted to the Generalised Poisson distribution
## 05/04/2024: changing in simulate.epidemic.CM time steps starting point from 1 to 0.
## and adapt !any into !all in if(!any(is.na(length.cnt)))
## and remove unnecessary functions
## 07/04/2024: Update on the simulate.baseline.CM to make it faster
## (deleted some unnecessary functions)
##################################################################################-

simulate.epidemic.CM <- function(data.epi = data.temp,
                                 contact.rates = cm,
                                 initial.case = 1,
                                 starting_day = "Mon",
                                 foi = 0.25,
                                 gamma = 0.22){
  
  dist.contact <- function(cnt.pat = cnt.pattern, 
                           day = day){
    
    # number of contacts daily weekday
    daily.cnt <- unique(choose.df(cnt.pat[cnt.pat$cnt_type == "daily", day]))
    
    # number of contacts weekly weekday
    weekly.cnt <- choose.df(cnt.pat[cnt.pat$cnt_type == "weekly", day])
    
    # number of contacts monthly weekday
    monthly.cnt <- choose.df(cnt.pat[cnt.pat$cnt_type == "monthly", day])
    
    # number of contacts few weekday
    few.cnt <- choose.df(cnt.pat[cnt.pat$cnt_type == "few", day])
    
    # number of contacts weekly weekday
    first.cnt <- choose.df(cnt.pat[cnt.pat$cnt_type == "first", day])
    
    return(list(daily.cnt = daily.cnt,
                weekly.cnt = weekly.cnt,
                monthly.cnt = monthly.cnt,
                few.cnt = few.cnt,
                first.cnt = first.cnt))
  }
  
  suscep.contact <- function(id.inf = id.inf, 
                             contact_mat = contact.rates,
                             pop_age = pop.age,
                             popvector = pop.vector,
                             dist.cnt = dist.cnt){
    
    sample_random_ids <- function(contacted_df) {
      sampled_ids <- vector("list", nrow(contacted_df))
      for (i in 1:nrow(contacted_df)) {
        age_row <- rownames(contacted_df)[i]
        num_contacts <- contacted_df[i,1]
        length_contacts <- as.numeric(rownames(pop_age[which(rownames(pop_age) %in% popvector & pop_age$part_age_cat == age_row), ]))
        ids <- sample(length_contacts, num_contacts)
        sampled_ids[[i]] <- ids
      }
      return(unlist(sampled_ids))
    }
    
    daily.df <- daily.cnt.diary
    # if True, the id.inf is already have a daily contacts
    if(id.inf %in% names(daily.df)){ # het was daily
      cnt.type <- names(dist.cnt)
      cnt <- list()
      # for each cnt, extract its age and their cnt.type
      # then weight them based on multinomial distributions
      for(t in cnt.type){
        temp <- dist.cnt[[t]]
        if(temp != 0){
          age <- pop.age[pop.age$part_id == id.inf, "part_age_cat"]
          contacted <- rmultinom(1, dist.cnt[[t]], contact_mat[[str_extract(t, "[^.]+")]][age,])
          cnt[[t]] <- sample_random_ids(contacted)
        } else {
          cnt[[t]] <- NA
        }
        if(t == "daily.cnt"){
          cnt[[t]] <- daily.df[[id.inf]]
        }
      }
      result <- sapply(cnt, function(x) if(length(x) == 0) NA else x, simplify = F)
    } else {
      cnt.type <- names(dist.cnt)
      cnt <- list()
      # for each cnt, extract its age and their cnt.type
      # then weight them based on multinomial distributions
      for(t in cnt.type){
        temp <- dist.cnt[[t]]
        if(temp != 0){
          age <- pop.age[pop.age$part_id == id.inf, "part_age_cat"]
          contacted <- rmultinom(1, dist.cnt[[t]], contact_mat[[str_extract(t, "[^.]+")]][age,])
          cnt[[t]] <- sample_random_ids(contacted)
        } else {
          cnt[[t]] <- NA
        }
      }
      result <- sapply(cnt, function(x) if(length(x) == 0) NA else x, simplify = F)
    } # end of else
    return(result)
  }
  
  daily.cnt.diary <- list()
  data.epi.cnt <- data.epi$result
  pop.id <- names(data.epi$result)
  pop.age <- data.epi$dataset.res
  pop.size <- length(pop.id)
  pop.vector <- 1:pop.size
  week <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
  
  initial.case.id <- sample(pop.id, initial.case)
  
  infected <- rep(0, pop.size)
  infected[which(pop.id %in% initial.case.id)] <- 1
  infectedMat <- infected
  incidenceMat <- infected
  infected.vector <- initial.case
  time_steps <- 0
  diary <- list()
  recovery_time <- rep(Inf, pop.size)
  recovery_time[pop.vector[infected == 1]] <- rgeom(1, gamma) + 1
  
  while(sum(infected, na.rm = TRUE) > 0){
    # Repeat loop until all infected individuals are recovered and no new infections have occurred
    # Infect each susceptible with a certain probability
    incidence <- rep(0, pop.size)
    infectedPrev <- infected	# indicator of infection in the previous time step
    cnt.vector <- time_steps %% 7 
    if(cnt.vector == 0) cnt.vector <- 7
    
    for(inf in pop.vector[infected == 1 & is.na(infected) == F]){
      day <- week[cnt.vector]
      id.inf <- pop.id[inf]
      cnt.pattern <- data.epi.cnt[[inf]]
      dist.cnt <- dist.contact(cnt.pattern, day = day)
      track.incidence <- infected
      
      suscep.cnt <- suscep.contact(id.inf = id.inf, 
                                   contact_mat = contact.rates,
                                   pop_age = pop.age,
                                   popvector = pop.vector,
                                   dist.cnt = dist.cnt)
      
      if(sum(is.na(unlist(suscep.cnt)) == F) > 0){
        for(i in names(suscep.cnt)){
          length.cnt <- suscep.cnt[[i]]
          if(!all(is.na(length.cnt))){
            for(j in 1:length(length.cnt)){
              id <- length.cnt[j]
              if(!is.na(infected[id]) & infected[id] == 0){
                if(runif(1) < foi){
                  infected[id] <- 1
                  recovery_time[id] <- time_steps + (rgeom(1, gamma) + 1)
                }
              }
            }
          }
        }
      }
      
      if(!id.inf %in% names(daily.cnt.diary)){
        daily.cnt.diary[[id.inf]] <- suscep.cnt$daily.cnt
      }
      
      if (recovery_time[inf] == time_steps){
        infected[inf] <- NA
      }
      
    }
    
    incitemp <- infected
    incidence <- incitemp - infectedPrev
    incidence[is.na(incidence)] = 0
    
    # Append current status to matrix and update time variable
    infectedMat <- cbind(infectedMat, infected)
    incidenceMat <- cbind(incidenceMat, incidence)
    time_steps <- time_steps + 1
    infected.daily <- sum(infected, na.rm = TRUE)
    infected.vector <- c(infected.vector, infected.daily)
    print(infected.daily)
  }
  
  output <- list(#full.diary = diary,
    infectedMat = infectedMat,
    incidenceMat = incidenceMat,
    #diary.weekday = daily.cnt.diary,
    infected.daily = infected.vector,
    time_steps = time_steps,
    input = c(paste0("foi = ", foi), 
              paste0("gamma = ", gamma),
              paste0("starting day = ", starting_day),
              paste0("initial case = ", initial.case)))
  return(output)
}

#' @param data.epi : input dataset, this consists of a list of the weekly temporal
#' a list of sample size, number of contacts, prop.week, and dataset.res (pop id and age category)
#' @param contact.rates : the contact matrices for daily, weekly, monthly, a few times a year, and first time contacts
#' @param initial.case : the number of initial case of the epidemic
#' @param starting_day : starting day of the epidemic
#' @param foi : disease-specific infectivity
#' @param gamma : recovery rate.
#' When the number of contacts made is greater than the number of recovered and susceptible people, 
#' the number of contacts will be adjusted to the number of people available in the population.

simulate.baseline.CM <- function(data_epi = data.temp,
                                 contact_rates = cm,
                                 raw_data = temp_dat,
                                 initial_case = 1,
                                 foi = 0.25,
                                 gamma = 0.22){
  
  suscep_cnt_general <- function(mean_input = mean_of_cnt, 
                                 pop_age = pop_age_data,
                                 age = age_inf,
                                 contact_mat = contact_rates){
    sample_random_ids <- function(contacted_df) {
      sampled_ids <- vector("list", nrow(contacted_df))
      for (i in 1:nrow(contacted_df)) {
        age_row <- rownames(contacted_df)[i]
        num_contacts <- contacted_df[i,1]
        
        # EDIT INI LENGTH_CONTACTS NYA HAYU
        length_contacts <- as.numeric(rownames(pop_age[which(pop_age$part_age_cat == age_row), ]))
        
        if(length(length_contacts) < num_contacts){
          num_contacts <- length(length_contacts)
        }
        
        ids <- sample(length_contacts, num_contacts)
        sampled_ids[[i]] <- ids
      }
      return(unlist(sampled_ids))
    }
    
    mean <- mean_input$mean
    var <- mean_input$var
    
    if(mean == 0){
      mean <- 1e-07
    }
    
    if(var == 0){
      var <- 1e-07
    }
    
    lambda_1 <- mean * sqrt(mean/var)
    lambda_2 <- (1- sqrt(mean/var))
    
    sample.no.cnt <- rgenpois(1, lambda_1, lambda_2)
    contacted <- rmultinom(1, sample.no.cnt, contact_mat[["all"]][age,])
    cnt <- sapply(sample_random_ids(contacted), function(x) if(length(x) == 0) NA else x, simplify = F)
    
    return(unlist(cnt))
  }
  
  mean_input <- function(data = data.input){
    
    number.contact.general <- data %>% 
      group_by(part_id, part_age_cat) %>%
      summarise(sum = sum(n),
                .groups = 'drop') %>% 
      group_by(part_age_cat) %>% 
      summarise(mean = mean(sum),
                var = var(sum),
                .groups = 'drop') %>% 
      na.omit()
    
    return(number.contact.general)
  }
  mean_cnt <- mean_input(raw_data)
  pop_id <- names(data_epi$result)
  pop_age_data <- data_epi$dataset.res
  n <- data_epi$sample.size
  population <- data.frame("ID"=1:n,"Status"=0)
  events_time <- data.frame("T"=0,"nI"=0)
  recovery_time <- rep(Inf,n)
  
  #seed
  first <- sample(population$ID,initial_case)
  population$Status[first] <- 1
  recovery_time[first] <- rgeom(1,gamma)+1 #mu
  infectives <- length(which(population$Status==1))
  t_step <- 0
  
  infected_vector <- initial_case
  infected <- population$Status
  infectedMat <- population$Status
  incidenceMat <- population$Status
  
  while(sum(population$Status, na.rm = T) >0) {
    incidence <- rep(0, n)
    infectedPrev <- population$Status
    current_infectives <- which(population$Status==1)
    
    for (i in 1:length(current_infectives)){
      track_incidence <- population$Status
      temp_inf <- current_infectives[i]
      id_inf <- pop_id[temp_inf]
      age_inf <- pop_age_data[pop_age_data$part_id == id_inf, ]$part_age_cat
      
      mean_of_cnt <- list(mean = mean_cnt[mean_cnt$part_age_cat == age_inf, ]$mean,
                          var = mean_cnt[mean_cnt$part_age_cat == age_inf, ]$var)
      
      suscep_cnt <- suscep_cnt_general(mean_input = mean_of_cnt)
      
      if (length(suscep_cnt)>0 & is.null(suscep_cnt) == FALSE){
        for (j in 1:length(suscep_cnt)){
          if (population$Status[suscep_cnt[j]]==0 & is.na(population$Status[suscep_cnt[j]]) == FALSE){
            if (runif(1) < foi){
              infectee <- suscep_cnt[j]
              population$Status[infectee] <- 1
              recovery_time[infectee] <- t_step + rgeom(1,gamma)+1
            }
          }
        }
        
      }
      
      if (recovery_time[temp_inf] == t_step){
        population$Status[temp_inf] <- NA
      }
    }
    t_step <- t_step + 1
    FinalSize <- length(which(is.na(population$Status)))
    incitemp <- population$Status
    incidence <- incitemp - infectedPrev
    incidence[is.na(incidence)] = 0
    # 
    # # Append current status to matrix and update time variable
    infectedMat <- cbind(infectedMat, population$Status)
    incidenceMat <- cbind(incidenceMat, incidence)
    infected_daily <- sum(population$Status, na.rm = TRUE)
    infected_vector <- c(infected_vector, infected_daily)
    print(infected_daily)
  }
  
  output <- list(
    infectedMat = infectedMat,
    incidenceMat = incidenceMat,
    infected_daily = infected_vector,
    time_steps = t_step,
    final_size = FinalSize,
    input = c(paste0("foi = ", foi), 
              paste0("gamma = ", gamma),
              paste0("initial case = ", initial_case)))
  return(output)
}

