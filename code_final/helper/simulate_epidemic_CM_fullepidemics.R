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
## 10/01/2023: simulate baseline CM is adjusted to the Generalised Poisson distribution
##################################################################################-

simulate.epidemic.CM <- function(data.epi = data.temp,
                                    contact.rates = cm,
                                    initial.case = 1,
                                    starting_day = "Mon",
                                    foi = 0.25,
                                    gamma = 0.22){
  
  rotate_vector <- function(day_input) {
    day <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
    if(day_input != 'Mon'){
      index <- match(day_input, day)
      if (is.na(index)) {
        stop("Invalid input day.")
      }
      rotated_vector <- c(day[index:length(day)], day[1:(index-1)])
    } else rotated_vector <- day
    return(rotated_vector)
  }
  
  day.change <- function(data.epi.cnt, week.str){
    modified_list <- lapply(data.epi.cnt, function(x) data.frame(cnt_type = x$cnt_type, x[, week.mod]))
    return(modified_list)
  }
  
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
                             day.cnt = day,
                             pop_age = pop.age,
                             pop.vector = pop.vector,
                             infected = infected,
                             dist.cnt = dist.cnt){
    
    sample_random_ids <- function(contacted_df) {
      sampled_ids <- vector("list", nrow(contacted_df))
      for (i in 1:nrow(contacted_df)) {
        age_row <- rownames(contacted_df)[i]
        num_contacts <- contacted_df[i,1]
        length_contacts <- as.numeric(rownames(pop_age[which(rownames(pop_age) %in% pop.vector & pop_age$part_age_cat == age_row), ]))
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
  weekday <- c('Mon','Tue','Wed','Thu','Fri')
  weekend <- c('Sat', 'Sun')
  
  initial.case.id <- sample(pop.id, initial.case)
  
  infected <- rep(0, pop.size)
  infected[which(pop.id %in% initial.case.id)] <- 1
  infectedMat <- infected
  incidenceMat <- infected
  infected.vector <- initial.case
  time_steps <- 1
  week.mod <- rotate_vector(starting_day)
  data.epi.cnt <- day.change(data.epi.cnt, week.mod)
  diary <- list()
  sub.diary <- list()
  
  while(sum(infected, na.rm = TRUE) > 0){
    # Repeat loop until all infected individuals are recovered and no new infections have occurred
    # Infect each susceptible with a certain probability
    incidence <- rep(0, pop.size)
    infectedPrev <- infected	# indicator of infection in the previous time step
    cnt.vector <- time_steps %% 7 
    if(cnt.vector == 0) cnt.vector <- 7
    diary[[time_steps]] <- list()
    
    for(inf in pop.vector[infected == 1 & is.na(infected) == F]){
      day <- week.mod[cnt.vector]
      id.inf <- pop.id[inf]
      cnt.pattern <- data.epi.cnt[[inf]]
      dist.cnt <- dist.contact(cnt.pattern, day = day)
      track.incidence <- infected
      
      suscep.cnt <- suscep.contact(id.inf = id.inf, 
                                   contact_mat = contact.rates,
                                   day.cnt = day,
                                   pop_age = pop.age,
                                   pop.vector = pop.vector,
                                   infected = infected,
                                   dist.cnt = dist.cnt)
      
      if(sum(unlist(suscep.cnt), na.rm = T) > 0){
        for(i in names(suscep.cnt)){
          length.cnt <- suscep.cnt[[i]]
          if(!any(is.na(length.cnt))){
            for(j in 1:length(length.cnt)){
              id <- length.cnt[j]
              if(!is.na(infected[id]) & infected[id] == 0){
                infected[id] <- ifelse(runif(1) < foi, 1, 0)
              }
            }
          }
        }
      }
      
      incidence.diary <- track.incidence
      incidence.diary <- infected - incidence.diary
      incidence.diary[is.na(incidence.diary)] <- 0
      
      sub.diary$id.inf <- id.inf
      sub.diary$incidence.diary <- pop.id[incidence.diary == 1]
      sub.diary$dist.cnt <- dist.cnt
      sub.diary$suscep.cnt <- suscep.cnt
      sub.diary$day <- day
      diary[[time_steps]][[id.inf]] <- sub.diary
      
      if(day %in% weekday & !id.inf %in% names(daily.cnt.diary)){
        daily.cnt.diary[[id.inf]] <- suscep.cnt$daily.cnt
      }
      
    }
    
    incitemp <- infected
    incidence <- incitemp - infectedPrev
    incidence[is.na(incidence)] = 0
    
    track.recovered <- pop.id[is.na(infected)]
    
    # at each time step, we pick a person and we see the probability this person got infected
    # Recover each infected with a certain probability
    for(infect in pop.vector[!is.na(infected) & infected == 1]){ #if they are infected;
      infected[infect] <- ifelse(runif(1) < gamma, NA, 1)
    }
    
    recovered.diary <- pop.id[is.na(infected)][!pop.id[is.na(infected)] %in% track.recovered]
    diary[[time_steps]]$recovered <- recovered.diary
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

simulate.baseline.CM <- function(data.epi = data.temp,
                                    contact.rates = cm,
                                    raw.data = temp_dat,
                                    initial.case = 1,
                                    starting_day = "Mon",
                                    foi = 0.25,
                                    gamma = 0.22){
  
  mean.input <- function(data = data.input){
    
    number.contact.general <- data %>% 
      group_by(part_id, part_age_cat) %>%
      summarise(sum = sum(n),
                .groups = 'drop') %>% 
      group_by(part_age_cat) %>% 
      summarise(mean = mean(sum),
                var = var(sum),
                .groups = 'drop') %>% 
      na.omit()
    
    number.contact.input <- list(number.contact.general = number.contact.general)
    
    return(number.contact.input)
  }
  
  rotate_vector <- function(day_input) {
    day <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
    if(day_input != 'Mon'){
      index <- match(day_input, day)
      if (is.na(index)) {
        stop("Invalid input day.")
      }
      rotated_vector <- c(day[index:length(day)], day[1:(index-1)])
    } else rotated_vector <- day
    return(rotated_vector)
  }
  
  day.change <- function(data.epi.cnt, week.str){
    modified_list <- lapply(data.epi.cnt, function(x) data.frame(cnt_type = x$cnt_type, x[, week.mod]))
    return(modified_list)
  }
  
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
  
  mean.of.cnt.funct <- function(data.mean,
                                age.cat = age.inf,
                                day.cnt = day){
    data.mean <- mean.cnt$number.contact.general
    output <- data.mean[data.mean$part_age_cat == age.inf,]$mean
    output.var <- data.mean[data.mean$part_age_cat == age.inf, ]$var
    
    return(list(mean = output, var = output.var))
  }
  
  suscep.cnt.general <- function(mean.input = mean.of.cnt, 
                                 infected.input = infected,
                                 pop.vct = pop.vector,
                                 pop.age = pop_age,
                                 age = age.inf,
                                 contact_mat = contact.rates){
    
    sample_random_ids <- function(contacted_df) {
      sampled_ids <- vector("list", nrow(contacted_df))
      for (i in 1:nrow(contacted_df)) {
        age_row <- rownames(contacted_df)[i]
        num_contacts <- contacted_df[i,1]
        length_contacts <- as.numeric(rownames(pop_age[which(rownames(pop_age) %in% pop.vector & pop_age$part_age_cat == age_row), ]))
        ids <- sample(length_contacts, num_contacts)
        sampled_ids[[i]] <- ids
      }
      return(unlist(sampled_ids))
    }
    
    mean <- mean.of.cnt$mean
    var <- mean.of.cnt$var
    
    if(mean == 0){
      mean <- 1e-07
    }
    
    if(var == 0){
      var <- 1e-07
    }
    
    lambda_1 <- mean * sqrt(mean/var)
    lambda_2 <- (1- sqrt(mean/var))
    
    sample.no.cnt <- rgenpois(1, lambda_1, lambda_2)
    while(sample.no.cnt > min(table(data.epi$dataset.res$part_age_cat))){
      sample.no.cnt <- rgenpois(1,lambda_1, lambda_2)
    }
    contacted <- rmultinom(1, sample.no.cnt, contact_mat[["all"]][age,])
    cnt <- sapply(sample_random_ids(contacted), function(x) if(length(x) == 0) NA else x, simplify = F)
    return(unlist(cnt))
  }
  
  daily.cnt.diary <- list()
  data.epi.age <- data.epi$dataset.res
  data.epi.cnt <- data.epi$result
  pop.id <- names(data.epi$result)
  pop_age <- data.epi$dataset.res
  mean.cnt <- mean.input(raw.data)
  pop.size <- length(pop.id)
  pop.vector <- 1:pop.size
  weekday <- c('Mon','Tue','Wed','Thu','Fri')
  weekend <- c('Sat', 'Sun')
  
  initial.case.id <- sample(pop.id, initial.case)
  infected <- rep(0, pop.size)
  infected[which(pop.id %in% initial.case.id)] <- 1
  infectedMat <- infected
  incidenceMat <- infected
  infected.vector <- initial.case
  time_steps <- 1
  week.mod <- rotate_vector(starting_day)
  data.epi.cnt <- day.change(data.epi.cnt, week.mod)
  diary <- list()
  sub.diary <- list()
  
  while(sum(infected, na.rm = TRUE) > 0){
    # Repeat loop until all infected individuals are recovered and no new infections have occurred
    # Infect each susceptible with a certain probability
    incidence <- rep(0, pop.size)
    infectedPrev <- infected	# indicator of infection in the previous time step
    cnt.vector <- time_steps %% 7 
    if(cnt.vector == 0) cnt.vector <- 7
    diary[[time_steps]] <- list()
    
    for(inf in pop.vector[infected == 1 & is.na(infected) == F]){
      day <- week.mod[cnt.vector]
      id.inf <- pop.id[inf]
      age.inf <- data.epi.age[data.epi.age$part_id == id.inf, ]$part_age_cat
      
      mean.of.cnt <- mean.of.cnt.funct(data.mean = mean.cnt,
                                       age.cat = age.inf,
                                       day.cnt = day)
      
      track.incidence <- infected
      suscep.cnt <- suscep.cnt.general(mean.input = mean.of.cnt)
      
      if(length(suscep.cnt) > 0){
        for(j in 1:length(suscep.cnt)){
          id <- suscep.cnt[j]
          if(!is.na(infected[id]) & infected[id] == 0){
            infected[id] <- ifelse(runif(1) < foi, 1, 0)
          }
        }
      }
      
      incidence.diary <- track.incidence
      incidence.diary <- infected - incidence.diary
      incidence.diary[is.na(incidence.diary)] <- 0
      
      sub.diary$id.inf <- id.inf
      sub.diary$incidence.diary <- pop.id[incidence.diary == 1]
      sub.diary$suscep.cnt <- suscep.cnt
      sub.diary$day <- day
      sub.diary$age.cat <- age.inf
      diary[[time_steps]][[id.inf]] <- sub.diary
      
    }
    
    incitemp <- infected
    incidence <- incitemp - infectedPrev
    incidence[is.na(incidence)] = 0
    
    track.recovered <- pop.id[is.na(infected)]
    
    # at each time step, we pick a person and we see the probability this person got infected
    # Recover each infected with a certain probability
    for(infect in pop.vector[!is.na(infected) & infected == 1]){ #if they are infected;
      infected[infect] <- ifelse(runif(1) < gamma, NA, 1)
    }
    
    recovered.diary <- pop.id[is.na(infected)][!pop.id[is.na(infected)] %in% track.recovered]
    diary[[time_steps]]$recovered <- recovered.diary
    
    # Append current status to matrix and update time variable
    infectedMat <- cbind(infectedMat, infected)
    incidenceMat <- cbind(incidenceMat, incidence)
    time_steps <- time_steps + 1
    infected.daily <- sum(infected, na.rm = TRUE)
    infected.vector <- c(infected.vector, infected.daily)
    print(infected.daily)
    
  }
  output <- list(#full.diary = diary,
                 infected.daily = infected.vector,
                 infectedMat = infectedMat,
                 incidenceMat = incidenceMat,
                 time_steps = time_steps,
                 input = c(paste0("foi = ", foi), 
                           paste0("gamma = ", gamma),
                           paste0("starting day = ", starting_day),
                           paste0("initial case = ", initial.case)))
  return(output)
}

# Here is the code to simulate an epidemic naively (or here, baseline) with
# a poisson distribution

simulate.baseline.CM.poisson <- function(data.epi = data.temp,
                                 contact.rates = cm,
                                 raw.data = temp_dat,
                                 initial.case = 1,
                                 starting_day = "Mon",
                                 foi = 0.25,
                                 gamma = 0.22){
  
  mean.input <- function(data = data.input){
    
    number.contact.general <- data %>% 
      group_by(part_id, part_age_cat) %>%
      dplyr::summarise(sum = sum(n),
                .groups = 'drop') %>% 
      group_by(part_age_cat) %>% 
      dplyr::summarise(mean = mean(sum),
                var = var(sum),
                .groups = 'drop') %>% 
      na.omit()
    
    number.contact.input <- list(number.contact.general = number.contact.general)
    
    return(number.contact.input)
  }
  
  rotate_vector <- function(day_input) {
    day <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
    if(day_input != 'Mon'){
      index <- match(day_input, day)
      if (is.na(index)) {
        stop("Invalid input day.")
      }
      rotated_vector <- c(day[index:length(day)], day[1:(index-1)])
    } else rotated_vector <- day
    return(rotated_vector)
  }
  
  day.change <- function(data.epi.cnt, week.str){
    modified_list <- lapply(data.epi.cnt, function(x) data.frame(cnt_type = x$cnt_type, x[, week.mod]))
    return(modified_list)
  }
  
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
  
  mean.of.cnt.funct <- function(data.mean,
                                age.cat = age.inf,
                                day.cnt = day){
    data.mean <- mean.cnt$number.contact.general
    output <- data.mean[data.mean$part_age_cat == age.inf,]$mean
    output.var <- data.mean[data.mean$part_age_cat == age.inf, ]$var
    
    return(list(mean = output, var = output.var))
  }
  
  suscep.cnt.general <- function(mean.input = mean.of.cnt, 
                                 infected.input = infected,
                                 pop.vct = pop.vector,
                                 pop.age = pop_age,
                                 age = age.inf,
                                 contact_mat = contact.rates){
    
    sample_random_ids <- function(contacted_df) {
      sampled_ids <- vector("list", nrow(contacted_df))
      for (i in 1:nrow(contacted_df)) {
        age_row <- rownames(contacted_df)[i]
        num_contacts <- contacted_df[i,1]
        length_contacts <- as.numeric(rownames(pop_age[which(rownames(pop_age) %in% pop.vector & pop_age$part_age_cat == age_row), ]))
        ids <- sample(length_contacts, num_contacts)
        sampled_ids[[i]] <- ids
      }
      return(unlist(sampled_ids))
    }
    
    sample.no.cnt <- rpois(1, mean.of.cnt$mean)
    while(sample.no.cnt > min(table(data.epi$dataset.res$part_age_cat))){
      sample.no.cnt <- rpois(1, mean.of.cnt$mean)
    }
    contacted <- rmultinom(1, sample.no.cnt, contact_mat[["all"]][age,])
    cnt <- sapply(sample_random_ids(contacted), function(x) if(length(x) == 0) NA else x, simplify = F)
    return(unlist(cnt))
  }
  
  daily.cnt.diary <- list()
  data.epi.age <- data.epi$dataset.res
  data.epi.cnt <- data.epi$result
  pop.id <- names(data.epi$result)
  pop_age <- data.epi$dataset.res
  mean.cnt <- mean.input(raw.data)
  pop.size <- length(pop.id)
  pop.vector <- 1:pop.size
  weekday <- c('Mon','Tue','Wed','Thu','Fri')
  weekend <- c('Sat', 'Sun')
  
  initial.case.id <- sample(pop.id, initial.case)
  infected <- rep(0, pop.size)
  infected[which(pop.id %in% initial.case.id)] <- 1
  infectedMat <- infected
  incidenceMat <- infected
  infected.vector <- initial.case
  time_steps <- 1
  week.mod <- rotate_vector(starting_day)
  data.epi.cnt <- day.change(data.epi.cnt, week.mod)
  diary <- list()
  sub.diary <- list()
  
  while(sum(infected, na.rm = TRUE) > 0){
    # Repeat loop until all infected individuals are recovered and no new infections have occurred
    # Infect each susceptible with a certain probability
    incidence <- rep(0, pop.size)
    infectedPrev <- infected	# indicator of infection in the previous time step
    cnt.vector <- time_steps %% 7 
    if(cnt.vector == 0) cnt.vector <- 7
    diary[[time_steps]] <- list()
    
    for(inf in pop.vector[infected == 1 & is.na(infected) == F]){
      day <- week.mod[cnt.vector]
      id.inf <- pop.id[inf]
      age.inf <- data.epi.age[data.epi.age$part_id == id.inf, ]$part_age_cat
      
      mean.of.cnt <- mean.of.cnt.funct(data.mean = mean.cnt,
                                       age.cat = age.inf,
                                       day.cnt = day)
      
      track.incidence <- infected
      suscep.cnt <- suscep.cnt.general(mean.input = mean.of.cnt)
      
      if(length(suscep.cnt) > 0){
        for(j in 1:length(suscep.cnt)){
          id <- suscep.cnt[j]
          if(!is.na(infected[id]) & infected[id] == 0){
            infected[id] <- ifelse(runif(1) < foi, 1, 0)
          }
        }
      }
      
      incidence.diary <- track.incidence
      incidence.diary <- infected - incidence.diary
      incidence.diary[is.na(incidence.diary)] <- 0
      
      sub.diary$id.inf <- id.inf
      sub.diary$incidence.diary <- pop.id[incidence.diary == 1]
      sub.diary$suscep.cnt <- suscep.cnt
      sub.diary$day <- day
      sub.diary$age.cat <- age.inf
      diary[[time_steps]][[id.inf]] <- sub.diary
      
    }
    
    incitemp <- infected
    incidence <- incitemp - infectedPrev
    incidence[is.na(incidence)] = 0
    
    track.recovered <- pop.id[is.na(infected)]
    
    # at each time step, we pick a person and we see the probability this person got infected
    # Recover each infected with a certain probability
    for(infect in pop.vector[!is.na(infected) & infected == 1]){ #if they are infected;
      infected[infect] <- ifelse(runif(1) < gamma, NA, 1)
    }
    
    recovered.diary <- pop.id[is.na(infected)][!pop.id[is.na(infected)] %in% track.recovered]
    diary[[time_steps]]$recovered <- recovered.diary
    
    # Append current status to matrix and update time variable
    infectedMat <- cbind(infectedMat, infected)
    incidenceMat <- cbind(incidenceMat, incidence)
    time_steps <- time_steps + 1
    infected.daily <- sum(infected, na.rm = TRUE)
    infected.vector <- c(infected.vector, infected.daily)
    print(infected.daily)
    
  }
  output <- list(#full.diary = diary,
    infected.daily = infected.vector,
    infectedMat = infectedMat,
    incidenceMat = incidenceMat,
    time_steps = time_steps,
    input = c(paste0("foi = ", foi), 
              paste0("gamma = ", gamma),
              paste0("starting day = ", starting_day),
              paste0("initial case = ", initial.case)))
  return(output)
}

