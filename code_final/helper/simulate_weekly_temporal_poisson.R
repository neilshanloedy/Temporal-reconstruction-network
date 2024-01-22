# redistribute uniformly througout the week

simulate_weekly_contact_poisson <- function(n_sample = 500,
                                            number.contacts = number.contact.input,
                                            proportion.week = prop.week,
                                            proportion.age = prop.age.partdata,
                                            is.diffweekend = F){
  
  data_partid = data.frame(part_id = paste0("id_", seq(1:n_sample)),
                           part_age_cat = (sample(proportion.age$part_age_cat, size = n_sample, replace = T,
                                                  prob = proportion.age$prop)))
  
  
  
  week <- c('Mon','Tue','Wed','Thu',
            'Fri','Sat','Sun')
  weekday <- c('Mon','Tue','Wed','Thu','Fri')
  weekend <- c('Sat', 'Sun')
  
  input.poisson <- function(dataset, period){
    mean <- dataset[dataset$frequency_multi == period,]$mean
    return(mean)
  }
  
  if(!is.diffweekend){
    number.contacts <- number.contacts$number.contact.general
  } else {
    number.contacts <- number.contacts$number.contact
  }
  
  list_id <- list()
  temp <- data.frame()
  for(id in data_partid$part_id){
    age_cat_loop <- data_partid[data_partid$part_id == id,]$part_age_cat
    
      number.cnt <- number.contacts[number.contacts$part_age_cat == age_cat_loop, ]
      input.daily <- input.poisson(number.cnt, "daily")
      input.weekly <- input.poisson(number.cnt, "weekly")
      input.monthly <- input.poisson(number.cnt, "monthly")
      input.few <- input.poisson(number.cnt, "a few times a year")
      input.first <- input.poisson(number.cnt, "first time")
      
      cnt.daily <- rpois(1, input.daily)
      cnt.weekly <- rpois(7, input.weekly)
      cnt.monthly <- rpois(7, input.monthly)
      cnt.few <- rpois(7, input.few)
      cnt.first <- rpois(7, input.first)
      
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
