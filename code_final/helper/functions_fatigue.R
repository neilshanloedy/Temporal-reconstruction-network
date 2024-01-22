combine_data <- function(participant_data,
                         sday_data,
                         hh_data,
                         participant_extra){
  output <- merge(merge(merge(participant_data, sday_data), hh_data), participant_extra)
  return(output)
}

prepare_data_tomodel <- function(dataset){
  dataset <- dataset %>% 
    mutate(part_uid = as.factor(part_uid),
           wave_bin = as.factor(wave_bin),
           part_face_mask = as.factor(part_face_mask),
           area_3_name = as.factor(area_3_name),
           holiday = as.factor(holiday),
           frequency_multi = as.factor(frequency_multi),
           contact_day = as.factor(contact_day),
           hh_size = as.factor(hh_size),
           part_age_cat = as.factor(part_age_cat))
  
  dataset <- within(dataset,
                    hh_size <- relevel(hh_size, ref = "2"))
  dataset <- within(dataset,
                    wave_bin <- relevel(wave_bin, ref = "1"))
  dataset <- within(dataset,
                    part_face_mask <- relevel(part_face_mask, ref = "no"))
  dataset <- within(dataset,
                    area_3_name <- relevel(area_3_name, ref = "Vlaams Gewest"))
  dataset <- within(dataset,
                    holiday <- relevel(holiday, ref = "0"))
  dataset <- within(dataset,
                    frequency_multi <- relevel(frequency_multi, ref = "daily"))
  dataset <- within(dataset,
                    contact_day <- relevel(contact_day, ref = "weekday"))
  dataset <- within(dataset,
                    part_age_cat <- relevel(part_age_cat, ref = "Adult"))
  
  return(dataset)
}

prepare_sumdata_tomodel <- function(dataset){
  dataset <- dataset %>% 
    mutate(part_uid = as.factor(part_uid),
           wave_bin = as.factor(wave_bin),
           part_face_mask = as.factor(part_face_mask),
           area_3_name = as.factor(area_3_name),
           holiday = as.factor(holiday),
           contact_day = as.factor(contact_day),
           hh_size = as.factor(hh_size),
           part_age_cat = as.factor(part_age_cat))
  
  dataset <- within(dataset,
                    hh_size <- relevel(hh_size, ref = "2"))
  dataset <- within(dataset,
                    wave_bin <- relevel(wave_bin, ref = "1"))
  dataset <- within(dataset,
                    part_face_mask <- relevel(part_face_mask, ref = "no"))
  dataset <- within(dataset,
                    area_3_name <- relevel(area_3_name, ref = "Vlaams Gewest"))
  dataset <- within(dataset,
                    holiday <- relevel(holiday, ref = "0"))
  dataset <- within(dataset,
                    contact_day <- relevel(contact_day, ref = "weekday"))
  dataset <- within(dataset,
                    part_age_cat <- relevel(part_age_cat, ref = "Adult"))
  
  return(dataset)
}

data_factor <- function(dataset, age_cat = "Children"){
  
  factor.col <- c("part_uid", "part_age_cat",
                  "wave_bin", "hh_size", "part_face_mask", 
                  "area_3_name", "holiday", "frequency_multi", "contact_day")
  
  if(age_cat %in% c("Adult", "Elderly")){
    factor.col <- c(factor.col, "part_vacc")  
  }
  
  dataset[factor.col] <- lapply(dataset[factor.col], factor)
  
  return(dataset)
}

references_data <- function(dataset, age_cat = "Children"){
  dataset <- within(dataset,
                    wave_bin <- relevel(wave_bin, ref = "1"))
  dataset <- within(dataset,
                    part_face_mask <- relevel(part_face_mask, ref = "no"))
  dataset <- within(dataset,
                    area_3_name <- relevel(area_3_name, ref = "Vlaams Gewest"))
  dataset <- within(dataset,
                    holiday <- relevel(holiday, ref = "0"))
  dataset <- within(dataset,
                    frequency_multi <- relevel(frequency_multi, ref = "daily"))
  dataset <- within(dataset,
                    contact_day <- relevel(contact_day, ref = "weekday"))
  
  if(age_cat %in% c("Children", "Teens")){
    dataset <- within(dataset,
                      hh_size <- relevel(hh_size, ref = "3"))
  } else {
    dataset <- within(dataset,
                      hh_size <- relevel(hh_size, ref = "2"))
    dataset <- within(dataset,
                      part_vacc <- relevel(part_vacc, ref = "No"))
  }
  return(dataset)
}

select_dataframe <- function(dataframe, age_cat_model){
  if(age_cat_model %in% c("Children", "Teens")){
    var_names <- c("hh_size", "area_3_name",
             "wave_bin", "part_face_mask",
             "holiday", "frequency_multi",
             "contact_day", "day_number", "part_uid")
  }
  
  dataframe <- dataframe[match(var_names, names(dataframe))]
  
  return(dataframe)
}

marginal_dataframe <- function(dataframe, model){
  
  dataframe <- na.omit(dataframe)
  
  dataframe_assume <- dataframe %>% 
    dplyr::mutate(wave_bin = "1") %>% 
    dplyr::mutate(wave_bin = as.factor(wave_bin))
  
  dataframe_assume$predict <- exp(predict(model,
                                          newdata = dataframe_assume,
                                          re.form = NA))
  
  dataframe$predict <- exp(predict(model,
                                   re.form = NA))
  
  output <- cbind(dataframe,'predict_f'= dataframe_assume$predict)  
  
  return(output)
}

redistribute_prediction <- function(cnt_model_dataframe,
                                    merged_dataframe){
  temp_data <- cnt_model_dataframe %>% 
    dplyr::select(-n)
  
  join_data <- merge(merged_dataframe, temp_data) %>% 
    arrange(part_id, part_uid, frequency_multi)
  
  part_i <- unique(join_data$part_id)
  output <- join_data
  
  for(i in 1:length(unique(output$part_id))){
    part_i <- unique(output$part_id)
    frequency_multi <- levels(join_data$frequency_multi)
    contact_predict_i <- rpois(1, unique(output[output$part_id %in% part_i[i], ]$predict_f))
    prob_i <- output[output$part_id %in% part_i[i], ]$n
    redist_i <- rmultinom(1, contact_predict_i, prob_i)
    
    for(j in 1:length(frequency_multi)){
      output[output$part_id %in% part_i[i] & output$frequency_multi %in% frequency_multi[j], ]$predict_f <- redist_i[j]
    }
    print(paste0("we are at ", round((i/length(unique(output$part_id))*100), 3), "%"))
  }
  
  return(output)
}

prepare_data_physical_fatigue <- function(data_fatigue_corrected = data_temp_freq,
                                          data_comix = data_comix){
  
  expand_and_remove <- function(data) {
    # Remove rows where n = 0
    data <- data[data$n > 0, ]
    
    # Duplicate rows based on the value of n
    expanded_data <- data[rep(row.names(data), data$n), ]
    
    return(expanded_data)
  }
  
  comix_data_participants <- data_comix$participants
  comix_data_contacts <- data_comix$contacts
  comix_data_physcont <- comix_data_contacts[comix_data_contacts$phys_contact == "1",] %>% 
    dplyr::select(cont_id, part_id, frequency_multi, phys_contact, cnt_age_est_min, cnt_age_est_max,
                  cnt_age_exact)
  
  comix_data_physcont$frequency_multi <- factor(comix_data_physcont$frequency_multi,
                                                levels=c('1','2','3','4','5'),
                                                labels=c('daily','weekly','monthly',
                                                         'a few times a year','first time'))
  
  result <- list()
  for(i in 1:length(unique(data_fatigue_corrected$part_id))){
    print(paste0("We are at ", round(i/length(unique(data_fatigue_corrected$part_id))*100, 2), "%"))
    id_i = unique(data_fatigue_corrected$part_id)[i]
    data_subset <- data_fatigue_corrected[data_fatigue_corrected$part_id == id_i,]
    data_contacts <- merge(data_subset[,c("part_id", "n", "frequency_multi")], comix_data_physcont, by = c("part_id", "frequency_multi"))
    result[[i]] <- unique(expand_and_remove(data_contacts))
  }
  
  result_newcontacts <- do.call("rbind", result)
  
  output(result_newcontacts)
}
