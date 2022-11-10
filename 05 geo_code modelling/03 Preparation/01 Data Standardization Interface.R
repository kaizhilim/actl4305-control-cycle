source("00 source.R")
source("05 geo_code modelling/03 Preparation/Scripts/02 add_postcode_dets.R")

## To find how many non distinct row, use
# filter(row_count > 1)

data_standardization_interface <- function(ass_clean_na){
  
  ## Any preprocessing
  ass_tidy <- ass_clean_na%>%
    rename(occupation_risk = occupation)
  
  ## 1. Standardise Policy Details ####
  policy_details_distinct<-ass_tidy%>%
    group_by(policyno, situation_num, effectdate, expirydate)%>%
    arrange(desc(ym), desc(xm),
            .by_group = T)%>%
    count(riskpostcode, state, building_age, building_type, 
          construction_walls, construction_floor, sprinkler_type, 
          occupation_risk)%>%
    
    # There are 24 rows where policy details change regarding riskpostcode &
    # Sprinkler information
    # 895 rows came from occupation changes
    mutate(row_count = n())%>%
    mutate(max_n = max(n))%>%
    filter(row_count == 1 | n == max_n)%>%
    
    # Keep first row 
    distinct(policyno, situation_num, effectdate, expirydate,.keep_all = T)
  
  # Unfortunately suminsured changes quite frequently
  # A single rule to all
  policy_suminsured_prop_distinct <- ass_tidy%>%
    group_by(policyno, situation_num, effectdate, expirydate)%>%
    arrange(desc(ym), desc(xm),
            .by_group = T)%>%
    count(suminsured_prop)%>%
    mutate(row_count = n())%>%
    mutate(max_n = max(n))%>%
    filter(row_count == 1 | n == max_n)%>%
    distinct(policyno, situation_num, effectdate, expirydate,.keep_all = T)
  
  stopifnot(!anyNA(ass_tidy %>%
                     filter(LossofIncome_cover)))
  
  policy_suminsured_lossofinc_distinct <- ass_tidy %>%
    filter(LossofIncome_cover)%>%
    group_by(policyno, situation_num, effectdate, expirydate)%>%
    arrange(desc(ym), desc(xm),
            .by_group = T)%>%
    count(suminsured_lossofinc, indem_per_grp)%>%
    mutate(row_count = n())%>%
    mutate(max_n = max(n))%>%
    filter(row_count == 1 | n == max_n)%>%
    distinct(policyno, situation_num, effectdate, expirydate,.keep_all = T)
  
  ## 2. Exposure Table
  min_date = min(ass_tidy$start)
  max_date = max(ass_tidy$start)
  
  tibble_days = tibble(days= 0:difftime(max_date, min_date))
  
  # Choose 0.99998
  # map_dfr(
  #   c(0.99, seq(0.999, 0.9999, 0.0002)),
  #   ~ tibble_days %>% mutate(base = factor(.x), value = .x ^ days)
  # ) %>%
  #   ggplot(aes(days, value, group = base, color = base)) +
  #   geom_line()
  
  weight_date_base = 0.9998
  
  ## 3. Generate Consolidated Data Frame ####
  policy_claims_consolidated <-ass_tidy%>%
    select(-c(riskpostcode, state, building_age, building_type, 
              construction_walls, construction_floor, sprinkler_type, 
              occupation_risk,
              suminsured_prop, suminsured_lossofinc, indem_per_grp))%>%
    
    # Date Weights
    # To add as weights, use 'recipes::importance_weights'
    # As exposure is used separately as a log offset, will not multiply
    mutate(date_weights = weight_date_base ^ as.numeric(
      difftime(max_date, start, units = "days")))%>%
    
    # Policy version
    group_by(policyno, situation_num, effectdate, expirydate)%>%
    arrange(policyno, situation_num, effectdate, expirydate)%>%
    
    # Sum of elements
    summarise(
      exposure = sum(epy),
      date_weights = mean(date_weights),
      claimcount_prop = sum(grossincurred_prop > 0, na.rm = T),
      claimcount_lossofinc = sum(grossincurred_lossofinc > 0, na.rm = T),
      grossincurred_prop = sum(grossincurred_prop,  na.rm = T),
      grossincurred_lossofinc = sum(grossincurred_lossofinc, na.rm = T),
      LossofIncome_cover = any(LossofIncome_cover),
      .groups = 'keep'
    )%>%
    
    ## Add back in distinct details
    left_join(
      policy_details_distinct%>%
        select(-c(n, row_count, max_n)), 
      by = c("policyno", "situation_num", "effectdate", "expirydate"))%>%
    left_join(
      policy_suminsured_prop_distinct%>%
        select(-c(n, row_count, max_n)),
      by = c("policyno", "situation_num", "effectdate", "expirydate"))%>%
    left_join(
      policy_suminsured_lossofinc_distinct%>%
        select(-c(n, row_count, max_n))%>%
        mutate(LossofIncome_cover = TRUE),
      by = c("policyno", "situation_num", "effectdate", "expirydate",
             "LossofIncome_cover"))%>%
    
    # Add details for geo_code
    add_postcode_dets()%>%
    
    # Policy version ranks
    group_by(policyno, situation_num)%>%
    mutate(policy_version = dense_rank(paste(effectdate, expirydate)))%>%
    relocate(policy_version, .after = 'situation_num')%>%
    ungroup()%>%
    select(-c(effectdate, expirydate))%>%
    mutate(building_info_na = is.na(building_age))%>%
    
    # If LoI doesn't exist, convert to 'no_LoI'
    mutate(indem_per_grp = as_factor(
      if_else(!LossofIncome_cover, "LoI_NULL", 
              as.character(indem_per_grp))
    ))
  
  ## 4. Remove NAs ####
  stopifnot(
    !any(policy_claims_consolidated%>%
           filter(LossofIncome_cover)%>%
           anyNA(),
         policy_claims_consolidated%>%
           filter(!LossofIncome_cover)%>%
           select(-suminsured_lossofinc)%>%
           anyNA()
    )
  )
  
  ## 5. Final Output ####
  # Also rearrange factors by exposure
  policy_claims <- policy_claims_consolidated%>%
    mutate(suminsured_lossofinc = if_else(!LossofIncome_cover, 0, 
                                          suminsured_lossofinc))%>%
    mutate(across(where(is.factor), fct_reorder, exposure, 
                  .fun = sum, na.rm = T, .desc = T))
  
  stopifnot(!anyNA(policy_claims))
  
  return(policy_claims)
}


