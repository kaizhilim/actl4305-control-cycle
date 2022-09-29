source("00 source.R")

ass_data = read_csv("Assignment Data/Assignment Data.csv")

## 1. Data Type Manipulation ####
ass_copy <- ass_data%>%
  mutate(
    # Convert Sprinkler to different safety levels
    sprinkler_type = as_factor(
      if_else(
        sprinkler_type == "None", 0,
        as.double(str_extract(sprinkler_type, "[:digit:]")))
    ),
    
    # Convert covers to Boolean
    across(c("property_cover", "LossofIncome_cover"), 
           ~.=="Y")
  )%>%
  
  # Convert characters to factor
  mutate(across(where(is.character), as_factor))%>%
  
  # Remove policies with no property cover
  filter(property_cover)%>%
  
  # Simplify occupation
  mutate(occupation_risk = factor(str_sub(occupation, 1, 1)))%>%
  
  # Remove redundant columns
  select(-property_cover, -occupation) 

## 2. Policyholder Details ####
policyholder_cols = c(
  # Policy Identifier
  "policyno", "situation_num",
  
  # Packaged policy
  "LossofIncome_cover",
  
  # Policy Dates
  "effectdate", "expirydate", "ym", "xm",
  
  # Building address
  "riskpostcode", "state",
  
  # Building details
  "building_age", "building_type", "construction_walls", "construction_floor",
  "sprinkler_type",
  
  # Occupation
  "occupation_risk"
)

# Policy version history of each date range
policy_df <- ass_copy%>%
  select(all_of(policyholder_cols))%>%
  relocate(all_of(policyholder_cols))%>%
  arrange(policyno, situation_num, effectdate, expirydate,
          desc(ym), desc(xm))%>%
  distinct(across(!c(ym, xm)), .keep_all = T)%>%
  group_by(policyno, situation_num)%>%
  mutate(policy_version = dense_rank(paste(effectdate, expirydate)))%>%
  ungroup()%>%
  mutate(cancelled = expirydate - as.Date(paste0(ym,"-", xm, "-01")) > 61)

# Overall history of each situation
# Note that policy_df has been arranged with asc expirydate
policy_distinct <- policy_df %>%
  group_by(policyno, situation_num) %>%
  mutate(
    LossofIncome_cover = any(LossofIncome_cover),
    # Earliest expiry date
    effectdate = min(effectdate)
  ) %>% 
  # This will keep the latest property information for the policy
  slice_tail() %>% 
  ungroup() 

# Claim information by policy version
policy_history_df <- ass_copy %>%
  group_by(policyno, situation_num) %>%
  summarise (
    # Number of LI Claims
    n_claims_lossofinc = sum(grossincurred_lossofinc > 0, na.rm = TRUE),
    
    # Total Severity of LI Claims
    total_grossincurred_lossofinc = sum(grossincurred_lossofinc, na.rm = TRUE),
    
    # Average LI Claim Size
    avgincurred_lossofinc = ifelse(
      n_claims_lossofinc == 0, NA, 
      total_grossincurred_lossofinc / n_claims_lossofinc),
    
    # Median Sum Insured LI
    median_SI_lossofinc = median(suminsured_lossofinc, na.rm = TRUE),
    
    # Number of Prop Claims
    n_claims_prop = sum(grossincurred_prop > 0, na.rm = TRUE),
    
    # Total Severity of Prop Claims
    total_grossincurred_prop = sum(grossincurred_prop, na.rm = TRUE),
    
    # Average Prop Claim Size
    avgincurred_prop = ifelse(
      n_claims_prop == 0, NA, 
      total_grossincurred_prop / n_claims_prop),
    
    # Median Sum Insured Prop
    median_SI_prop = median(suminsured_prop, na.rm = TRUE),
    
    .groups = "keep"
  ) %>%
  left_join(policy_distinct, by = c('policyno', 'situation_num'))

# Save as R File
save(ass_copy, file = "00 envr/Compulsory/ass_copy.R")
save(policy_df, file = "00 envr/Compulsory/policy_df.R")
save(policy_history_df, file = "00 envr/Compulsory/policy_history_df.R")

rm(policyholder_cols)
rm(policy_distinct)
