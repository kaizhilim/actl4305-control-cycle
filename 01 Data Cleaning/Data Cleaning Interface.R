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
  select(-property_cover)

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

# Save as R File
save(ass_copy, file = "00 envr/Compulsory/ass_copy.R")
save(policy_df, file = "00 envr/Compulsory/policy_df.R")

rm(policyholder_cols)