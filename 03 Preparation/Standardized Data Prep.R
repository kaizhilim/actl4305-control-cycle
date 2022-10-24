library(tidyverse)
library(lubridate)
library(insurancerating)

load("00 envr/Compulsory/ass_copy.R")

## 1. Standardise Policy Details ####
policy_details_distinct<-ass_copy%>%
  group_by(policyno, situation_num, effectdate, expirydate)%>%
  arrange(desc(ym), desc(xm),
          .by_group = T)%>%
  count(riskpostcode, state, building_age, building_type, 
        construction_walls, construction_floor, sprinkler_type, 
        occupation_risk)%>%
  
  # There are 771 rows where policy details change within each policy version
  # Get the most frequent detail, leaving 20 rows
  mutate(row_count = n())%>%
  mutate(max_n = max(n))%>%
  filter(row_count == 1 | n == max_n)%>%
  
  # Keep first row 
  distinct(policyno, situation_num, effectdate, expirydate,.keep_all = T)

ass_main <- ass_copy%>%
  select(-c(riskpostcode, state, building_age, building_type, 
                 construction_walls, construction_floor, sprinkler_type, 
                 occupation_risk))%>%
  left_join(policy_details_distinct, 
            by = c("policyno", "situation_num", "effectdate", "expirydate"))
  
## 2. Exposure Table



