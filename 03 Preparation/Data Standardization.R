source("00 source.R")

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
  left_join(policy_details_distinct%>%
              select(-c(n, row_count, max_n)), 
            by = c("policyno", "situation_num", "effectdate", "expirydate"))

policy_sumins_distinct <- ass_main%>%
  group_by(policyno, situation_num, effectdate, expirydate)%>%
  arrange(desc(ym), desc(xm),
          .by_group = T)%>%
  count(suminsured_prop, suminsured_lossofinc, indem_per_grp)%>%
  mutate(row_count = n())%>%
  mutate(max_n = max(n))%>%
  filter(row_count == 1 | !is.na(suminsured_lossofinc) | n == max_n)%>%
  distinct(policyno, situation_num, effectdate, expirydate,.keep_all = T)

## 2. Exposure Table
min_date = min(ass_main$effectdate)
max_date = max(ass_main$expirydate)

tibble_days = tibble(days= 0:difftime(max_date, min_date))

# Choose 0.9985 
map_dfr(
  c(0.99, seq(0.998, 0.9999, 0.00025)),
  ~ tibble_days %>% mutate(base = factor(.x), value = .x ^ days)
) %>%
  ggplot(aes(days, value, group = base, color = base)) +
  geom_line()

weight_date_base = 0.9985

policy_claims<-ass_copy%>%
  select(-c(riskpostcode, state, building_age, building_type, 
            construction_walls, construction_floor, sprinkler_type, 
            occupation_risk,
            suminsured_prop, suminsured_lossofinc, indem_per_grp))%>%
  
  # Date Weights
  mutate(date_weights = weight_date_base ^ as.numeric(
    difftime(max_date, start, units = "days")))%>%
  # To add as weights, use 'recipes::importance_weights'
  mutate(exposure = epy * date_weights)%>%
  
  # Policy version
  group_by(policyno, situation_num, effectdate, expirydate)%>%
  arrange(policyno, situation_num, effectdate, expirydate)%>%
  
  # Sum of elements
  summarise(
    exposure = sum(exposure),
    grossincurred_prop = sum(grossincurred_prop),
    grossincurred_lossofinc = sum(grossincurred_lossofinc),
    LossofIncome_cover = any(LossofIncome_cover),
    .groups = 'keep'
  )%>%
  left_join(policy_details_distinct%>%
              select(-c(n, row_count, max_n)), 
            by = c("policyno", "situation_num", "effectdate", "expirydate"))%>%
  left_join(policy_sumins_distinct%>%
              select(-c(n, row_count, max_n)),
            by = c("policyno", "situation_num", "effectdate", "expirydate"))%>%
  
  mutate(policy_version = dense_rank(paste(effectdate, expirydate)))
  
save(ass_main, file = "00 envr/Compulsory/ass_main.R")
save(policy_claims, file = "00 envr/Compulsory/policy_claims.R")