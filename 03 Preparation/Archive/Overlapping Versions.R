source("03 Preparation/Standardized Data Prep.R")

# Check if any effect date and expiry dates overlap
#   - There are a total of 92 (policyno, situation_num) with inconsistent overlaps
#   - This is 2030 rows
# Allowing for 1 month discrepancies, there are 68 (1424 rows)
# Decision was made to ignore this issue as this only affected 0.125% of the data
policy_lastyearmon <- ass_copy%>%
  mutate(yearmon = as.Date(paste(ym, xm, "1", sep = "-")))%>%
  group_by(policyno, situation_num, effectdate, expirydate)%>%
  summarise(last_yearmon = max(yearmon),
            .groups = 'drop')%>%
  group_by(policyno, situation_num)%>%
  mutate(prev_endDate = lag(last_yearmon))

overlapping_versions <- policy_details_distinct%>%
  left_join(policy_lastyearmon,
            by = c("policyno", "situation_num", "effectdate", "expirydate"))%>%
  arrange(policyno, situation_num, effectdate, expirydate)%>%
  group_by(policyno, situation_num)%>%
  mutate(overlap_lag = effectdate < lag(expirydate),
         renewal_lag = effectdate < prev_endDate - month(1),
         overlap_lead = lead(effectdate) < expirydate)%>%
  mutate(renewal_lead = lead(renewal_lag))%>%
  relocate(prev_endDate,overlap_lead, renewal_lead,
           overlap_lag, renewal_lag,
           .after = "expirydate")%>%
  filter((overlap_lag & renewal_lag) | (overlap_lead & renewal_lead))