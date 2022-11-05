split_by_LoI <- function(policy_claims, LossofIncome_cover, 
                       geo_code_encoding_vec) {
  
  ## 1. Split based on policy coverage ####
  if (LossofIncome_cover) {
    claims_finalized <- policy_claims%>%
      filter(LossofIncome_cover)%>%
      filter(suminsured_prop > 0, suminsured_lossofinc > 0)%>%
      mutate(has_claim = claimcount_lossofinc > 0)
  } else {
    claims_finalized <- policy_claims%>%
      filter(!LossofIncome_cover)%>%
      select(-contains(c("lossofinc", "loi", "indem")))%>%
      filter(suminsured_prop > 0)%>%
      mutate(has_claim = claimcount_prop > 0)
  }
  
  ## 2. Add geo_code
  source("04 Modelling/00 Scripts/01 add_geo_code.R")
  claims_finalized <- claims_finalized%>%
    add_geo_code(geo_code_encoding_vec)
  
  return(claims_finalized)
}


