split_by_LoI <- function(policy_claims, LossofIncome_cover, 
                         geo_code_encoding_vec) {
  
  policy_claims <- policy_claims%>%
    select(-c("sa3name", "sa4name", "electoraterating"))
  
  ## 1. Split based on policy coverage ####
  if (LossofIncome_cover) {
    claims_finalized <- policy_claims%>%
      filter(suminsured_prop > 0, suminsured_lossofinc > 0)%>%
      mutate(
        has_claim = claimcount_lossofinc > 0)
  } else {
    claims_finalized <- policy_claims%>%
      select(-contains(c("lossofinc", "loi", "indem")))%>%
      filter(suminsured_prop > 0)%>%
      mutate(
        has_claim = claimcount_prop > 0)
  }
  
  return(claims_finalized)
}


