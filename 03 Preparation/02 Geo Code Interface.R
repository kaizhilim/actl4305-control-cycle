source("03 Preparation/Scripts/03 geo_code.R")

geo_code_interface <- function(geo_code_grid, policy_claims){
  policy_claims_grid <- geo_code_grid%>%
    mutate(
      geo_code_encoding = pmap(list(
        riskpostcode = threshold_riskpostcode,
        sa3name = threshold_sa3name,
        sa4name = threshold_sa4name),
        generate_geo_encoding,
        ass_clean_na = policy_claims)
    )%>%
    mutate(
      n_geo_code = map(geo_code_encoding, n_distinct),
      policy_data = map(
        geo_code_encoding,
        function(geo_code_encoding) {
          output = policy_claims%>%
            mutate(geo_code = geo_code_encoding[riskpostcode])%>%
            select(-c("riskpostcode", "state", 
                      "sa3name", "sa4name", "electoraterating"))%>%
            mutate(geo_code = fct_reorder(as_factor(geo_code), exposure, 
                                          .fun = sum, na.rm = T, .desc = T))
          
          stopifnot(!anyNA(output))
          return(output)
        })
    )
  
  return(policy_claims_grid)
}