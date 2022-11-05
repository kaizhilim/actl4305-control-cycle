source("03 Preparation/Scripts/03 geo_code.R")

geo_code_interface <- function(geo_code_grid, policy_claims){
  policy_claims_grid <- geo_code_grid%>%
    mutate(
      geo_code_encoding = pmap(list(
        riskpostcode = threshold_riskpostcode,
        sa3name = threshold_sa3name,
        sa4name = threshold_sa4name
        ),
        generate_geo_encoding,
        ass_clean_na = policy_claims)
    )%>%
    mutate(
      n_geo_code = map_int(geo_code_encoding, n_distinct)
    )
  
  return(policy_claims_grid)
}