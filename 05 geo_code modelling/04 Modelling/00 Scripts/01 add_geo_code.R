add_geo_code <- function(data, geo_code_encoding_vec) {
  output = data%>%
    mutate(geo_code = geo_code_encoding_vec[riskpostcode])%>%
    select(-c("riskpostcode", "sa3name", "sa4name", "electoraterating"))%>%
    mutate(geo_code = fct_reorder(as_factor(geo_code), exposure, 
                                  .fun = sum, na.rm = T, .desc = T))
  
  stopifnot(!anyNA(output))
  return(output)
}