ass_refactor <- function(ass_copy, threshold_postcode = 30) {
  ## 1. Explore Dataset Factors ####
  # Note we will assume exposure column is clean
  # First use min exposure of 10, then manually combine
  
  threshold_exposure = 5
  
  ass_refactored <- ass_copy%>%
    mutate(across(where(is.factor) & !riskpostcode & !state, fct_lump_min, 
                  w = epy, min = threshold_exposure, other_level = "NEW"))
  
  ass_fct_lump_summary <- tibble(
    var = ass_copy%>%
      select(where(is.factor) & !riskpostcode & !state)%>%
      colnames()
  )%>%
    mutate(summary = map(
      var,
      ~ass_refactored %>%
        group_by(across(.x))%>%
        summarise(exposure = sum(epy))%>%
        arrange(exposure)
    ))
  
  ## 2. Decide Changes ####
  
  ## Changes:
  # Combine walls - Polystyrene, Glass into Other
  construction_wall_vec <- c("Other", "Other")
  names(construction_wall_vec) <- c("Polystyrene", "Glass")
  
  # Combine Occupation by letter, except top 10 + Unknown
  occupation_excl <- c(
    "Unknown",
    "O_Other Community Services",
    "D- Electricity, Gas & Water")
  
  occupation_refactor <- ass_fct_lump_summary$summary[[7]]%>%
    filter(!occupation %in% occupation_excl)%>%
    arrange(desc(exposure))%>%
    
    # Top 10 keep
    mutate(occupation_risk = if_else(
      row_number() > 10,
      paste0(str_sub(occupation, 1, 1),"_General"),
      as.character(occupation)))%>%
    
    # Bundle any non-top 15 as Other
    mutate(occupation_risk = fct_lump_n(
      as_factor(occupation_risk), w = exposure, n = 15))
  
  occupation_vec = c(as.character(occupation_refactor$occupation_risk), 
                     occupation_excl)
  names(occupation_vec) <- c(as.character(occupation_refactor$occupation), 
                             occupation_excl)
  
  ## 3. Postcode Simplification ####
  postcode_epy <- ass_copy%>%
    group_by(state, sa4name, sa3name, riskpostcode)%>%
    summarise(epy = sum(epy),
              .groups = "drop")
  
  affected_sa3 <- postcode_epy%>%
    filter(epy <threshold_postcode)
  
  convert_to_sa3 <- postcode_epy%>%
    inner_join(affected_sa3%>%
                 distinct(state, sa4name, sa3name),
               by = c("state", "sa4name", "sa3name"))%>%
    select(riskpostcode)
  
  ass_recode_sa3<-ass_copy%>%
    mutate(postcode_encoding = if_else(
      riskpostcode %in% convert_to_sa3$riskpostcode,
      sa3name,
      riskpostcode))
  
  postcode_encoding_epy <- ass_recode_sa3%>%
    group_by(state, sa4name, postcode_encoding)%>%
    summarise(epy = sum(epy),
              .groups = "drop")
  
  # sa4 requires a bigger threshold
  affected_sa4 <- postcode_encoding_epy%>%
    filter(epy <threshold_postcode * 4)
  
  convert_to_sa4 <- postcode_encoding_epy%>%
    inner_join(affected_sa4%>%
                 distinct(state, sa4name),
               by = c("state", "sa4name"))%>%
    select(postcode_encoding)
  
  ass_recode_sa4<-ass_recode_sa3%>%
    mutate(geo_code = if_else(
      postcode_encoding %in% convert_to_sa4$postcode_encoding,
      sa4name,
      postcode_encoding
    ))
  
  ass_encoding<-list(
    "construction_wall_code" = construction_wall_vec,
    "occupation_code" = occupation_vec,
    "geo_code" = ass_recode_sa4%>%
      select(state,riskpostcode, postcode_encoding, geo_code)%>%
      mutate(origin = case_when(
        geo_code == riskpostcode ~ "riskpostcode",
        geo_code == postcode_encoding ~ "sa3name",
        T ~ "sa4name"
      ))%>%
      distinct(state, riskpostcode, geo_code, origin)
  )
  
  return(ass_encoding)
}
