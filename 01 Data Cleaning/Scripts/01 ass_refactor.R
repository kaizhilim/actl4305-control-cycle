ass_fct_encoding <- function(
    ass_copy, threshold_exposure = 10, 
    occupation_top_n = 10, occupation_top_mean_severity_n = 2,
    occupation_excl = c("Unknown")) {
  
  ## Description ####
  " Function that refactors the columns: construction_wall, 
    * construction_wall 
    * occupation
    
    Logic for manual vectors:
    ass_copy%>%
      filter(grossincurred_prop > 0)%>%
      group_by(__factor__)%>%
      summarise(exposure = sum(epy))
    
    Outputs:
    Named vectors describing the factors, with levels (ignoring NAs)
    * construction_wall = c('Brick/Concrete', 'Other')
    * construction_floor = c('Concrete', 'Other')
    * building_age = c(
        'Greater Than 20 Years', 'Less Than 20 Years',
        'After 1980', 'Before 1980')
    * building_type = c(
        'Warehouse low rise', 'Tilt Up Concrete low rise', 'Mid-High rise',
        'Other')
    * sprinkler_type = c('0', '>0')
    
    * occupation = named vector based on threshold input
  "
  
  OCCUPATION_nLVL_MULTIPLE = 1.3
  
  #### Manual Named Vectors ####
  # 1. construction_walls ####
  construction_walls_vec <- c("Other", "Other", "Other")
  names(construction_walls_vec) <- c(
    "Polystyrene", "Glass", 
    "Aluminum/Fibro/Iron/Steel (on Steel Frame)")
  
  # 2. construction_floor ####
  construction_floor_vec <- c("Other", "Other")
  names(construction_floor_vec) <- c("Timber", "Other")
  
  # 3. building_age ####
  building_age_vec = c('Less Than 20 Years', 'Before 1980', 'Before 1980')
  names(building_age_vec) = c('Less than 2 Years', '1960 - 1980', 'Before 1960')
  
  # 4. building_type ####
  building_type_vec = c('Mid-High rise','Mid-High rise')
  names(building_type_vec) = c('Medium rise', 'High rise')
  
  # 5. sprinkler_type ####
  sprinkler_type_vec = c('>0', '>0', '>0')
  names(sprinkler_type_vec) = c('1','2','3')
  
  #### Automatic Named Vectors ####
  
  # a. Take the top occupation_top_mean_severity_n number of occupation
  occupation_mean_severity_excl_prop <- ass_copy%>%
    filter(grossincurred_prop >0)%>%
    group_by(occupation)%>%
    summarise(severity_prop = mean(grossincurred_prop, na.rm = T))%>%
    arrange(desc(severity_prop))%>%
    slice_head(n = occupation_top_mean_severity_n)%>%
    pull(occupation)%>%
    as.character()
  
  occupation_mean_severity_excl_loi <- ass_copy%>%
    filter(grossincurred_lossofinc >0)%>%
    group_by(occupation)%>%
    summarise(severity_loi = mean(grossincurred_lossofinc, na.rm = T))%>%
    arrange(desc(severity_loi))%>%
    slice_head(n = occupation_top_mean_severity_n)%>%
    pull(occupation)%>%
    as.character()
  
  # b. Exclusion vector
  occupation_excl_vec = c(
    occupation_mean_severity_excl_prop,
    occupation_mean_severity_excl_loi,
    occupation_excl
  )
  
  # Combine Occupation by letter and extract occupation_top_n
  occupation_refactor <- ass_copy%>%
    filter(!occupation %in% occupation_excl_vec)%>%
    group_by(occupation)%>%
    summarise(exposure = sum(epy))%>%
    arrange(desc(exposure))%>%
    
    # Top 10 keep
    mutate(occupation_risk = if_else(
      row_number() > occupation_top_n,
      paste0(str_sub(occupation, 1, 1),"_General"),
      as.character(occupation)))%>%
    
    # Bundle any non-top (OCCUPATION_nLVL_MULTIPLE * occupation_top_n) as Other
    mutate(occupation_risk = fct_lump_n(
      as_factor(occupation_risk), w = exposure, 
      floor(OCCUPATION_nLVL_MULTIPLE * occupation_top_n)))
  
  # Combine occupation risk
  occupation_output_vec = c(as.character(occupation_refactor$occupation_risk), 
                     occupation_excl_vec)
  # Names using occupation
  names(occupation_output_vec) <- c(
    as.character(occupation_refactor$occupation), 
    occupation_excl_vec)

  ## 3. Output ass_encoding_list  
  ass_encoding_list <-list(
    "construction_walls" = construction_walls_vec,
    "construction_floor" = construction_floor_vec,
    "building_age" = building_age_vec,
    "building_type" = building_type_vec,
    "sprinkler_type" = sprinkler_type_vec,
    "occupation" = occupation_output_vec
  )
  
  # All the vectors should be named
  stopifnot(!any(ass_encoding_list%>%
                   map_lgl(~is_null(names(.)))))
  
  # All of the vector names should be in the column
  stopifnot(
    all(ass_encoding_list%>%
          imap_lgl(
            ~all(names(.x) %in% levels(ass_copy[[.y]])))
    )
  )
  
  return(ass_encoding_list)
}
