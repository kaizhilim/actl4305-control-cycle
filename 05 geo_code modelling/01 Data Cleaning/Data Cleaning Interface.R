source("00 source.R")
source("05 geo_code modelling/01 Data Cleaning/Scripts/01 ass_refactor.R")

data_cleaning_interface <- function(ass_data) {
  ## 1. Data Type Manipulation ####
  ass_copy <- ass_data%>%
    mutate(
      # Convert Sprinkler to different safety levels
      sprinkler_type = as_factor(
        if_else(
          sprinkler_type == "None", 0,
          as.double(str_extract(sprinkler_type, "[:digit:]")))
      ),
      
      # Convert covers to Boolean
      across(c("property_cover", "LossofIncome_cover"), 
             ~.=="Y")
    )%>%
    
    # Remove policies with no property cover
    filter(property_cover)%>%
    select(-property_cover)%>%
    
    # Simplify small values
    mutate(across(c(suminsured_lossofinc, suminsured_prop,
                    grossincurred_lossofinc, grossincurred_prop),
                  ~ if_else(. < 10, 0, .)))%>%
    
    # Convert characters to factor
    mutate(across(where(is.character), as_factor))
  
  ## 2. Simplify Factors based on Exposure ####
  ass_encoding_list <- ass_fct_encoding(ass_copy)
  save(ass_encoding_list, file = "05 geo_code modelling/00 envr/Cleaning/ass_encoding_list.Rda")
  
  
  ## Applying encoding changes
  ass_rfct <- ass_copy%>%
    
    # Convert factors to character
    mutate(across(all_of(names(ass_encoding_list)), as.character))%>%
    
    # Using named vector to recode factor
    mutate(across(
      all_of(names(ass_encoding_list)), 
      ~if_else(
        .x %in% names(ass_encoding_list[[cur_column()]]),
        ass_encoding_list[[cur_column()]][.x], .x)
    ))%>%
    
    # Convert NAs to 'Unknown'
    mutate(across(all_of(names(ass_encoding_list)),
                  replace_na, "Unknown"))%>%
    
    # Convert back to factor (rearranging exposure is done in prep)
    mutate(across(all_of(names(ass_encoding_list)), as_factor))
  
  stopifnot(all(
    ass_encoding_list%>%
      imap_lgl(~!anyNA(ass_rfct[[.y]]))
  ))
  
  ## 3. NA treatment for LossofIncome_cover ####
  ass_clean_na <- ass_rfct %>%
    
    # If cover exists yet is na, convert to zero
    mutate(across(c('suminsured_lossofinc', 'grossincurred_lossofinc'),
                  ~if_else(LossofIncome_cover & is.na(.x), 0, .x)))%>%
    
    # If LoI doesn't exist, convert to 'no_LoI'
    mutate(indem_per_grp = as_factor(
      if_else(!LossofIncome_cover, "LoI_NULL", 
              as.character(indem_per_grp))
    ))
  
  stopifnot(!any(
    ass_clean_na%>%
      filter(LossofIncome_cover)%>%
      anyNA(),
    ass_clean_na%>%
      filter(!LossofIncome_cover)%>%
      select(-c('suminsured_lossofinc', 'grossincurred_lossofinc'))%>%
      anyNA()
  ))
  
  return(ass_clean_na)
}
