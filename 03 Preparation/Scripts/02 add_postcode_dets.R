add_postcode_dets <- function(ass_tmp) {
  source("03 Preparation/Scripts/01 au_riskpostcode.R")
  au_riskpostcode<-get_au_riskpostcode(ass_tmp)
  
  ## 1. Ammend Known Postcodes with Issues ####
  ass_tmp <- 
    ass_tmp%>%
    mutate(across(c(riskpostcode, state), as.character))%>%
    
    ## Modify postcodes
    mutate(state = if_else(riskpostcode == "004375", "QLD", state),
           riskpostcode = if_else(riskpostcode == "002309", "002083", 
                                  riskpostcode))
    
  ## 2. Record Invalid postcodes ####
  invalid_postcodes <- ass_tmp%>%
    distinct(riskpostcode)%>%
    filter(!riskpostcode %in% au_riskpostcode$postcode)%>%
    pull(riskpostcode)
  
  n_invalid_rows <- ass_tmp%>%
    filter(!riskpostcode %in% au_riskpostcode$postcode)%>%
    nrow()
  
  ## 3. Which postcodes are not joining? ####
  # Check row removals:
  
  # Where au_riskpostcode is wrong
  anti_postcodes <- au_riskpostcode%>%
    anti_join(ass_tmp, by = c("postcode" = "riskpostcode", "state"))
  
  anti_postcodes_vec<-ass_tmp%>%
    distinct(riskpostcode, state)%>%
    filter(riskpostcode %in% anti_postcodes$postcode)%>%
    pull(state)%>%
    as.character()
  
  names(anti_postcodes_vec) <- ass_tmp%>%
    distinct(riskpostcode, state)%>%
    filter(riskpostcode %in% anti_postcodes$postcode)%>%
    pull(riskpostcode)
  
  au_riskpostcode<-au_riskpostcode%>%
    mutate(state = if_else(postcode %in% names(anti_postcodes_vec),
                           anti_postcodes_vec[postcode],
                           state))%>%
    distinct(postcode, state, .keep_all = T)
  
  # Where ass_tmp is wrong
  anti_riskpostcodes<-ass_tmp%>%
    anti_join(au_riskpostcode, by = c("riskpostcode" = "postcode", "state"))%>%
    filter(!riskpostcode %in% invalid_postcodes)
  
  anti_riskpostcodes_vec<-au_riskpostcode%>%
    distinct(postcode, state)%>%
    filter(postcode %in% anti_riskpostcodes$riskpostcode)%>%
    pull(state)%>%
    as.character()
  
  names(anti_riskpostcodes_vec) <- au_riskpostcode%>%
    distinct(postcode, state)%>%
    filter(postcode %in% anti_riskpostcodes$riskpostcode)%>%
    pull(postcode)
  
  ass_tmp <- ass_tmp%>%
    mutate(state = as.character(state))%>%
    mutate(state = if_else(riskpostcode %in% names(anti_riskpostcodes_vec),
                           anti_riskpostcodes_vec[as.character(riskpostcode)],
                           state))%>%
    mutate(state = as_factor(state))
  
  stopifnot(!anyNA(ass_tmp$state))
  
  ## 4. Execute Inner Join ####
  ass_joined <- ass_tmp%>%
    inner_join(au_riskpostcode,
               by = c("riskpostcode" = "postcode", "state"))
  
  stopifnot(nrow(ass_tmp) - nrow(ass_joined) == n_invalid_rows)
  
  return(ass_joined)
  
}
