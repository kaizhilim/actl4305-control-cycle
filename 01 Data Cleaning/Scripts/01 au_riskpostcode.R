get_au_riskpostcode <-function(ass_tmp){
  
  au_postcode <- read_csv(file = "00 envr/australian_postcodes.csv",
                          show_col_types = F)%>%
    mutate(postcode = str_pad(postcode, 6, "left", "0"))
  
  ## Standardise postcode details
  get_freq_obv <- function(data, ...) {
    data%>%
      count(...)%>%
      mutate(row_count = n())%>%
      mutate(max_n = max(n))
  }
  
  check_au_postcode_na<-au_postcode%>%
    filter(postcode %in% ass_tmp$riskpostcode)%>%
    group_by(postcode, state)%>%
    summarise(sa3_na = any(!is.na(sa3name)),
              sa4_na = any(!is.na(sa4name)),
              elec_na = any(!is.na(electoraterating)),
              .groups = "keep")
  
  # The postcodes 002309 and 004375 is NA
  # 004375 make into QLD state
  # 002309 change to 2083
  # Thus there are 1615 valid postcode, state combinations
  
  au_post <- au_postcode%>%
    filter(postcode %in% ass_tmp$riskpostcode)%>%
    mutate(state = if_else(postcode == "004375","QLD", state),
           postcode = if_else(postcode == "002309", "002083", postcode))
  
  au_sa3 <- au_post%>%
    distinct(postcode, state, sa3name)%>%
    filter(!is.na(sa3name))
  
  # Only 1 duplicated sa3
  au_sa3<-au_sa3%>%
    filter(!(postcode =="002304" & sa3name!="Shoalhaven"))
  
  ## Statistical district 4
  
  au_sa4 <- au_post%>%
    distinct(postcode, state, sa4name)%>%
    filter(!is.na(sa4name))
  
  au_sa4%>%
    group_by(postcode, state)%>%
    mutate(row_count = n())%>%
    filter(row_count>1)
  
  # Only 1 duplicated sa4
  au_sa4<-au_sa4%>%
    filter(!(postcode =="002304" & sa4name!="Southern Highlands and Shoalhaven"))
  
  # Can combine as non-NA postcodes have only 1 electoraterating
  au_stat_dist <- au_sa3%>%
    inner_join(au_sa4, by = c("postcode","state"))%>%
    left_join(au_post%>%
                distinct(postcode, state, electoraterating)%>%
                filter(!is.na(electoraterating)),
              by = c("postcode","state"))
  
  # Get electoraterating
  sa3_electoraterating<-au_post%>%
    group_by(sa3name)%>%
    get_freq_obv(electoraterating)%>%
    filter(!is.na(electoraterating))%>%
    mutate(max_n = max(n))%>%
    filter(n == max_n)%>%
    distinct(sa3name, electoraterating)%>%
    get_freq_obv(electoraterating)%>%
    filter(row_count == 1)
  
  sa3_elec_vec = c(sa3_electoraterating$electoraterating)
  names(sa3_elec_vec) = sa3_electoraterating$sa3name
  
  sa4_electoraterating<-au_post%>%
    group_by(sa4name)%>%
    get_freq_obv(electoraterating)%>%
    filter(!is.na(electoraterating))%>%
    mutate(max_n = max(n))%>%
    filter(n == max_n)%>%
    distinct(sa4name, electoraterating)%>%
    get_freq_obv(electoraterating)%>%
    filter(row_count == 1)
  
  sa4_elec_vec = c(sa4_electoraterating$electoraterating)
  names(sa4_elec_vec) = sa4_electoraterating$sa4name
  
  # Check inner join
  na_elec<-au_post%>%
    group_by(postcode, state)%>%
    mutate(all_elec_na = any(!is.na(electoraterating)))%>%
    filter(!all_elec_na)
  
  na_elec%>%
    filter(!sa4name %in% sa4_electoraterating$sa4name)
  
  # Final join
  au_riskpostcode<-au_stat_dist%>%
    mutate(electoraterating = if_else(is.na(electoraterating), 
                                      sa3_elec_vec[sa3name],
                                      electoraterating))%>%
    mutate(electoraterating = if_else(
      is.na(electoraterating), 
      sa4_elec_vec[sa4name],
      electoraterating))
  
  return(au_riskpostcode)
}
