generate_geo_encoding <- function(ass_clean_na, ...) {
  ## 0. Set up Input ####
  threshold_default = list(
    'riskpostcode' = 50,
    'sa3name' = 100,
    'sa4name' = 200
  )
  
  threshold_input = list(...)
  
  stopifnot(all(names(threshold_input) %in% names(threshold_default)))
  threshold_exposure = threshold_default%>%
    imap_dbl(~if_else(is_null(threshold_input[[.y]]),
                      .x, threshold_input[[.y]]))
  
  # Scope
  scope_options = c('state', rev(names(threshold_exposure)))
  scope_options = set_names(1:length(scope_options), scope_options)
  
  reduce_scope <- function(data, scope, cur_threshold) {
    grp_scope = names(scope_options[scope_options < scope_options[scope]])
    grp_scope = c(grp_scope, "geo_code")
    
    lower_scope = last(grp_scope)
    upper_scope = grp_scope[-length(grp_scope)]
    
    lower_scope_exposure <- data%>%
      group_by(across(all_of(grp_scope)))%>%
      summarise(exposure = sum(exposure),
                .groups = "drop")
    
    affected_upper_scope <- lower_scope_exposure%>%
      filter(exposure < cur_threshold)
    
    if (last(upper_scope) != 'state') {
      
      # Aggregate all those within the upper scope
      affected_lower_scope <- lower_scope_exposure%>%
        inner_join(affected_upper_scope%>%
                     distinct(across(all_of(upper_scope))),
                   by = upper_scope)%>%
        pull(lower_scope)
      
    } else {
      affected_lower_scope = affected_upper_scope%>%
        distinct(across(all_of(lower_scope)))%>%
        pull(lower_scope)
    }
    
    data <- data%>%
      mutate(geo_code = if_else(
        geo_code %in% affected_lower_scope,
        !!sym(last(upper_scope)),
        geo_code))
    
    return(data)
  }
  
  # 1. Reduce the scope progressively ####
  ass_output<-threshold_exposure%>%
    reduce2(
      names(threshold_exposure),
      function(out, input, arg) {
        reduce_scope(out, scope = arg, cur_threshold = input)
      }, 
      .init = ass_clean_na%>%
        mutate(geo_code = riskpostcode))
  
  ## 2. Combine ACT, NT ####
  geo_code_encoding <- ass_output%>%
    mutate(geo_code = if_else(state %in% c("ACT", "NT"),
                              state, geo_code))%>%
    distinct(riskpostcode, geo_code)%>%
    deframe()
  
  return(geo_code_encoding)
}


