source("00 source.R")
source("01 Data Cleaning/Scripts/02 add_postcode_dets.R")
source("01 Data Cleaning/Scripts/03 ass_refactor.R")

ass_data = read_csv("Assignment Data/Assignment Data.csv")

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

  
  ## Modify postcodes
  mutate(state = if_else(riskpostcode == "004375", "QLD", state),
         riskpostcode = if_else(riskpostcode == "002309", "002083", 
                                riskpostcode))%>%
  
  # Convert characters to factor
  mutate(across(where(is.character), as_factor))%>%
  
  # Join Postcode data
  add_postcode_dets()

# Outliers
ass_outlier_prop <- ass_copy%>%
  filter(occupation %in% c("O_Other Community Services",
                           "D- Electricity, Gas & Water"))%>%
  filter(grossincurred_prop >0)%>%
  arrange(desc(grossincurred_prop))

view(ass_outlier_prop)

ass_outlier_lossofinc <- ass_copy%>%
  arrange(desc(grossincurred_lossofinc))%>%
  slice(1:20)

view(ass_outlier_lossofinc)

ass_copy%>%
  arrange(desc(grossincurred_prop))%>%
  slice(1:30)%>%view()

## 2. Simplify Factors based on Exposure ####
ass_encoding <- ass_refactor(ass_copy, 20)
save(ass_encoding, file = "00 envr/Cleaning/ass_encoding.Rda")

ass_rfct <- ass_copy%>%
  mutate(across(c(construction_walls, occupation), as.character))%>%
  mutate(
    construction_walls = if_else(
      construction_walls %in% names(ass_encoding$construction_wall_code),
      ass_encoding$construction_wall_code[construction_walls],
      construction_walls),
    occupation_risk = ass_encoding$occupation_code[occupation]
  )%>%
  mutate(across(c(construction_walls, occupation_risk), as_factor))%>%
  inner_join(ass_encoding$geo_code%>%
               distinct(state, riskpostcode, geo_code),
             by = c("riskpostcode", "state"))%>%
  select(-occupation, -sa3name, -sa4name, -electoraterating)

stopifnot(!anyNA(ass_rfct$occupation_risk))
stopifnot(!anyNA(ass_rfct$geo_code))
stopifnot(!anyNA(
  ass_rfct$construction_walls[!is.na(ass_copy$construction_walls)]))

rm(ass_data, get_au_riskpostcode, add_postcode_dets, ass_refactor)
