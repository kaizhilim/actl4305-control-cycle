source("00 source.R")

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
  
  # Convert characters to factor
  mutate(across(where(is.character), as_factor))%>%
  
  # Remove policies with no property cover
  filter(property_cover)%>%
  select(-property_cover)

## 2. NA Cleaning ####

# KELLY WRITE CODE HERE

# Save as R File
save(ass_copy, file = "00 envr/Compulsory/ass_copy.R")
