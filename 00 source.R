# Packages
package_list = c(
  "tidyverse",
  "lubridate",
  "insurancerating",
  "rsample",
  "statmod",
  "keras"
)

new_package_list = package_list[
  !(package_list %in% installed.packages()[,"Package"])]

if(length(new_package_list)) install.packages(new_package_list)

lapply(package_list, library, character.only = T)

rm(new_package_list)