# Packages
package_list = c(
  "tidyverse",
  "lubridate",
  "insurancerating",
  "rsample",
  "statmod",
  "naniar"
)

new_package_list = package_list[
  !(package_list %in% installed.packages()[,"Package"])]

if(length(new_package_list)) install.packages(new_package_list)

# Neural Network Packages
load_nn = FALSE
if (load_nn) {
  lapply(setdiff(package_list, "keras"), library, character.only = T)
}

rm(new_package_list)