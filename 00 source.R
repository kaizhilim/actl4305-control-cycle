# Packages

# to use missCompare
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("pcaMethods")

package_list = c(
  "tidyverse",
  "lubridate",
  "formattable",
  "naniar"
)

new_package_list = package_list[
  !(package_list %in% installed.packages()[,"Package"])]

if(length(new_package_list)) install.packages(new_package_list)

lapply(package_list, library, character.only = T)