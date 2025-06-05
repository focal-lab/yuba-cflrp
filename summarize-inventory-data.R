library(tidyverse)
library(readxl)

source("constants.R")

plots = read_excel(RAW_INEVNTORY_DATA_FILEPATH, sheet = "plot")
trees = read_excel(RAW_INEVNTORY_DATA_FILEPATH, sheet = "tree")
fuels = read_excel(RAW_INEVNTORY_DATA_FILEPATH, sheet = "fuels")
