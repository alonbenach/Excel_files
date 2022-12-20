##integrated datasets

library(tidyverse)
library(lubridate)
install.packages("readxl")

Sys.setlocale("LC_ALL", "Polish")

Stores <- read_excel("Stores.xlsx")
Stores
