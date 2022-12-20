##integrated datasets

install.packages("readxl")
library(tidyverse)
library(lubridate)
Sys.setlocale("LC_ALL", "Polish")

readxl::read_excel(path = "C:/Users/Alon/OneDrive/Docs/GitHub/Excel_files/Stores.xlsx")

Stores
read_csv("C:/Users/Alon/OneDrive/Docs/GitHub/Excel_files/Summary of Sales January 2020.csv")
