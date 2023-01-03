##integrated datasets

library("readxl")
library("tidyverse")
library("tidyselect")

Sys.setlocale("LC_ALL", "Polish")
Sys.getlocale()

stores <- readxl::read_excel(path = "C:/Users/Alon/OneDrive/Docs/GitHub/Excel_files/Stores.xlsx")

sales <- read_csv("C:/Users/Alon/OneDrive/Docs/GitHub/Excel_files/Summary of Sales January 2020.csv")

#truncating store names
truncate_sales <- function(df) {
  df$'STORE NAME' <- gsub(' GROSS', '', df$'STORE NAME')
  df$'STORE NAME' <- gsub(' OTHER', '', df$'STORE NAME')
  df$'STORE NAME' <- gsub(' RETAIL', '', df$'STORE NAME')
  df$'STORE NAME' <- gsub("\xd3", 'Ó', df$'STORE NAME')
}

sales$'STORE NAME' <- truncate_sales(sales)

#replacing false names with proper ones from the file

sales[sales=="PARVIFLORA ?OM?A"] <- "Parviflora Łomża"
sales[sales=="PARVIFLORA WROC?AW"] <- "Parviflora Wrocław"
sales[sales=="PARVIFLORA ?\xd3D?"] <- "Parviflora Łódź" #problem with identifying the false name - Ó
sales[sales=="PARVIFLORA POZNA?"] <- "Parviflora Poznań"
sales[sales=="PARVIFLORA KRAK\xd3W"] <- "Parviflora Kraków" #problem with identifying the false name Ó
sales[sales=="PARVIFLORA GDA?SK"] <- "Parviflora Gdańsk"
sales[sales=="PARVIFLORA CHE?M"] <- "Parviflora Chełm"
sales[sales=="PARVIFLORA BIA?YSTOK"] <- "Parviflora Białystok"
sales[sales=="PARVIFLORA SUWA?KI"] <- "Parviflora Suwałki"
sales[sales=="PARVIFLORA TORU?"] <- "Parviflora Toruń"
sales[sales=="PARVIFLORA GORZ\xd3W WLKP."] <- "Parviflora Gorzów Wlkp." #problem with identifying the false name Ó
sales[sales=="PARVIFLORA PRZEMY?L"] <- "Parviflora Przemyśl"
sales[sales=="PARVIFLORA RZESZ\xd3W"] <- "Parviflora Rzeszów"
sales[sales=="PARVIFLORA OSTRO??KA"] <- "Parviflora Ostrołęka"
sales[sales=="PARVIFLORA W?CHOCK"] <- "Parviflora Wąchock"
sales[sales=="PARVIFLORA ?WIEBODZIN"] <- "Parviflora Świebodzin"

sales$'STORE NAME'

