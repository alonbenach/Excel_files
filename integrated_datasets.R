##integrated datasets

library("readxl")
library("tidyverse")
library("tidyselect")

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
for(i in sales$`STORE NAME`){
  if(i == "PARVIFLORA ?OM?A"){
    sales[sales == "PARVIFLORA ?OM?A"] <- "PARVIFLORA ŁOMŻA"
  } else if(i == "PARVIFLORA WROC?AW"){
    sales[sales == "PARVIFLORA WROC?AW"] <- "PARVIFLORA WROCŁAW"
  } else if(i == "PARVIFLORA ?ÓD?"){
    sales[sales =="PARVIFLORA ?ÓD?"] <- "PARVIFLORA ŁÓDŹ"
  } else if(i == "PARVIFLORA POZNA?"){
    sales[sales=="PARVIFLORA POZNA?"] <- "PARVIFLORA POZNAŃ"
  } else if(i == "PARVIFLORA GDA?SK"){
    sales[sales == "PARVIFLORA GDA?SK"] <- "PARVIFLORA GDAŃSK"
  } else if(i == "PARVIFLORA CHE?M"){
    sales[sales == "PARVIFLORA CHE?M"] <- "PARVIFLORA CHEŁM"
  } else if(i == "PARVIFLORA BIA?YSTOK"){
    sales[sales == "PARVIFLORA BIA?YSTOK"] <- "PARVIFLORA BIAŁYSTOK"
  } else if(i == "PARVIFLORA SUWA?KI"){
    sales[sales == "PARVIFLORA SUWA?KI"] <- "PARVIFLORA SUWAŁKI"
  } else if(i == "PARVIFLORA TORU?"){
    sales[sales == "PARVIFLORA TORU?"] <- "PARVIFLORA TORUŃ"
  } else if(i == "PARVIFLORA PRZEMY?L"){
    sales[sales == "PARVIFLORA PRZEMY?L"] <- "PARVIFLORA PRZEMŚL"
  } else if(i == "PARVIFLORA OSTRO??KA"){
    sales[sales == "PARVIFLORA OSTRO??KA"] <- "PARVIFLORA OSTROŁĘKA"
  } else if(i == "PARVIFLORA W?CHOCK"){
    sales[sales == "PARVIFLORA W?CHOCK"] <- "PARVIFLORA WĄCHOCK"
  } else if(i == "PARVIFLORA ?WIEBODZIN"){
    sales[sales == "PARVIFLORA ?WIEBODZIN"] <- "PARVIFLORA ŚWIEBODZIN"
  }
}


sales$'STORE NAME'

