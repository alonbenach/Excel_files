##integrated datasets

install.packages("readxl")
library("tidyverse")

Sys.setlocale("LC_ALL", "Polish")

stores <- readxl::read_excel(path = "C:/Users/Alon/OneDrive/Docs/GitHub/Excel_files/Stores.xlsx")

salesJan2020 <- read_csv("C:/Users/Alon/OneDrive/Docs/GitHub/Excel_files/Summary of Sales January 2020.csv")

#replacing false names with proper ones from the file
for (store in salesJan2020$'STORE NAME') {
  salesJan2020[salesJan2020=="PARVIFLORA ?OM?A"] <- "Parviflora Łomża"
  salesJan2020[salesJan2020=="PARVIFLORA WROC?AW"] <- "Parviflora Wrocław"
  salesJan2020[salesJan2020=="PARVIFLORA ?\xd3D?"] <- "Parviflora Łódź" #problem with identifying the false name - Ó
  salesJan2020[salesJan2020=="PARVIFLORA POZNA?"] <- "Parviflora Poznań"
  salesJan2020[salesJan2020=="PARVIFLORA KRAKOW"] <- "Parviflora Kraków" #problem with identifying the false name Ó
  salesJan2020[salesJan2020=="PARVIFLORA GDA?SK"] <- "Parviflora Gdańsk"
  salesJan2020[salesJan2020=="PARVIFLORA CHE?M"] <- "Parviflora Chełm"
  salesJan2020[salesJan2020=="PARVIFLORA BIA?YSTOK"] <- "Parviflora Białystok"
  salesJan2020[salesJan2020=="PARVIFLORA SUWA?KI"] <- "Parviflora Suwałki"
  salesJan2020[salesJan2020=="PARVIFLORA TORU?"] <- "Parviflora Toruń"
  salesJan2020[salesJan2020=="PARVIFLORA GORZOW WLKP."] <- "Parviflora Gorzów Wlkp." #problem with identifying the false name Ó
  salesJan2020[salesJan2020=="PARVIFLORA PRZEMY?L"] <- "Parviflora Przemyśl"
  salesJan2020[salesJan2020=="PARVIFLORA RZESZOW"] <- "Parviflora Rzeszów"
  salesJan2020[salesJan2020=="PARVIFLORA OSTRO??KA"] <- "Parviflora Ostrołęka"
  salesJan2020[salesJan2020=="PARVIFLORA W?CHOCK"] <- "Parviflora Wąchock"
  salesJan2020[salesJan2020=="PARVIFLORA ?WIEBODZIN"] <- "Świebodzin"
}
salesJan2020$'STORE NAME'
