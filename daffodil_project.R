#reading an infinite number of tabs into a list of DFs
# load library
install.packages("gdata", dependencies = TRUE)
install.packages("openxlsx", dependencies = TRUE)
library("openxlsx")
library("gdata")
library("readxl")
library("readxl")
library("tidyverse")
library("tidyselect")

#myDir <- getwd()
mylist <- lapply(1:gdata::sheetCount('Daffodils2020.xls'), function(i) read_excel('Daffodils2020.xls', sheet = i))
names(mylist) <- paste0(excel_sheets(path = 'Daffodils2020.xls'))

#reading the stores file
stores1<- read_excel("Stores.xlsx")

for (i in names(mylist)){
  if (startsWith(i, 'Jan')){
    daff_sales_jan <- mylist[[i]]
  } else if (startsWith(i, 'Feb')){
    daff_sales_feb <- mylist[[i]]
  } else if (startsWith(i, 'Mar')){
    daff_sales_mar <- mylist[[i]]
  } else if (startsWith(i, 'Apr')){
    daff_sales_apr <- mylist[[i]]
  } else if (startsWith(i, 'May')){
    daff_sales_may <- mylist[[i]]
  } else if (startsWith(i, 'Jun')){
    daff_sales_jun <- mylist[[i]]
  } else if (startsWith(i, 'Jul')){
    daff_sales_jul <- mylist[[i]]
  } else if (startsWith(i, 'Aug')){
    daff_sales_aug <- mylist[[i]]
  } else if (startsWith(i, 'Sep')){
    daff_sales_sep <- mylist[[i]]
  } else if (startsWith(i, 'Oct')){
    daff_sales_oct <- mylist[[i]]
  } else if (startsWith(i, 'Nov')){
    daff_sales_nov <- mylist[[i]]
  } else if (startsWith(i, 'Dec')){
    daff_sales_dec <- mylist[[i]]
  }
}

#setting column names for each daffodils file
colnames(daff_sales_jan)[1]  <- "col1"
colnames(daff_sales_jan)[2]  <- "col2"
colnames(daff_sales_jan)[3]  <- "col3"

#creating a tibble of location ID extensions from location IDs
daff_sales_jan_loc <- daff_sales_jan %>% select("col1","col2","col3") %>% 
  filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
#creating a tibble of totals from sales
daff_sales_jan_totals <- daff_sales_jan %>% select("col1","col2","col3") %>% 
  filter(col1 == "Totals") %>% select(-col1) %>% 
  rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% 
  mutate(across(DAFFODIL, as.numeric))
#binding the loc_ID with the Totals
daff_jan <- cbind(daff_sales_jan_loc, daff_sales_jan_totals)

#repeating the process for February
colnames(daff_sales_feb)[1]  <- "col1"
colnames(daff_sales_feb)[2]  <- "col2"
colnames(daff_sales_feb)[3]  <- "col3"
daff_sales_feb_loc <- daff_sales_feb %>% select("col1","col2","col3") %>% 
  filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
daff_sales_feb_totals <- daff_sales_feb %>% select("col1","col2","col3") %>% 
  filter(col1 == "Totals") %>% select(-col1) %>% 
  rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>%
  mutate(across(DAFFODIL, as.numeric))

daff_feb <- cbind(daff_sales_feb_loc, daff_sales_feb_totals)

#repeating the process for March
colnames(daff_sales_mar)[1]  <- "col1"
colnames(daff_sales_mar)[2]  <- "col2"
colnames(daff_sales_mar)[3]  <- "col3"
daff_sales_mar_loc <- daff_sales_mar %>% select("col1","col2","col3") %>% 
  filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
daff_sales_mar_totals <- daff_sales_mar %>% select("col1","col2","col3") %>% 
  filter(col1 == "Totals") %>% select(-col1) %>% 
  rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% 
  mutate(across(DAFFODIL, as.numeric))

daff_mar <- cbind(daff_sales_mar_loc, daff_sales_mar_totals)

#preparing stores1 for binding
stores <- stores1 %>% rename("loc_ID" = "Store ID","store_name" = "Store Name") %>% 
  mutate(across(loc_ID, as.character)) %>% 
  mutate(store_name = str_replace(store_name,"Swiebodzin","Parviflora Swiebodzin"))

#binding store names to each location
jan_daff_sales <- daff_jan %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
feb_daff_sales <- daff_feb %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
mar_daff_sales <- daff_mar %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')

#Alon's code, relates only to January for now
sales <- read_csv("Summary of Sales January 2020.csv", col_names = TRUE)
colnames(sales) <- (c('STORE NAME', 'STORE #', 'COUNT AZALEA', 'AZALEA', 'COUNT BEGONIA', 'BEGONIA', 'COUNT CARNATION', 'CARNATION', 'COUNT DAFFODILS', 'DAFFODILS', 'COUNT_TOTAL', 'TOTAL'))

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
    sales[sales == "PARVIFLORA ?OM?A"] <- "PARVIFLORA LOMZA"
  } else if(i == "PARVIFLORA WROC?AW"){
    sales[sales == "PARVIFLORA WROC?AW"] <- "PARVIFLORA WROCLAW"
  } else if(i == "PARVIFLORA ?ÓD?"){
    sales[sales =="PARVIFLORA ?ÓD?"] <- "PARVIFLORA LÓDZ"
  } else if(i == "PARVIFLORA POZNA?"){
    sales[sales=="PARVIFLORA POZNA?"] <- "PARVIFLORA POZNAN"
  } else if(i == "PARVIFLORA GDA?SK"){
    sales[sales == "PARVIFLORA GDA?SK"] <- "PARVIFLORA GDANSK"
  } else if(i == "PARVIFLORA CHE?M"){
    sales[sales == "PARVIFLORA CHE?M"] <- "PARVIFLORA CHELM"
  } else if(i == "PARVIFLORA BIA?YSTOK"){
    sales[sales == "PARVIFLORA BIA?YSTOK"] <- "PARVIFLORA BIALYSTOK"
  } else if(i == "PARVIFLORA SUWA?KI"){
    sales[sales == "PARVIFLORA SUWA?KI"] <- "PARVIFLORA SUWALKI"
  } else if(i == "PARVIFLORA TORU?"){
    sales[sales == "PARVIFLORA TORU?"] <- "PARVIFLORA TORUN"
  } else if(i == "PARVIFLORA PRZEMY?L"){
    sales[sales == "PARVIFLORA PRZEMY?L"] <- "PARVIFLORA PRZEMSL"
  } else if(i == "PARVIFLORA OSTRO??KA"){
    sales[sales == "PARVIFLORA OSTRO??KA"] <- "PARVIFLORA OSTROLEKA"
  } else if(i == "PARVIFLORA W?CHOCK"){
    sales[sales == "PARVIFLORA W?CHOCK"] <- "PARVIFLORA WACHOCK"
  } else if(i == "PARVIFLORA ?WIEBODZIN"){
    sales[sales == "PARVIFLORA ?WIEBODZIN"] <- "PARVIFLORA SWIEBODZIN"
  }
}
#binding
sales1 <- sales %>% select('store' = `STORE NAME`,3:8) %>% group_by(`store`) %>% 
  summarize(across(everything(), sum)) %>% left_join(jan_daff_sales, by = 'store')

sales1 <- sales1 %>%
  rowwise() %>% 
  mutate(count_total = sum(c_across(starts_with("COUNT")), na.rm = T)) %>% 
  ungroup()

sales1 <- sales1 %>%
  rowwise() %>% 
  mutate(sum_total = sum(c_across(-starts_with(c("COUNT", 'store', 'sum'))), na.rm = T)) %>% 
  ungroup()


