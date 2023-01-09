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

#reading the daffodils sale summary file with all sheets
mylist <- lapply(1:gdata::sheetCount('Daffodils2020.xls'), function(i) read_excel('Daffodils2020.xls', sheet = i))
names(mylist) <- paste0(excel_sheets(path = 'Daffodils2020.xls'))

#reading the stores file
stores1<- read_excel("Stores.xlsx")
#cleaning stores1
stores <- stores1 %>% rename("loc_ID" = "Store ID","store_name" = "Store Name") %>% 
  mutate(across(loc_ID, as.character)) %>% 
  mutate(store_name = str_replace(store_name,"Swiebodzin","Parviflora Swiebodzin"))

#setting column names for each daffodils file
colnames <- c("col1", "col2", "col3", "col4", "col5")

for (i in seq_along(mylist)){
  colnames(mylist[[i]]) <- colnames
}

#1. creating a tibble of location ID extensions from location IDs
#2. creating a tibble of totals from sales
#3. binding the loc_ID with the Totals
#4. #binding store names to each location


daffList <- list()
for (i in seq_along(mylist)){
  if (names(mylist[i]) == "Jan20"){
    daff_sales_jan_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_jan_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_jan <- cbind(daff_sales_jan_loc, daff_sales_jan_totals)
    jan_daff_sales <- daff_jan %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- jan_daff_sales
    names(daffList)[i] <- 'jan_daff_sales'
  } else if (names(mylist[i]) == "Feb20"){
    daff_sales_feb_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_feb_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_feb <- cbind(daff_sales_feb_loc, daff_sales_feb_totals)
    feb_daff_sales <- daff_feb %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- feb_daff_sales
    names(daffList)[[length(daffList)]] <- 'feb_daff_sales'
  } else if (names(mylist[i]) == "Mar20"){
    daff_sales_mar_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_mar_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_mar <- cbind(daff_sales_mar_loc, daff_sales_mar_totals)
    mar_daff_sales <- daff_mar %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- mar_daff_sales
    names(daffList)[[length(daffList)]] <- 'mar_daff_sales'
  } else if (names(mylist[i]) == "Apr20"){
    daff_sales_apr_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_apr_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_apr <- cbind(daff_sales_apr_loc, daff_sales_apr_totals)
    apr_daff_sales <- daff_apr %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- apr_daff_sales
    names(daffList)[[length(daffList)]] <- 'apr_daff_sales'
  } else if (names(mylist[i]) == "May20"){
    daff_sales_may_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_may_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_may <- cbind(daff_sales_may_loc, daff_sales_may_totals)
    mar_daff_sales <- daff_mar %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- may_daff_sales
    names(daffList)[[length(daffList)]] <- 'may_daff_sales'
  } else if (names(mylist[i]) == "Jun20"){
    daff_sales_jun_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_jun_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_jun <- cbind(daff_sales_jun_loc, daff_sales_jun_totals)
    jun_daff_sales <- daff_jun %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- jun_daff_sales
    names(daffList)[[length(daffList)]] <- 'jun_daff_sales'
  } else if (names(mylist[i]) == "Jul20"){
    daff_sales_jul_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_jul_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_jul <- cbind(daff_sales_jul_loc, daff_sales_jul_totals)
    jul_daff_sales <- daff_jul %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- jul_daff_sales
    names(daffList)[[length(daffList)]] <- 'jul_daff_sales'
  } else if (names(mylist[i]) == "Aug20"){
    daff_sales_aug_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_aug_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_aug <- cbind(daff_sales_aug_loc, daff_sales_aug_totals)
    aug_daff_sales <- daff_aug %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- aug_daff_sales
    names(daffList)[[length(daffList)]] <- 'aug_daff_sales'
  } else if (names(mylist[i]) == "Sep20"){
    daff_sales_sep_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_sep_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_sep <- cbind(daff_sales_sep_loc, daff_sales_sep_totals)
    sep_daff_sales <- daff_sep %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- sep_daff_sales
    names(daffList)[[length(daffList)]] <- 'sep_daff_sales'
  } else if (names(mylist[i]) == "Oct20"){
    daff_sales_oct_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_oct_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_oct <- cbind(daff_sales_oct_loc, daff_sales_oct_totals)
    oct_daff_sales <- daff_oct %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- oct_daff_sales
    names(daffList)[[length(daffList)]] <- 'oct_daff_sales'
  } else if (names(mylist[i]) == "Nov20"){
    daff_sales_nov_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_nov_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_nov <- cbind(daff_sales_nov_loc, daff_sales_nov_totals)
    nov_daff_sales <- daff_nov %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- nov_daff_sales
    names(daffList)[[length(daffList)]] <- 'nov_daff_sales'
  } else if (names(mylist[i]) == "Dec20"){
    daff_sales_dec_loc <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Submitting Location:") %>% select(col3) %>% rename("loc_ID" = "col3")
    daff_sales_dec_totals <- mylist[[i]] %>% select("col1","col2","col3") %>% filter(col1 == "Totals") %>% select(-col1) %>% rename("DAFFODIL" = "col2","COUNT" = "col3") %>% mutate(across(COUNT, as.numeric)) %>% mutate(across(DAFFODIL, as.numeric))
    daff_dec <- cbind(daff_sales_dec_loc, daff_sales_dec_totals)
    dec_daff_sales <- daff_dec %>% left_join(stores, by= "loc_ID") %>% mutate(store=toupper(`store_name`)) %>% select(-loc_ID, -store_name) %>% select('store', 'COUNT DAFFODIL' = 'COUNT', 'DAFFODIL')
    daffList[[length(daffList)+1]] <- dec_daff_sales
    names(daffList)[[length(daffList)]] <- 'dec_daff_sales'
  }
}
#reading all sales summary files regardless of names and dates
lsummary_by_month <- lapply(list.files(path=".", pattern="csv", all.files=TRUE,full.names=TRUE), read.csv)
names(lsummary_by_month) <- paste0(list.files(path=".", pattern="csv", all.files=TRUE,full.names=FALSE))

colnames_lsummary <- c('STORE NAME', 'STORE #', 'COUNT AZALEA', 'AZALEA', 'COUNT BEGONIA', 'BEGONIA', 'COUNT CARNATION', 'CARNATION', 'COUNT DAFFODILS', 'DAFFODILS', 'COUNT_TOTAL', 'TOTAL')

for (i in seq_along(lsummary_by_month)){
  colnames(lsummary_by_month[[i]]) <- colnames_lsummary
}

#truncating store names
truncate_sales <- function(df) {
  df$'STORE NAME' <- gsub(' GROSS', '', df$'STORE NAME')
  df$'STORE NAME' <- gsub(' OTHER', '', df$'STORE NAME')
  df$'STORE NAME' <- gsub(' RETAIL', '', df$'STORE NAME')
  df$'STORE NAME' <- gsub("\xd3", 'Ó', df$'STORE NAME')
}

for (i in seq_along(lsummary_by_month)){
  lsummary_by_month[[i]]$'STORE NAME' <- truncate_sales(lsummary_by_month[[i]])
}

#fixing false letters in store names
for (df in seq_along(lsummary_by_month)){
  for(i in lsummary_by_month[[df]]$'STORE NAME'){
    if(i == "PARVIFLORA ?OM?A"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] == "PARVIFLORA ?OM?A"] <- "PARVIFLORA LOMZA"
    } else if(i == "PARVIFLORA WROC?AW"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] == "PARVIFLORA WROC?AW"] <- "PARVIFLORA WROCLAW"
    } else if(i == "PARVIFLORA ?ÓD?"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] =="PARVIFLORA ?ÓD?"] <- "PARVIFLORA LÓDZ"
    } else if(i == "PARVIFLORA POZNA?"){
      lsummary_by_month[[df]][lsummary_by_month[[df]]=="PARVIFLORA POZNA?"] <- "PARVIFLORA POZNAN"
    } else if(i == "PARVIFLORA GDA?SK"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] == "PARVIFLORA GDA?SK"] <- "PARVIFLORA GDANSK"
    } else if(i == "PARVIFLORA CHE?M"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] == "PARVIFLORA CHE?M"] <- "PARVIFLORA CHELM"
    } else if(i == "PARVIFLORA BIA?YSTOK"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] == "PARVIFLORA BIA?YSTOK"] <- "PARVIFLORA BIALYSTOK"
    } else if(i == "PARVIFLORA SUWA?KI"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] == "PARVIFLORA SUWA?KI"] <- "PARVIFLORA SUWALKI"
    } else if(i == "PARVIFLORA TORU?"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] == "PARVIFLORA TORU?"] <- "PARVIFLORA TORUN"
    } else if(i == "PARVIFLORA PRZEMY?L"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] == "PARVIFLORA PRZEMY?L"] <- "PARVIFLORA PRZEMSL"
    } else if(i == "PARVIFLORA OSTRO??KA"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] == "PARVIFLORA OSTRO??KA"] <- "PARVIFLORA OSTROLEKA"
    } else if(i == "PARVIFLORA W?CHOCK"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] == "PARVIFLORA W?CHOCK"] <- "PARVIFLORA WACHOCK"
    } else if(i == "PARVIFLORA ?WIEBODZIN"){
      lsummary_by_month[[df]][lsummary_by_month[[df]] == "PARVIFLORA ?WIEBODZIN"] <- "PARVIFLORA SWIEBODZIN"
    }
  }
}

#binding the two lists together

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


