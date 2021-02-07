#Globals----------------------------------------------------------
data_in <- "Data"
data_out <- "Dataout"
images <- "Images"

data_in <- "C:/Users/cadelson/documents/GitHub/bigfoot/data"
data_out <- "C:/Users/cadelson/documents/GitHub/bigfoot/Dataout"


#read in data----------------------------------------------------
# read in data SC_FACT data

PPMR <- file.path(data_in, "PPMR-HIV_Masterfile_Feb-July 2020 for USAID.csv") %>% 
  vroom() %>% 
  rename_all(~tolower(.))

