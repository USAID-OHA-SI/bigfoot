## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  stitch together crosswalk data
## DETAIL :  append 'crosswalk' files downloaded from https://drive.google.com/drive/u/0/folders/10CIWaSqinny3RjCfsFfMPsf_Xl7GIAUj
## update:  5.28, adjust for new file structure from PSM

#Dependancies-----------------------------------------------------
library(tidyverse)
library(vroom)
library(readxl)
library(fs)


#Globals----------------------------------------------------------
data_in <- "data"
data_out <- "C:/Users/Josh/Documents/data/fy20_q2_v1/psm/data"
images <- "Images"

prinf <- function(df) {
  print(df, n = Inf)
}

#read in and munge-----------------------------------------------

#list files
files <- list.files(data_in, ".xlsx", full.names = TRUE)

#delete

#function to stitch together and fix
foo <- function(file){
  
  df <- read_xlsx(file,
                  sheet = "Mapped Facilities",
                  skip = 1,
                  col_types = c("text")) %>%
    mutate(ou = basename(file)) %>% 
    rename_all(~tolower(.)) %>% 
    fill(`similarity threshold`, .direction = "updown")
  
    return(df)
}

#create df
df_cross <- map_dfr(.x = files,
                    .f = ~ foo(.x))

 
## clean up, fix ou

df_cross <- df_cross %>% 
  mutate(ou = str_remove(ou, " LMIS MER Mapping.xlsx")) %>% 
  select(-`...6`, -`...7`, -`...8`, -`...9`) %>% 
  rename(threshold = `similarity threshold`)

#write
df_cross %>%
  write_csv(file.path(data_out, "lmis_mer_crosswalk_all.csv"))
