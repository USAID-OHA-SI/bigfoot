## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  stitch together crosswalk data
## DETAIL :  append 'crosswalk' files downloaded from https://drive.google.com/drive/u/0/folders/10CIWaSqinny3RjCfsFfMPsf_Xl7GIAUj
##  on 4.30.20

#Dependancies-----------------------------------------------------
library(tidyverse)
library(vroom)
library(readxl)
library(fs)


#Globals----------------------------------------------------------
data_in <- "data"
data_out <- "C:/Users/Josh/Documents/data/fy20_q1_v2/scm/crosswalk"
images <- "Images"

prinf <- function(df) {
  print(df, n = Inf)
}

#read in and munge-----------------------------------------------

files <- list.files(data_in, ".xlsx", full.names = TRUE)

##delete me
file <- file.path(data_in, "Angola LMIS-DATIM.xlsx")

df_cross <- map_dfr(.x = files,
                    .f = ~ foo(.x))

foo <- function(file){
  
  filename <- paste(file)
  
  df <- read_xlsx(file, col_types = c("text")) %>%
    mutate(ou = filename)
  
    return(df)
}
 
## fix ou

df_cross <- df_cross %>% 
  mutate(ou = str_remove(ou, "data/")) %>% 
  mutate(ou = str_remove(ou, " LMIS-DATIM.xlsx"))

## examine
glimpse(df_cross)

df_cross <- df_cross %>% 
  filter_at(vars(starts_with("level")), any_vars(!is.na(.)))

#look at uids
df_cross %>% 
  group_by(ou) %>% 
  summarise(unqiue_uid = n_distinct(uid),
            unqiue_cd = n_distinct(FacilityCD),
            unique_name = n_distinct(Facility))









