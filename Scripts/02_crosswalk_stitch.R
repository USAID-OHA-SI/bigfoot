## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  stitch together crosswalk data
## DETAIL :  append 'crosswalk' files downloaded from https://drive.google.com/drive/u/0/folders/10CIWaSqinny3RjCfsFfMPsf_Xl7GIAUj
## update:  5.28, adjust for new file structure from PSM
##          7.6 update for new file structure from PSM



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
stitch_xwalk <- function(file){
  
  df <- read_xlsx(file,
                  sheet = "Mapped Facilities v2",
                  col_types = c("text")) %>%
    mutate(ou = basename(file)) %>% 
    rename_all(~tolower(.))
  
    return(df)
}

#create df
df_cross <- map_dfr(.x = files,
                    .f = ~ stitch_xwalk(.x))

 
## clean up, fix ou

xwalk <- df_cross %>% 
  mutate(ou = str_remove(ou, " LMIS MER Mapping v2.xlsx")) %>% 
  select(-`...7`) %>%
  rename(threshold = `similarity threshold`) %>%
  mutate(across((c("similarity", "threshold")), as.numeric(vars)))

  group_by(ou) %>% 
  fill(threshold, .direction = "updown") %>% 
  ungroup()

## examine

# how many obs have a 0 value for similarity, ie they didn't match
xwalk %>% 
  group_by(ou) %>% 
  tally(similarity == "0")

#how many have a non-missing orgunituid, meaning they did match
xwalk %>% 
  group_by(ou) %>% 
  tally(!is.na(orgunituid))



#write
df_cross %>%
  write_csv(file.path(data_out, "lmis_mer_crosswalk_all.csv"))

##


## scratch
## function with `fill` for threshold inputation
stitch_xwalk <- function(file){
  
  df <- read_xlsx(file,
                  sheet = "Mapped Facilities v2",
                  skip = 1,
                  col_types = c("text")) %>%
    mutate(ou = basename(file)) %>% 
    rename_all(~tolower(.)) %>% 
    fill(`similarity threshold`, .direction = "updown")
  
  return(df)
}










