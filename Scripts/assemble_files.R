##scratch for merging files together
library(here)
library(tidyverse)
library(readr)


### 5.2.21 update
## Per 7 emails Mary sent on Apr 30
## 5.2.21 submissin updated with dec,jan,feb new file

files <- list.files("Data/sc_fact/raw", pattern = ".csv", full.names = TRUE)

df <- purrr::map_dfr(.x = files,
                     .f = ~ readr::read_csv(.x,
                                            col_types = c(.default = "c")))


df %>% readr::write_csv(., paste0(Dataout, "/sc_fact_raw_",
                                  format(Sys.Date(),"%Y%m%d"), ".csv"))

# this is to upload, pause to re-consider given the long time to upload
drive_upload(media = "Dataout/sc_fact_raw_20210907.csv",
             path = as_id("1og4f-ZVzIF2H3TjfxfvLlJv1HYRU_0zD"),
             type = "csv",
             overwrite = TRUE)


