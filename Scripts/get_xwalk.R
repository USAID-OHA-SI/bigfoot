## PROJECT:  Bigfoot
## AUTHOR:   j.davis | USAID
## LICENSE:  MIT
## PURPOSE:  Read crosswalk file from gdrive, munge, and read into mem as df
## Date:     2021-01-23

## read crosswalk from google drive

get_xwalk <- function(){

file <- googledrive::drive_ls(googledrive::as_id("1akQsYUCMYORFlmt--nWrx850Gw8l39sk"))

filename <- file %>%
  dplyr::filter(stringr::str_detect(name, pattern = ".xlsx")) %>%
  dplyr::pull(name)


glamr::import_drivefile(drive_folder = "1akQsYUCMYORFlmt--nWrx850Gw8l39sk",
                        filename = filename,
                        folderpath = "Data",
                        zip = FALSE)

xwalk <- readxl::read_xlsx(file.path(Data, filename)) %>% 
  rename_all(~tolower(.)) %>% 
  rename(orgunituid = datim_orgunituid,
         site_name = datim_facility)
  

}


