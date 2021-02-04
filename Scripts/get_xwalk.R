#' Download and munge crosswalk file from PSM
#'
#' @param path path where you want file stored, default = "Data/xwalk"

#'
#' @examples


get_xwalk <- function(path = crosswalk){

file <- googledrive::drive_ls(googledrive::as_id("1akQsYUCMYORFlmt--nWrx850Gw8l39sk"))

filename <- file %>%
  dplyr::filter(stringr::str_detect(name, pattern = ".csv")) %>%
  dplyr::pull(name)


glamr::import_drivefile(drive_folder = googledrive::as_id("1akQsYUCMYORFlmt--nWrx850Gw8l39sk"),
                        filename = filename,
                        folderpath = path,
                        zip = FALSE)

xwalk <- readr::read_csv(file.path(path, filename)) %>% 
  rename_all(~tolower(.)) %>% 
  rename(orgunituid = datim_orgunituid,
         site_name = datim_facility) %>% 
  tidyr::unite(join_var, lmis_snl1, lmis_snl2, lmis_facility, sep = "_", na.rm = TRUE, remove = FALSE)

xwalk <- xwalk %>% 
  dplyr::select(orgunituid, lmis_facility, join_var)

return(xwalk)
  

}


