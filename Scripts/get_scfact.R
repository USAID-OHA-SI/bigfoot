#' get_scfact
#'
#' @param path folderpath where you want to save
#'
#' @export
#'
#' @examples
get_scfact <- function(path){
  
  file <- googledrive::drive_ls(googledrive::as_id("1og4f-ZVzIF2H3TjfxfvLlJv1HYRU_0zD"))
  
  filename <- file %>%
    dplyr::filter(stringr::str_detect(name, pattern = ".csv")) %>%
    dplyr::pull(name)
  
  
  glamr::import_drivefile(drive_folder = "1og4f-ZVzIF2H3TjfxfvLlJv1HYRU_0zD",
                          filename = filename,
                          folderpath = "Data",
                          zip = FALSE)
}