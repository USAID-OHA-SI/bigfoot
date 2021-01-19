#' get_scfact
#'
#' @param filename filename of most current SC_FACT dataset
#' https://drive.google.com/drive/u/1/folders/1og4f-ZVzIF2H3TjfxfvLlJv1HYRU_0zD
#' @param path folderpath where you want to save
#'
#' @export
#'
#' @examples
get_scfact <- function(filename, path){
  
glamr::import_drivefile(drive_folder = "1og4f-ZVzIF2H3TjfxfvLlJv1HYRU_0zD",
                        filename = filename,
                        folderpath = "Data",
                        zip = FALSE)
}