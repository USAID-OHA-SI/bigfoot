#' sc_fact_df
#'
#' @param  
#'
#' @return
#' @export
#'
#' @examples
sc_fact_df <- function() {
  
  ##read in and munge sc_fact
  
  sc_fact_filename <- glamr::return_latest("Data", "*.csv")
  
  df <- readr::read_csv(sc_fact_filename,
                        col_types = cols(.default = "c"),
                        locale = readr::locale(encoding = "latin1")) %>% 
    dplyr::rename_all(~tolower(.)) %>% 
    dplyr::mutate_at(vars(country, facility), ~tolower(.)) %>% 
    dplyr::mutate_at(vars(soh, ami, mos), ~as.numeric(.))

  ##read in meta

  df_meta <- googlesheets4::read_sheet("1O-rwWWp-8GsbqWhfcr01S9glMazAdEGvdkwhMZZcf0Y",
                                       sheet = "updated_regimen",
                                       col_types= c(.default = "c")) %>%
    dplyr::filter(is.na(`include in analysis?`)) %>% 
    dplyr::select(-`include in analysis?`, -`...11`, -`...12`) %>% 
    dplyr::rename_all(~tolower(.)) %>% 
    dplyr::mutate(mot = as.numeric(mot))

  ##join df + meta
  
  df <- df %>% 
    left_join(df_meta, by = "product")
  
  df <- df %>%
    mutate(mot_ami = ami*mot,
           mot_soh = soh*mot)
  
  #create indicator field, reshape, and select
  df <- df %>% 
    gather(indicator, value, colnames(select_if(., is.numeric)), na.rm = TRUE) %>% 
    select(-facilitycd, -datimcode, -facility_mapped, -source, -`datim facility`)
  
  return(df)
    
}
