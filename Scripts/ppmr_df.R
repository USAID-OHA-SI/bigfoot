#' ppmr_df
#'
#' @param filepath 
#'
#' @return
#' @export
#'
#' @examples
ppmr_df <- function(filepath = ppmr){
  
  ppmr_filename <- glamr::return_latest(filepath, ".csv")
  
  df <- read_csv(ppmr_filename)
 
  #munge, fix period to match sc_fact 
  df <- df %>% 
    janitor::clean_names() %>%
    dplyr::select(-x1, -notes) %>%
    dplyr::rename(product = standardized_product) %>% 
    dplyr::mutate(country = stringr::str_to_sentence(country),
                  period = str_sub(period, 1, 7))
    
  #bring in meta
  
  df_meta <- googlesheets4::read_sheet("1UJv_LAzcD-lkteET9wGPGmdeFn07WnFf7g8sjs-upgk",
                                       sheet = "regimen",
                                       col_types= c(.default = "c")) %>%
    janitor::clean_names() %>%
    dplyr::mutate(mot = as.numeric(mot))
  
  #join on meta
  df <- df %>%
    left_join(df_meta) %>% 
    mutate(mot_ami = round(h_ami_amc*mot,0),
           mot_soh = round(soh*mot,0)) %>% 
    pivot_longer(cols = c("soh", "h_ami_amc", "lmi", "mot", "mot_ami", "mot_soh"),
                 names_to = "indicator",
                 values_to = "value",
                 values_drop_na = TRUE) %>% 
    filter(value !=0) %>% 
    mutate(value = round(value,0))
  
  return(df)
  
}


  

