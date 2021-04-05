#' sc_fact_df
#'
#' @param filepath where sc_fact data set was read in via `get_scfact`
#'
#' @return returns a dataframe with munged and joined sc_fact data
#' @export
#'
sc_fact_df <- function(filepath = sc_fact) {
  
  ##read in and munge sc_fact
  
  sc_fact_filename <- glamr::return_latest(filepath, "*.csv")
  
  df <- readr::read_csv(sc_fact_filename,
                        col_types = cols(.default = "c")) %>%
    janitor::clean_names() %>% 
    dplyr::mutate_at(vars(soh, ami, mos), ~as.numeric(.))
  
  ##read in meta

  df_meta <- googlesheets4::read_sheet("1UJv_LAzcD-lkteET9wGPGmdeFn07WnFf7g8sjs-upgk",
                                       sheet = "regimen",
                                       col_types= c(.default = "c")) %>%
    dplyr::filter(is.na(`include in analysis?`)) %>% 
    dplyr::select(-`include in analysis?`) %>% 
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
    gather(indicator, value, colnames(select_if(., is.numeric)), na.rm = TRUE)
  
  #generate snl1+snl2+facility for joining

  df <- df %>%
    tidyr::unite(join_var, snl1, snl2, facility, sep = "_", na.rm = TRUE, remove = FALSE)
  
  df %>% readr::write_csv(., paste0(Dataout, "/sc_fact_processed_",
                                        format(Sys.Date(),"%Y%m%d"), ".csv"))
  
  return(df)
    
}







