#' sc_fact_df
#'
#' @param filepath where sc_fact data set was read in via `get_scfact`
#'
#' @return returns a dataframe with munged and joined sc_fact data
#' @export
#'
sc_fact_df <- function(filepath = sc_fact) {
  
  ##read in and munge sc_fact
  
  glamr::load_secrets()
  
  sc_fact_filename <- glamr::return_latest(filepath, "sc_fact_raw")
  
  df <- readr::read_csv(sc_fact_filename,
                        col_types = cols(.default = "c")) %>%
    janitor::clean_names() %>% 
    dplyr::mutate_at(vars(soh, ami, mos), ~as.numeric(.)) %>%
    dplyr::mutate(country = str_to_sentence(country)) %>% 
    dplyr::rename(orgunituid = datim_code) %>% 
    dplyr::select(-facility_mapped, -facility_cd, -source)
  
  ## create mer period values
  
  df <- df %>%
    mutate(period = as.Date(as.yearmon(period)),
           mer_pd = paste0("fy",lubridate::quarter(x = period, with_year = TRUE, fiscal_start = 10),"q"),
           fiscal_year = lubridate::quarter(x = period, with_year = TRUE, fiscal_start = 10),
           fiscal_year = as.character(fiscal_year),
           fiscal_year = stringr::str_remove(fiscal_year, "\\..*"))

  ##read in meta

  df_meta <- googlesheets4::read_sheet("1UJv_LAzcD-lkteET9wGPGmdeFn07WnFf7g8sjs-upgk",
                                       sheet = "regimen",
                                       col_types= c(.default = "c")) %>%
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
  
  # #generate snl1+snl2+facility for joining (depreciate for now 9.7.21)
  # df <- df %>%
  #   tidyr::unite(join_var, snl1, snl2, facility, sep = "_", na.rm = TRUE, remove = FALSE)
  
  df %>% readr::write_csv(., paste0(Dataout, "/sc_fact_processed_",
                                        format(Sys.Date(),"%Y%m%d"), ".csv"))
  
  return(df)
    
}







