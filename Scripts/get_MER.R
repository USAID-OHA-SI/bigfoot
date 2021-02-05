#' get_MER
#'
#' @param filepath 
#' @param filename name of the MSD or genie export
#'
#' @return
#' @export
#'
#' @examples

get_mer <- function(filepath, filename) {
  
  indc <- c("PrEP_CURR", "TB_PREV", "HTS_TST", "HTS_TST_POS", "SC_CURR", "SC_ARVDISP")
  
  df <- ICPIutilities::read_msd(file.path(filepath, filename))
  df <- ICPIutilities::read_msd(file.path(filepath, filename))
  
  df <- df_mer %>%
    select(sitename, orgunituid, fundingagency, operatingunit, snu1, psnu, standardizeddisaggregate,
           otherdisaggregate, indicator, fiscal_year, targets, qtr1, qtr2, qtr3, qtr4, cumulative, trendscoarse)
  
  df <- df %>% 
    ICPIutilities::reshape_msd("long") 
  
  df <- df %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW") &
        standardizeddisaggregate %in%
        c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Age/Sex/HIVStatus")) |
        indicator %in% indc %>%
    filter(standardizeddisaggregate != "KeyPop/HIVStatus") %>% 
    group_by(sitename, orgunituid, operatingunit, snu1, psnu, standardizeddisaggregate,
             otherdisaggregate, indicator, period) %>%
    summarise(value = sum(val)) %>%
    ungroup()

  
  #Add months of treatment
  df <- df %>% 
    mutate(value = case_when(otherdisaggregate %in% c("ARV Bottles - TLD 90-count", "ARV Bottles - TLE/400 90-count") ~ (value*3),
                             otherdisaggregate=="ARV Bottles - TLD 180-count" ~ (value*6),
                             TRUE ~ value))
}