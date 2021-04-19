#' get_MER
#'
#' @param filepath path of mer data. Default = mer_data
#' @param filename name of the MSD or genie export
#'
#' @return
#' @export
#'
#' @examples

get_mer <- function(filepath = mer_data, filename) {
  
  indc <- c("PrEP_CURR", "TB_PREV", "HTS_TST", "HTS_TST_POS", "SC_CURR", "SC_ARVDISP")
  
  df <- ICPIutilities::read_msd(file.path(filepath, filename))
  
  df <- df %>%
    dplyr::select(sitename, orgunituid, fundingagency, operatingunit, snu1, psnu, standardizeddisaggregate,
           otherdisaggregate, trendscoarse, indicator, fiscal_year, targets, qtr1, qtr2, qtr3, qtr4, cumulative)
  
  #split off txcurr
  dftx <- df %>% 
    dplyr::filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    dplyr::group_by(across(c(sitename:fiscal_year))) %>% 
    dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  #create df for the rest
  dfo <- df %>% 
    dplyr::filter((indicator == "TX_CURR" &
              standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator")) |
             (indicator %in% indc & standardizeddisaggregate == "Total Numerator")) %>%
    dplyr::filter(standardizeddisaggregate != "KeyPop/HIVStatus")
  
  ##bind them and make long, remove periods that don't align w/SC_FACT
  dfall <- 
    dplyr::bind_rows(dftx, dfo) %>% 
    ICPIutilities::reshape_msd("long") %>% 
    dplyr::filter(!period %in% c("fy2019q1", "fy2019q2"))
  
  
  #Add months of treatment
  dfall <- dfall %>% 
    dplyr::mutate(val = case_when(otherdisaggregate %in% c("ARV Bottles - TLD 90-count",
                                                    "ARV Bottles - TLE/400 90-count") ~ (val*3),
                           otherdisaggregate=="ARV Bottles - TLD 180-count" ~ (val*6), TRUE ~ val)) %>% 
    dplyr::rename(value = val)
  
  return(dfall)
  
}