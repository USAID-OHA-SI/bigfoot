## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  read in MER data and munge
## DETAIL :  Read in site level data for 12 OUs and change to format for use in the DDC
## Date: 01-23-2021



#function to create ddc compliant dataset
#' get_mer_ddc
#'
#' @param filepath path where your mer data is located
#' @param filename name of your site level mer data
#'
#' @return
#' @export
#'
#' @examples
get_mer_ddc <- function(filepath, filename) {
  
  indc <- c("PrEP_CURR", "TB_PREV", "HTS_TST", "HTS_TST_POS")
  
  df <- ICPIutilities::read_msd(file.path(filepath, filename))
  
  df <- df_mer %>%
    select(sitename, orgunituid, operatingunit, snu1, psnu, standardizeddisaggregate,
           otherdisaggregate, indicator, fiscal_year, targets, qtr1, qtr2, qtr3, qtr4, cumulative, trendscoarse)
    
    df <- df %>% 
    ICPIutilities::reshape_msd("long") 
    
    df <- df %>%
    filter(indicator == "TX_CURR" & (
             is.na(trendscoarse) | trendscoarse == "15+" &
               standardizeddisaggregate %in%
               c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Age/Sex/HIVStatus")) |
             indicator %in% indc & standardizeddisaggregate == "Total Numerator") %>%
    filter(standardizeddisaggregate != "KeyPop/HIVStatus") %>% 
    group_by(sitename, orgunituid, operatingunit, snu1, psnu, standardizeddisaggregate,
             otherdisaggregate, indicator, period) %>%
    summarise(value = sum(val)) %>%
    ungroup()
  
}


##write it
df %>% write_csv(file.path(Dataout, "mer_ddc_output_fy20q2_v2.csv"))


## some tests to make sure it worked
df %>% distinct(period)
df %>% distinct(indicator)

df %>% distinct(indicator, period) %>% arrange(period)
df %>% distinct(operatingunit)
df %>%
  filter(standardizeddisaggregate == "Total Numerator") %>% 
  group_by(operatingunit, indicator, period) %>% 
  summarise(val = sum(value)) %>% prinf()

df %>% distinct(indicator, standardizeddisaggregate, otherdisaggregate) %>%
  arrange(indicator) %>% 
  prinf()
