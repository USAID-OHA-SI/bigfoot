# PURPOSE: Munge and Analysis of
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2021-10-13
# NOTES: Analysis for Dianna looking at 25% deviation

# LOCALS & SETUP ============================================================================

  # Libraries
    oha
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
    nvp <- c("Nevirapine 50 mg Dispersible Tablet, 30 Tablets",
             "Nevirapine 50 mg Dispersible Tablet, 60 Tablets",
             "Nevirapine/Lamivudine/Zidovudine 50/30/60 mg Dispersible Tablet, 60 Tablets")  
  

# LOAD DATA ============================================================================  

    # df_sc <- read_csv("Dataout/sc_fact_processed_20211004.csv")

# MUNGE ============================================================================
  
  #  subset to fy21 and RTK and ARVs
    df %>% distinct(product_category)
    df %>% distinct(period, mer_pd, fiscal_year) %>% arrange(period) %>% prinf
    df %>% distinct(fiscal_year)
  
    
    df_sc_anal <- df %>% 
      filter(product_category %in% c("Adult ARV", "HIV RTK"),
             fiscal_year == "2021",
             indicator == "ami") %>% 
      group_by(country, period, facility, product_category) %>% 
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    
    df_sc_anal <- df_sc_anal %>%
      tsibble::as_tsibble(key = c("country", "facility", "product_category"),
                          index = period) %>%
      group_by(facility, product_category) %>% 
      mutate(avg = round(mean(value, na.rm = TRUE),0),
             dev = ((value-avg)/avg)) %>% 
      ungroup() %>%
      mutate(flag = ifelse(abs(dev > .25),1,0)) %>% 
      filter(period == "2021-07-01")
    
    df_sc_anal %>% 
      group_by(country, product_category) %>% 
      summarise(total = sum(flag, na.rm = TRUE)) %>% 
      pivot_wider(names_from = product_category, values_from = total)
      
    df_sc_anal %>%
      filter(!is.na(flag)) %>%
      mutate(flag = flag*100) %>% 
      group_by(country, product_category) %>% 
      summarise(cntry_avg =  mean(flag)) %>% 
      pivot_wider(names_from = product_category, values_from = cntry_avg)

    df_sc_anal %>%
      group_by(country, product_category) %>%
      summarise(facility = n()) %>% 
      pivot_wider(names_from = product_category, values_from = facility)
    
    #switching to nvp
    df %>% filter(product_category == "Pediatric ARV") %>% arrange(product) %>%
      distinct(product, regimen_optimized) %>% prinf
    
    # look at issuance of not-optimized peds regimens
    
    peds_df <- df_sc %>% 
      filter(product %in% nvp,
             indicator == "ami",
             value > 0) %>% 
      group_by(country, facility, fiscal_year, period) %>% 
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

    df %>%
      filter(product %in% nvp,
             indicator == "ami",
             value > 0) %>% 
      group_by(period, country) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>% 
      pivot_wider(names_from = period, values_from = value) %>% 
      write_csv("Dataout/nvp_ami.csv")

    

 # VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

  