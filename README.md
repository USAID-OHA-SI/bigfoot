# bigfoot
Data triangulation of MER and non-MER 
---

Functions for working with SC_FACT dataset from PSM. Users should start with 00_setup.R


-get_MER
  -Requires a genie download with the following filters selected:'
  -Site by IM extract
  -Following OUs selected
   "Angola",
   "Botswana",
   "Cameroon",
   "Haiti",
   "Lesotho",
   "Malawi",
   "Mozambique",
   "Namibia",
   "Nigeria",
   "Uganda",
   "Zambia",
   "Zimbabwe"
  -Frozen
  -Following indicators: HTS_TST, HTS_TST_POS, TB_PREV, PrEP_CURR, SC_CURR, SC_ARVDISP, TX_CURR
  -Standdarddizeddisaggregate: Total Numerator, Age/Sex/HIVStatus,                Age/Sex/ARVDispense/HIVStatus
  -Fiscal_year: 2019, 2020, 2021
  -File can be found here <https://drive.google.com/drive/u/0/folders/19xSyTi96oBiNueicPPi8P7iaBctbW5mX>
  
  

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*