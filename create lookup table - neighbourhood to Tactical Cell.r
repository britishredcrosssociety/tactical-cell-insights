###
## make LSOA/MSOA to LA to Tactical Cell lookup
##
library(tidyverse)

source("load lookup tables.r")

lsoa_msoa_lad = load_lookup_lsoa_msoa_lad()

lsoa_lad = lsoa_msoa_lad %>% 
  select(LSOA11CD, LAD17CD) %>% 
  distinct()

msoa_lad = lsoa_msoa_lad %>% 
  select(MSOA11CD, LAD17CD) %>% 
  distinct()

lad_17_19 = read_csv("data/LAD 2017 to LAD 2019 codes.csv")

lad_tc = read_csv("data/lookup local authority to tactical cell.csv")

lsoa_lad_tc = lsoa_lad %>% 
  left_join(lad_17_19, by = "LAD17CD") %>% 
  left_join(lad_tc, by = "LAD19CD")

msoa_lad_tc = msoa_lad %>% 
  left_join(lad_17_19, by = "LAD17CD") %>% 
  left_join(lad_tc, by = "LAD19CD")

# make list of LA names in each TC
# lad_names = read_csv("https://opendata.arcgis.com/datasets/c3ddcd23a15c4d7985d8b36f1344b1db_0.csv") %>% 
#   select(LAD19CD, LAD19NM)
# 
# lad_tc %>% 
#   left_join(lad_names, by = "LAD19CD") %>% 
#   arrange(TacticalCell, LAD19NM) %>% 
#   write_csv("data/local authorities and tactical cells.csv")

# save
msoa_lad_tc %>% write_csv("data/lookup msoa to tactical cell.csv")
