##
## Maps of hot and cold spots for each category of need within each Tactical Cell
## - food: access to supermarkets and food shops
## - health/wellbeing vulnerability
## - clinical vulnerability
## - economic vulnerability
## - digital exclusion
## - people seeking asylum
##
## Hot spots = 20% most vulnerable
## Cold spots = 20% least vulnerable
##
## Maps should show:
## - Tactical Cell boundaries
## - Local Authorities
## - vunerable MSOAs
##
library(tidyverse)
library(lubridate)
library(tmap)
library(sf)

source("load lookup tables.r")
source("https://github.com/matthewgthomas/brclib/raw/master/R/colours.R")  # for get_brc_colours()
source("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/functions.r")  # for calc_risk_quantiles

brc_cols = get_brc_colours()


##
## Load boundaries
##
tc = read_sf("data/boundaries/Tactical_cells.shp") %>% 
  st_transform(crs = 27700)

# Local Authority Districts (December 2019) Boundaries UK BUC
# source: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc
lads = read_sf("https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson") %>% 
  st_transform(crs = 27700)

# lookup which Tactical Cells each LA is in
lads_tc = lads %>% 
  st_centroid() %>% 
  st_join(tc) %>% 
  st_drop_geometry() %>% 
  select(lad19cd, name)

lads = lads %>% 
  left_join(lads_tc, by = "lad19cd")

lads = lads %>% 
  mutate(name = case_when(
    str_sub(lad19cd, 1, 1) == "W" ~ "Wales",
    str_sub(lad19cd, 1, 1) == "S" ~ "Scotland",
    str_sub(lad19cd, 1, 1) == "N" ~ "Northern Ireland and Isle of Man",
    TRUE ~ name
  ))

# Major Towns and Cities (December 2015) Boundaries
# source: https://geoportal.statistics.gov.uk/datasets/major-towns-and-cities-december-2015-boundaries
# towns = read_sf("https://opendata.arcgis.com/datasets/58b0dfa605d5459b80bf08082999b27c_0.geojson") %>% 
#   st_transform(crs = 27700)
# 
# # lookup which Tactical Cells each LA is in
# towns_tc = towns %>% 
#   st_centroid() %>% 
#   st_join(tc) %>% 
#   st_drop_geometry() %>% 
#   select(tcity15cd, name)
# 
# towns = towns %>% 
#   left_join(towns_tc, by = "tcity15cd")


##
## make MSOA to LA to Tactical Cell lookup
##
msoa_lad = load_lookup_lsoa_msoa_lad() %>% 
  select(MSOA11CD, LAD17CD) %>% 
  distinct()

lad_17_19 = read_csv("data/LAD 2017 to LAD 2019 codes.csv")

lad_tc = read_csv("data/lookup local authority to tactical cell.csv")

msoa_lad_tc = msoa_lad %>% 
  left_join(lad_17_19, by = "LAD17CD") %>% 
  left_join(lad_tc, by = "LAD19CD")


##
## vulnerability index
##
vi = read_sf("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/output/vulnerability-MSOA-UK.geojson")

# load food
vi_food_eng = read_sf("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/bespoke%20vulnerability%20index%20-%20food/food-vulnerability-MSOA-England.geojson")
vi_food_wal = read_sf("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/bespoke%20vulnerability%20index%20-%20food/food-vulnerability-MSOA-Wales.geojson")
vi_food_sco = read_sf("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/bespoke%20vulnerability%20index%20-%20food/food-vulnerability-MSOA-Scotland.geojson")
vi_food_ni  = read_sf("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/bespoke%20vulnerability%20index%20-%20food/food-vulnerability-SOA-NI.geojson")

vi_food = rbind(vi_food_eng %>% select(Code, Food.Vulnerability.decile),
                vi_food_wal %>% select(Code, Food.Vulnerability.decile),
                vi_food_sco %>% select(Code, Food.Vulnerability.decile),
                vi_food_ni  %>% select(Code, Food.Vulnerability.decile))


##
## asylum data
##
asylum = read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/asylum-LA.csv")


##
## digital exclusion
##
# load digital exclusion
caci_vuln_lsoa = read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/CACI/digital-exclusion-lsoa.csv")
caci_vuln_msoa = read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/CACI/digital-exclusion-msoa.csv")

# merge SOAs for Northern Ireland into the MSOA dataframe
digital = caci_vuln_msoa %>% 
  filter(!startsWith(MSOA11CD, "N")) %>%  # no MSOAs in Northern Ireland
  
  bind_rows( caci_vuln_lsoa %>% filter(startsWith(LSOA11CD, "9")) %>% rename(MSOA11CD = LSOA11CD) ) %>% 
  
  select(MSOA11CD, `Digital Vulnerability score`) %>% 
  
  mutate(`Digital Vulnerability decile` = calc_risk_quantiles(`Digital Vulnerability score`, quants = 10))

# add digital exclusion to boundaries
# Middle Layer Super Output Areas (December 2011) Boundaries EW BSC
# source: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bsc
msoa = read_sf("https://opendata.arcgis.com/datasets/c661a8377e2647b0bae68c4911df868b_3.geojson") %>%
  st_transform(crs = 27700)

# Intermediate zones
iz = read_sf("data/boundaries/SG_IntermediateZone_Bdry_2011.shp") %>% 
  st_transform(crs = 27700)

# Super Output Areas
# soa = read_sf("https://cc-p-ni.ckan.io/dataset/678697e1-ae71-41f3-abba-0ef5f3f352c2/resource/80392e82-8bee-42de-a1e3-82d1cbaa983f/download/soa2001.json") %>% 
soa = read_sf("data/boundaries/SOA2011.shp") %>% 
  st_transform(crs = 27700)

# combine into a single spatial dataframe and merge in vulnerability index
msoa_uk = rbind(msoa %>% select(Code = msoa11cd), 
                iz %>% select(Code = InterZone),
                soa %>% select(Code = SOA_CODE))

digital = msoa_uk %>% 
  left_join(digital, by = c("Code" = "MSOA11CD"))

rm(caci_vuln_lsoa, caci_vuln_msoa)

# lookup Tactical Cells
vi = vi %>% left_join(msoa_lad_tc, by = c("Code" = "MSOA11CD"))
vi_food = vi_food %>% left_join(msoa_lad_tc, by = c("Code" = "MSOA11CD"))
digital = digital %>% left_join(msoa_lad_tc, by = c("Code" = "MSOA11CD"))
asylum = asylum %>% left_join(lad_tc, by = "LAD19CD")

# manually point out if Northern Ireland cell
vi = vi %>% mutate(TacticalCell = ifelse(str_sub(Code, 1, 1) == "9", "Northern Ireland and Isle of Man", TacticalCell))
vi_food = vi_food %>% mutate(TacticalCell = ifelse(str_sub(Code, 1, 1) == "9", "Northern Ireland and Isle of Man", TacticalCell))
digital = digital %>% mutate(TacticalCell = ifelse(str_sub(Code, 1, 1) == "9", "Northern Ireland and Isle of Man", TacticalCell))
asylum = asylum %>% mutate(TacticalCell = ifelse(str_sub(LAD19CD, 1, 1) == "N", "Northern Ireland and Isle of Man", TacticalCell))


##
## load data on shielded people by Local Authority
##
# Coronavirus Shielded Patient List, England - Local Authority: https://digital.nhs.uk/data-and-information/publications/statistical/mi-english-coronavirus-covid-19-shielded-patient-list-summary-totals/latest
shielded = read_csv("https://files.digital.nhs.uk/96/69FFAA/Coronavirus%20%28COVID-19%29%20Shielded%20Patient%20List%2C%20England%20-%20Open%20Data%20-%20LA%20-%202020-05-15.csv")

shielded = shielded %>% 
  # keep only latest values (if more than one extraction happens to be in this file)
  mutate(`Extract Date` = dmy(`Extract Date`)) %>% 
  filter(`Extract Date` == max(`Extract Date`)) %>% 
  
  filter(`LA Code` != "ENG") %>%  # don't need England-wide figures

  # lookup tactical cells
  rename(Code = `LA Code`) %>% 
  left_join(lad_tc, by = c("Code" = "LAD19CD"))

shielded_vars = unique(shielded$`Breakdown Field`)


##
## loop over Tactical Cells, creating maps for each
##
# helper function to mark 20% most vulnerable and 20% least vulnerable
hotcold = function(x) case_when(
  x <= 2 ~ "Least vulnerable",
  x >= 9 ~ "Most vulnerable",
  TRUE ~ ""
)


tc_names = unique(vi$TacticalCell)

# tc_names = tc_names[ !tc_names %in% c("London", "South East")]  # uncomment line if you need to drop any Cells from the loop
# tc_curr = "London"  # manual set TC if debugging

for (tc_curr in tc_names) {

  # get subsets of boundaries within current Cell
  lads_s = lads %>% filter(name == tc_curr)
  # tc_s = tc %>% filter(name == tc_curr)
  # towns_s = towns %>% filter(name == tc_curr)
  
  # get vulnerability scores within this Cell
  digital_s = digital %>% 
    filter(TacticalCell == tc_curr) %>% 
    mutate(HotCold = hotcold(`Digital Vulnerability decile`)) %>% 
    filter(HotCold != "")
  
  vi_s_economic = vi %>% 
    filter(TacticalCell == tc_curr) %>% 
    mutate(HotCold = hotcold(Economic.Vulnerability.decile)) %>% 
    filter(HotCold != "")
  
  vi_s_health = vi %>% 
    filter(TacticalCell == tc_curr) %>% 
    mutate(HotCold = hotcold(Health.Wellbeing.Vulnerability.decile)) %>% 
    filter(HotCold != "")
  
  vi_s_clinical = vi %>% 
    filter(TacticalCell == tc_curr) %>% 
    mutate(HotCold = hotcold(Clinical.Vulnerability.decile)) %>% 
    filter(HotCold != "")
  
  vi_s_food = vi_food %>% 
    filter(TacticalCell == tc_curr) %>% 
    mutate(HotCold = hotcold(Food.Vulnerability.decile)) %>% 
    filter(HotCold != "")
  
  asylum_s = lads_s %>% 
    left_join(asylum, by = c("lad19cd" = "LAD19CD")) 

  
  ##
  ## make hot/cold spot maps
  ##
  # basemap showing tactical cell and local authorities
  basemap = tm_shape(lads_s) +
    tm_polygons(col = "white", border.alpha = 0.5)
  
  # digital exclusion
  map_digital = basemap +
    tm_shape(digital_s) +
    tm_polygons(col = "HotCold", palette = c(brc_cols$teal_light, brc_cols$red), border.alpha = 0, title = "Digital exclusion")
  
  # clinical vulnerability
  map_clinical = basemap +
    tm_shape(vi_s_clinical) +
    tm_polygons(col = "HotCold", palette = c(brc_cols$teal_light, brc_cols$red), border.alpha = 0, title = "Clinical vulnerability")
  
  # economic vulnerability
  map_economic = basemap +
    tm_shape(vi_s_economic) +
    tm_polygons(col = "HotCold", palette = c(brc_cols$teal_light, brc_cols$red), border.alpha = 0, title = "Economic/financial vulnerability")
  
  # health/wellbeing vulnerability
  map_health = basemap +
      tm_shape(vi_s_health) +
      tm_polygons(col = "HotCold", palette = c(brc_cols$teal_light, brc_cols$red), border.alpha = 0, title = "Health/wellbeing vulnerability")
  
  # food vulnerability
  map_food = basemap +
      tm_shape(vi_s_food) +
      tm_polygons(col = "HotCold", palette = c(brc_cols$teal_light, brc_cols$red), border.alpha = 0, title = "Food insecurity")
  
  # asylum support
  map_asylum = basemap +
    tm_shape(asylum_s) +
    tm_polygons(col = "People receiving Section 95 support", palette = "Reds", border.alpha = 0.5, title = "People seeking asylum")
  
  # save maps
  if (!dir.exists(file.path("output", tc_curr))) dir.create(file.path("output", tc_curr))  # create folder for this Cell if needed
  
  tmap_save(map_digital,  file.path("output", tc_curr, "digital.png"))
  tmap_save(map_clinical, file.path("output", tc_curr, "clinical.png"))
  tmap_save(map_economic, file.path("output", tc_curr, "economic.png"))
  tmap_save(map_health,   file.path("output", tc_curr, "health.png"))
  tmap_save(map_food,     file.path("output", tc_curr, "food.png"))
  tmap_save(map_asylum,   file.path("output", tc_curr, "asylum.png"))
  
  ##
  ## maps for shielded people in England
  ##
  if (!tc_curr %in% c("Scotland", "Wales", "Northern Ireland and Isle of Man")) {
    
    # loop over every breakdown in data (by ages and by genders)
    for (field in shielded_vars) {
      print(field)
      
      shielded_s = lads_s %>% 
        left_join(shielded, by = c("lad19cd" = "Code")) %>% 
        filter(TacticalCell == tc_curr & `Breakdown Field` == field)
      
      for (field_value in unique(shielded_s$`Breakdown Value`)) {
        print(field_value)

        shielded_s2 = shielded_s %>% 
          filter(`Breakdown Value` == field_value)
        
        shielded_title = case_when(
          field == "Age" ~ paste0("Ages ", field_value),
          field == "Gender" ~ paste0(field_value, "s"),
          field == "ALL" ~ "All shielded people",
          TRUE ~ field_value
        )
        
        map_shielded = basemap + 
          tm_shape(shielded_s2) +
          tm_polygons(col = "Patient Count", palette = "Reds", border.alpha = 0.5, title = "No. people") +
          tm_layout(title = shielded_title)
        
        tmap_save(map_shielded, file.path("output", tc_curr, paste0("shielded - ", shielded_title, ".png")))
        
      } # end for loop for field_value
    }   # end for loop for field
  }     # end if for England's Tactical Cells
  
  print(paste0("Finished ", tc_curr))

}  # end main for loop
