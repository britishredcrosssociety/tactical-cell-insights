##
## Demographic analyses: age and sex structure in England and Wales
##
library(tidyverse)
library(readxl)
library(httr)

source("create lookup table - neighbourhood to Tactical Cell.r")

##
## calculate proportion of older population from MSOA population estimates
## source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates
##
GET("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fmiddlesuperoutputareamidyearpopulationestimates%2fmid2018sape21dt3a/sape21dt3amid2018msoaon2019lasyoaestimatesformatted.zip",
    write_disk(tf <- tempfile(fileext = ".zip")))

unzip(tf, exdir = "data/population")
unlink(tf); rm(tf)

pop_m = read_excel("data/population/SAPE21DT3a-mid-2018-msoa-on-2019-LA-syoa-estimates-formatted.xlsx", sheet = "Mid-2018 Males", skip = 4)
pop_f = read_excel("data/population/SAPE21DT3a-mid-2018-msoa-on-2019-LA-syoa-estimates-formatted.xlsx", sheet = "Mid-2018 Females", skip = 4)


###############################################################################
## Age structure
##
pop_m_age = pop_m %>% 
  filter(!is.na(MSOA)) %>%  # drop Local Authorities
  
  # convert age columns to long format
  rename(`90` = `90+`) %>% 
  pivot_longer(cols = `0`:`90`, names_to = "Age", values_to = "n_people") %>% 
  mutate(Age = as.integer(Age)) %>% 
  
  mutate(AgeGroup = case_when(
    Age <= 17 ~ "Age 0 to 17",
    Age >= 18 & Age <= 64 ~ "Age 18 to 64",
    Age >= 65 & Age <= 79 ~ "Age 65 to 79",
    Age >= 80 ~ "Age 80+"
  )) %>% 
  
  # summarise into age groups
  group_by(`Area Codes`, AgeGroup) %>% 
  summarise(n_people = sum(n_people, na.rm = TRUE)) %>% 
  
  mutate(Sex = "Male")

pop_f_age = pop_f %>% 
  filter(!is.na(MSOA)) %>%  # drop Local Authorities
  
  # convert age columns to long format
  rename(`90` = `90+`) %>% 
  pivot_longer(cols = `0`:`90`, names_to = "Age", values_to = "n_people") %>% 
  mutate(Age = as.integer(Age)) %>% 
  
  mutate(AgeGroup = case_when(
    Age <= 17 ~ "Age 0 to 17",
    Age >= 18 & Age <= 64 ~ "Age 18 to 64",
    Age >= 65 & Age <= 79 ~ "Age 65 to 79",
    Age >= 80 ~ "Age 80+"
  )) %>% 
  
  # summarise into age groups
  group_by(`Area Codes`, AgeGroup) %>% 
  summarise(n_people = sum(n_people, na.rm = TRUE)) %>% 
  
  mutate(Sex = "Female")

pop_age = bind_rows(pop_m_age, pop_f_age)

# get proportions of people within age groups for whole nation
age_sex_uk = pop_age %>% 
  group_by(Sex) %>% 
  summarise(total = sum(n_people))

age_sex_uk = pop_age %>% 
  group_by(Sex, AgeGroup) %>% 
  summarise(n_people = sum(n_people, na.rm = TRUE)) %>% 
  
  left_join(age_sex_uk, by = "Sex") %>% 
  mutate(prop_people = n_people / total)


###############################################################################
## Plot age and sex structure for each Cell
##
tc = unique(lad_tc$TacticalCell)
tc = tc[ !tc %in% c("Scotland", "Northern Ireland and Isle of Man")]  # drop these two Cells - their population data will come from elsewhere

for (tc_curr in tc) {
  
  ##
  ## get proportions of people in the current Cell
  ##
  age_sex_cell = pop_age %>% 
    # get tactical cells
    left_join(msoa_lad_tc, by = c("Area Codes" = "MSOA11CD")) %>% 
    filter(TacticalCell == tc_curr) %>% 
    
    group_by(Sex) %>% 
    summarise(total = sum(n_people))
  
  age_sex_cell = pop_age %>% 
    # get tactical cells
    left_join(msoa_lad_tc, by = c("Area Codes" = "MSOA11CD")) %>% 
    filter(TacticalCell == tc_curr) %>% 
    
    group_by(Sex, AgeGroup) %>% 
    summarise(n_people = sum(n_people, na.rm = TRUE)) %>% 
    
    left_join(age_sex_cell, by = "Sex") %>% 
    mutate(prop_people = n_people / total)
  
  
  ##
  ## plot age pyramid
  ##
  # make male age counts negative so the age pyramid displays properly
  age_sex_uk   = age_sex_uk   %>% mutate(prop_people2 = ifelse(Sex == "Female", -1 * prop_people, prop_people))
  age_sex_cell = age_sex_cell %>% mutate(prop_people2 = ifelse(Sex == "Female", -1 * prop_people, prop_people))
  
  n_breaks = 0.1
  
  plt_pyramid = ggplot(age_sex_cell, aes(x = AgeGroup, y = prop_people2, fill = Sex, shape = Sex)) +
    geom_col(data = subset(age_sex_cell, Sex == "Female")) +
    geom_col(data = subset(age_sex_cell, Sex == "Male")) +
    
    geom_point(data = subset(age_sex_uk, Sex == "Female"), size = 2.5) +
    geom_point(data = subset(age_sex_uk, Sex == "Male"),   size = 2.5) +
    
    coord_flip() +
    
    scale_fill_brewer(palette = "Accent") +
    
    # make sure the scales are symmetrical and the -ve values (for plotting) are converted to their actual positive values
    scale_y_continuous(labels = scales::percent(c(rev(seq(0, max(age_sex_cell$prop_people2), n_breaks)), seq(0, max(age_sex_cell$prop_people2), n_breaks))),
                       breaks = c(-1 * rev(seq(0, max(age_sex_cell$prop_people2), n_breaks)), seq(0, max(age_sex_cell$prop_people2), n_breaks))) +
    
    labs(x = NULL, y = "Percentage of people", title = "Age and sex pyramid within the Tactical Cell", 
         subtitle = "Bars show the proportion of females and males within the Cell;\nfor comparison, black dots show UK-wide proportions") +
    
    theme_light() +
    theme(panel.border        = element_blank()
          ,axis.text          = element_text(size = 8)
          ,legend.text        = element_text(size = 8)
          ,legend.title       = element_blank()
          ,legend.position    = "bottom"
          ,panel.grid.major.x = element_blank()
          ,panel.grid.minor.x = element_blank()
          ,plot.margin        = margin(0.5, 0, 0.2, 0, "cm")
    )

  if (!dir.exists(file.path("output", tc_curr))) dir.create(file.path("output", tc_curr))  # create folder for this Cell if needed
  
  ggsave(file.path("output", tc_curr, "age and sex pyramid.png"), plot = plt_pyramid, width = 175, height = 75, units = "mm")

  print(paste0("Finished ", tc_curr))
  
}
