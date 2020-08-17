# ---- Load libraries ----
library(tidyverse)
library(readxl)

# ---- Load data ----
scot_eth <- read_csv("data/census/Scotland/KS201SC.csv")
brc_eth <- read_excel("data/census/Ethnic group mapping from census.xlsx",
                      sheet = "Ethnic group mapping")

# ---- Clean data ----
scot_eth <-
  scot_eth %>% 
  rename(MSOA11CD = X1) %>% 
  select(-`All people`) %>% 
  pivot_longer(cols = !MSOA11CD,
               names_to = "category",
               values_to = "count")

# Join brc cats
scot_eth <- scot_eth %>% 
  left_join(
    brc_eth %>% 
      filter(Country == "Scotland") %>% 
      select(category = `Census category`,
             Ethnicity = `BRC category`),
    by = "category"
  ) %>% 
  drop_na() # Remove summary rows

# National counts
scot_eth <- scot_eth %>% 
  filter(MSOA11CD != "S92000003") %>% 
  group_by(Ethnicity) %>% 
  summarise(count = sum(count)) %>% 
  mutate(count = as.integer(count))

# National props
scot_eth <- scot_eth %>% 
  mutate(Prop = count/sum(count))

# ---- Plot ----
ggplot(scot_eth, aes(x = Ethnicity, y = Prop)) + 
  
  geom_col(fill = "cornflowerblue") +
  
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent) +
  
  labs(x = NULL, y = "Percentage of people") +
  
  theme_light() +
  theme(panel.border        = element_blank()
        ,axis.text          = element_text(size = 8)
        ,legend.text        = element_text(size = 8)
        ,legend.title       = element_blank()
        ,legend.position    = "bottom"
        ,panel.grid.major.x = element_blank()
        ,panel.grid.minor.x = element_blank()
        ,plot.margin        = margin(0.5, 0, 0.2, 0, "cm")
        ,panel.grid.major   = element_blank()
        ,panel.grid.minor   = element_blank()
        ,panel.background   = element_rect(fill = "transparent", colour = NA)
        ,plot.background    = element_rect(fill = "transparent", colour = NA)
  )

ggsave("output/Scotland/ethnicity.png", bg = "transparent", width = 200, height = 85, units = "mm")
