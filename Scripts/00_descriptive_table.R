rm(list=ls())
gc()

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyr)){install.packages("tidyr"); library(tidyr)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(gtsummary)){install.packages("gtsummary"); library(gtsummary)}
if(!require(webshot)){install.packages("webshot"); library(webshot)}

# setwd("~/Desktop/dengue_t2m_severity_paper//")

table_ready<-vroom("Data/table_ready.csv")

# Dengue Table by Region
dengue_table_region<-table_ready %>% 
  dplyr::select(age, age_cat, race, 
                region, sex, 
                time_interna, outcome) %>% 
  mutate(region = factor(region,
                         levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>%
  tbl_summary(by = region,
              missing = "ifany",
              missing_text = "(Missing)",
              percent = "column",
              type = all_dichotomous() ~ "categorical",
              label = list(age ~ "Age",
                           age_cat ~ "Age in Categories",
                           race ~ "Self-Reported Race",
                           sex ~ "Sexo",
                           time_interna ~ "Time of Hospitalization",
                           outcome ~ "Outcome"
                           )
  )%>% 
  modify_header(label ~ "**Characteristics Variables**") %>%
  add_overall() %>% 
  add_n() %>% 
  bold_labels() %>% 
  modify_caption("Table 1. Raw data Dengue Hospitalizations, by Regions")

dengue_table_region

dengue_table_region %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = "Outputs/Tables/dengue_region_aih_summary.docx")

dengue_table_region %>% 
  as_flex_table() %>% 
  flextable::save_as_pptx(path = "Outputs/Tables/dengue_region_aih_summary.pptx")

vroom_write(dengue_table_region %>% 
              as_tibble(), 
            file = "Outputs/Tables/dengue_region_aih_summary.csv")


#
