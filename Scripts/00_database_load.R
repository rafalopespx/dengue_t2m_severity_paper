rm(list=ls())
gc()

### Loading packages
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(tsibble)){install.packages("tsibble"); library(tsibble)}
if(!require(dlnm)){install.packages("dlnm"); library(dlnm)}
if(!require(splines)){install.packages("splines"); library(splines)}
if(!require(tibble)){install.packages("tibble"); library(tibble)}
if(!require(stringr)){install.packages("stringr"); library(stringr)}
if(!require(gnm)){install.packages("gnm"); library(gnm)}
if(!require(parallel)){install.packages("parallel"); library(parallel)}
if(!require(foreach)){install.packages("foreach"); library(foreach)}
if(!require(mixmeta)){install.packages("mixmeta"); library(mixmeta)}
if(!require(mvmeta)){install.packages("mvmeta"); library(mvmeta)}
if(!require(geofacet)){install.packages("geofacet"); library(geofacet)}

setwd("~/Desktop/dengue_t2m_severity_paper/")

dengue_t2m<-vroom("Data/dengue_t2m_stratas_2010_2019.csv.xz")

dengue_t2m <- 
  dengue_t2m %>% 
  mutate(month_city          = factor(paste(month, name_muni, sep = "_"))
  )

# estados
dengue_t2m<-dengue_t2m %>% 
  mutate(code_stacked = code_state)

# Selecting unique values to the stacked level choosed
stacked_levels<-27L ## 27 States
codes_stacked<-c(11,12,13,14,15,16,17, ## Code number for the states in the North Region
                 21,22,23,24,25,26,27,28,29, ## Code number for the states in the Northeast Region
                 31,32,33,35, ## Code number for the states in the Southeast Region
                 41,42,43, ## Code number for the states in the South Region
                 50,51,52,53) ## Code number for the states in the Center-West Region
states<-names_stacked<-c("RO","AC","AM","RR","PA","AP","TO", ## Abbreviation for the states in the North Region
                         "MA","PI","CE","RN","PB","PE","AL","SE","BA", ## Abbreviation for the states in the Northeast Region
                         "MG","ES","RJ","SP", ## Abbreviation for the states in the Southeast Region
                         "PR","SC","RS", ## Abbreviation for the states in the South Region
                         "MS","MT","GO","DF") ## Abbreviation for the states in the Center-West Region

#