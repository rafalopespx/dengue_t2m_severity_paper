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

setwd("~/Desktop/dengue_t2m_severity_paper/")

# Loading databases
dengue_t2m_2010_2019<-vroom("Data/dengue_t2m_muni_2010_2019.csv.xz")

dengue_t2m <- dengue_t2m_2010_2019 %>% 
  mutate(year = year(date),
         month = month(date),
         dow   = factor(wday(date)),
         dom   = factor(day(date))
  )

## Stratas
## DO NOT RUN VARIOUS STRATAS, IT TAKE TOO MUCH TIME TO MAKE IT
dengue_t2m <- 
  dengue_t2m %>% 
  mutate(month_city          = factor(paste(month, name_muni, sep = "_"))
  )

## Saving the database with Stratas
vroom_write(dengue_t2m, file = "Data/dengue_t2m_stratas_2010_2019.csv.xz")

#