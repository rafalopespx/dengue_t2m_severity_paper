## Cleaning the workspace
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

# # Setting the working directory
# setwd("~/Desktop/dengue_t2m_severity_paper/")

## Loading the hospitalizations cases
dengue_t2m<-vroom("Data/dengue_t2m_stratas_2010_2019.csv.xz")

## Setting up month_city and month_city_dow factors
dengue_t2m <- 
  dengue_t2m %>% 
  mutate(month_city          = factor(paste(month, name_muni, sep = "_")),
         month_city_dow      = factor(paste(month, name_muni, dow, sep = "_"))
  ) |> 
  arrange(code_muni, date) |> 
  data.table::as.data.table()

## Column to signalizing which rows to keep
dengue_t2m[,  keep:=sum(Cases)>0, by=month_city_dow]

## Cases dataset with moving averages
dengue_confirmed<-vroom("Data/dengue_cases_confirmed_muni_2000_2020_complete.csv.xz") |> 
  rename(date = date_symptoms)

## Building the moving averages to run into gnm
## All dates for the whole period
date_series <-
  tibble(
    date = seq.Date(dmy('01-01-2010'), dmy('31-12-2019'), by = 'day')
  )

## All municipalities codes
code_muni<-unique(dengue_t2m$code_muni)

date_series<-expand_grid(date_series, code_muni)

dengue_confirmed<-date_series |> 
  left_join(dengue_confirmed) |> 
  mutate(confirmed = replace_na(confirmed, 0))
  
dengue_confirmed<-dengue_confirmed  |> 
  arrange(date) |>
  group_by(code_muni) |> 
  mutate(cases_confirmed_7ma  = zoo::rollmean(confirmed, k = 7, fill = NA, align = 'right'),
         cases_confirmed_14ma = zoo::rollmean(confirmed, k = 14, fill = NA, align = 'right'))

## Confirmar se tem todas as possíeveis para evitar bias na média móvel
# States
dengue_t2m<-dengue_t2m |> 
  rename(code_stacked = code_state)

dengue_confirmed<-dengue_confirmed |> 
  rename(code_stacked = code_state)

# Selecting unique values to the stacked level choose
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

# ## Cros-basis parametrization
# # Defining basis and grid
# dengue_t2m_means<-dengue_t2m %>% 
#   group_by(abbrev_state) %>% 
#   summarise(tmin = min(temp_mean), 
#             tmax = max(temp_mean)) %>%
#   ungroup() %>% 
#   summarise(tmin = mean(tmin), 
#             tmax = mean(tmax))
# 
# knotsper<-equalknots(dengue_t2m_means$tmin:dengue_t2m_means$tmax, nk = 2)
# varfun<-"ns"
# 
# nlag<-21
# xlag<-0:nlag
# lagnk <- 3
# klag<-logknots(nlag,lagnk)
# lagfun<-"ns"
# 
# argvar<-list(fun=varfun, knots=knotsper, int=F)
# arglag<-list(fun=lagfun, knots=klag,int=T)
# tpred<-quantile(dengue_t2m$temp_mean, probs=(1:99)/100, na.rm=T)
# range_cb<-c(min(dengue_t2m$temp_mean):max(dengue_t2m$temp_mean))
# cb <- crossbasis(dengue_t2m$temp_mean, lag=nlag, argvar=argvar, arglag=arglag, group = dengue_t2m$code_muni)
# bvar <- do.call("onebasis",c(list(x=dengue_t2m$temp_mean),attr(cb,"argvar")))
# blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))

#