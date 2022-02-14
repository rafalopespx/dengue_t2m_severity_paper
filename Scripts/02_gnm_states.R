rm(list=ls())

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

setwd("~/Desktop/Dengue_severity/")

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

# dengue_t2m<-vroom("Data/dengue_t2m_stratas_2010_2019.csv.xz"))

# estados
dengue_t2m<-dengue_t2m %>% 
  mutate(code_stacked = code_state)

# Selecting unique values to the stacked level choosed
stacked_levels<-length(unique(dengue_t2m$code_stacked))
levels_stacked<-unique(dengue_t2m$code_stacked)
names_stacked<-unique(dengue_t2m$abbrev_state)

## Objects to keep interest values
# plot_list<-vector("list", stacked_levels)
RRVal_lag_list<-vector("list", stacked_levels)
RR_overall_list<-vector("list", stacked_levels)
# names(plot_list)<-names_stacked
names(RRVal_lag_list)<-names_stacked
names(RR_overall_list)<-names_stacked

## Objects to keep the results
## Coefficients
coef<-matrix(NA,
             stacked_levels,
             3,dimnames=list(names_stacked))
coef_cen<-coef

## Covariance Matrix
vcov<- vector("list",stacked_levels)
names(vcov) <- names_stacked
vcov_cen<-vcov

## Looping for GNM Stacked analysis

#foreach(i=1:stacked_levels) %dopar% {
for (i in 1:stacked_levels){
  data<-dengue_t2m %>% 
    filter(code_stacked == levels_stacked[i])
  
  # Number of lags, in days
  nlag<-21
  # Lag number of knots and placing of knots, on the log-scale equally spaced
  lagnk <- 3; klag<-logknots(nlag,lagnk) #
  
  ## argument to the lag being modeled, 21 days of temperature before
  # structure lag-response
  arglag<-list(fun="ns",knots=klag,int=T)
  
  ## 2 knots equally spaced
  knotsper<-equalknots(data$temp_mean, nk = 2)
  argvar<-list(fun="ns",knots=knotsper,int=F)
  
  cb<-crossbasis(data$temp_mean, lag=nlag, argvar=argvar, arglag=arglag)
  
  ## DLNM
  nyear<-length(unique(data$year))
  formula.gnm<-"Cases ~ cb+dow+ns(date, df=7*nyear)"
  
  model.gnm<-gnm(as.formula(formula.gnm), 
                 eliminate = month_city, 
                 data=data, 
                 family = quasipoisson, 
                 na.action="na.exclude")  
  
  ## Cross-pred
  tpred<-quantile(data$temp_mean, probs=(1:99)/100, na.rm=T)
  pred.gnm<-crosspred(cb,model.gnm, at=tpred) 
  
  #Reduced Prediction: overall
  red <- crossreduce(cb,model.gnm,at=tpred)
  coef[i,] <- red$coef
  vcov[[i]] <- red$vcov
  
  ### Centered Cross-pred
  mmt.gnm<-pred.gnm$predvar[which.min(pred.gnm$allRRfit)] 
  
  predcen.gnm<-crosspred(cb,model.gnm, at=tpred,cen=mmt.gnm)
  
  #Reduced Prediction: overall
  red_cen <- crossreduce(cb,model.gnm,at=tpred, cen=mmt.gnm)
  coef_cen[i,] <- red$coef
  vcov_cen[[i]] <- red$vcov
  
  RR_list<-effects_on_dlnm(50, 95, predcen.gnm)
  
  RRVal_lag_list[[i]]<-RRVal_lag
  RR_overall_list[[i]]<-RR_overall
  
}

## Binding objects to be saved
## Lag
RRVal_lag_list<-RRVal_lag_list %>% 
  bind_rows(.id = "abbrev_state")
## Overall
RR_overall_list <- RR_overall_list %>% 
  bind_rows(.id = "abbrev_state")

# Saving RR, overall and lags for each state
vroom_write(RRVal_lag_list, file = "Outputs/Tables/RRVal_lag_gnm_absolute_scale.csv.xz")
vroom_write(RR_overall_list, file = "Outputs/Tables/RR_overall_gnm_absolute_scale.csv.xz")

#Salving objects
## Coefficients Matrix
## Non-centered
coef_df<-coef %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "abbrev_state") %>% 
  setNames(c("abbrev_state", "b1", "b2", "b3"))

vroom_write(coef_df, file = "Outputs/Tables/coefficients_gnm_absolute_scale_for_all.csv.xz")
## Centered
coef_df_cen<-coef_cen %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "abbrev_state") %>% 
  setNames(c("abbrev_state", "b1", "b2", "b3"))

vroom_write(coef_df_cen, file = "Outputs/Tables/coefficients_gnm_cen_absolute_scale_for_all.csv.xz")

## Covariance Matrix
## Binding the Covariance Matrix
## Non-centered
vcov_df<-lapply(vcov, function(x){
  x<-x %>% 
    as.data.frame() %>% 
    rownames_to_column(c("coef"))
  return(x)
}
) %>% 
  bind_rows(.id = "abbrev_state")

vroom_write(vcov_df, file = "Outputs/Tables/vcov_gnm_absolute_scale_for_all.csv.xz")
## Centered
vcov_df_cen<-lapply(vcov_cen, function(x){
  x<-x %>% 
    as.data.frame() %>% 
    rownames_to_column(c("coef"))
  return(x)
}
) %>% 
  bind_rows(.id = "abbrev_state")

vroom_write(vcov_df_cen, file = "Outputs/Tables/vcov_gnm_cen_absolute_scale_for_all.csv.xz")


#
