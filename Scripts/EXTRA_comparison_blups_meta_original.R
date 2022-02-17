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
if(!require(mvmeta)){install.packages("mvmeta"); library(mvmeta)}


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
levels_stacked<-c(11,12,13,14,15,16,17, ## Code number for the states in the North Region
                  21,22,23,24,25,26,27,28,29, ## Code number for the states in the Northeast Region
                  31,32,33,35, ## Code number for the states in the Southeast Region
                  41,42,43, ## Code number for the states in the South Region
                  50,51,52,53) ## Code number for the states in the Center-West Region
states<-names_stacked<-c("RO","AC","AM","RR","PA","AP","TO", ## Abbreviation for the states in the North Region
                         "MA","PI","CE","RN","PB","PE","AL","SE","BA", ## Abbreviation for the states in the Northeast Region
                         "MG","ES","RJ","SP", ## Abbreviation for the states in the Southeast Region
                         "PR","SC","RS", ## Abbreviation for the states in the South Region
                         "MS","MT","GO","DF") ## Abbreviation for the states in the Center-West Region

## Loading Coef and Vcov, in case to not re-run the model all again
## Coef and Vcov for states
coef<-vroom("Outputs/Tables/coefficients_gnm_for_all.csv.xz")
vcov<-vroom("Outputs/Tables/vcov_gnm_for_all.csv.xz")

## Coef and Vcov for states, CENTERED
coef_cen<-vroom("Outputs/Tables/coefficients_gnm_cen_for_all.csv.xz")
vcov_cen<-vroom("Outputs/Tables/vcov_gnm_cen_for_all.csv.xz")

# Putting coef and vcov in the right format again
# coef 
coef<-coef %>% 
  select(b1, b2, b3)
coef<-as.matrix(coef)
rownames(coef)<-states
# coef cen
coef_cen<-coef_cen %>% 
  select(b1, b2, b3)
coef_cen<-as.matrix(coef_cen)
rownames(coef_cen)<-states
# vcov 
vcov_list<-vector("list", length = length(states))
vcov_cen_list<-vector("list", length = length(states))

for (j in 1:length(states)) {
  #  Vcov
  vcov_list[[j]]<-vcov %>% 
    filter(abbrev_state == states[j])%>% 
    select(b1, b2, b3)
  vcov_list[[j]]<-as.matrix(vcov_list[[j]])
  rownames(vcov_list[[j]])<-c("b1", "b2", "b3")
  # cen Vcov
  vcov_cen_list[[j]]<-vcov_cen %>% 
    filter(abbrev_state == states[j])%>% 
    select(b1, b2, b3)
  vcov_cen_list[[j]]<-as.matrix(vcov_cen_list[[j]])
  rownames(vcov_cen_list[[j]])<-c("b1", "b2", "b3")
}

## Cross-basis Parametrization
# Number of lags, in days
nlag<-21
# Lag number of knots and placing of knots, on the log-scale equally spaced
lagnk <- 3; klag<-logknots(nlag,lagnk) #

## argument to the lag being modeled, 21 days of temperature before
# structure lag-response
arglag<-list(fun="ns",knots=klag,int=T)

## 2 knots equally spaced
knotsper<-equalknots(dengue_t2m$temp_mean, nk = 2)
argvar<-list(fun="ns",knots=knotsper,int=F)

tpred<-quantile(dengue_t2m$temp_mean, probs=(1:99)/100, na.rm=T)
range_cb<-c(min(dengue_t2m$temp_mean):max(dengue_t2m$temp_mean))
cb<-crossbasis(range_cb, lag=nlag, argvar=argvar, arglag=arglag)
blag <- do.call("onebasis",c(list(x=0:nlag),attr(cb,"arglag")))
bvar <- do.call("onebasis",c(list(x=range_cb),attr(cb,"argvar")))

## Meta-analysis
## 
mv<- mvmeta(coef~1,
            vcov_list,
            method="reml",
            control=list(showiter=T))
summary(mv)
## cen
mv_cen<- mvmeta(coef_cen~1,
                vcov_cen_list,
                method="reml",
                control=list(showiter=T))
summary(mv_cen)

## Predictions from the meta-analysis

# 3.1. Prediction overall without centering
##  non-cen
Metapred<-crosspred(basis=bvar,
                    coef=coef(mv),
                    vcov=vcov(mv),
                    at=tpred,
                    model.link="log")  
plot(Metapred)
## cen 
Metapred_cen<-crosspred(basis=bvar,
                        coef=coef(mv_cen),
                        vcov=vcov(mv_cen),
                        at=tpred,
                        model.link="log")  
plot(Metapred_cen)

# 3.2 Prediction overall centering mht
##  non-cen
(metaMHT<-Metapred$predvar[which.min(Metapred$allfit)])  
#MHT Remember this to be used on the next script, 03b_meta_lag_gnm_for_all.R
Metapred<-crosspred(basis=bvar,
                    coef=coef(mv),
                    vcov=vcov(mv),
                    cen=metaMHT,
                    at=tpred,
                    model.link="log")  #centering
plot(Metapred)
## cen 
(metaMHT_cen<-Metapred_cen$predvar[which.min(Metapred_cen$allfit)])  
#MHT Remember this to be used on the next script, 03b_meta_lag_gnm_for_all.R
Metapred_cen<-crosspred(basis=bvar,
                        coef=coef(mv_cen),
                        vcov=vcov(mv_cen),
                        cen=metaMHT_cen,
                        at=tpred,
                        model.link="log")  #centering
plot(Metapred_cen)

# 3.3. Prediction state-specific from the meta-analysis (fixef+ranef)
##  non-cen
blups<- blup(mv,vcov=TRUE)  
## cen 
blups_cen <- blup(mv_cen,vcov=TRUE)  

# 3.4 Prediction state-specific from the meta-analysis (only fixef)
##  non-cen
meta<-predict(mv, vcov=T, interval="confidence")  
## cen 
meta_cen<-predict(mv_cen, vcov=T, interval="confidence")  

## Comparison BLUPS, Meta and Original
source("functions/comparison_function.R")

##  non-cen
RR_list<-comparison_blup_meta_original(blups = blups, 
                                       meta = meta, 
                                       original = list(coef = coef, vcov = vcov_list), 
                                       names_stacked = states, 
                                       basis = bvar, 
                                       tpred = tpred, 
                                       data = dengue_t2m,
                                       var = 'temp_mean')
## cen 
RR_list_cen<-comparison_blup_meta_original(blups = blups_cen, 
                                           meta = meta_cen, 
                                           original = list(coef = coef_cen, vcov = vcov_cen_list), 
                                           names_stacked = states, 
                                           basis = bvar, 
                                           tpred = tpred,
                                           data = dengue_t2m, 
                                           var = 'temp_mean')
# Binding the RR_list
##  non-cen
RR_list<-RR_list %>% 
  bind_rows()

vroom_write(RR_list, file = "Outputs/Tables/meta_gnm_RR_overall_for_all.csv.xz")

## cen 
RR_list_cen<-RR_list_cen %>% 
  bind_rows()

vroom_write(RR_list_cen, file = "Outputs/Tables/meta_gnm_RR_overall_cen_for_all.csv.xz")
