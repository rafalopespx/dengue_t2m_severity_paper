## Clean the environment
rm(list=ls())
gc()

### Loading packages
source("Scripts/Sensitivity_addingcases/01_parametrizations.R")

## Objects to keep interest values
RRVal_lag_list<-vector("list", stacked_levels)
RR_overall_list<-vector("list", stacked_levels)
names(RRVal_lag_list)<-names_stacked
names(RR_overall_list)<-names_stacked

## Objects to keep the results
## Coefficients
coef_cen<-coef<-matrix(NA,
             stacked_levels,
             3,dimnames=list(names_stacked))
coef_q50<-coef_q95<-matrix(NA,
                           stacked_levels,
                           2+3,dimnames=list(names_stacked))

## Covariance Matrix
vcov<- vector("list",stacked_levels)
names(vcov) <- names_stacked
vcov_q50<-vcov_q95<-vcov_cen<-vcov

## Minimum Hospitalization Temperature vector
mht.gnm<-c()

## Looping for GNM Stacked analysis
for (i in 1:stacked_levels){
  
  cat("Starting state", states[i], "\n")
  
  ## Filtering for the cities within a state
  data<-dengue_t2m |> 
    filter(code_stacked == codes_stacked[i])|>
    arrange(code_muni, date) |>
    data.table::as.data.table()
  
  ## 7 days Moving Average
  data_ma<-dengue_confirmed |> 
    filter(code_stacked == codes_stacked[i])|>
    arrange(code_muni, date) |>
    data.table::as.data.table()
  
  ## Data final
  data<-data |> 
    left_join(data_ma)|> 
    mutate(confirmed = replace_na(confirmed, 0))|> 
    arrange(date) |>
    group_by(code_muni) |>
    mutate(cases_confirmed_7ma  = zoo::rollmean(confirmed, k = 7, fill = NA, align = 'right'),
           cases_confirmed_14ma = zoo::rollmean(confirmed, k = 14, fill = NA, align = 'right'))
  
  data <- data %>% ungroup()
  ## Taking out strata with no Cases, to avoid bias in gnm
  data <- data.table::as.data.table(data)
  data <- data[,  keep:=sum(Cases)>0, by=month_city_dow]
  # data <- data[keep!=0]
  
  knotsper<-equalknots(data$temp_mean, nk = 2) ## 
  varfun<-"ns"

  nlag<-21
  xlag<-0:nlag
  lagnk <- 3
  klag<-logknots(nlag,lagnk)
  lagfun<-"ns"

  argvar<-list(fun=varfun, knots=knotsper, int=F)
  arglag<-list(fun=lagfun, knots=klag,int=T)
  tpred_state<-quantile(data$temp_mean, probs=(1:99)/100, na.rm=T)
  range_cb<-c(min(data$temp_mean, na.rm = T):max(data$temp_mean, na.rm = T))
  cb <- crossbasis(data$temp_mean, lag=nlag, argvar=argvar, arglag=arglag, group = data$code_muni) 
  
  ## DLNM
  nyear<-length(unique(data$year))
  formula.gnm<-"Cases ~ cb + ns(date, df=7*nyear) + cases_confirmed_7ma"
  
  model.gnm<-tryCatch(gnm(as.formula(formula.gnm), 
                 eliminate = month_city_dow, 
                 data=data, 
                 # offset = log(cases_confirmed_7ma),
                 family = quasipoisson(link = "log"), 
                 na.action="na.exclude",
                 subset = keep), 
           error = function(err) NULL)
  
  if(is.null(model.gnm)){next}
  
  ## Cross-pred
  pred.gnm<-crosspred(cb,model.gnm, at=tpred_state) 
  
  #Reduced Prediction: overall
  red <- crossreduce(cb,model.gnm,at=tpred_state)
  coef[i,] <- red$coef
  vcov[[i]] <- red$vcov
  
  ### Centered Cross-pred
  mht.gnm[i]<-pred.gnm$predvar[which.min(pred.gnm$allRRfit)] 
  
  predcen.gnm<-crosspred(cb,model.gnm, at=tpred_state,cen=mht.gnm[i])
  
  #Reduced Prediction: overall
  red_cen <- crossreduce(cb,model.gnm, at=tpred_state, cen=mht.gnm[i])
  coef_cen[i,] <- red_cen$coef
  vcov_cen[[i]] <- red_cen$vcov
  
  ## For the lag metas analysis
  red_lag_50<-crossreduce(cb, model.gnm, 
                          type = "var", 
                          cen = 10.7, ## MHT for Brazil
                          value = tpred_state[50])
  coef_q50[i,]<-red_lag_50$coef
  vcov_q50[[i]]<-red_lag_50$vcov
  red_lag_95<-crossreduce(cb, model.gnm, 
                          type = "var", 
                          cen = 10.7, ## MHT for Brazil
                          value = tpred_state[95])
  coef_q95[i,]<-red_lag_95$coef
  vcov_q95[[i]]<-red_lag_95$vcov
  
  source("functions/effects_on_dlnm.R")
  RR_list<-effects_on_dlnm(predcen.gnm)
  
  RRVal_lag_list[[i]]<-RR_list$lag
  RR_overall_list[[i]]<-RR_list$overall
  
  gc()
  
}

## Binding objects to be saved
## Lag
RRVal_lag_list<-RRVal_lag_list %>% 
  bind_rows(.id = "abbrev_state")
## Overall
RR_overall_list <- RR_overall_list %>% 
  bind_rows(.id = "abbrev_state")

# Saving RR, overall and lags for each state
vroom_write(RRVal_lag_list, file = "Outputs/Tables/adding_cases/RRVal_lag_gnm.csv.xz")
vroom_write(RR_overall_list, file = "Outputs/Tables/adding_cases/RR_overall_gnm.csv.xz")

#Salving objects
## Coefficients Matrix
## Non-centered
coef_df<-coef %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "abbrev_state") %>% 
  setNames(c("abbrev_state", "b1", "b2", "b3"))

vroom_write(coef_df, file = "Outputs/Tables/adding_cases/coefficients_gnm_for_all.csv.xz")
## Centered
coef_df_cen<-coef_cen %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "abbrev_state") %>% 
  setNames(c("abbrev_state", "b1", "b2", "b3"))

vroom_write(coef_df_cen, file = "Outputs/Tables/adding_cases/coefficients_gnm_cen_for_all.csv.xz")

## q50
coef_df_q50<-coef_q50 %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "abbrev_state") %>% 
  setNames(c("abbrev_state", "b1", "b2", "b3"))

vroom_write(coef_df_q50, file = "Outputs/Tables/adding_cases/coefficients_gnm_q50_for_all.csv.xz")

## q95
coef_df_q95<-coef_q95 %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "abbrev_state") %>% 
  setNames(c("abbrev_state", "b1", "b2", "b3"))

vroom_write(coef_df_q95, file = "Outputs/Tables/adding_cases/coefficients_gnm_q95_for_all.csv.xz")

## Covariance Matrix
## Binding the Covariance Matrix
vcov_fun<-function(x){
  list_vcov<-lapply(x, function(x){
    x<-x %>% 
      as.data.frame() %>% 
      rownames_to_column(c("coef"))
  }) %>% 
    bind_rows(.id = "abbrev_state")
  return(list_vcov)
}
## Non-centered
vcov_df<-vcov_fun(vcov)
vroom_write(vcov_df, file = "Outputs/Tables/adding_cases/vcov_gnm_for_all.csv.xz")

## Centered
vcov_df_cen<-vcov_fun(vcov_cen)
vroom_write(vcov_df_cen, file = "Outputs/Tables/adding_cases/vcov_gnm_cen_for_all.csv.xz")

## vcov q50
vcov_df_q50<-vcov_fun(vcov_q50)
vroom_write(vcov_df_q50, file = "Outputs/Tables/adding_cases/vcov_gnm_q50_for_all.csv.xz")

## vcov q95
vcov_df_q95<-vcov_fun(vcov_q95)
vroom_write(vcov_df_q95, file = "Outputs/Tables/adding_cases/vcov_gnm_q95_for_all.csv.xz")


#
