rm(list=ls())
gc()

### Loading packages
source("Scripts/01_parametrizations.R")

## number of lags knots
lagnk<-3

# Storing variance matrices and covariance matrices from crossbasis
coef<-matrix(NA,
             stacked_levels,
             2+lagnk,
             dimnames=list(names_stacked)) 
vcov<- vector("list",stacked_levels)
names(vcov) <-  names_stacked

# Running DLNMs over the cities
metaMHT<-10.7 ## Got from the 03a_meta_gnm_for_all.R script on the line, 
mmp_vector<-percentile_vector<-c(0.05, 0.50, 0.95)
mhp_list<-vector("list", stacked_levels)
names(mhp_list)<-names_stacked
RR_list<-vector("list", length(mmp_vector))
names(RR_list)<-mmp_vector

coef_list<-vector("list", length = length(mmp_vector))
names(coef_list)<-percentile_vector
vcov_list<-vector("list", length = length(mmp_vector))
names(vcov_list)<-percentile_vector

#   # Looping for each state
for(i in 1:stacked_levels) {
  #extracting the data
  data<-dengue_t2m %>% 
    filter(code_stacked == codes_stacked[i])
  
  knotsper<-equalknots(data$temp_mean, nk = 2)
  varfun<-"ns"
  
  nlag<-21
  xlag<-0:nlag
  lagnk <- 3
  klag<-logknots(nlag,lagnk)
  lagfun<-"ns"
  
  argvar<-list(fun=varfun, knots=knotsper, int=F)
  arglag<-list(fun=lagfun, knots=klag,int=T)
  tpred_state<-quantile(data$temp_mean, probs=(1:99)/100, na.rm=T)
  # range_cb<-c(min(data$temp_mean):max(data$temp_mean))
  cb <- crossbasis(data$temp_mean, lag=nlag, argvar=argvar, arglag=arglag)
  
  nyear<-length(unique(data$year))
  formula.gnm<-"Cases ~ cb+dow+ns(date, df=7*nyear)"
  
  # Model fitting
  model.gnm<-gnm(as.formula(formula.gnm), 
                 eliminate = month_city, 
                 data=data, 
                 family = quasipoisson, 
                 na.action="na.exclude")
  
  for (j in 1:length(percentile_vector)) {
    data_value<-quantile(data$temp_mean, probs=percentile_vector[j], na.rm=T)
    
    # Prediction reduced lag-distributed on each Percentile and centered in mht
    red <- crossreduce(cb, model.gnm, type = "var", cen = metaMHT, value = data_value)
    mhp_list[[i]]  <- which.min(red$RRfit)
    
    coef[i,]  <- red$coef
    vcov[[i]] <- red$vcov
    
    if(i==27){
      coef_list[[j]][[i]]<-coef
    }
    vcov_list[[j]][[i]]<-vcov[[i]] 
  }
  
  cat("\t","DLNM on",names_stacked[i], "finished!", "\t")
  gc()
}

#Salving objects
## Coefficients Matrix as Data.Frame
coef_df<-lapply(coef_list, function(x){
  y<-x[[27]]
  x<-y
  x<-x %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "abbrev_state") %>% 
    setNames(c("abbrev_state", "b1", "b2", "b3", "b4", "b5"))
}) %>% 
  bind_rows(.id = "percentile")

## Saving all df

vroom_write(coef_df, file = "Outputs/Tables/coeffcients_gnm_meta_for_all_percentile.csv.xz")

## Covariance Matrix
## Binding the Covariance Matrix as Data.Frame
vcov_df<-lapply(vcov_list, function(vcov){
  names(vcov)<-names_stacked
  vcov_df<-lapply(vcov, function(x){
    x<-x %>% 
      as.data.frame() %>% 
      rownames_to_column(c("coef"))
    return(x)
  }
  ) %>% 
    bind_rows(.id = "abbrev_state")
}) %>% 
  bind_rows(.id = "percentile")

vroom_write(vcov_df, file = "Outputs/Tables/vcov_gnm_meta_for_all_percentile.csv.xz")

RR_list<-vector("list", length = length(percentile_vector))

for (i in 1:length(percentile_vector)) {
  
  coef_mv<-coef_list[[i]][[27]]
  vcov_mv<-vcov_list[[i]]
  
  # Meta-analysis on each MMP
  mv<- mvmeta(coef_mv~1,vcov_mv,method="reml",control=list(showiter=T))
  
  # Predicction without centering, because we wanna see the effects by percentile centered
  Metapred<-crosspred(basis=blag,coef=coef(mv),vcov=vcov(mv), model.link="log")
  
  # Storing data from the percentile centered lag effect
  RR<-data.frame(lag= Metapred$predvar, 
                 RR=Metapred$allRRfit,
                 LowRR=Metapred$allRRlow,
                 HighRR=Metapred$allRRhigh)
  RR$percentil<-percentile_vector[i]
  RR_list[[i]]<-RR
}


# Binding the rows for the RR list
RR_list<-RR_list %>% 
  bind_rows()

# Salving the Meta-analysis RR list
vroom_write(RR_list, file = "Outputs/Tables/meta_RR_gnm_lags_for_all.csv.xz")

## To save each percentile run individually
# for (i in 1:length(percentile_vector)) {
#   x<-coef_df %>% 
#     filter(percentile == percentile_vector[i])
#   
#   # Saving the coefficients matrix for the j-th percentile
#   vroom_write(x,
#               file = paste0("Outputs/Tables/coeffcients_gnm_meta_for_all_percentile_",
#                             percentile_vector[i],
#                             ".csv.xz"))
#   
#   y<-vcov_df %>% 
#     filter(percentile == percentile_vector[i])
#   
#   # Saving the Covariance Matrix for the j-th percentile
#   vroom_write(y,
#               file = paste0("Outputs/Tables/vcov_gnm_meta_for_all_percentile_",
#                             percentile_vector[i],
#                             ".csv.xz"))
# }

#
