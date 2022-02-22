rm(list=ls())
gc()

### Loading packages
source("Scripts/01_parametrizations.R")

## Loading Coef and Vcov, in case to not re-run the model all again
## Coef and Vcov for states
coef<-vroom("Outputs/Tables/coefficients_gnm_for_all.csv.xz")
vcov<-vroom("Outputs/Tables/vcov_gnm_for_all.csv.xz")

source("functions/functions.R")
regions<-names_stacked 
regions<-regions %>% 
  as.data.frame() %>% 
  setNames("abbrev_state")
regions<-regiao(regions, english = T)

regions_names<-unique(regions$region)

## Objects to keep results
res_region<-vector("list", 5)
metaMHT_region<-c()
RR_region<-vector("list", 5)
RR_list_region<-vector("list", 5)

for (i in 1:5) {
  region_filter<-regions %>% 
    filter(region == regions_names[i])
  
  data_region<-dengue_t2m %>% 
    filter(abbrev_state %in% region_filter$abbrev_state)
  
  tpred_region<-quantile(data_region$temp_mean, probs=(1:99)/100, na.rm=T)
  
  cb_region<-crossbasis(data_region$temp_mean, lag=nlag, argvar = argvar, arglag = arglag)
  bvar_region<-do.call("onebasis", c(list(x=tpred_region), attr(cb_region, "argvar")))
  
  # Filtering Coef Matrix and VCOV matrix to the states for the region
  # coef
  coef_function<-function(x){
    x<- x%>% 
      filter(abbrev_state %in% region_filter$abbrev_state) %>% 
      select(b1, b2, b3)
    x<-as.matrix(x)
    rownames(x)<-region_filter$abbrev_state
    return(x)
  }
  ## 
  coef_region<-coef_function(coef)
  
  # vcov
  vcov_function<-function(x){
    vcov_list<-vector("list", length = length(states_region))
    for (j in 1:length(states_region)) {
      vcov_list[[j]]<-x %>% 
        filter(abbrev_state == states_region[j])%>% 
        select(b1, b2, b3)
      vcov_list[[j]]<-as.matrix(vcov_list[[j]])
      rownames(vcov_list[[j]])<-c("b1", "b2", "b3")
    }
    return(vcov_list)
  }
  states_region<-unique(region_filter$abbrev_state)
  ## 
  vcov_region<-vcov_function(vcov)
  
  ## Meta-analysis
  ### 
  mv_region<- mvmeta(coef_region~1,
                     vcov_region,
                     method="reml",
                     control=list(showiter=T))
  
  ## Predictions from the meta-analysis
  # 3.1. Prediction overall without centering
  ## 
  Metapred_region<-crosspred(basis=bvar_region,
                             coef=coef(mv_region),
                             vcov=vcov(mv_region),
                             at=tpred_region,
                             model.link="log")
  plot(Metapred_region)
  
  # 3.2 Prediction overall centering mht
  ## 
  (metaMHT_region[i]<-Metapred_region$predvar[which.min(Metapred_region$allfit)])  #MHT    
  Metapred_region<-crosspred(basis=bvar_region,
                             coef=coef(mv_region),
                             vcov=vcov(mv_region),
                             cen=metaMHT_region[i],
                             at=tpred_region,
                             model.link="log")  #centering
  plot(Metapred_region)
  ## Results
  ### 
  res_region[[i]]<-data.frame(temp_mean = Metapred_region$predvar, 
                              RR=Metapred_region$allRRfit,
                              LowRR=Metapred_region$allRRlow,
                              HighRR=Metapred_region$allRRhigh)
  res_region[[i]]$region<-regions_names[i]
  
  ## Salving the metanalysis
  ### 
  vroom_write(res_region[[i]], 
              file = paste0("Outputs/Tables/meta_gnm_overall_region_", regions_names[i],".csv.xz"))
  
  gc()
}

# Binding by Regions
## 
res_region<-res_region %>% 
  bind_rows()

# Saving
vroom_write(data.frame(MHT = metaMHT_region, region = regions_names), 
            file = "Outputs/Tables/metamht_regions.csv.xz")

vroom_write(res_region, 
            file = "Outputs/Tables/meta_gnm_overall_all_regions.csv.xz")

#