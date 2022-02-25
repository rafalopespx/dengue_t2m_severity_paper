rm(list=ls())
gc()

source("Scripts/00_database_load.R")
source("Scripts/Sensitivity_analysis/sa1&2_parametrization.R")

## Loading Coef and Vcov, in case to not re-run the model all again
## Coef and Vcov SA1
coef_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/SA1_coefficients_gnm_for_all.csv.xz")
vcov_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/SA1_vcov_gnm_for_all.csv.xz")

## Coef and Vcov SA2
coef_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/SA2_coefficients_gnm_for_all.csv.xz")
vcov_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/SA2_vcov_gnm_for_all.csv.xz")

source("functions/functions.R")
regions<-names_stacked 
regions<-regions %>% 
  as.data.frame() %>% 
  setNames("abbrev_state")
regions<-regiao(regions, english = T)

regions_names<-unique(regions$region)

## Objects to keep results
res_region_sa1<-res_region_sa2<-vector("list", 5)
metaMHT_region_sa1<-metaMHT_region_sa2<-c()
RR_region_sa1<-RR_region_sa2<-vector("list", 5)
RR_list_region_sa1<-RR_list_region_sa2<-vector("list", 5)

for (i in 1:5) {
  region_filter<-regions %>% 
    filter(region == regions_names[i])
  
  data_region<-dengue_t2m %>% 
    filter(abbrev_state %in% region_filter$abbrev_state)
  
  tpred_region<-quantile(data_region$temp_mean, probs=(1:99)/100, na.rm=T)
  
  ## SA1 Crossbasis parametrization
  knotsper_sa1<-equalknots(data_region$temp_mean, nk = 3)
  cb_sa1 <- crossbasis(data_region$temp_mean, lag=nlag_sa1, argvar=argvar_sa1, arglag=arglag_sa1)
  bvar_sa1 <- do.call("onebasis",c(list(x=tpred_region),attr(cb_sa1,"argvar")))
  
  ## SA2 Crossbasis parametrization
  knotsper_sa2<-equalknots(data_region$temp_mean, nk = 2)
  cb_sa2 <- crossbasis(data_region$temp_mean, lag=nlag_sa2, argvar=argvar_sa2, arglag=arglag_sa2)
  bvar_sa2 <- do.call("onebasis",c(list(x=tpred_region),attr(cb_sa2,"argvar")))
  
  # Filtering Coef Matrix and VCOV matrix to the states for the region
  # coef
  coef_function<-function(x){
    x<- x%>% 
      filter(abbrev_state %in% region_filter$abbrev_state) %>% 
      select(-abbrev_state)
    x<-as.matrix(x)
    rownames(x)<-region_filter$abbrev_state
    return(x)
  }
  ## 
  coef_region_sa1<-coef_function(coef_sa1)
  coef_region_sa2<-coef_function(coef_sa2)
  
  # vcov
  vcov_function<-function(x, sa){
    vcov_list<-vector("list", length = length(states_region))
    for (j in 1:length(states_region)) {
      vcov_list[[j]]<-x %>% 
        filter(abbrev_state == states_region[j])%>% 
        select(-abbrev_state, -coef)
      vcov_list[[j]]<-as.matrix(vcov_list[[j]])
      if(sa==1){
        rownames(vcov_list[[j]])<-c("b1", "b2", "b3", "b4")
      }
      if(sa==2){
        rownames(vcov_list[[j]])<-c("b1", "b2", "b3")
      }
    }
    return(vcov_list)
  }
  states_region<-unique(region_filter$abbrev_state)
  ## 
  vcov_region_sa1<-vcov_function(vcov_sa1, sa = 1)
  vcov_region_sa2<-vcov_function(vcov_sa2, sa = 2)
  
  ## Meta-analysis
  ### 
  mv_region_sa1<- mvmeta(coef_region_sa1~1,
                         vcov_region_sa1,
                         method="reml",
                         control=list(showiter=T))
  mv_region_sa2<- mvmeta(coef_region_sa2~1,
                         vcov_region_sa2,
                         method="reml",
                         control=list(showiter=T))
  
  ## Predictions from the meta-analysis
  # 3.1. Prediction overall without centering
  ## FUNCIONA, BASICAMENTE LEMBRAR QUE A BVAR TEM QUE PUXAR O PARAMETRO ARGVAR E NAO ARGVAR_SA1 OU SA2
  Metapred_region_sa1<-crosspred(basis=bvar_sa1,
                                 coef=coef(mv_region_sa1),
                                 vcov=vcov(mv_region_sa1),
                                 at=tpred_region,
                                 model.link="log")
  plot(Metapred_region_sa1)
  
  
  Metapred_region_sa2<-crosspred(basis=bvar_sa2,
                                 coef=coef(mv_region_sa2),
                                 vcov=vcov(mv_region_sa2),
                                 at=tpred_region,
                                 model.link="log")
  plot(Metapred_region_sa2)
  
  # 3.2 Prediction overall centering mht
  ## SA1
  (metaMHT_region_sa1[i]<-Metapred_region_sa1$predvar[which.min(Metapred_region_sa1$allfit)])  #MHT    
  Metapred_region_sa1<-crosspred(basis=bvar_sa1,
                             coef=coef(mv_region_sa1),
                             vcov=vcov(mv_region_sa1),
                             cen=metaMHT_region_sa1[i],
                             at=tpred_region,
                             model.link="log")  #centering
  plot(Metapred_region_sa1)
  
  
  (metaMHT_region_sa2[i]<-Metapred_region_sa2$predvar[which.min(Metapred_region_sa2$allfit)])  #MHT    
  Metapred_region_sa2<-crosspred(basis=bvar_sa2,
                                 coef=coef(mv_region_sa2),
                                 vcov=vcov(mv_region_sa2),
                                 cen=metaMHT_region_sa2[i],
                                 at=tpred_region,
                                 model.link="log")  #centering
  plot(Metapred_region_sa2)
  ## Results
  ### SA1 
  res_region_sa1[[i]]<-data.frame(temp_mean = Metapred_region_sa1$predvar, 
                              RR=Metapred_region_sa1$allRRfit,
                              LowRR=Metapred_region_sa1$allRRlow,
                              HighRR=Metapred_region_sa1$allRRhigh)
  res_region_sa1[[i]]$region<-regions_names[i]
  
  ## SA2
  res_region_sa2[[i]]<-data.frame(temp_mean = Metapred_region_sa2$predvar, 
                                  RR=Metapred_region_sa2$allRRfit,
                                  LowRR=Metapred_region_sa2$allRRlow,
                                  HighRR=Metapred_region_sa2$allRRhigh)
  res_region_sa2[[i]]$region<-regions_names[i]
  
  ## Salving the metanalysis
  ### 
  vroom_write(res_region_sa1[[i]], 
              file = paste0("Outputs/Tables/Sensitivity_analysis/SA1_meta_gnm_overall_region_", regions_names[i],".csv.xz"))
  
  
  vroom_write(res_region_sa2[[i]], 
              file = paste0("Outputs/Tables/Sensitivity_analysis/SA2_meta_gnm_overall_region_", regions_names[i],".csv.xz"))
  
  gc()
}

# Binding by Regions
## 
res_region_sa1<-res_region_sa1 %>% 
  bind_rows()
res_region_sa2<-res_region_sa2 %>% 
  bind_rows()

# Saving
vroom_write(data.frame(MHT = metaMHT_region_sa1, region = regions_names), 
            file = "Outputs/Tables/Sensitivity_analysis/SA1_metamht_regions.csv.xz")

vroom_write(res_region_sa1, 
            file = "Outputs/Tables/Sensitivity_analysis/SA1_meta_gnm_overall_all_regions.csv.xz")

vroom_write(data.frame(MHT = metaMHT_region_sa2, region = regions_names), 
            file = "Outputs/Tables/Sensitivity_analysis/SA2_metamht_regions.csv.xz")

vroom_write(res_region_sa2, 
            file = "Outputs/Tables/Sensitivity_analysis/SA2_meta_gnm_overall_all_regions.csv.xz")

#