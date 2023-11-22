rm(list=ls())
gc()

### Loading packages
source("Scripts/01_parametrizations.R")
source("Scripts/Sensitivity_analysis/sa1&2_parametrization.R")

## Percentile Coef and Vcov loading
percentile_vector<-c(0.50, 0.95)

## Loading Coef and Vcov, in case to not re-run the model all again
## Coef and Vcov for states
## SA1
coef_q50_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_coefficients_gnm_q50_for_all.csv.xz")
vcov_q50_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_vcov_gnm_q50_for_all.csv.xz")
coef_list_sa1<-list(coef_q50_sa1, coef_q95_sa1)
## SA2
coef_q50_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_coefficients_gnm_q50_for_all.csv.xz")
vcov_q50_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_vcov_gnm_q50_for_all.csv.xz")
coef_list_sa2<-list(coef_q50_sa2, coef_q95_sa2)
## Coef and Vcov for states
## SA1
coef_q95_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_coefficients_gnm_q95_for_all.csv.xz")
vcov_q95_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_vcov_gnm_q95_for_all.csv.xz")
vcov_list_sa1<-list(vcov_q50_sa1, vcov_q95_sa1)
## SA2
coef_q95_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_coefficients_gnm_q95_for_all.csv.xz")
vcov_q95_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_vcov_gnm_q95_for_all.csv.xz")
vcov_list_sa2<-list(vcov_q50_sa2, vcov_q95_sa2)

# Meta by Region Variables
source("functions/functions.R")
regions<-names_stacked 
regions<-regions %>% 
  as.data.frame() %>% 
  setNames("abbrev_state")
regions<-regiao(regions, english = T)

regions_names<-unique(regions$region)

RR_list_sa1<-RR_list_sa2<-vector("list", length(percentile_vector))

# Lopping over each percentile j-th
for (j in 1:length(percentile_vector)) {
  
  # Coefficients for each percentile
  coef_sa1<-coef_list_sa1[[j]]
  coef_sa2<-coef_list_sa2[[j]]
  # Covariance matrix for each percentile
  vcov_sa1<-vcov_list_sa1[[j]]
  vcov_sa2<-vcov_list_sa2[[j]]
  
  # Looping over all the Regions on the j-th percentile
  for (i in 1:length(regions_names)) {
    # filtering the regions data.frame for the i-th region
    region_filter<-regions %>% 
      filter(region == regions_names[i])
    
    data_region<-dengue_t2m %>%
      filter(abbrev_state %in% region_filter$abbrev_state)
    
    tpred_region<-quantile(data_region$temp_mean, probs=(1:99)/100, na.rm=T)
    
    cb_sa1 <- crossbasis(data_region$temp_mean, lag=nlag_sa1, argvar=argvar_sa1, arglag=arglag_sa1)
    blag_sa1 <- do.call("onebasis",c(list(x=xlag_sa1),attr(cb_sa1,"arglag")))
    cb_sa2 <- crossbasis(data_region$temp_mean, lag=nlag_sa2, argvar=argvar_sa2, arglag=arglag_sa2)
    blag_sa2 <- do.call("onebasis",c(list(x=xlag_sa2),attr(cb_sa2,"arglag")))
    
    # Filtering Coef Matrix and VCOV matrix to the states for the region
    # coef by Region
    ## SA1
    coef_region_sa1<-coef_sa1 %>% 
      filter(abbrev_state %in% region_filter$abbrev_state) %>% 
      select(-abbrev_state) %>% 
      setNames(c("b1", "b2", "b3", "b4", "b5"))
    coef_region_sa1<-as.matrix(coef_region_sa1)
    rownames(coef_region_sa1)<-region_filter$abbrev_state
    ## SA2
    coef_region_sa2<-coef_sa2 %>% 
      filter(abbrev_state %in% region_filter$abbrev_state) %>% 
      select(-abbrev_state) %>% 
      setNames(c("b1", "b2", "b3", "b4", "b5", "b6"))
    coef_region_sa2<-as.matrix(coef_region_sa2)
    rownames(coef_region_sa2)<-region_filter$abbrev_state
    # vcov by Region
    ## SA1
    states_region<-unique(region_filter$abbrev_state)
    vcov_region_sa1<-vcov_region_sa2<-vector("list", length = length(states_region))
    # Looping over all the states on the i-th region
    for (l in 1:length(states_region)) {
      vcov_region_sa1[[l]]<-vcov_sa1 %>% 
        filter(abbrev_state == states_region[l])%>% 
        select(b1, b2, b3, b4, b5)
      vcov_region_sa1[[l]]<-as.matrix(vcov_region_sa1[[l]])
      rownames(vcov_region_sa1[[l]])<-c("b1", "b2", "b3", "b4", "b5")
      
      vcov_region_sa2[[l]]<-vcov_sa2 %>% 
        filter(abbrev_state == states_region[l])%>% 
        select(b1, b2, b3, b4, b5, b6)
      vcov_region_sa2[[l]]<-as.matrix(vcov_region_sa2[[l]])
      rownames(vcov_region_sa2[[l]])<-c("b1", "b2", "b3", "b4", "b5", "b6")
    }
    # Meta-analysis on each MMP
    mv_sa1<- mvmeta(coef_region_sa1~1,
                    vcov_region_sa1,
                    method="reml",
                    control=list(showiter=T))
    
    mv_sa2<- mvmeta(coef_region_sa2~1,
                    vcov_region_sa2,
                    method="reml",
                    control=list(showiter=T))
    
    # Predicciton without centering, because we wanna see the effects by percentile centered
    Metapred_sa1<-crosspred(basis=blag_sa1,
                            coef=coef(mv_sa1),
                            vcov=vcov(mv_sa1), 
                            model.link="log")
    # plot(Metapred_sa1)
    
    Metapred_sa2<-crosspred(basis=blag_sa2,
                            coef=coef(mv_sa2),
                            vcov=vcov(mv_sa2), 
                            model.link="log")
    # plot(Metapred_sa2)
    
    # Storing data from the percentil centered lag effect
    RR_sa1<-data.frame(lag= Metapred_sa1$predvar, 
                       RR=Metapred_sa1$allRRfit,
                       LowRR=Metapred_sa1$allRRlow,
                       HighRR=Metapred_sa1$allRRhigh)
    RR_sa1$percentil<-percentile_vector[j]
    RR_sa1$region<-regions_names[i]
    RR_list_sa1[[j]][[i]]<-assign(paste0("RR_P", percentile_vector[j], sep=""), RR_sa1)
    
    RR_sa2<-data.frame(lag= Metapred_sa2$predvar, 
                       RR=Metapred_sa2$allRRfit,
                       LowRR=Metapred_sa2$allRRlow,
                       HighRR=Metapred_sa2$allRRhigh)
    RR_sa2$percentil<-percentile_vector[j]
    RR_sa2$region<-regions_names[i]
    RR_list_sa2[[j]][[i]]<-assign(paste0("RR_P", percentile_vector[j], sep=""), RR_sa2)
    
    # Saving RR for the i-th region in the j-th percentile
    ## SA1
    vroom_write(RR_list_sa1[[j]][[i]], 
                file = paste0("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_meta_RR_gnm_lags_region_", 
                              regions_names[i],
                              "_percentile_", 
                              percentile_vector[j], 
                              ".csv.xz"))
    ## SA2
    vroom_write(RR_list_sa2[[j]][[i]], 
                file = paste0("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_meta_RR_gnm_lags_region_", 
                              regions_names[i],
                              "_percentile_", 
                              percentile_vector[j], 
                              ".csv.xz"))
    
    cat("\t","Meta-analysis on",regions_names[i], "region finished!", "\t")
  }
  # Binding the Regions lists
  RR_list_sa1[[j]]<-RR_list_sa1[[j]] %>% 
    bind_rows()
  RR_list_sa2[[j]]<-RR_list_sa2[[j]] %>% 
    bind_rows()
  
  # Saving RR for all regions in the j-th percentile
  vroom_write(RR_list_sa1[[j]], 
              file = paste0("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_meta_RR_gnm_lags_all_regions_percentile_", 
                            percentile_vector[j], ".csv.xz"))
  vroom_write(RR_list_sa2[[j]], 
              file = paste0("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_meta_RR_gnm_lags_all_regions_percentile_", 
                            percentile_vector[j], ".csv.xz"))
  
  cat("\t","Meta-analysis on", percentile_vector[j], "percentile over all Regions finished!", "\t")
  
  gc()
}

# Binding the rows for the RR list
RR_list_sa1<-RR_list_sa1 %>% 
  bind_rows()
RR_list_sa2<-RR_list_sa2 %>% 
  bind_rows()

# Salving the Metanalysis RR list
vroom_write(RR_list_sa1, 
            file = "Outputs/Tables/Sensitivity_analysis/Newrun/SA1_meta_RR_gnm_lags_all_regions_all_percentile.csv.xz")
vroom_write(RR_list_sa2, 
            file = "Outputs/Tables/Sensitivity_analysis/Newrun/SA2_meta_RR_gnm_lags_all_regions_all_percentile.csv.xz")

#