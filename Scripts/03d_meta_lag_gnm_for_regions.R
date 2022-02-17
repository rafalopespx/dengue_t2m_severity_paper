rm(list=ls())
gc()

### Loading packages
source("Scripts/01_parametrizations.R")

## Percentile Coef and Vcov loading
percentile_vector<-c(0.05, 0.50, 0.95)

# Reading the coefficients matrix for the j-th percentile
coef_names<-list.files(path = "Outputs/Tables/", 
                       pattern = "coeffcients_gnm_meta_for_all_percentile_", full.names = T)
coef_list<-lapply(coef_names, function(x){
  x<-vroom(x)
}) %>% 
  bind_rows()

# Reading the Covariance Matrix for the j-th percentile
vcov_names<-list.files(path = "Outputs/Tables/", 
                       pattern = "vcov_gnm_meta_for_all_percentile_", full.names = T)
vcov_list<-lapply(vcov_names, function(x){
  x<-vroom(x)
})%>% 
  bind_rows()

# Meta by Region Variables
source("functions/functions.R")
regions<-names_stacked 
regions<-regions %>% 
  as.data.frame() %>% 
  setNames("abbrev_state")
regions<-regiao(regions, english = T)

regions_names<-unique(regions$region)

RR_list<-vector("list", length(percentile_vector))

# Lopping over each percentile j-th
for (j in 1:length(percentile_vector)) {
  # Coefficients for each percentile
  coef<-coef_list %>% 
    filter(percentile == percentile_vector[j])
  # Covariance matrix for each percentile
  vcov<-vcov_list %>% 
    filter(percentile == percentile_vector[j])
  
  # Looping over all the Regions on the j-th percentile
  for (i in 1:length(regions_names)) {
    # filtering the regions data.frame for the i-th region
    region_filter<-regions %>% 
      filter(region == regions_names[i])
    
    data_region<-dengue_t2m %>% 
      filter(abbrev_state %in% region_filter$abbrev_state)
    
    # Filtering Coef Matrix and VCOV matrix to the states for the region
    # coef by Region
    coef_region<-coef %>% 
      filter(abbrev_state %in% region_filter$abbrev_state) %>% 
      select(b1, b2, b3, b4, b5)
    coef_region<-as.matrix(coef_region)
    rownames(coef_region)<-region_filter$abbrev_state
    # vcov by Region
    states_region<-unique(region_filter$abbrev_state)
    vcov_region<-vector("list", length = length(states_region))
    # Looping over all the states on the i-th region
    for (l in 1:length(states_region)) {
      vcov_region[[l]]<-vcov %>% 
        filter(abbrev_state == states_region[l])%>% 
        select(b1, b2, b3, b4, b5)
      vcov_region[[l]]<-as.matrix(vcov_region[[l]])
      rownames(vcov_region[[l]])<-c("b1", "b2", "b3", "b4", "b5")
    }
    # Meta-analysis on each MMP
    mv<- mvmeta(coef_region~1,vcov_region,method="reml",control=list(showiter=T))
    
    # Predicciton without centering, because we wanna see the effects by percentile centered
    Metapred<-crosspred(basis=blag,coef=coef(mv),vcov=vcov(mv), model.link="log", cen = )
    plot(Metapred)
    
    # Storing data from the percentil centered lag effect
    RR<-data.frame(lag= Metapred$predvar, RR=Metapred$allRRfit,LowRR=Metapred$allRRlow,HighRR=Metapred$allRRhigh)
    RR$percentil<-percentile_vector[j]
    RR$region<-regions_names[i]
    RR_list[[j]][[i]]<-assign(paste0("RR_P", percentile_vector[j], sep=""), RR)
    
    # Saving RR for the i-th region in the j-th percentile
    vroom_write(RR_list[[j]][[i]], 
                file = paste0("Outputs/Tables/meta_RR_gnm_lags_region_", 
                              regions_names[i],
                              "_percentile_", 
                              percentile_vector[j], 
                              ".csv.xz"))
    
    cat("\t","Meta-analysis on",regions_names[i], "region finished!", "\t")
  }
  # Binding the Regions lists
  RR_list[[j]]<-RR_list[[j]] %>% 
    bind_rows()
  
  # Saving RR for all regions in the j-th percentile
  vroom_write(RR_list[[j]], 
              file = paste0("Outputs/Tables/meta_RR_gnm_lags_all_regions_percentile_", percentile_vector[j], ".csv.xz"))
  
  cat("\t","Meta-analysis on", percentile_vector[j], "percentile over all Regions finished!", "\t")
}

# Binding the rows for the RR list
RR_list<-RR_list %>% 
  bind_rows()

# Salving the Metanalysis RR list
vroom_write(RR_list, file = "Outputs/Tables/meta_RR_gnm_lags_all_regions_all_percentile.csv.xz")

#