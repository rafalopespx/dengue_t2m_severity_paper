rm(list=ls())
gc()

### Loading packages
source("Scripts/01_parametrizations.R")
# source("Scripts/Sensitivity_analysis/sa1&2_parametrization.R")

## Loading Coef and Vcov, in case to not re-run the model all again
## SA1
## Coef and Vcov for states
coef_q50_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_coefficients_gnm_q50_for_all.csv.xz")
vcov_q50_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_vcov_gnm_q50_for_all.csv.xz")
## Coef and Vcov for states
coef_q95_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_coefficients_gnm_q95_for_all.csv.xz")
vcov_q95_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_vcov_gnm_q95_for_all.csv.xz")
## SA2
## Coef and Vcov for states
coef_q50_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_coefficients_gnm_q50_for_all.csv.xz")
vcov_q50_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_vcov_gnm_q50_for_all.csv.xz")
## Coef and Vcov for states
coef_q95_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_coefficients_gnm_q95_for_all.csv.xz")
vcov_q95_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_vcov_gnm_q95_for_all.csv.xz")

# Running DLNMs over the cities
metaMHT<-10.7 ## Got from the 03a_meta_gnm_for_all.R script on the line, 
mmp_vector<-percentile_vector<-c(0.50, 0.95)
mhp_list<-vector("list", stacked_levels)
names(mhp_list)<-names_stacked
## SA1
RR_list_sa1<-vector("list", length(mmp_vector))
names(RR_list_sa1)<-mmp_vector

coef_list_sa1<-list(coef_q50_sa1, coef_q95_sa1)
vcov_list_sa1<-list(vcov_q50_sa1, vcov_q95_sa1)
## SA2
RR_list_sa2<-vector("list", length(mmp_vector))
names(RR_list_sa2)<-mmp_vector

coef_list_sa2<-list(coef_q50_sa2, coef_q95_sa2)
vcov_list_sa2<-list(vcov_q50_sa2, vcov_q95_sa2)

for (i in 1:length(percentile_vector)) {
  
  ## SA1
  coef_mv_sa1<-coef_list_sa1[[i]][,-1] %>% 
    setNames(c("b1", "b2", "b3", "b4", "b5"))
  coef_mv_sa1<-as.matrix(coef_mv_sa1)
  rownames(coef_mv_sa1)<-names_stacked
  ## SA2
  coef_mv_sa2<-coef_list_sa2[[i]][,-1] %>% 
    setNames(c("b1", "b2", "b3", "b4", "b5", "b6"))
  coef_mv_sa2<-as.matrix(coef_mv_sa2)
  rownames(coef_mv_sa2)<-names_stacked
  
  vcov_mv_sa1<-vcov_mv_sa2<-vector("list", length = length(states))
  # Looping over all the states on the i-th region
  for (l in 1:length(states)) {
    ## SA1
    vcov_mv_sa1[[l]]<-vcov_list_sa1[[i]] %>% 
      filter(abbrev_state == states[l])%>% 
      select(b1, b2, b3, b4, b5)
    vcov_mv_sa1[[l]]<-as.matrix(vcov_mv_sa1[[l]])
    rownames(vcov_mv_sa1[[l]])<-c("b1", "b2", "b3", "b4", "b5")
    ## SA2
    vcov_mv_sa2[[l]]<-vcov_list_sa2[[i]] %>% 
      filter(abbrev_state == states[l])%>% 
      select(b1, b2, b3, b4, b5, b6)
    vcov_mv_sa2[[l]]<-as.matrix(vcov_mv_sa2[[l]])
    rownames(vcov_mv_sa2[[l]])<-c("b1", "b2", "b3", "b4", "b5", "b6")
  }
  
  # Meta-analysis on each MMP
  mv_sa1<- mvmeta(coef_mv_sa1~1,
                  vcov_mv_sa1,
                  method="reml",
                  control=list(showiter=T))
  
  mv_sa2<- mvmeta(coef_mv_sa2~1,
                  vcov_mv_sa2,
                  method="reml",
                  control=list(showiter=T))
  
  # Predicction without centering, because we wanna see the effects by percentile centered
  Metapred_sa1<-crosspred(basis=blag_sa1,
                          coef=coef(mv_sa1),
                          vcov=vcov(mv_sa1), 
                          model.link="log")
  plot(Metapred_sa1)
  Metapred_sa2<-crosspred(basis=blag_sa2,
                          coef=coef(mv_sa2),
                          vcov=vcov(mv_sa2), 
                          model.link="log")
  plot(Metapred_sa2)
  # Storing data from the percentile centered lag effect
  RR_sa1<-data.frame(lag= Metapred_sa1$predvar, 
                     RR=Metapred_sa1$allRRfit,
                     LowRR=Metapred_sa1$allRRlow,
                     HighRR=Metapred_sa1$allRRhigh)
  RR_sa1$percentil<-percentile_vector[i]
  RR_list_sa1[[i]]<-RR_sa1
  
  RR_sa2<-data.frame(lag= Metapred_sa2$predvar, 
                     RR=Metapred_sa2$allRRfit,
                     LowRR=Metapred_sa2$allRRlow,
                     HighRR=Metapred_sa2$allRRhigh)
  RR_sa2$percentil<-percentile_vector[i]
  RR_list_sa2[[i]]<-RR_sa2
}

# Binding the rows for the RR list
RR_list_sa1<-RR_list_sa1 %>% 
  bind_rows()

# Salving the Meta-analysis RR list
vroom_write(RR_list_sa1, file = "Outputs/Tables/Sensitivity_analysis/Newrun/SA1_meta_RR_gnm_lags_for_all.csv.xz")

# Binding the rows for the RR list
RR_list_sa2<-RR_list_sa2 %>% 
  bind_rows()

# Salving the Meta-analysis RR list
vroom_write(RR_list_sa2, file = "Outputs/Tables/Sensitivity_analysis/Newrun/SA2_meta_RR_gnm_lags_for_all.csv.xz")

#
