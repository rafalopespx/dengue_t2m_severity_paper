rm(list=ls())
gc()

### Loading packages
source("Scripts/01_parametrizations.R")

## Loading Coef and Vcov, in case to not re-run the model all again
## Coef and Vcov for states
coef_q50<-vroom("Outputs/Tables/coefficients_gnm_q50_for_all.csv.xz")
vcov_q50<-vroom("Outputs/Tables/vcov_gnm_q50_for_all.csv.xz")
## Coef and Vcov for states
coef_q95<-vroom("Outputs/Tables/coefficients_gnm_q95_for_all.csv.xz")
vcov_q95<-vroom("Outputs/Tables/vcov_gnm_q95_for_all.csv.xz")

# Running DLNMs over the cities
metaMHT<-10.7 ## Got from the 03a_meta_gnm_for_all.R script on the line, 
mmp_vector<-percentile_vector<-c(0.50, 0.95)
mhp_list<-vector("list", stacked_levels)
names(mhp_list)<-names_stacked
RR_list<-vector("list", length(mmp_vector))
names(RR_list)<-mmp_vector

coef_list<-list(coef_q50, coef_q95)
vcov_list<-list(vcov_q50, vcov_q95)

RR_list<-vector("list", length = length(percentile_vector))

for (i in 1:length(percentile_vector)) {
  
  coef_mv<-coef_list[[i]][,-1] %>% 
    setNames(c("b1", "b2", "b3", "b4", "b5"))
  coef_mv<-as.matrix(coef_mv)
  rownames(coef_mv)<-names_stacked
  
  vcov_mv<-vector("list", length = length(states))
  # Looping over all the states on the i-th region
  for (l in 1:length(states)) {
    vcov_mv[[l]]<-vcov_list[[i]] %>% 
      filter(abbrev_state == states[l])%>% 
      select(b1, b2, b3, b4, b5)
    vcov_mv[[l]]<-as.matrix(vcov_mv[[l]])
    rownames(vcov_mv[[l]])<-c("b1", "b2", "b3", "b4", "b5")
  }
  
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
