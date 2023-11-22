rm(list=ls())
gc()

### Loading packages
source("Scripts/01_parametrizations.R")
# source("Scripts/Sensitivity_analysis/sa1&2_parametrization.R")

states<-names_stacked

## Loading Coef and Vcov, in case to not re-run the model all again
## Coef and Vcov SA1
coef_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_coefficients_gnm_for_all.csv.xz")
colnames(coef_sa1) <- c("abbrev_state", "b1", "b2", "b3", "b4")
vcov_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_vcov_gnm_for_all.csv.xz")

## Coef and Vcov SA2
coef_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_coefficients_gnm_for_all.csv.xz")
vcov_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_vcov_gnm_for_all.csv.xz")

# Putting coef and vcov in the right format again
# coef 
coef_fun<-function(x){
  x<-x %>% 
    select(-abbrev_state)
  x<-as.matrix(x)
  rownames(x)<-states
  return(x)
}
coef_sa1_mat<-coef_fun(coef_sa1)
coef_sa2_mat<-coef_fun(coef_sa2)

# vcov 
vcov_sa1_list<-vector("list", length = length(states))
vcov_sa2_list<-vector("list", length = length(states))

for (j in 1:length(states)) {
  # SA1 Vcov
  vcov_sa1_list[[j]]<-vcov_sa1 %>% 
    filter(abbrev_state == states[j])%>% 
    select(-abbrev_state, -coef)
  vcov_sa1_list[[j]]<-as.matrix(vcov_sa1_list[[j]])
  rownames(vcov_sa1_list[[j]])<-c("b1", "b2", "b3", "b4")
  # SA2 Vcov
  vcov_sa2_list[[j]]<-vcov_sa2 %>% 
    filter(abbrev_state == states[j])%>% 
    select(-abbrev_state, -coef)
  vcov_sa2_list[[j]]<-as.matrix(vcov_sa2_list[[j]])
  rownames(vcov_sa2_list[[j]])<-c("b1", "b2", "b3")
}

## Meta-analysis
## SA1
mv_sa1<- mvmeta(coef_sa1_mat~1,
            vcov_sa1_list,
            method="reml",
            control=list(showiter=T))
summary(mv_sa1)
## SA2
mv_sa2<- mvmeta(coef_sa2_mat~1,
                vcov_sa2_list,
                method="reml",
                control=list(showiter=T))
summary(mv_sa2)

## Predictions from the meta-analysis
# 3.1. Prediction overall without centering
dengue_t2m_means<-dengue_t2m %>%
  group_by(abbrev_state) %>%
  summarise(tmin = min(temp_mean),
            tmax = max(temp_mean)) %>%
  ungroup() %>%
  summarise(tmin = mean(tmin),
            tmax = mean(tmax))

## SA 1, 3 knots, equally distant
knotsper_sa1<-equalknots(dengue_t2m_means$tmin:dengue_t2m_means$tmax, 
                         nk = 3)
varfun_sa1<-"ns"

nlag_sa1<-21
xlag_sa1<-0:nlag_sa1
lagnk_sa1 <- 3
klag_sa1<-logknots(nlag_sa1,lagnk_sa1)
lagfun_sa1<-"ns"

#
argvar_sa1<-list(fun=varfun_sa1, 
             Bound = range(dengue_t2m$temp_mean, na.rm = T),
             knots=knotsper_sa1, 
             int=F)
arglag_sa1<-list(fun=lagfun_sa1, 
             knots=klag_sa1,
             int=T)

## Tpred
tpred<-quantile(dengue_t2m$temp_mean, probs=(1:99)/100, na.rm=T)

cb_sa1 <- crossbasis(tpred, 
                     lag=nlag_sa1, 
                     argvar=argvar_sa1, 
                     arglag=arglag_sa1)
bvar_sa1 <- do.call("onebasis",
                    c(list(x=tpred),
                      attr(cb_sa1,
                           "argvar")))
blag_sa1 <- do.call("onebasis",
                    c(list(x=xlag_sa1),
                      attr(cb_sa1,
                           "arglag")))

## SA1
Metapred_sa1<-crosspred(basis=bvar_sa1,
                    coef=coef(mv_sa1),
                    vcov=vcov(mv_sa1),
                    at=tpred,
                    model.link="log")  
plot(Metapred_sa1)

## SA 2, 2 knots, equally distant, but fixed knots for the lag
knotsper_sa2<-equalknots(dengue_t2m_means$tmin:dengue_t2m_means$tmax, 
                         nk = 2)
varfun_sa2<-"ns"

nlag_sa2<-21
xlag_sa2<-0:nlag_sa2
lagnk_sa2 <- 4
klag_sa2<-c(1,2,7,14)
lagfun_sa2<-"ns"

#
argvar_sa2<-list(fun=varfun_sa2, 
                 Bound = range(dengue_t2m$temp_mean, na.rm = T), 
                 knots=knotsper_sa2, 
                 int=F)
arglag_sa2<-list(fun=lagfun_sa2, 
                 knots=klag_sa2,
                 int=T)

## Tpred
tpred<-quantile(dengue_t2m$temp_mean, probs=(1:99)/100, na.rm=T)

cb_sa2 <- crossbasis(tpred, 
                     lag=nlag_sa2, 
                     argvar=argvar_sa2, 
                     arglag=arglag_sa2)
bvar_sa2 <- do.call("onebasis",
                    c(list(x=tpred),
                      attr(cb_sa2,
                           "argvar")))
blag_sa2 <- do.call("onebasis",
                    c(list(x=xlag_sa2),
                      attr(cb_sa2,
                           "arglag")))

## SA2
Metapred_sa2<-crosspred(basis=bvar_sa2,
                        coef=coef(mv_sa2),
                        vcov=vcov(mv_sa2),
                        at=tpred,
                        model.link="log")  
plot(Metapred_sa2)

# 3.2 Prediction overall centering mht
##  SA1
(metaMHT_sa1<-Metapred_sa1$predvar[which.min(Metapred_sa1$allfit)])  
#MHT Remember this to be used on the next script, 03b_meta_lag_gnm_for_all.R
Metapred_sa1<-crosspred(basis=bvar_sa1,
                    coef=coef(mv_sa1),
                    vcov=vcov(mv_sa1),
                    cen=metaMHT_sa1,
                    at=tpred,
                    model.link="log")  #centering
plot(Metapred_sa1)
## SA2
(metaMHT_sa2<-Metapred_sa2$predvar[which.min(Metapred_sa2$allfit)])  
#MHT Remember this to be used on the next script, 03b_meta_lag_gnm_for_all.R
Metapred_sa2<-crosspred(basis=bvar_sa2,
                        coef=coef(mv_sa2),
                        vcov=vcov(mv_sa2),
                        cen=metaMHT_sa2,
                        at=tpred,
                        model.link="log")  #centering
plot(Metapred_sa2)

## Results
##  SA1
res_sa1<-data.frame(temp_mean = Metapred_sa1$predvar, 
                RR=Metapred_sa1$allRRfit,
                LowRR=Metapred_sa1$allRRlow,
                HighRR=Metapred_sa1$allRRhigh)
## SA2
res_sa2<-data.frame(temp_mean = Metapred_sa2$predvar, 
                    RR=Metapred_sa2$allRRfit,
                    LowRR=Metapred_sa2$allRRlow,
                    HighRR=Metapred_sa2$allRRhigh)

## Salving the meta-analysis
##  SA1
vroom_write(res_sa1, file = "Outputs/Tables/Sensitivity_analysis/Newrun/SA1_meta_gnm_overall_for_all.csv.xz")
## cen 
vroom_write(res_sa2, file = "Outputs/Tables/Sensitivity_analysis/Newrun/SA2_meta_gnm_overall_for_all.csv.xz")
## MHT
MHT<-data.frame(MHT_sa1 = metaMHT_sa1, MHT_sa2 = metaMHT_sa2)
vroom_write(MHT, file = "Outputs/Tables/Sensitivity_analysis/Newrun/mht.csv.xz")

#

