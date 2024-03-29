rm(list=ls())
gc()

### Loading packages
source("Scripts/01_parametrizations.R")

## TIRAR CENTERED VCOV AND COEF
states<-names_stacked

## Loading Coef and Vcov, in case to not re-run the model all again
## Coef and Vcov for states
coef<-vroom("Outputs/Tables/New_run/coefficients_gnm_for_all.csv.xz")
vcov<-vroom("Outputs/Tables/New_run/vcov_gnm_for_all.csv.xz")

## Coef and Vcov for states, CENTERED
coef_cen<-vroom("Outputs/Tables/New_run/coefficients_gnm_cen_for_all.csv.xz")
vcov_cen<-vroom("Outputs/Tables/New_run/vcov_gnm_cen_for_all.csv.xz")

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
dengue_t2m_means<-dengue_t2m %>%
  group_by(abbrev_state) %>%
  summarise(tmin = min(temp_mean),
            tmax = max(temp_mean)) %>%
  ungroup() %>%
  summarise(tmin = mean(tmin),
            tmax = mean(tmax))

knotsper<-equalknots(dengue_t2m_means$tmin:dengue_t2m_means$tmax, nk = 2)
varfun<-"ns"

nlag<-21
xlag<-0:nlag
lagnk <- 3
klag<-logknots(nlag,lagnk)
lagfun<-"ns"
#
argvar<-list(fun=varfun, 
             Bound = range(dengue_t2m$temp_mean, na.rm = T), 
             knots=knotsper, int=F)
arglag<-list(fun=lagfun, 
             knots=klag,int=T)
tpred<-quantile(dengue_t2m$temp_mean, probs=(1:99)/100, na.rm=T)

cb <- crossbasis(tpred, lag=nlag, argvar=argvar, arglag=arglag)
bvar <- do.call("onebasis",c(list(x=tpred),attr(cb,"argvar")))
blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))

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

## Results
##  non-cen
res<-data.frame(temp_mean = Metapred$predvar, 
                RR=Metapred$allRRfit,
                LowRR=Metapred$allRRlow,
                HighRR=Metapred$allRRhigh)
## cen 
res_cen<-data.frame(temp_mean = Metapred_cen$predvar, 
                    RR=Metapred_cen$allRRfit,
                    LowRR=Metapred_cen$allRRlow,
                    HighRR=Metapred_cen$allRRhigh)

## Salving the meta-analysis
##  non-cen
vroom_write(res, file = "Outputs/Tables/New_run/meta_gnm_overall_for_all.csv.xz")
## cen 
vroom_write(res_cen, file = "Outputs/Tables/New_run/meta_gnm_overall_cen_for_all.csv.xz")
## MHT
MHT<-data.frame(MHT_non_cen = metaMHT, MHT_cen = metaMHT)
vroom_write(MHT, file = "Outputs/Tables/New_run/mht.csv.xz")

#

