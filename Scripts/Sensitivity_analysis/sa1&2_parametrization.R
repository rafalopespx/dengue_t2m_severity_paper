## Cros-basis parametrization
# Defining basis and grid
dengue_t2m_means<-dengue_t2m %>% 
  group_by(abbrev_state) %>% 
  summarise(tmin = min(temp_mean), 
            tmax = max(temp_mean)) %>%
  ungroup() %>% 
  summarise(tmin = mean(tmin), 
            tmax = mean(tmax))

## SA1 Crossbasis parametrization
knotsper_sa1<-equalknots(dengue_t2m_means$tmin:dengue_t2m_means$tmax, nk = 3)
varfun_sa1<-"ns"

nlag_sa1<-21
xlag_sa1<-0:nlag_sa1
lagnk_sa1 <- 3
klag_sa1<-logknots(nlag_sa1,lagnk_sa1)
lagfun_sa1<-"ns"

argvar_sa1<-list(fun=varfun_sa1, knots=knotsper_sa1, int=F)
arglag_sa1<-list(fun=lagfun_sa1, knots=klag_sa1,int=T)
tpred_sa1<-quantile(dengue_t2m$temp_mean, probs=(1:99)/100, na.rm=T)
range_cb_sa1<-c(min(dengue_t2m$temp_mean):max(dengue_t2m$temp_mean))
cb_sa1 <- crossbasis(dengue_t2m$temp_mean, lag=nlag_sa1, argvar=argvar_sa1, arglag=arglag_sa1)
bvar_sa1 <- do.call("onebasis",c(list(x=dengue_t2m$temp_mean),attr(cb_sa1,"argvar_sa1")))
blag_sa1 <- do.call("onebasis",c(list(x=xlag_sa1),attr(cb_sa1,"arglag_sa1")))

## SA2 Crossbasis parametrization
knotsper_sa2<-equalknots(dengue_t2m_means$tmin:dengue_t2m_means$tmax, nk = 2)
varfun_sa2<-"ns"

nlag_sa2<-21
xlag_sa2<-0:nlag_sa2
klag_sa2<-c(1,2,7,14)
lagfun_sa2<-"ns"

argvar_sa2<-list(fun=varfun_sa2, knots=knotsper_sa2, int=F)
arglag_sa2<-list(fun=lagfun_sa2, knots=klag_sa2,int=T)
tpred_sa2<-quantile(dengue_t2m$temp_mean, probs=(1:99)/100, na.rm=T)
range_cb_sa2<-c(min(dengue_t2m$temp_mean):max(dengue_t2m$temp_mean))
cb_sa2 <- crossbasis(dengue_t2m$temp_mean, lag=nlag_sa2, argvar=argvar_sa2, arglag=arglag_sa2)
bvar_sa2 <- do.call("onebasis",c(list(x=dengue_t2m$temp_mean),attr(cb_sa2,"argvar_sa2")))
blag_sa2 <- do.call("onebasis",c(list(x=xlag_sa2),attr(cb_sa2,"arglag_sa2")))
