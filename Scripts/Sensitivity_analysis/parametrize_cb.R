parametrize_cb<-function(x, all = T, bvar_only, blag_only, tpred_region){
  
  ## SA1 Crossbasis parametrization
  source("Scripts/Sensitivity_analysis/sa1_parametrizations.R")
  cb_sa1 <- crossbasis(x$temp_mean, lag=nlag, argvar=argvar, arglag=arglag)
  bvar_sa1 <- do.call("onebasis",c(list(x=tpred_region),attr(cb_sa1,"argvar")))
  blag_sa1 <- do.call("onebasis",c(list(x=xlag),attr(cb_sa1,"arglag")))
  
  ## SA2 Crossbasis parametrization
  source("Scripts/Sensitivity_analysis/sa2_parametrizations.R")
  cb_sa2 <- crossbasis(x$temp_mean, lag=nlag, argvar=argvar, arglag=arglag)
  bvar_sa2 <- do.call("onebasis",c(list(x=tpred_region),attr(cb_sa2,"argvar")))
  blag_sa2 <- do.call("onebasis",c(list(x=xlag),attr(cb_sa2,"arglag")))
  
  
  if(all){
    return(list(cb_sa1, bvar_sa1, blag_sa2, cb_sa2, bvar_sa1, blag_sa2))
  }
  if(bvar_only){
    return(list(bvar_sa1, bvar_sa2))
  }
  if(blag_only){
    return(list(blag_sa1, blag_sa2))
  }
  
}
