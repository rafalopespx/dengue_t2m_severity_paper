#' Title
#'
#' @param blups 
#' @param meta 
#' @param original 
#' @param basis 
#' @param names_stacked 
#' @param save 
#' @param names_files 
#' @param tpred 
#' @param data 
#' @param var 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
comparison_blup_meta_original<-function(blups, 
                                        meta, 
                                        original, 
                                        basis,
                                        names_stacked,
                                        save = FALSE,
                                        names_files,
                                        tpred,
                                        data,
                                        var,
                                        ...){
  # Requiring the packages needed
  require(dlnm)
  require(dplyr)
  require(vroom)
  
  ## Stopping triggers
  if(missing(blups) & missing(meta) & missing(original)){
    stop("Nothing to do here!!!")
  }
  if(missing(basis)){
    stop("'basis' is missing, \t
         it can be a 'cross-basis', a 'onebasis' for variable and/or a 'onebasis' for the lag")
  }
  if(!is.list(blups)){
    stop("'Blups' is not a list!")
  }
  if(!is.list(meta)){
    stop("'Meta' is not a list!")
  }
  if(!is.list(original)){
    stop("'Original' is not a list!")
  }
  if(!is.list(original$vcov)){
    stop("'Vcov' is not a list!")
  }
  if(missing(tpred) && missing(data)){
    stop("'tpred' and 'data' missing, please give one of them")
  }
  ## Warning triggers
  if(missing(var)){
    warning("'var' is missing, using as relative scale")
  }
  if(missing(tpred)){
    warning("'tpred' missing! Constructing on data and assuming relative scale")
  }
  if(missing(blups)){
    warning("Warning: 'BLUPS' is missing!")
    blups<-NA
  }
  if(missing(meta)){
    warning("Warning: 'Meta' is missing!")
    meta<-NA
  }
  if(missing(original)){
    warning("Warning: 'Original' is missing!")
    original<-NA
  }
  
  ## Procedures if not all parameters are setted
  if(missing(names_stacked)){
    stacked_levels<-length(blups)
    names_stacked<-1:length(blups)
  }
  stacked_levels<-length(names_stacked)
  
  ## Comparison BLUPS, Meta and Original
  RR_list<-vector("list", stacked_levels)
  
  for(i in 1:stacked_levels){
    if(!missing(data)){
      data_filtered<-data %>% 
        filter(abbrev_state == names_stacked[i]) %>% 
        select({{var}}, abbrev_state)
      range_data<-range(data_filtered[,1])
      tpred<-quantile(data_filtered[,1], probs=(1:99)/100, na.rm=T)
    } else {
      tpred<-tpred_relative<-(1:99)/100
      message("Using 'tpred' in relative scale")
    } 
    
    # Pred by BLUPS
    predB<-crosspred(basis=basis,
                     coef=blups[[i]]$blup,
                     vcov=blups[[i]]$vcov,
                     # from = range_data[1], ## Can be used instead at, create an option to choose
                     # to = range_data[2],
                     at = tpred,
                     model.link="log")
    # Pred by Original
    predO<-crosspred(basis=basis,
                     coef=original$coef[i,],
                     vcov=original$vcov[[i]],
                     # from = range_data[1], ## Can be used instead at, create an option to choose
                     # to = range_data[2],
                     at = tpred,
                     model.link="log")
    # Pred by Meta-analysis
    predM<-crosspred(basis=basis,
                     coef=meta[[i]]$fit,
                     vcov=meta[[i]]$vcov,
                     # from = range_data[1], ## Can be used instead at, create an option to choose
                     # to = range_data[2],
                     at = tpred,
                     model.link="log")
    
    # Minimum Hospitalizations Temperatures
    ## BLUPS MHT
    (MHTBlups<-predB$predvar[which.min(predB$allfit)])
    print(paste0("MHT for BLUPS is:", round(MHTBlups, 2)))
    ## Original MHT
    (MHTOriginal<-predO$predvar[which.min(predO$allfit)])
    print(paste0("MHT for Original is:", round(MHTOriginal, 2)))
    ## Meta MHT
    (MHTMeta<-predM$predvar[which.min(predM$allfit)])
    print(paste0("MHT for Meta is:", round(MHTMeta, 2)))
    
    # Centering each crosspred on MHTs
    predB<-crosspred(basis=basis,
                     coef=blups[[i]]$blup,
                     vcov=blups[[i]]$vcov,
                     cen=MHTBlups,
                     # from = range_data[1], ## Can be used instead at, create an option to choose
                     # to = range_data[2],
                     at = tpred,
                     model.link="log")
    predO<-crosspred(basis=basis,
                     coef=original$coef[i,],
                     vcov=original$vcov[[i]],
                     cen=MHTOriginal,
                     # from = range_data[1], ## Can be used instead at, create an option to choose
                     # to = range_data[2],
                     at = tpred,
                     model.link="log")
    predM<-crosspred(basis=basis,
                     coef=meta[[i]]$fit,
                     vcov=meta[[i]]$vcov,
                     cen=MHTMeta,
                     # from = range_data[1], ## Can be used instead at, create an option to choose
                     # to = range_data[2],
                     at = tpred,
                     model.link="log")
    
    # Dataframes with each RR for each way of prediction
    ## Original DF
    RR_O_i<-data.frame(predvar=predO$predvar, 
                       RR=predO$allRRfit,
                       LowRR=predO$allRRlow,
                       HighRR=predO$allRRhigh, 
                       state=names_stacked[i], 
                       typepred="original")
    RR_B_i<-data.frame(predvar=predB$predvar, 
                       RR=predB$allRRfit,
                       LowRR=predB$allRRlow,
                       HighRR=predB$allRRhigh, 
                       state=names_stacked[i],
                       typepred="blups")
    RR_M_i<-data.frame(predvar=predM$predvar, 
                       RR=predM$allRRfit,
                       LowRR=predM$allRRlow,
                       HighRR=predM$allRRhigh, 
                       state=names_stacked[i],
                       typepred="meta")
    # Relative Risks and CI by lags
    RR_list[[i]]<-bind_rows(RR_O_i,RR_B_i,RR_M_i)%>%
      mutate_at(5:6,"factor") # Certifying that the state column and type of prediction are factors.
  }
  
  # Binding the RR_list
  RR_list<-RR_list %>% 
    bind_rows()
  
  if(save == TRUE){
    if(missing(names_files)){
      wd<-getwd()
      warning("Using as saving name for the files, 'blups_meta_original.csv.xz', \t
              in the actual Working Directory")
      names_files<-"blups_meta_original"
      vroom_write(RR_list, 
                  file = paste0(wd, names_files, ".csv.xz"))
    }
    vroom_write(RR_list, 
                file = paste0("Outputs/Tables/", names_files, ".csv.xz"))
    }
  return(RR_list)
}


