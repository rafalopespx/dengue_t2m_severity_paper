
effects_on_dlnm<-function(percentil_min, percentil_max, predcen.gnm){
  #Getting the effects of hot and cold
  # percentil_min<-50
  # percentil_max<-95
  
  percentil_df<-lapply(percentil_values, function(x){
    y<-data.frame(RR=predcen.gnm$matRRfit[percentil_values,], )
  })
  
  percentil_min_df.gnm<-data.frame(RR=predcen.gnm$matRRfit[percentil_min,],
                                   LowRR=predcen.gnm$matRRlow[percentil_min,],
                                   HighRR=predcen.gnm$matRRhigh[percentil_min,],idE="50th")
  percentil_max_df.gnm<-data.frame(RR=predcen.gnm$matRRfit[percentil_max,],
                                   LowRR=predcen.gnm$matRRlow[percentil_max,],
                                   HighRR=predcen.gnm$matRRhigh[percentil_max,],idE="95th")
  
  f1<- function(x) { as.numeric(str_sub(x, 4)) }
  
  percentil_min_df.gnm<-rownames_to_column(percentil_min_df.gnm, "lag")%>% 
    mutate_at(1, f1) 
  percentil_max_df.gnm<-rownames_to_column(percentil_max_df.gnm, "lag")%>% 
    mutate_at(1, f1)  
  
  RRVal_lag.gnm<-bind_rows(percentil_min_df.gnm,percentil_max_df.gnm)%>%
    mutate_at(5,"factor")
  
  RRVal_lag <- 
    bind_rows(
      RRVal_lag.gnm %>% mutate(model = "GNM")
    ) %>% 
    mutate(model = factor(model)) 
  
  #RR total
  RR_overall.gnm<-data.frame(RR=predcen.gnm$allRRfit,
                             LowRR=predcen.gnm$allRRlow,
                             HighRR=predcen.gnm$allRRhigh)
  
  
  RR_overall.gnm<-rownames_to_column(RR_overall.gnm, "temp_mean") %>% 
    mutate_at(1, as.numeric)
  
  RR_overall <- 
    bind_rows(
      RR_overall.gnm %>% mutate(model = "GNM")
    ) %>% 
    mutate(model = factor(model))
  
  return(list(overall = RR_overall, lag = RRVal_lag))
}

#