
effects_on_dlnm<-function(predcen.gnm){
  #Getting the effects of hot and cold
  # percentil_min<-50
  # percentil_max<-95
  
  RR<-as.data.frame(predcen.gnm$matRRfit)
  RR$idE<-seq(1,99,1)
  RR_low<-as.data.frame(predcen.gnm$matRRlow)
  RR_low$idE<-seq(1,99,1)
  RR_high<-as.data.frame(predcen.gnm$matRRhigh)
  RR_high$idE<-seq(1,99,1)
  
  f1<- function(x) { as.numeric(str_sub(x, length(x))) }
  
  RR_df<-rownames_to_column(RR, "lag") %>% 
    mutate_at(1, f1)
  RR_low_df<-rownames_to_column(RR_low, "lag") %>% 
    mutate_at(1, f1)
  RR_high_df<-rownames_to_column(RR_high, "lag") %>% 
    mutate_at(1, f1)
  
  RRVal_lag.gnm<-bind_rows(RR_df, RR_low_df, RR_high_df)%>%
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