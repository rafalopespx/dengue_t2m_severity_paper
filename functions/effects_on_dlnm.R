
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
  
  RR_df<-rownames_to_column(RR, "temp_mean")%>% 
    mutate(temp_mean = round(as.numeric(temp_mean), 2)) %>% 
    pivot_longer(cols = -c("temp_mean", "idE"), names_to = "lag", values_to = "RR") %>% 
    mutate(lag = as.numeric(gsub(lag, pattern = "lag", replacement = "")))
  RR_low_df<-rownames_to_column(RR_low, "temp_mean")%>% 
    mutate(temp_mean = round(as.numeric(temp_mean), 2)) %>% 
    pivot_longer(cols = -c("temp_mean", "idE"), names_to = "lag", values_to = "LowRR") %>% 
    mutate(lag = as.numeric(gsub(lag, pattern = "lag", replacement = "")))
  RR_high_df<-rownames_to_column(RR_high, "temp_mean")%>% 
    mutate(temp_mean = round(as.numeric(temp_mean), 2)) %>% 
    pivot_longer(cols = -c("temp_mean", "idE"), names_to = "lag", values_to = "HighRR") %>% 
    mutate(lag = as.numeric(gsub(lag, pattern = "lag", replacement = "")))
  
  RRVal_lag.gnm<-RR_df %>% 
    left_join(RR_low_df, by = c("temp_mean", "idE", "lag")) %>% 
    left_join(RR_high_df, by = c("temp_mean", "idE", "lag"))
  
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