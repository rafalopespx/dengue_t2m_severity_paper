RRVal_lag_list |> 
  ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
  geom_line(aes(y=1, x = lag))+
  geom_linerange(aes(x = lag, y = RR,
                ymin = LowRR, ymax = HighRR,
                colour = idE),
                 size = .8, show.legend = FALSE) +
  geom_point(shape = 21, fill = "white", size = 2, aes(colour = idE),
             show.legend = TRUE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  # scale_y_continuous(breaks = ylab) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ", 
       title = "Effects on lags", 
       subtitle = "All States, 2010-2019", 
       caption = "On the Absolute Scale")+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")+
  theme_minimal()+
  theme(legend.position = "bottom")

MHT_states<-RR_overall_list |> 
  filter(RR == 1)


RR_overall_list |> 
  ggplot(aes(temp_mean, RR)) + 
  geom_line(aes(y=1, x=temp_mean))+
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(data = MHT_states, aes(temp_mean,1),
             shape = 21,
             fill = "white",
             size = 2,
             colour="#cb181d",
             show.legend = FALSE) +
  # scale_x_continuous(breaks = xlab) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Mean Temperature [ºC]", 
       y = "Dengue Hosp. RR",
       title="Temperature and Dengue Hospitalization",
       subtitle=paste0("Overall by State, 2010-2019"))+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")
