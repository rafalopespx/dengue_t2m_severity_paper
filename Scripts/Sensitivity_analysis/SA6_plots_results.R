rm(list=ls())
gc()

### Loading packages
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}
if(!require(geofacet)){install.packages("geofacet"); library(geofacet)}

# Loading databases
## Lag and Overall
## SA1
RRVal_lag_list_sa1 <- vroom("Outputs/Tables/Sensitivity_analysis/SA1_RRVal_lag_gnm.csv.xz")
RR_overall_list_sa1 <- vroom("Outputs/Tables/Sensitivity_analysis/SA1_RR_overall_gnm.csv.xz")
## SA2
RRVal_lag_list_sa2 <- vroom("Outputs/Tables/Sensitivity_analysis/SA2_RRVal_lag_gnm.csv.xz")
RR_overall_list_sa2 <- vroom("Outputs/Tables/Sensitivity_analysis/SA2_RR_overall_gnm.csv.xz")

## Quantiles t2m
quantiles_t2m<-vroom("Outputs/Tables/quantiles_temp_mean.csv.xz")

## Plot looping
plot_final<-vector("list", 27)
plot_list<-vector("list", 27)
names_stacked_plot<-unique(RR_overall_list_sa1$abbrev_state)
stacked_levels<-length(names_stacked_plot)

## RR 95% t2m
RR_95_05_overall<-vector("list", stacked_levels)
names(RR_95_05_overall)<-names_stacked_plot

for (i in 1:stacked_levels) {
  quantile_state<-quantiles_t2m %>% 
    filter(abbrev_state == names_stacked_plot[i]) %>% 
    as.data.frame()
  # creating plot_list
  ## 
  RR_lag_sa1<-RRVal_lag_list_sa1 %>% 
    filter(abbrev_state == names_stacked_plot[i] & idE %in% c(50, 95))
  RR_lag_sa2<-RRVal_lag_list_sa2 %>% 
    filter(abbrev_state == names_stacked_plot[i] & idE %in% c(50, 95))
  
  RR_overall_sa1<-RR_overall_list_sa1 %>% 
    filter(abbrev_state == names_stacked_plot[i])
  RR_overall_sa2<-RR_overall_list_sa2 %>% 
    filter(abbrev_state == names_stacked_plot[i])
  
  ## RR 95% t2m dist
  ## 
  RR_95_05_overall[[i]]$sa1<-RR_overall_sa1 %>% 
    filter(temp_mean >= quantile_state$q05) %>% 
    filter(temp_mean <= quantile_state$q95)
  RR_95_05_overall[[i]]$sa2<-RR_overall_sa2 %>% 
    filter(temp_mean >= quantile_state$q05) %>% 
    filter(temp_mean <= quantile_state$q95)
  
  # Plot RR Lag
  ## SA1
  ylab<-pretty(c(RR_lag_sa1$LowRR,RR_lag_sa1$HighRR))
  
  plot_rr_lag_sa1<-RR_lag_sa1 %>% 
    ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
    geom_hline(yintercept = 1, size = 0.25) +
    geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = as.factor(idE)),
                   size = .8, show.legend = FALSE) +
    geom_point(shape = 21, fill = "white", size = 2, aes(colour = as.factor(idE)),
               show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0, 21, 2)) +
    # scale_y_continuous(breaks = ylab) +
    scale_colour_manual(values = c( "#4575b4","#d73027")) +
    labs(x = "lag (days)", y = "Dengue Hosp. RR ", title = names_stacked_plot[i])+
    facet_wrap(vars(idE),nrow=2)+
    theme_minimal()
  
  ## SA2
  ylab<-pretty(c(RR_lag_sa2$LowRR,RR_lag_sa2$HighRR))
  
  plot_rr_lag_sa2<-plot_rr_lag_sa1 %+% RR_lag_sa2
  
  # Plot RR Overall
  ## SA1
  xlab<-pretty(RR_overall_sa1$temp_mean)
  ylab<-pretty(c(RR_overall_sa1$LowRR,RR_overall_sa1$HighRR))
  mht<-RR_overall_sa1$temp_mean[which.min(RR_overall_sa1$RR)]
  
  plot_rr_overall_sa1<-RR_overall_sa1 %>% 
    ggplot(aes(temp_mean, RR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_vline(xintercept = c(quantile_state$q05,quantile_state$q95), size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
    geom_ribbon(aes(ymin = LowRR, 
                    ymax = HighRR),
                fill="grey80",alpha=0.2) +
    geom_line(colour="#cb181d",size=1) +
    geom_point(aes(x = mht, y = 1),shape = 21, fill = "white", size = 2, colour="#cb181d",
               show.legend = FALSE) +
    scale_x_continuous(breaks = xlab) +
    scale_y_continuous(breaks = ylab) +
    theme_minimal()+
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Mean Temperature [ºC]", y = "Dengue Hosp. RR", title = names_stacked_plot[i])
  
  ## SA2
  xlab<-pretty(RR_overall_sa2$temp_mean)
  ylab<-pretty(c(RR_overall_sa2$LowRR,RR_overall_sa1$HighRR))
  mht<-RR_overall_sa2$temp_mean[which.min(RR_overall_sa2$RR)]
  
  plot_rr_overall_sa2<-plot_rr_overall_sa1 %+% RR_overall_sa2
  
  plot_list[[i]]$sa1<-list(rr_lag = plot_rr_lag_sa1, 
                       rr_overall = plot_rr_overall_sa1)
  plot_list[[i]]$sa2<-list(rr_lag = plot_rr_lag_sa2, 
                           rr_overall = plot_rr_overall_sa2)
  
  # Patchwork Plot to be saved
  plot_final[[i]]$sa1<-(plot_rr_lag_sa1 | plot_rr_overall_sa1)+
    plot_layout(guides = "collect")
  plot_final[[i]]$sa2<-(plot_rr_lag_sa2 | plot_rr_overall_sa2)+
    plot_layout(guides = "collect")
  
  # PAY ATTENTION TO THE NAME OF FILES YOU ARE SAVING, 
  # MAKE SURE IT IS THE CORRECT, TO NOT SUBSTITUTE FOR AN EXISTING ONE!
  ggsave(paste0("Outputs/Plots/Sensitivity_analysis/SA1_gnm_", names_stacked_plot[i], "_state_overall_effects.png"),
         plot = plot_final[[i]]$sa1,
         width = 9,
         height = 7,
         dpi = 300)
  ggsave(paste0("Outputs/Plots/Sensitivity_analysis/SA2_gnm_", names_stacked_plot[i], "_state_overall_effects.png"),
         plot = plot_final[[i]]$sa2,
         width = 9,
         height = 7,
         dpi = 300)
}

## Precisa manejar as listas pra ter elas separadas
## RR 95% t2m
RR_95_05_overall<-RR_95_05_overall %>% 
  purrr::transpose() %>% 
  map(bind_rows)
## sa1
RR_95_05_overall_sa1<-RR_95_05_overall$sa1

vroom_write(file = "Outputs/Tables/Sensitivity_analysis/SA1_RR_95_05_t2m_overall.csv.xz",
            RR_95_05_overall_sa1)
## sa2
RR_95_05_overall_sa2<-RR_95_05_overall$sa2

vroom_write(file = "Outputs/Tables/Sensitivity_analysis/SA2_RR_95_05_t2m_overall.csv.xz", 
            RR_95_05_overall_sa2)

## Overall Brasil Plot
## sa1
MHT_states_sa1<-RR_overall_list_sa1 %>% 
  filter(RR == 1) %>% 
  select(temp_mean, abbrev_state)
## sa2
MHT_states_sa2<-RR_overall_list_sa2 %>% 
  filter(RR == 1) %>% 
  select(temp_mean, abbrev_state)

## sa1
xlab<-pretty(RR_overall_list_sa1$temp_mean)

plot_overall_state_sa1<-RR_overall_list_sa1 %>% 
  ggplot(aes(temp_mean, RR)) + 
  geom_line(aes(y=1, x=temp_mean))+
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(data = MHT_states_sa1, aes(temp_mean,1),
             shape = 21,
             fill = "white",
             size = 2,
             colour="#cb181d",
             show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Mean Temperature [ºC]", 
       y = "Dengue Hosp. RR",
       title="Temperature and Dengue Hospitalization",
       subtitle=paste0("Overall by State, 2010-2019"))+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")

plot_overall_state_sa1

ggsave(paste0("Outputs/Plots/Sensitivity_analysis/SA1_gnm_all_state_overall_effects.png"),
       plot = plot_overall_state_sa1,
       width = 9,
       height = 7,
       dpi = 300)

## sa2
xlab<-pretty(RR_overall_list_sa2$temp_mean)

plot_overall_state_sa2<-RR_overall_list_sa2 %>% 
  ggplot(aes(temp_mean, RR)) + 
  geom_line(aes(y=1, x=temp_mean))+
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(data = MHT_states_sa2, aes(temp_mean,1),
             shape = 21,
             fill = "white",
             size = 2,
             colour="#cb181d",
             show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Mean Temperature [ºC]", 
       y = "Dengue Hosp. RR",
       title="Temperature and Dengue Hospitalization",
       subtitle=paste0("Overall by State, 2010-2019"))+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")

plot_overall_state_sa2

ggsave(paste0("Outputs/Plots/Sensitivity_analysis/SA2_gnm_all_state_overall_effects.png"),
       plot = plot_overall_state_sa2,
       width = 9,
       height = 7,
       dpi = 300)

## Cold effects on lag, 5th percentile
## sa1
cold_5th_val_lag<-RRVal_lag_list_sa1 %>% 
  filter(idE == "50")

plot_rr_lag_5th_sa1<-cold_5th_val_lag %>% 
  ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
  geom_line(aes(y=1, x = lag))+
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = factor(idE)),
                 size = .8, show.legend = FALSE) +
  geom_point(shape = 21, fill = "white", size = 2, aes(colour = factor(idE)),
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  scale_colour_manual(values = c("#4575b4")) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ", 
       title = "Cold (5th) effects on lags", 
       subtitle = "All States, 2010-2019", 
       caption = "On the sa1 Scale")+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")+
  theme_minimal()
plot_rr_lag_5th_sa1

ggsave(paste0("Outputs/Plots/Sensitivity_analysis/SA1_gnm_all_state_on_50th_effects.png"),
       plot = plot_rr_lag_5th_sa1,
       width = 9,
       height = 7,
       dpi = 300)

## SA2
cold_5th_val_lag<-RRVal_lag_list_sa2 %>% 
  filter(idE == "50")

plot_rr_lag_5th_sa2<-cold_5th_val_lag %>% 
  ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
  geom_line(aes(y=1, x = lag))+
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = factor(idE)),
                 size = .8, show.legend = FALSE) +
  geom_point(shape = 21, fill = "white", size = 2, aes(colour = factor(idE)),
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  scale_colour_manual(values = c("#4575b4")) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ", 
       title = "Cold (5th) effects on lags", 
       subtitle = "All States, 2010-2019", 
       caption = "On the sa2 Scale")+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")+
  theme_minimal()

plot_rr_lag_5th_sa2

ggsave(paste0("Outputs/Plots/Sensitivity_analysis/SA2_gnm_all_state_on_50th_effects.png"),
       plot = plot_rr_lag_5th_sa2,
       width = 9,
       height = 7,
       dpi = 300)

## Heat effects on lag, 95th percentile
## sa2
heat_95th_val_lag<-RRVal_lag_list_sa1 %>% 
  filter(idE == "95")

plot_rr_lag_95th_sa1<-heat_95th_val_lag %>% 
  ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
  geom_line(aes(y=1, x = lag))+
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = factor(idE)),
                 size = .8, show.legend = FALSE) +
  geom_point(shape = 21, fill = "white", size = 2, aes(colour = factor(idE)),
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  scale_colour_manual(values = c("#d73027")) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ", 
       title = "Heat (95th) effects on lags", 
       subtitle = "All States, 2010-2019", caption = "On the sa2 Scale")+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")+
  theme_minimal()
plot_rr_lag_95th_sa1

ggsave(paste0("Outputs/Plots/Sensitivity_analysis/SA1_gnm_scale_all_state_heat_95th_effects.png"),
       plot = plot_rr_lag_95th_sa1,
       width = 9,
       height = 7,
       dpi = 300)


## SA2
heat_95th_val_lag<-RRVal_lag_list_sa2 %>% 
  filter(idE == "95")

plot_rr_lag_95th_sa2<-heat_95th_val_lag %>% 
  ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
  geom_line(aes(y=1, x = lag))+
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = factor(idE)),
                 size = .8, show.legend = FALSE) +
  geom_point(shape = 21, fill = "white", size = 2, aes(colour = factor(idE)),
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  scale_colour_manual(values = c("#d73027")) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ", 
       title = "Heat (95th) effects on lags", 
       subtitle = "All States, 2010-2019", caption = "On the sa2 Scale")+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")+
  theme_minimal()

plot_rr_lag_95th_sa2

ggsave(paste0("Outputs/Plots/Sensitivity_analysis/SA2_gnm_scale_all_state_heat_95th_effects.png"),
       plot = plot_rr_lag_95th_sa2,
       width = 9,
       height = 7,
       dpi = 300)


#