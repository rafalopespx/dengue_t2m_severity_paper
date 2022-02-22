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
RRVal_lag_list <- vroom("Outputs/Tables/RRVal_lag_gnm.csv.xz")
RR_overall_list <- vroom("Outputs/Tables/RR_overall_gnm.csv.xz")
## Quantiles t2m
quantiles_t2m<-vroom("Outputs/Tables/quantiles_temp_mean.csv.xz")

## Plot looping
plot_final<-vector("list", 27)
plot_list<-vector("list", 27)
names_stacked_plot<-unique(RR_overall_list$abbrev_state)
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
  RR_lag<-RRVal_lag_list %>% 
    filter(abbrev_state == names_stacked_plot[i] & idE %in% c(50, 95))
  
  RR_overall<-RR_overall_list %>% 
    filter(abbrev_state == names_stacked_plot[i])
  
  ## RR 95% t2m dist
  ## 
  RR_95_05_overall[[i]]<-RR_overall %>% 
    filter(temp_mean >= quantile_state$q05) %>% 
    filter(temp_mean <= quantile_state$q95)
  
  # Plot RR Lag
  ## 
  ylab<-pretty(c(RR_lag$LowRR,RR_lag$HighRR))
  
  plot_rr_lag<-RR_lag %>% 
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
  
  # Plot RR Overall
  ## Absolute
  xlab<-pretty(RR_overall$temp_mean)
  ylab<-pretty(c(RR_overall$LowRR,RR_overall$HighRR))
  mht<-RR_overall$temp_mean[which.min(RR_overall$RR)]
  
  plot_rr_overall<-RR_overall %>% 
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
  
  plot_list[[i]]<-list(rr_lag = plot_rr_lag, 
                       rr_overall = plot_rr_overall)
  
  # Patchwork Plot to be saved
  plot_final[[i]]<-(plot_rr_lag | plot_rr_overall)+
    plot_layout(guides = "collect")
  
  # PAY ATTENTION TO THE NAME OF FILES YOU ARE SAVING, 
  # MAKE SURE IT IS THE CORRECT, TO NOT SUBSTITUTE FOR AN EXISTING ONE!
  ggsave(paste0("Outputs/Plots/gnm_", names_stacked_plot[i], "_state_overall_effects.png"),
         plot = plot_final[[i]],
         width = 9,
         height = 7,
         dpi = 300)
}

## RR 95% t2m
## Relative
RR_95_05_overall_relative<-RR_95_05_overall_relative %>% 
  bind_rows()

vroom_write(file = "Outputs/Tables/RR_95_05_t2m_overall_relative.csv.xz",
            RR_95_05_overall_relative)
## Aboslute
RR_95_05_overall<-RR_95_05_overall %>% 
  bind_rows()

vroom_write(file = "Outputs/Tables/RR_95_05_t2m_overall.csv.xz", 
            RR_95_05_overall)

## Overall Brasil Plot
## Absolute
MHT_states<-RR_overall_list %>% 
  filter(RR == 1) %>% 
  select(temp_mean, abbrev_state)

xlab<-pretty(RR_overall_list$temp_mean)
# ylab<-pretty(c(RR_overall_list$LowRR, RR_overall_list$HighRR))
plot_overall_state<-RR_overall_list %>% 
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

plot_overall_state

ggsave(paste0("Outputs/Plots/gnm_all_state_overall_effects.png"),
       plot = plot_overall_state,
       width = 9,
       height = 7,
       dpi = 300)

## Cold effects on lag, 5th percentile
## Absolute
cold_5th_val_lag<-RRVal_lag_list %>% 
  filter(idE == "50")

# ylab<-pretty(c(cold_5th_val_lag$LowRR,
#                cold_5th_val_lag$HighRR))

plot_rr_lag_5th<-cold_5th_val_lag %>% 
  ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
  geom_line(aes(y=1, x = lag))+
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = factor(idE)),
                 size = .8, show.legend = FALSE) +
  geom_point(shape = 21, fill = "white", size = 2, aes(colour = factor(idE)),
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  # scale_y_continuous(breaks = ylab) +
  scale_colour_manual(values = c("#4575b4")) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ", 
       title = "Cold (5th) effects on lags", 
       subtitle = "All States, 2010-2019", 
       caption = "On the Absolute Scale")+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")+
  theme_minimal()
plot_rr_lag_5th

ggsave(paste0("Outputs/Plots/gnm_all_state_on_50th_effects.png"),
       plot = plot_rr_lag_5th,
       width = 9,
       height = 7,
       dpi = 300)

## Heat effects on lag, 95th percentile
## Absolute
heat_95th_val_lag<-RRVal_lag_list %>% 
  filter(idE == "95")

# ylab<-pretty(c(heat_95th_val_lag$LowRR,
#                heat_95th_val_lag$HighRR))

plot_rr_lag_95th<-heat_95th_val_lag %>% 
  ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
  geom_line(aes(y=1, x = lag))+
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = factor(idE)),
                 size = .8, show.legend = FALSE) +
  geom_point(shape = 21, fill = "white", size = 2, aes(colour = factor(idE)),
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  # scale_y_continuous(breaks = ylab) +
  scale_colour_manual(values = c("#d73027")) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ", 
       title = "Heat (95th) effects on lags", 
       subtitle = "All States, 2010-2019", caption = "On the Absolute Scale")+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")+
  theme_minimal()
plot_rr_lag_95th


ggsave(paste0("Outputs/Plots/gnm_scale_all_state_heat_95th_effects.png"),
       plot = plot_rr_lag_95th,
       width = 9,
       height = 7,
       dpi = 300)

#