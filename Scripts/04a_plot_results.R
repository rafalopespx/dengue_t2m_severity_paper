rm(list=ls())

### Loading packages
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(tsibble)){install.packages("tsibble"); library(tsibble)}
if(!require(dlnm)){install.packages("dlnm"); library(dlnm)}
if(!require(splines)){install.packages("splines"); library(splines)}
if(!require(tibble)){install.packages("tibble"); library(tibble)}
if(!require(stringr)){install.packages("stringr"); library(stringr)}
if(!require(geofacet)){install.packages("geofacet"); library(geofacet)}

# setwd("~/Desktop/Dengue_severity/")

# Loading databases
## Relative
RRVal_lag_relative_list <- vroom("Outputs/Tables/RRVal_lag_gnm_relative_scale.csv.xz")
RR_overall_relative_list <- vroom("Outputs/Tables/RR_overall_gnm_relative_scale.csv.xz")
## Absolute
RRVal_lag_absolute_list <- vroom("Outputs/Tables/RRVal_lag_gnm_absolute_scale.csv.xz")
RR_overall_absolute_list <- vroom("Outputs/Tables/RR_overall_gnm_asboslute_scale.csv.xz")
## Quantiles t2m
quantiles_t2m<-vroom("Outputs/Tables/quantiles_temp_mean.csv.xz")

library(patchwork)

## Plot looping
# Relative
plot_final_relative<-vector("list", 27)
plot_list_relative<-vector("list", 27)
# Absolute
plot_final_absolute<-vector("list", 27)
plot_list_absolute<-vector("list", 27)
names_stacked_plot<-unique(RR_overall_relative_list$abbrev_state)
stacked_levels<-length(names_stacked_plot)

## RR 95% t2m
### Relative
RR_95_05_overall_relative<-vector("list", stacked_levels)
names(RR_95_05_overall_relative)<-names_stacked_plot
### Absolutev
RR_95_05_overall_absolute<-vector("list", stacked_levels)
names(RR_95_05_overall_absolute)<-names_stacked_plot

for (i in 1:stacked_levels) {
  quantile_state<-quantiles_t2m %>% 
    filter(abbrev_state == names_stacked_plot[i]) %>% 
    as.data.frame()
  # creating plot_list
  ## Relative
  RR_lag_relative<-RRVal_lag_relative_list %>% 
    filter(abbrev_state == names_stacked_plot[i])
  RR_overall_relative<-RR_overall_relative_list %>% 
    filter(abbrev_state == names_stacked_plot[i])
  ## Absolute
  RR_lag_absolute<-RRVal_lag_absolute_list %>% 
    filter(abbrev_state == names_stacked_plot[i])
  RR_overall_absolute<-RR_overall_absolute_list %>% 
    filter(abbrev_state == names_stacked_plot[i])
  
  ## RR 95% t2m dist
  ## Relative
  RR_95_05_overall_relative[[i]]<-RR_overall_relative %>% 
    filter(ptmean >= 0.05) %>% 
    filter(ptmean <= 0.95)
  ## Absolute
  RR_95_05_overall_absolute[[i]]<-RR_overall_absolute %>% 
    filter(temp_mean >= quantile_state$q05) %>% 
    filter(temp_mean <= quantile_state$q95)
    
  # Plot RR Lag
  ## Relative
  ylab<-pretty(c(RR_lag_relative$LowRR,RR_lag_relative$HighRR))
  
  plot_rr_lag_relative<-RR_lag_relative %>% 
    ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
    geom_hline(yintercept = 1, size = 0.25) +
    geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = idE),
                   size = .8, show.legend = FALSE) +
    geom_point(shape = 21, fill = "white", size = 2, aes(colour = idE),
               show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0, 21, 2)) +
    scale_y_continuous(breaks = ylab) +
    scale_colour_manual(values = c( "#4575b4","#d73027")) +
    labs(x = "lag (days)", y = "Dengue Hosp. RR ", title = names_stacked_plot[i])+
    facet_wrap(vars(idE),nrow=2)+
    theme_minimal()
  
  ## Absolute
  ylab<-pretty(c(RR_lag_absolute$LowRR,RR_lag_absolute$HighRR))
  
  plot_rr_lag_absolute<-RR_lag_absolute %>% 
    ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
    geom_hline(yintercept = 1, size = 0.25) +
    geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = idE),
                   size = .8, show.legend = FALSE) +
    geom_point(shape = 21, fill = "white", size = 2, aes(colour = idE),
               show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0, 21, 2)) +
    scale_y_continuous(breaks = ylab) +
    scale_colour_manual(values = c( "#4575b4","#d73027")) +
    labs(x = "lag (days)", y = "Dengue Hosp. RR ", title = names_stacked_plot[i])+
    facet_wrap(vars(idE),nrow=2)+
    theme_minimal()
  
  # Plot RR Overall
  ## Relative
  xlab<-pretty(RR_overall_relative$ptmean)
  ylab<-pretty(c(RR_overall_relative$LowRR,RR_overall_relative$HighRR))
  mht_relative<-RR_overall_relative$ptmean[which.min(RR_overall_relative$RR)]
  
  plot_rr_overall_relative<-RR_overall_relative %>% 
    ggplot(aes(ptmean, RR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_vline(xintercept = c(0.05,0.95), size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
    geom_ribbon(aes(ymin = LowRR, 
                    ymax = HighRR),
                fill="grey80",alpha=0.2) +
    geom_line(colour="#cb181d",size=1) +
    geom_point(aes(x = mht_relative, y = 1),shape = 21, fill = "white", size = 2, colour="#cb181d",
               show.legend = FALSE) +
    scale_x_continuous(breaks = xlab) +
    scale_y_continuous(breaks = ylab) +
    theme_minimal()+
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Temperature Percentiles", y = "Dengue Hosp. RR", title = names_stacked_plot[i])
  
  plot_list_relative[[i]]<-list(rr_lag = plot_rr_lag_relative, 
                                rr_overall = plot_rr_overall_relative)
  
  # Patchwork Plot to be saved
  plot_final_relative[[i]]<-(plot_list_relative[[i]]$rr_lag | plot_list_relative[[i]]$rr_overall)+
    plot_layout(guides = "collect")
  
  # PAY ATTENTION TO THE NAME OF FILES YOU ARE SAVING, 
  # MAKE SURE IT IS THE CORRECT, TO NOT SUBSTITUTE FOR AN EXISTING ONE!
  ggsave(paste0("Outputs/Plots/gnm_", names_stacked_plot[i], "_relative_scale_stacked_state_overall_effects.png"),
         plot = plot_final_relative[[i]],
         width = 9,
         height = 7,
         dpi = 300)
  
  ## Absolute
  xlab<-pretty(RR_overall_absolute$temp_mean)
  ylab<-pretty(c(RR_overall_absolute$LowRR,RR_overall_absolute$HighRR))
  mht_absolute<-RR_overall_absolute$temp_mean[which.min(RR_overall_absolute$RR)]
  
  plot_rr_overall_absolute<-RR_overall_absolute %>% 
    ggplot(aes(temp_mean, RR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_vline(xintercept = c(quantile_state$q05,quantile_state$q95), size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
    geom_ribbon(aes(ymin = LowRR, 
                    ymax = HighRR),
                fill="grey80",alpha=0.2) +
    geom_line(colour="#cb181d",size=1) +
    geom_point(aes(x = mht_absolute, y = 1),shape = 21, fill = "white", size = 2, colour="#cb181d",
               show.legend = FALSE) +
    scale_x_continuous(breaks = xlab) +
    scale_y_continuous(breaks = ylab) +
    theme_minimal()+
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Mean Temperature [ºC]", y = "Dengue Hosp. RR", title = names_stacked_plot[i])
  
  plot_list_absolute[[i]]<-list(rr_lag = plot_rr_lag_absolute, 
                                rr_overall = plot_rr_overall_absolute)
  
  # Patchwork Plot to be saved
  plot_final_absolute[[i]]<-(plot_list_absolute[[i]]$rr_lag | plot_list_absolute[[i]]$rr_overall)+
    plot_layout(guides = "collect")
  
  # PAY ATTENTION TO THE NAME OF FILES YOU ARE SAVING, 
  # MAKE SURE IT IS THE CORRECT, TO NOT SUBSTITUTE FOR AN EXISTING ONE!
  ggsave(paste0("Outputs/Plots/gnm_", names_stacked_plot[i], "_absolute_scale_stacked_state_overall_effects.png"),
         plot = plot_final_absolute[[i]],
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
RR_95_05_overall_absolute<-RR_95_05_overall_absolute %>% 
  bind_rows()

vroom_write(file = "Outputs/Tables/RR_95_05_t2m_overall_absolute.csv.xz", 
            RR_95_05_overall_absolute)

## Overall Brasil Plot
### Relative
MHT_states_relative<-RR_overall_relative_list %>% 
  filter(RR == 1) %>% 
  select(ptmean, abbrev_state)

xlab<-pretty(RR_overall_relative_list$ptmean)
ylab<-pretty(c(RR_overall_relative_list$LowRR, RR_overall_relative_list$HighRR))
plot_overall_state_relative<-RR_overall_relative_list %>% 
  ggplot(aes(ptmean, RR)) + 
  geom_line(aes(y=1, x=ptmean))+
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(data = MHT_states_relative, aes(ptmean,1),
             shape = 21,
             fill = "white",
             size = 2,
             colour="#cb181d",
             show.legend = FALSE) +
  # scale_x_continuous(breaks = xlab) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Temperature Percentiles", 
       y = "Dengue Hosp. RR",
       title="Temperature and Dengue Hospitalization",
       subtitle=paste0("Overall by State, 2010-2019"))+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free_y")

plot_overall_state_relative

ggsave(paste0("Outputs/Plots/gnm_relative_scale_all_state_overall_effects.png"),
       plot = plot_overall_state_relative,
       width = 9,
       height = 7,
       dpi = 300)

## Absolute
MHT_states_absolute<-RR_overall_absolute_list %>% 
  filter(RR == 1) %>% 
  select(temp_mean, abbrev_state)

xlab<-pretty(RR_overall_absolute_list$temp_mean)
# ylab<-pretty(c(RR_overall_absolute_list$LowRR, RR_overall_absolute_list$HighRR))
plot_overall_state_absolute<-RR_overall_absolute_list %>% 
  ggplot(aes(temp_mean, RR)) + 
  geom_line(aes(y=1, x=temp_mean))+
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(data = MHT_states_absolute, aes(temp_mean,1),
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

plot_overall_state_absolute

ggsave(paste0("Outputs/Plots/gnm_absolute_scale_all_state_overall_effects.png"),
       plot = plot_overall_state_absolute,
       width = 9,
       height = 7,
       dpi = 300)

## Cold effects on lag, 5th percentile
### Relative
cold_5th_val_lag_relative<-RRVal_lag_relative_list %>% 
  filter(idE == "5th")

# ylab<-pretty(c(cold_5th_val_lag_relative$LowRR,
#                cold_5th_val_lag_relative$HighRR))

plot_rr_lag_5th_relative<-cold_5th_val_lag_relative %>% 
  ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
  geom_line(aes(y=1, x = lag))+
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = idE),
                 size = .8, show.legend = FALSE) +
  geom_point(shape = 21, fill = "white", size = 2, aes(colour = idE),
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  # scale_y_continuous(breaks = ylab) +
  scale_colour_manual(values = c("#4575b4")) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ", 
       title = "Cold (5th) effects on lags", 
       subtitle = "All States, 2010-2019", 
       caption = "On the Relative Scale")+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")+
  theme_minimal()
plot_rr_lag_5th_relative

ggsave(paste0("Outputs/Plots/gnm_relative_scale_all_state_cold_5th_effects.png"),
       plot = plot_rr_lag_5th_relative,
       width = 9,
       height = 7,
       dpi = 300)

## Absolute
cold_5th_val_lag_absolute<-RRVal_lag_absolute_list %>% 
  filter(idE == "5th")

# ylab<-pretty(c(cold_5th_val_lag$LowRR,
#                cold_5th_val_lag$HighRR))

plot_rr_lag_5th_absolute<-cold_5th_val_lag_absolute %>% 
  ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
  geom_line(aes(y=1, x = lag))+
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = idE),
                 size = .8, show.legend = FALSE) +
  geom_point(shape = 21, fill = "white", size = 2, aes(colour = idE),
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
plot_rr_lag_5th_absolute

ggsave(paste0("Outputs/Plots/gnm_absolute_scale_all_state_cold_5th_effects.png"),
       plot = plot_rr_lag_5th_absolute,
       width = 9,
       height = 7,
       dpi = 300)

## Heat effects on lag, 95th percentile
### Relative
heat_95th_val_lag_relative<-RRVal_lag_relative_list %>% 
  filter(idE == "95th")

# ylab<-pretty(c(heat_95th_val_lag$LowRR,
#                heat_95th_val_lag$HighRR))

plot_rr_lag_95th_relative<-heat_95th_val_lag_relative %>% 
  ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
  geom_line(aes(y=1, x = lag))+
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = idE),
                 size = .8, show.legend = FALSE) +
  geom_point(shape = 21, fill = "white", size = 2, aes(colour = idE),
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  # scale_y_continuous(breaks = ylab) +
  scale_colour_manual(values = c("#d73027")) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ", 
       title = "Heat (95th) effects on lags", 
       subtitle = "All States, 2010-2019", caption = "On the Relative Scale")+
  facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")+
  theme_minimal()
plot_rr_lag_95th_relative


ggsave(paste0("Outputs/Plots/gnm_relative_scale_all_state_heat_95th_effects.png"),
       plot = plot_rr_lag_95th_relative,
       width = 9,
       height = 7,
       dpi = 300)

## Absolute
heat_95th_val_lag_absolute<-RRVal_lag_absolute_list %>% 
  filter(idE == "95th")

# ylab<-pretty(c(heat_95th_val_lag$LowRR,
#                heat_95th_val_lag$HighRR))

plot_rr_lag_95th_absolute<-heat_95th_val_lag_absolute %>% 
  ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
  geom_line(aes(y=1, x = lag))+
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = idE),
                 size = .8, show.legend = FALSE) +
  geom_point(shape = 21, fill = "white", size = 2, aes(colour = idE),
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
plot_rr_lag_95th_absolute


ggsave(paste0("Outputs/Plots/gnm_absolute_scale_all_state_heat_95th_effects.png"),
       plot = plot_rr_lag_95th_absolute,
       width = 9,
       height = 7,
       dpi = 300)


source("Functions/functions.R")

RR_overall_list<-regiao(RR_overall_list, english = T)

max_rr_state<-RR_overall_list %>% 
  select(abbrev_state, RR, temp_mean) %>% 
  group_by(abbrev_state) %>% 
  filter(RR == max(RR))
max_rr_state<-regiao(max_rr_state, english = T)

plot_stacked_all_states<-RR_overall_list %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South")), 
         abbrev_state = factor(abbrev_state, levels = 
                                 c("AM", "AC", "AP", "PA", "RO", "RR", "TO", 
                                   "BA", "PE", "PB", "MA", "CE", "RN", "AL", "SE", "PI", 
                                   "MS", "MT", "GO", "DF", 
                                   "SP", "RJ", "MG", "ES", 
                                   "RS", "SC", "PR"))) %>% 
  ggplot(aes(temp_mean, RR, group = abbrev_state, col = region)) + 
  geom_line(size=1) +
  geom_text(data = max_rr_state, aes(y = RR, x = temp_mean, label = abbrev_state, col = "black"))+
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  scale_color_viridis_d(name = "State", option = "turbo", alpha = 0.7)+
  labs(x = "Mean Temperature [ºC] ", 
       y = "Dengue Hosp. RR",
       title="Temperature and Dengue Hospitalization",
       subtitle=paste0("Overall by State, 2010-2019"))+
  facet_wrap(region~., scales = "free_y")

plot_stacked_all_states

ggsave(paste0("Outputs/Plots/gnm_absolute_scale_all_state_overall_effects.png"),
       plot = plot_overall_state,
       width = 9,
       height = 7,
       dpi = 300)

#