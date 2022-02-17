rm(list=ls())
gc()

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(geofacet)){install.packages("geofacet"); library(geofacet)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}

## Regions Names
regions_names<-c("North", "Northeast", "Center-West", "Southeast", "South")

## Relative
### Overall
res_region_relative<-vroom("Outputs/Tables/meta_gnm_overall_relative_scale_all_regions.csv.xz")
### RR
RR_list_region_relative<-vroom("Outputs/Tables/meta_gnm_RR_relative_scale_all_regions.csv.xz")

## Absolute
### Overall
res_region_absolute<-vroom("Outputs/Tables/meta_gnm_overall_absolute_scale_all_regions.csv.xz")
### RR
RR_list_region_absolute<-vroom("Outputs/Tables/meta_gnm_RR_absolute_scale_all_regions.csv.xz")

### Coef list by percentile lags
# coef_list<-vroom("Outputs/Tables/coeffcients_gnm_meta_for_regions_percentile.csv.xz")
### Covariance Matrix by percentile lags
# vcov_list<-vroom("Outputs/Tables/vcov_gnm_meta_for_regions_percentile.csv.xz")
### RR list for lags by percentile
RR_lag_region<-vroom("Outputs/Tables/meta_RR_gnm_lags_all_regions_all_percentile.csv.xz")

## Dose-respostas by Region
## Relative
metaMHT_relative<-res_region_relative %>% 
  filter(RR == 1) %>% 
  select(ptmean, region) %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South")))

xlab<-pretty(res_region_relative$ptmean)
ylab<-pretty(c(res_region_relative$LowRR, res_region_relative$HighRR))
plot_overall_region_relative<-res_region_relative %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
  ggplot(aes(ptmean, RR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(data = metaMHT_relative, aes(ptmean,1),
             shape = 21,
             fill = "white",
             size = 2,
             colour="#cb181d",
             show.legend = FALSE) +
  scale_x_continuous(breaks = xlab) +
  # scale_y_continuous(breaks = ylab) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Temperature Percentiles ", 
       y = "Dengue Hosp. RR",
       title="Temperature and Dengue Hospitalization",
       subtitle=paste0("Meta by Regions, 2010-2019"))+
  facet_wrap(region~., scales = "free")

plot_overall_region_relative

ggsave(plot = plot_overall_region_relative,
       filename = paste0("Outputs/Plots/plot_overall_relative_scale_all_regions_gnm_meta.png"),
       width = 9,
       height = 7,
       dpi = 300)

## Absolute
metaMHT_absolute<-res_region_absolute %>% 
  filter(RR == 1) %>% 
  select(ptmean, region) %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South")))

xlab<-pretty(res_region_absolute$ptmean)
ylab<-pretty(c(res_region_absolute$LowRR, res_region_absolute$HighRR))
plot_overall_region_absolute<-res_region_absolute %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
  ggplot(aes(ptmean, RR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(data = metaMHT_absolute, aes(ptmean,1),
             shape = 21,
             fill = "white",
             size = 2,
             colour="#cb181d",
             show.legend = FALSE) +
  scale_x_continuous(breaks = xlab) +
  # scale_y_continuous(breaks = ylab) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Mean Temperature [ºC] ", 
       y = "Dengue Hosp. RR",
       title="Temperature and Dengue Hospitalization",
       subtitle=paste0("Meta by Regions, 2010-2019"))+
  facet_wrap(region~., scales = "free")

plot_overall_region_absolute

ggsave(plot = plot_overall_region_absolute,
       filename = paste0("Outputs/Plots/plot_overall_absolute_scale_all_regions_gnm_meta.png"),
       width = 9,
       height = 7,
       dpi = 300)

## RR comparison BLUPS, Meta, Original, with meta by Region
### Relative
plot_RR_relative<-RR_list_region_relative %>% 
  ggplot(aes(predvar, RR)) + 
  geom_line(aes(y=1, x = predvar))+
  geom_line(aes(x = predvar, y = RR, colour = typepred),size=1,
            show.legend = T) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom") + 
  scale_color_brewer(palette = "Set1")+
  labs(x = "Temperature Percentiles", y = "Dengue Hosp. RR",
       title="Temperature and Dengue Hospitalization",
       subtitle="Meta by Region, 2010-2019")+
  facet_geo(state~., grid = "br_states_grid1", scales = "free")
plot_RR_relative

ggsave(plot = plot_RR_relative, 
       filename = "Outputs/Plots/plot_RR_relative_scale_gnm_meta_regions.png", 
       width = 9, 
       height = 7, 
       dpi = 300)

### Abosulte
plot_RR_absolute<-RR_list_region_absolute %>% 
  ggplot(aes(predvar, RR)) + 
  geom_line(aes(y=1, x = predvar))+
  geom_line(aes(x = predvar, y = RR, colour = typepred),size=1,
            show.legend = T) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom") + 
  scale_color_brewer(palette = "Set1")+
  labs(x = "Mean Temperature [ºC]", y = "Dengue Hosp. RR",
       title="Temperature and Dengue Hospitalization",
       subtitle="Meta by Region, 2010-2019")+
  facet_geo(state~., grid = "br_states_grid1", scales = "free")
plot_RR_absolute

ggsave(plot = plot_RR_absolute, 
       filename = "Outputs/Plots/plot_RR_absolute_scale_gnm_meta_regions.png", 
       width = 9, 
       height = 7, 
       dpi = 300)

## Lags Plots by Region

# Percentile Plots Meta
## Plot effects over P0.05
ylab<-RR_lag_region %>% 
  filter(percentil == 0.05)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p5<-RR_lag_region %>%
  filter(percentil == 0.05 & lag %in% seq(0,21, 1)) %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
  ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                 size = .8, show.legend = FALSE, colour="#d73027") +
  geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0,21, 2))+
  # scale_y_continuous(breaks = ylab) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ",
       title= paste0("Effect on P0.05"),
       subtitle="Meta by Region, 2010-2019")+
  theme_bw()+
  theme_minimal()+
  facet_wrap(region~., scales = "free")
plot_p5

## Plot effects over P0.1
ylab<-RR_lag_region %>% 
  filter(percentil == 0.1)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p10<-RR_lag_region %>%
  filter(percentil == 0.1 & lag %in% seq(0,21, 1)) %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
  ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                 size = .8, show.legend = FALSE, colour="#d73027") +
  geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  # scale_y_continuous(breaks = ylab) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ",
       title= paste0("Effect on P0.1"),
       subtitle="Meta by Region, 2010-2019")+
  theme_bw()+
  theme_minimal()+
  facet_wrap(region~., scales = "free")
plot_p10


## Plot effects over P0.50
ylab<-RR_lag_region %>% 
  filter(percentil == 0.50)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p50<-RR_lag_region %>%
  filter(percentil == 0.50 & lag %in% seq(0,21, 1)) %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
  ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                 size = .8, show.legend = FALSE, colour="#d73027") +
  geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0,21, 2))+
  # scale_y_continuous(breaks = ylab) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ",
       title= paste0("Effect on P0.50"),
       subtitle="Meta by Region, 2010-2019")+
  theme_bw()+
  theme_minimal()+
  facet_wrap(region~., scales = "free")
plot_p50

## Plot effects over P0.95
ylab<-RR_lag_region %>% 
  filter(percentil == 0.95)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p95<-RR_lag_region %>%
  filter(percentil == 0.95 & lag %in% seq(0,21, 1)) %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
  ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                 size = .8, show.legend = FALSE, colour="#d73027") +
  geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  # scale_y_continuous(breaks = ylab) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ",
       title= paste0("Effect on P0.95"),
       subtitle="Meta by Region, 2010-2019")+
  theme_bw()+
  theme_minimal()+
  facet_wrap(region~., scales = "free")
plot_p95

## Plots over P0.99
ylab<-RR_lag_region %>% 
  filter(percentil == 0.99)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p99<-RR_lag_region %>%
  filter(percentil == .99 & lag %in% seq(0,21, 1)) %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
  ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                 size = .8, show.legend = FALSE, colour="#d73027") +
  geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  # scale_y_continuous(breaks = ylab) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ",
       title= paste0("Effect on P0.99"),
       subtitle="Meta by Region, 2010-2019")+
  theme_bw()+
  theme_minimal()+
  facet_wrap(region~., scales = "free")
plot_p99

## Plot all together
plot_percent<-gridExtra::grid.arrange(plot_p5, plot_p10, plot_p95, plot_p99)

ggsave(filename = "Outputs/Plots/percentil_effects_regions.png", 
       plot = plot_percent, 
       width = 9, 
       height = 7, dpi = 300)

# Plot grid
plot_percent_grid<-RR_lag_region %>%
  filter(lag %in% seq(0,21, 1)) %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
  ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                 size = .8, show.legend = FALSE, colour="#d73027") +
  geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  # scale_y_continuous(breaks = ylab) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ",
       title= paste0("Effect on each Percentile by Region"),
       subtitle="Meta by Region, 2010-2019")+
  theme_bw()+
  theme_minimal()+
  facet_grid(region~percentil, scales = "free")

plot_percent_grid

ggsave(filename = "Outputs/Plots/percentil_effects_regions_grid.png", 
       plot = plot_percent_grid, 
       width = 9, 
       height = 7, dpi = 300)


## patchwork plots
plot_figure_relative<-vector("list", 5)
plot_figure_absolute<-vector("list", 5)
## Overall Effects and 95th,5th lags effect
# for each region in a separate plot
for (i in 1:5) {
  ## Relative
  overall_data_relative<-res_region_relative %>% 
    filter(region == regions_names[i])
  ## Absolute
  overall_data_absolute<-res_region_absolute %>% 
    filter(region == regions_names[i])
  lag_data<-RR_lag_region %>% 
    filter(region == regions_names[i])
  
  ## Overall Plot
  ### Relative
  MHT_relative<-overall_data_relative %>% 
    filter(RR == 1) %>% 
    select(ptmean, region)
  
  xlab<-pretty(overall_data_relative$ptmean)
  ylab<-pretty(c(overall_data_relative$LowRR, overall_data_relative$HighRR))
  plot_overall_relative<-overall_data_relative %>% 
    mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
    ggplot(aes(ptmean, RR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_vline(xintercept = c(0.50,0.95), 
               size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed")+
    geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
    geom_line(colour="#cb181d",size=1) +
    geom_point(data = MHT_relative, aes(ptmean,1),
               shape = 21,
               fill = "white",
               size = 2,
               colour="#cb181d",
               show.legend = FALSE) +
    scale_x_continuous(breaks = xlab) +
    scale_y_continuous(breaks = ylab) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Temperature Percentiles", 
         y = "Dengue Hosp. RR",
         subtitle="Cumulative Effects by Temp. Percentile")
  plot_overall_relative
  
  ### Aboslute
  MHT_absolute<-overall_data_absolute %>% 
    filter(RR == 1) %>% 
    select(ptmean, region)
  
  Fd<-ecdf(overall_data_absolute$ptmean)
  ptmean<-Fd(overall_data_absolute$ptmean)
  ptmean<-as.data.frame(ptmean) %>% 
    mutate(ptmean = round(ptmean, 2))
  ptmean$temp_mean<-overall_data_absolute$ptmean
  
  xlab<-pretty(overall_data_absolute$ptmean)
  ylab<-pretty(c(overall_data_absolute$LowRR, overall_data_absolute$HighRR))
  plot_overall_absolute<-overall_data_absolute %>% 
    mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
    ggplot(aes(ptmean, RR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_vline(xintercept = c(ptmean[which(ptmean$ptmean == 0.49),"temp_mean"],
                              ptmean[which(ptmean$ptmean ==0.95),"temp_mean"]), 
               size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
    geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
    geom_line(colour="#cb181d",size=1) +
    geom_point(data = MHT_absolute, aes(ptmean,1),
               shape = 21,
               fill = "white",
               size = 2,
               colour="#cb181d",
               show.legend = FALSE) +
    scale_x_continuous(breaks = xlab) +
    scale_y_continuous(breaks = ylab) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Mean Temperature [ºC]", 
         y = "Dengue Hosp. RR",
         subtitle="Cumulative Effects by Temp. Degrees")
  plot_overall_absolute
  
  # Percentile Plots Meta
  ## Plot effects over P0.05
  ylab<-lag_data %>% 
    filter(percentil == 0.50)
  ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
  plot_p50<-lag_data %>%
    filter(percentil == 0.50 & lag %in% seq(0,21, 1)) %>% 
    mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
    ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                   size = .8, show.legend = FALSE, colour="#d73027") +
    geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
    # scale_x_continuous(breaks = seq(0, 21, 2)) +
    scale_y_continuous(breaks = ylab) +
    labs(x = "", 
         y = "Dengue Hosp. RR ",
         # title= paste0("Effect on P0.05"),
         subtitle="Lag Effects on 50th Temperature Percentile"
    )+
    theme_minimal()+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
  plot_p50
  
  ## Plot effects over P0.95
  ylab<-lag_data %>% 
    filter(percentil == 0.95)
  ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
  plot_p95<-lag_data %>%
    filter(percentil == 0.95 & lag %in% seq(0,21, 1)) %>% 
    mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
    ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                   size = .8, show.legend = FALSE, colour="#d73027") +
    geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0, 21, 2)) +
    scale_y_continuous(breaks = ylab) +
    labs(x = "lag (days)", 
         y = "Dengue Hosp. RR ",
         # title= paste0("Effect on P0.95"),
         subtitle="Lag Effects on 95th Temperature Percentile"
    )+
    theme_bw()+
    theme_minimal()
  plot_p95
  
  ## Relative
  plot_figure_relative[[i]]<-(plot_overall_relative | (plot_p50 / plot_p95))+
    plot_layout(guides = "collect")+
    plot_annotation(title = paste0("Meta-analysis Results, ", regions_names[i], " Region"), 
                    tag_levels = c("A", "B", "C"), 
                    theme = theme_minimal())
  ## Absolute
  plot_figure_absolute[[i]]<-(plot_overall_absolute | (plot_p50 / plot_p95))+
    plot_layout(guides = "collect")+
    plot_annotation(title = paste0("Meta-analysis Results, ", regions_names[i], " Region"), 
                    tag_levels = c("A", "B", "C"), 
                    theme = theme_minimal())
  
  ggsave(filename = paste0("Outputs/Plots/figure_SM_relative_scale_", regions_names[i], "_region_meta_analysis.png"),
         plot = plot_figure_relative[[i]],
         width = 9,
         height = 7, dpi = 300)
  ggsave(filename = paste0("Outputs/Plots/figure_SM_absolute_scale_", regions_names[i], "_region_meta_analysis.png"),
         plot = plot_figure_absolute[[i]],
         width = 9,
         height = 7, dpi = 300)
  
  
}


#