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

### Overall
res_region_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_meta_gnm_overall_all_regions.csv.xz")
res_region_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_meta_gnm_overall_all_regions.csv.xz")
### RR list for lags by percentile
RR_lag_region_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA1_meta_RR_gnm_lags_all_regions_all_percentile.csv.xz")
RR_lag_region_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/Newrun/SA2_meta_RR_gnm_lags_all_regions_all_percentile.csv.xz")

## Dose-response by Region
## SA1
metaMHT_region_sa1<-res_region_sa1 %>% 
  filter(RR == 1) %>% 
  select(temp_mean, region) %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South")))

xlab<-pretty(res_region_sa1$temp_mean)
ylab<-pretty(c(res_region_sa1$LowRR, res_region_sa1$HighRR))
plot_overall_region_sa1<-res_region_sa1 %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
  ggplot(aes(temp_mean, RR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(data = metaMHT_region_sa1, aes(temp_mean,1),
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

plot_overall_region_sa1

ggsave(plot = plot_overall_region_sa1,
       filename = paste0("Outputs/Plots/Sensitivity_analysis/Newrun/SA1_plot_overall_regions_all_regions_gnm_meta.png"),
       width = 9,
       height = 7,
       dpi = 300)

## SA2
metaMHT_region_sa2<-res_region_sa2 %>% 
  filter(RR == 1) %>% 
  select(temp_mean, region) %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South")))

xlab<-pretty(res_region_sa2$temp_mean)
ylab<-pretty(c(res_region_sa2$LowRR, res_region_sa2$HighRR))
plot_overall_region_sa2<-res_region_sa2 %>% 
  mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
  ggplot(aes(temp_mean, RR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(data = metaMHT_region_sa2, aes(temp_mean,1),
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
plot_overall_region_sa2

ggsave(plot = plot_overall_region_sa2,
       filename = paste0("Outputs/Plots/Sensitivity_analysis/Newrun/SA2_plot_overall_regions_all_regions_gnm_meta.png"),
       width = 9,
       height = 7,
       dpi = 300)

## Lags Plots by Region

# Percentile Plots Meta
## Plot effects over P0.50
ylab<-RR_lag_region_sa1 %>% 
  filter(percentil == 0.50)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p50_sa1<-RR_lag_region_sa1 %>%
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
plot_p50_sa1

ylab<-RR_lag_region_sa2 %>% 
  filter(percentil == 0.50)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p50_sa2<-RR_lag_region_sa2 %>%
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
plot_p50_sa2

## Plot effects over P0.95
ylab<-RR_lag_region_sa1 %>% 
  filter(percentil == 0.95)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p95_sa1<-RR_lag_region_sa1 %>%
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
plot_p95_sa1

ylab<-RR_lag_region_sa2 %>% 
  filter(percentil == 0.95)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p95_sa2<-RR_lag_region_sa2 %>%
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
plot_p95_sa2

## Plot all together
plot_percent_sa1<-gridExtra::grid.arrange(plot_p50_sa1, plot_p95_sa1)
plot_percent_sa2<-gridExtra::grid.arrange(plot_p50_sa2, plot_p95_sa2)

ggsave(filename = "Outputs/Plots/Sensitivity_analysis/Newrun/SA1_percentil_effects_regions.png", 
       plot = plot_percent_sa1, 
       width = 9, 
       height = 7, dpi = 300)

ggsave(filename = "Outputs/Plots/Sensitivity_analysis/Newrun/SA2_percentil_effects_regions.png", 
       plot = plot_percent_sa2, 
       width = 9, 
       height = 7, dpi = 300)

## patchwork plots
plot_figure_region_sa1<-plot_figure_region_sa2<-vector("list", 2)
## Overall Effects and 95th,5th lags effect
# for each region in a separate plot
for (i in 1:5) {
  overall_data_region_sa1<-res_region_sa1 %>% 
    filter(region == regions_names[i])
  lag_data_sa1<-RR_lag_region_sa1 %>% 
    filter(region == regions_names[i])
  overall_data_region_sa2<-res_region_sa2 %>% 
    filter(region == regions_names[i])
  lag_data_sa2<-RR_lag_region_sa2 %>% 
    filter(region == regions_names[i])
  
  ## Overall Plot
  MHT_region_sa1<-overall_data_region_sa1 %>% 
    filter(RR == 1) %>% 
    select(temp_mean, region)
  
  MHT_region_sa2<-overall_data_region_sa2 %>% 
    filter(RR == 1) %>% 
    select(temp_mean, region)
  
  Fd<-ecdf(overall_data_region_sa1$temp_mean)
  ptmean<-Fd(overall_data_region_sa1$temp_mean)
  ptmean<-as.data.frame(ptmean) %>% 
    mutate(ptmean = round(ptmean, 2))
  ptmean$temp_mean<-overall_data_region_sa1$temp_mean
  
  xlab<-pretty(overall_data_region_sa1$temp_mean)
  ylab<-pretty(c(overall_data_region_sa1$LowRR, overall_data_region_sa1$HighRR))
  plot_overall_region_sa1<-overall_data_region_sa1 %>% 
    mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
    ggplot(aes(temp_mean, RR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_vline(xintercept = c(ptmean[0.50],
                              ptmean[0.95]), 
               size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
    geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
    geom_line(colour="#cb181d",size=1) +
    geom_point(data = MHT_region_sa1, aes(x=temp_mean,y=1),
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
  plot_overall_region_sa1
  
  Fd<-ecdf(overall_data_region_sa2$temp_mean)
  ptmean<-Fd(overall_data_region_sa2$temp_mean)
  ptmean<-as.data.frame(ptmean) %>% 
    mutate(ptmean = round(ptmean, 2))
  ptmean$temp_mean<-overall_data_region_sa2$temp_mean
  
  xlab<-pretty(overall_data_region_sa2$temp_mean)
  ylab<-pretty(c(overall_data_region_sa2$LowRR, overall_data_region_sa2$HighRR))
  plot_overall_region_sa2<-overall_data_region_sa2 %>% 
    mutate(region = factor(region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
    ggplot(aes(temp_mean, RR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_vline(xintercept = c(ptmean[0.50],
                              ptmean[0.95]), 
               size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
    geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
    geom_line(colour="#cb181d",size=1) +
    geom_point(data = MHT_region_sa2, aes(x=temp_mean,y=1),
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
  plot_overall_region_sa2
  
  # Percentile Plots Meta
  ## Plot effects over P0.05
  ylab<-lag_data_sa1 %>% 
    filter(percentil == 0.50)
  ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
  plot_p50_sa1<-lag_data_sa1 %>%
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
  plot_p50_sa1
  
  ylab<-lag_data_sa2 %>% 
    filter(percentil == 0.50)
  ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
  plot_p50_sa2<-lag_data_sa2 %>%
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
  plot_p50_sa2
  
  ## Plot effects over P0.95
  ylab<-lag_data_sa1 %>% 
    filter(percentil == 0.95)
  ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
  plot_p95_sa1<-lag_data_sa1 %>%
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
  plot_p95_sa1
  
  ylab<-lag_data_sa2 %>% 
    filter(percentil == 0.95)
  ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
  plot_p95_sa2<-lag_data_sa2 %>%
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
  plot_p95_sa2
  
  plot_figure_region_sa1[[i]]<-(plot_overall_region_sa1 | (plot_p50_sa1 / plot_p95_sa1))+
    plot_layout(guides = "collect")+
    plot_annotation(title = paste0("Meta-analysis Results, ", regions_names[i], " Region"), 
                    tag_levels = c("A", "B", "C"), 
                    theme = theme_minimal())
  
  ggsave(filename = paste0("Outputs/Plots/Sensitivity_analysis/Newrun/SA1_figure_SM_", regions_names[i], "_region_meta_analysis.png"),
         plot = plot_figure_region_sa1[[i]],
         width = 9,
         height = 7, dpi = 300)
  
  plot_figure_region_sa2[[i]]<-(plot_overall_region_sa2 | (plot_p50_sa2 / plot_p95_sa2))+
    plot_layout(guides = "collect")+
    plot_annotation(title = paste0("Meta-analysis Results, ", regions_names[i], " Region"), 
                    tag_levels = c("A", "B", "C"), 
                    theme = theme_minimal())
  
  ggsave(filename = paste0("Outputs/Plots/Sensitivity_analysis/Newrun/SA2_figure_SM_", regions_names[i], "_region_meta_analysis.png"),
         plot = plot_figure_region_sa2[[i]],
         width = 9,
         height = 7, dpi = 300)
}


#