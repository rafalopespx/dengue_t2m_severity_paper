rm(list=ls())
gc()

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyrverse)){install.packages("tidyrverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}

## Relative Scale
## Overall Meta 
res_relative<-vroom("Outputs/Tables/meta_gnm_overall_relative_scale_for_all.csv.xz")
## RR Meta
RR_list_relative<-vroom("Outputs/Tables/meta_gnm_RR_overall_relative_scale_for_all.csv.xz")
## RR over lag Meta
RR_list_lag<-vroom("Outputs/Tables/meta_RR_gnm_lags_for_all.csv.xz")
## Absolute Scale
## Overall Meta 
res_absolute<-vroom("Outputs/Tables/meta_gnm_overall_absolute_scale_for_all.csv.xz")
## RR Meta
RR_list_absolute<-vroom("Outputs/Tables/meta_gnm_RR_overall_absolute_scale_for_all.csv.xz")
## RR over lag Meta
RR_list_lag<-vroom("Outputs/Tables/meta_RR_gnm_lags_for_all.csv.xz")


## Dose-respostas Plot Meta
## Relative Scale
metaMHT_relative<-res_relative$ptmean[res_relative$RR ==1]

xlab<-pretty(res_relative$ptmean)
ylab<-pretty(c(res_relative$LowRR, res_relative$HighRR))
plot_overall_relative<-res_relative %>% 
  ggplot(aes(ptmean, RR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_vline(xintercept = c(0.05,0.95), size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.3) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(aes(metaMHT_relative,1),shape = 21, fill = "white", size = 3, colour="#cb181d",
             show.legend = FALSE) +
  scale_x_continuous(breaks = xlab) +
  scale_y_continuous(breaks = ylab) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Temperature Percentiles", 
       y = "Dengue Hosp. RR",
       # title="Temperature and Dengue Hospitalization",
       subtitle="Cumulative Effects by Temperature Percentile"
       )
plot_overall_relative

ggsave(plot = plot_overall_relative, 
       filename = "Outputs/Plots/plot_overall_relative_scale_gnm_meta.png", 
       width = 9, 
       height = 7, 
       dpi = 300)

## Absolute Scale
metaMHT_absolute<-res_absolute$temp_mean[res_absolute$RR ==1]

Fd<-ecdf(res_absolute$temp_mean)
ptmean<-Fd(res_absolute$temp_mean)
ptmean<-as.data.frame(ptmean) %>% 
  mutate(ptmean = round(ptmean, 2))
ptmean$temp_mean<-res_absolute$temp_mean

xlab<-pretty(res_absolute$temp_mean)
ylab<-pretty(c(res_absolute$LowRR, res_absolute$HighRR))
plot_overall_absolute<-res_absolute %>% 
  ggplot(aes(temp_mean, RR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_vline(xintercept = c(ptmean[which(ptmean$ptmean == 0.49),"temp_mean"],
                            ptmean[which(ptmean$ptmean ==0.95),"temp_mean"]), 
             size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.3) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(aes(metaMHT_absolute,1),shape = 21, fill = "white", size = 3, colour="#cb181d",
             show.legend = FALSE) +
  scale_x_continuous(breaks = xlab) +
  scale_y_continuous(breaks = ylab) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Mean Temperature [ºC]", 
       y = "Dengue Hosp. RR",
       # title="Temperature and Dengue Hospitalization",
       subtitle="Cumulative Effects by Temperature Degrees"
  )
plot_overall_absolute

ggsave(plot = plot_overall_absolute, 
       filename = "Outputs/Plots/plot_overall_absolute_scale_gnm_meta.png", 
       width = 9, 
       height = 7, 
       dpi = 300)

## Plot RR meta
## Relative Scale
# xlab<-pretty(RR_list_relative$predvar)
# ylab<-pretty(c(RR_list_relative$LowRR, RR_list_relative$HighRR))
plot_RR_relative<-RR_list_relative %>% 
  ggplot(aes(predvar, RR)) + 
  geom_line(aes(x = predvar, y = RR, colour = typepred),size=1,
            show.legend = T) +
  scale_color_brewer(palette = "Set1")+
  geom_line(aes(x=predvar, y=1), col = "black")+
  # scale_x_continuous(breaks = xlab) +
  # geom_hline(yintercept = 1, size = 0.5) +
  # scale_y_continuous(breaks = ylab) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom") + 
  labs(x = "Temperature Percentiles", y = "Dengue Hosp. RR",
       title="Temperature and Dengue Hospitalization",
       subtitle="Meta over all States, 2010-2019"
       )+
  facet_geo(~state, grid = "br_states_grid1", scales = "free")
plot_RR_relative

ggsave(plot = plot_RR_relative, 
       filename = "Outputs/Plots/plot_RR_gnm_meta_relative_scale.png", 
       width = 9, 
       height = 7, 
       dpi = 300)

## absolute Scale
# xlab<-pretty(RR_list_absolute$predvar)
# ylab<-pretty(c(RR_list_absolute$LowRR, RR_list_absolute$HighRR))
plot_RR_absolute<-RR_list_absolute %>% 
  ggplot(aes(predvar, RR)) + 
  geom_line(aes(x = predvar, y = RR, colour = typepred),size=1,
            show.legend = T) +
  scale_color_brewer(palette = "Set1")+
  geom_line(aes(x=predvar, y=1), col = "black")+
  # scale_x_continuous(breaks = xlab) +
  # geom_hline(yintercept = 1, size = 0.5) +
  # scale_y_continuous(breaks = ylab) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom") + 
  labs(x = "Mean Temperature [ºC", y = "Dengue Hosp. RR",
       title="Temperature and Dengue Hospitalization",
       subtitle="Meta over all States, 2010-2019"
  )+
  facet_geo(~state, grid = "br_states_grid1", scales = "free")
plot_RR_absolute

ggsave(plot = plot_RR_absolute, 
       filename = "Outputs/Plots/plot_RR_gnm_meta_absolute_scale.png", 
       width = 9, 
       height = 7, 
       dpi = 300)

## Precisa rodar meta-lags relative e absolute scale

# Percentile Plots Meta
## Plot effects over P0.05
ylab<-RR_list_lag %>% 
  filter(percentil == 0.05)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p5<-RR_list_lag %>%
  filter(percentil == 0.05 & lag %in% seq(0,21,1)) %>% 
  ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                 size = .8, show.legend = FALSE, colour="#d73027") +
  geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  scale_y_continuous(breaks = ylab) +
  labs(x = "", 
       y = "Dengue Hosp. RR ",
       # title= paste0("Effect on P0.05"),
       subtitle="Lag Effects on 5th Temperature Percentile"
  )+
  theme_minimal()+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
plot_p5

## Plot effects over P0.1
ylab<-RR_list_lag %>% 
  filter(percentil == 0.1)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p10<-RR_list_lag %>%
  filter(percentil == 0.1 & lag %in% seq(0,21,1)) %>% 
  ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                 size = .8, show.legend = FALSE, colour="#d73027") +
  geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  scale_y_continuous(breaks = ylab) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ",
       # title= paste0("Effect on P0.1"),
       subtitle="Lag Effects on 10th Temperature Percentile")+
  theme_bw()+
  theme_minimal()
plot_p10

## Plot effects on the P50
ylab<-RR_list_lag %>% 
  filter(percentil == 0.50)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p50<-RR_list_lag %>%
  filter(percentil == 0.50 & lag %in% seq(0,21,1)) %>% 
  ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                 size = .8, show.legend = FALSE, colour="#d73027") +
  geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
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
ylab<-RR_list_lag %>% 
  filter(percentil == 0.95)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p95<-RR_list_lag %>%
  filter(percentil == 0.95 & lag %in% seq(0,21,1)) %>% 
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

## Plots over P0.99
ylab<-RR_list_lag %>% 
  filter(percentil == 0.99)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p99<-RR_list_lag %>%
  filter(percentil == .99 & lag %in% seq(0,21,1)) %>% 
  ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                 size = .8, show.legend = FALSE, colour="#d73027") +
  geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 21, 2)) +
  scale_y_continuous(breaks = ylab) +
  labs(x = "lag (days)", 
       y = "Dengue Hosp. RR ",
       # title= paste0("Effect on P0.99"),
       subtitle="Lag Effects on 99th Temperature Percentile")+
  theme_bw()+
  theme_minimal()
plot_p99

## Plot all together
plot_percent<-gridExtra::grid.arrange(plot_p5, plot_p10, plot_p95, plot_p99)

ggsave(filename = "Outputs/Plots/percentil_effects.png", 
       plot = plot_percent, 
       width = 9, 
       height = 7, dpi = 300)

## Figure 1 - paper
library(patchwork)

## Relative Scale
plot_figure1_relative<-(plot_overall_relative | (plot_p50 / plot_p95))+
  plot_layout(guides = "collect")+
  plot_annotation(title = "Meta-analysis Results", tag_levels = c("A", "B", "C"), theme = theme_minimal())
plot_figure1_relative

ggsave(filename = "Outputs/Plots/figure_1_meta_analysis_relative_scale.png", 
       plot = plot_figure1_relative, 
       width = 9, 
       height = 7, dpi = 300)

## Absolute Scale
plot_figure1_absolute<-(plot_overall_absolute | (plot_p50 / plot_p95))+
  plot_layout(guides = "collect")+
  plot_annotation(
    # title = "Meta-analysis Results", 
    tag_levels = c("A", "B", "C"), theme = theme_minimal())
plot_figure1_absolute

ggsave(filename = "Outputs/Plots/figure_1_meta_analysis_absolute_scale.png", 
       plot = plot_figure1_absolute, 
       width = 9, 
       height = 7, dpi = 300)

#