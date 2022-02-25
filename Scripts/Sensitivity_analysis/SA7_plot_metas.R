rm(list=ls())
gc()

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}

## Relative Scale
## SA1
res_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/SA1_meta_gnm_overall_for_all.csv.xz")
## RR Meta
RR_list_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/SA1_meta_gnm_overall_for_all.csv.xz")
## RR over lag Meta
RR_list_lag_sa1<-vroom("Outputs/Tables/Sensitivity_analysis/SA1_meta_RR_gnm_lags_for_all.csv.xz")
## SA2
res_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/SA2_meta_gnm_overall_for_all.csv.xz")
## RR Meta
RR_list_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/SA2_meta_gnm_overall_for_all.csv.xz")
## RR over lag Meta
RR_list_lag_sa2<-vroom("Outputs/Tables/Sensitivity_analysis/SA2_meta_RR_gnm_lags_for_all.csv.xz")

## Dose-response Plot Meta
## SA1
metaMHT_sa1<-res_sa1$temp_mean[res_sa1$RR ==1]
ptmean<-quantile(res_sa1$temp_mean, probs = (1:99)/100, na.rm = T)
p50<-ptmean[50]
p95<-ptmean[95]

xlab<-pretty(res_sa1$temp_mean)
ylab<-pretty(c(res_sa1$LowRR, res_sa1$HighRR))
plot_overall_sa1<-res_sa1 %>% 
  ggplot(aes(temp_mean, RR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_vline(xintercept = c(ptmean[50],
                            ptmean[95]), 
             size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.3) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(aes(metaMHT_sa1,1),shape = 21, fill = "white", size = 3, colour="#cb181d",
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
plot_overall_sa1

ggsave(plot = plot_overall_sa1, 
       filename = "Outputs/Plots/Sensitivity_analysis/SA1_plot_overall_gnm_meta.png", 
       width = 9, 
       height = 7, 
       dpi = 300)

## SA2
metaMHT_sa2<-res_sa2$temp_mean[res_sa2$RR ==1]
ptmean<-quantile(res_sa1$temp_mean, probs = (1:99)/100, na.rm = T)
p50<-ptmean[50]
p95<-ptmean[95]

xlab<-pretty(res_sa1$temp_mean)
ylab<-pretty(c(res_sa1$LowRR, res_sa1$HighRR))
plot_overall_sa2<-res_sa2 %>% 
  ggplot(aes(temp_mean, RR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_vline(xintercept = c(ptmean[50],
                            ptmean[95]), 
             size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.3) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(aes(metaMHT_sa2,1),shape = 21, fill = "white", size = 3, colour="#cb181d",
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
plot_overall_sa2

ggsave(plot = plot_overall_sa2, 
       filename = "Outputs/Plots/Sensitivity_analysis/SA2_plot_overall_gnm_meta.png", 
       width = 9, 
       height = 7, 
       dpi = 300)

# Percentile Plots Meta
## Plot effects on the P50
## SA1
ylab<-RR_list_lag_sa1 %>% 
  filter(percentil == 0.50)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p50_sa1<-RR_list_lag_sa1 %>%
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
plot_p50_sa1
## SA2
ylab<-RR_list_lag_sa2 %>% 
  filter(percentil == 0.50)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p50_sa2<-RR_list_lag_sa2 %>%
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
plot_p50_sa2


## Plot effects over P0.95
## SA1
ylab<-RR_list_lag_sa1 %>% 
  filter(percentil == 0.95)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p95_sa1<-RR_list_lag_sa1 %>%
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
plot_p95_sa1
## SA2
ylab<-RR_list_lag_sa2 %>% 
  filter(percentil == 0.95)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p95_sa2<-RR_list_lag_sa2 %>%
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
plot_p95_sa2

## Plot all together
## SA1
plot_lag_sa1<-gridExtra::grid.arrange(plot_p50_sa1, plot_p95_sa1)

ggsave(filename = "Outputs/Plots/Sensitivity_analysis/SA1_percentil_effects.png", 
       plot = plot_lag_sa1, 
       width = 9, 
       height = 7, dpi = 300)
## SA2
plot_lag_sa2<-gridExtra::grid.arrange(plot_p50_sa2, plot_p95_sa2)

ggsave(filename = "Outputs/Plots/Sensitivity_analysis/SA2_percentil_effects.png", 
       plot = plot_lag_sa2, 
       width = 9, 
       height = 7, dpi = 300)

## Figure 1 - paper
## SA1
plot_figure1_sa1<-(plot_overall_sa1 | (plot_p50_sa1 / plot_p95_sa1))+
  plot_layout(guides = "collect")+
  plot_annotation(
    # title = "Meta-analysis Results", 
    tag_levels = c("A", "B", "C"), theme = theme_minimal())
plot_figure1_sa1

ggsave(filename = "Outputs/Plots/Sensitivity_analysis/SA1_figure_1_meta_analysis.png", 
       plot = plot_figure1_sa1, 
       width = 9, 
       height = 7, dpi = 300)

## SA2
plot_figure1_sa2<-(plot_overall_sa2 | (plot_p50_sa2 / plot_p95_sa2))+
  plot_layout(guides = "collect")+
  plot_annotation(
    # title = "Meta-analysis Results", 
    tag_levels = c("A", "B", "C"), theme = theme_minimal())
plot_figure1_sa2

ggsave(filename = "Outputs/Plots/Sensitivity_analysis/SA2_figure_1_meta_analysis.png", 
       plot = plot_figure1_sa2, 
       width = 9, 
       height = 7, dpi = 300)

#