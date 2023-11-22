rm(list=ls())
gc()

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}
if(!require(geofacet)){install.packages("geofacet"); library(geofacet)}

## Relative Scale
## Overall Meta 
res<-vroom("Outputs/Tables/adding_cases/meta_gnm_overall_for_all.csv.xz")
## RR Meta
# RR_list<-vroom("Outputs/Tables/New_run/meta_gnm_RR_overall_for_all.csv.xz")
## RR over lag Meta
RR_list_lag<-vroom("Outputs/Tables/adding_cases/meta_RR_gnm_lags_for_all.csv.xz")

## Dose-response Plot Meta
metaMHT<-res$temp_mean[res$RR ==1]
ptmean<-quantile(res$temp_mean, probs = (1:99)/100, na.rm = T)
p50<-ptmean[50]
p95<-ptmean[95]

xlab<-pretty(res$temp_mean)
ylab<-pretty(c(res$LowRR, res$HighRR))
plot_overall<-res %>% 
  ggplot(aes(temp_mean, RR)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_vline(xintercept = c(ptmean[50],
                            ptmean[95]), 
             size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
  geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.3) +
  geom_line(colour="#cb181d",size=1) +
  geom_point(aes(metaMHT,1),shape = 21, fill = "white", size = 3, colour="#cb181d",
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
plot_overall

ggsave(plot = plot_overall, 
       filename = "Outputs/Plots/adding_cases/plot_overall_gnm_meta.png", 
       width = 11, 
       height = 9, 
       dpi = 200)

# ## Plot RR meta
# xlab<-pretty(RR_list$predvar)
# ylab<-pretty(c(RR_list$LowRR, RR_list$HighRR))
# plot_RR<-RR_list %>%
#   ggplot(aes(predvar, RR)) +
#   geom_line(aes(x = predvar, y = RR, colour = typepred),size=1,
#             show.legend = T) +
#   scale_color_brewer(palette = "Set1")+
#   geom_line(aes(x=predvar, y=1), col = "black")+
#   # scale_x_continuous(breaks = xlab) +
#   # geom_hline(yintercept = 1, size = 0.5) +
#   # scale_y_continuous(breaks = ylab) +
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
#   labs(x = "Mean Temperature [ºC]", y = "Dengue Hosp. RR",
#        title="Temperature and Dengue Hospitalization",
#        subtitle="Meta over all States, 2010-2019"
#   )+
#   facet_geo(~state, grid = "br_states_grid1", scales = "free")
# plot_RR
# 
# ggsave(plot = plot_RR,
#        filename = "Outputs/Plots/plot_RR_gnm_meta.png",
#        width = 9,
#        height = 7,
#        dpi = 300)

# Percentile Plots Meta
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

## Plot all together
plot_percent<-(plot_p50 / plot_p95)+
  plot_annotation(title = "Lag Effects", tag_levels = 'a')+
  plot_layout(guides = 'collect')
plot_percent

ggsave(filename = "Outputs/Plots/adding_cases/percentil_effects.png", 
       plot = plot_percent, 
       width = 11, 
       height = 9, 
       dpi = 200)

## Figure 1 - paper
library(patchwork)

plot_figure1<-(plot_overall | (plot_p50 / plot_p95))+
  plot_layout(guides = "collect")+
  plot_annotation(
    # title = "Meta-analysis Results", 
    tag_levels = c("A", "B", "C"), theme = theme_minimal())
plot_figure1

ggsave(filename = "Outputs/Plots/adding_cases/figure_1_meta_analysis.png", 
       plot = plot_figure1, 
       width = 11, 
       height = 9, 
       dpi = 200)

# ## All lags together
# plot_lags<-RR_list_lag |>
#   ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR, col = percentil)) + 
#   geom_hline(yintercept = 1, size = 0.5) +
#   geom_pointrange(aes(x = lag, y = RR, 
#                 ymin = LowRR, ymax = HighRR
#                 ),
#                  size = .8, 
#             show.legend = FALSE, 
#             colour="#d73027") +
#   geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
#   scale_x_continuous(breaks = seq(0, 21, 2)) +
#   # scale_y_continuous(breaks = ylab) +
#   labs(x = "lag (days)", 
#        y = "Dengue Hosp. RR ",
#        # title= paste0("Effect on P0.95"),
#        subtitle="Lag Effects on 95th Temperature Percentile"
#   )+
#   theme_bw()+
#   theme_minimal()
# plot_lags

#