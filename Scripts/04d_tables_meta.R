rm(list=ls())

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(tidyselect)){install.packages("tidyselect"); library(tidyselect)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(gtsummary)){install.packages("gtsummary"); library(gtsummary)}
if(!require(ggforce)){install.packages("ggforce"); library(ggforce)}


source("Functions/functions.R")

# Already run and saved. Read with code below
#  
# dengue_t2m<-vroom("Data/dengue_t2m_muni_2010_2019.csv.xz")
# 
# q05_q95_brazil<-dengue_t2m %>% 
#   summarise(q95 = quantile(temp_mean, 0.95), 
#             q05 = quantile(temp_mean, 0.05), 
#             max = max(temp_mean), 
#             min = min(temp_mean), 
#             q33 = quantile(temp_mean, 0.33), 
#             q66 = quantile(temp_mean, 0.66), 
#             q50 = quantile(temp_mean, 0.5), 
#             q25 = quantile(temp_mean, 0.25), 
#             q75 = quantile(temp_mean, 0.75)) %>% 
#   mutate(abbrev_state = "Brazil") %>% 
#   as.data.frame()
# 
# q05_q95_states<-dengue_t2m %>% 
#   group_by(abbrev_state) %>% 
#   summarise(q95 = quantile(temp_mean, 0.95), 
#             q05 = quantile(temp_mean, 0.05), 
#             max = max(temp_mean), 
#             min = min(temp_mean), 
#             q33 = quantile(temp_mean, 0.33), 
#             q66 = quantile(temp_mean, 0.66), 
#             q50 = quantile(temp_mean, 0.5), 
#             q25 = quantile(temp_mean, 0.25), 
#             q75 = quantile(temp_mean, 0.75)) %>% 
#   as.data.frame()
# 
# dengue_t2m<-regiao(dengue_t2m, english = T)
# 
# q05_q95_regions<-dengue_t2m %>% 
#   group_by(region) %>% 
#   summarise(q95 = quantile(temp_mean, 0.95), 
#             q05 = quantile(temp_mean, 0.05), 
#             max = max(temp_mean), 
#             min = min(temp_mean), 
#             q33 = quantile(temp_mean, 0.33), 
#             q66 = quantile(temp_mean, 0.66), 
#             q50 = quantile(temp_mean, 0.5), 
#             q25 = quantile(temp_mean, 0.25), 
#             q75 = quantile(temp_mean, 0.75)) %>% 
#   mutate(abbrev_state  = region) %>% 
#   as.data.frame()
# 
# quantiles_temp<-q05_q95_brazil %>% 
#   bind_rows(
#     q05_q95_states, q05_q95_regions
#   ) %>% 
#   select(-region)
# 
# vroom_write(quantiles_temp, file = "Outputs/Tables/quantiles_temp_mean.csv.xz")


quantiles_temp <- vroom("Outputs/Tables/quantiles_temp_mean.csv.xz")
# Whole Country Meta-analysis Data
# ## Relative
# ## Overall Meta 
# res_state_relative<-vroom("Outputs/Tables/meta_gnm_overall_relative_scale_for_all.csv.xz")
# ## RR Meta
# RR_list_relative<-vroom("Outputs/Tables/meta_gnm_RR_overall_relative_scale_for_all.csv.xz")
# ## RR over lag Meta
# RR_list_lag<-vroom("Outputs/Tables/meta_RR_gnm_lags_for_all.csv.xz")
## Absolute
## Overall Meta 
res_state_absolute<-vroom("Outputs/Tables/meta_gnm_overall_absolute_scale_for_all.csv.xz")
## RR Meta
RR_list_absolute<-vroom("Outputs/Tables/meta_gnm_RR_overall_absolute_scale_for_all.csv.xz")
## RR over lag Meta
RR_list_lag<-vroom("Outputs/Tables/meta_RR_gnm_lags_for_all.csv.xz")

# Descriptive table for temperature

table_quantiles_temp <- quantiles_temp %>%
  mutate(state = abbrev_state) %>% 
  select(state, min, q05, q25, q50, q75, q95, max) %>% 
  mutate(state = factor(state, 
                        levels = c(
                          "Brazil",
                          "North", "AC", "AM", "AP", "PA", "RO", "RR", "TO",
                          "Northeast", "AL","BA","CE", "MA", "PB", "PE",  "PI", "RN",  "SE",
                          "Center-West", "DF", "GO", "MS", "MT",  
                          "Southeast", "ES","MG", "SP", "RJ", 
                          "South","PR", "RS", "SC"
                        )
  )
  ) %>% 
  mutate(min = round(min, 2), 
         q05 = round(q05, 2), 
         q25 = round(q25, 2), 
         q50 = round(q50, 2),
         q75 = round(q75, 2), 
         q95 = round(q95, 2),
         max = round(max,2)) %>% 
  arrange(state) %>% 
  setNames(c("State Region Brazil", "Minimum", "5th", "25th", "50th", "75th", "95th", "Maximum"))

vroom_write(table_quantiles_temp,
            file ="Outputs/Tables/table_s1_t2m_descriptive.csv")

# Per Region Meta-analysis Data
## Regions Names
regions_names<-c("North", "Northeast", "Center-West", "Southeast", "South")

# ## Relative
# ### Overall
# res_region_relative<-vroom("Outputs/Tables/meta_gnm_overall_relative_scale_all_regions.csv.xz")
# ### RR
# RR_list_region_relative<-vroom("Outputs/Tables/meta_gnm_RR_relative_scale_all_regions.csv.xz")
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
# RR_lag_region<-vroom("Outputs/Tables/meta_RR_gnm_lags_all_regions_all_percentile.csv.xz")

## MHTs
## Relative
# MHT_meta_all_relative<-RR_list_relative %>% 
#   filter(typepred == "original") %>% 
#   filter(RR == 1) %>% 
#   select(predvar, state)
# MHT_meta_regions_relative<-RR_list_region_relative %>% 
#   filter(typepred == "original") %>% 
#   filter(RR == 1) %>% 
#   select(predvar, state)
# all.equal(MHT_meta_all_relative, MHT_meta_regions_relative)
## A test to certify that MHT for each state are the same

## Absolute
MHT_meta_all_absolute<-RR_list_absolute %>% 
  filter(typepred == "original") %>% 
  filter(RR == 1) %>% 
  select(predvar, state)
MHT_meta_regions_absolute<-RR_list_region_absolute %>% 
  filter(typepred == "original") %>% 
  filter(RR == 1) %>% 
  select(predvar, state)
all.equal(MHT_meta_all_absolute, MHT_meta_regions_absolute)
## A test to certify that MHT for each state are the same
# 
# ## Relative
# MHT_per_region_relative<-res_region_relative %>% 
#   filter(RR == 1)
# MHT_brasil_relative<-res_state_relative %>% 
#   filter(RR == 1)
## Absolute
MHT_per_region_absolute<-res_region_absolute %>% 
  filter(RR == 1)
MHT_brasil_absolute<-res_state_absolute %>% 
  filter(RR == 1)
# 
# # Relative
# ## 95th, 5th percentile
# percentiles_meta_all_relative<-RR_list_relative %>% 
#   filter(typepred == "original") %>% 
#   filter(predvar == 0.05 | predvar == 0.95 | RR == 1) %>% 
#   select(predvar, state, RR, LowRR, HighRR) %>% 
#   setNames(c("ptmean", "state", "RR", "LowRR", "HighRR"))
# percentiles_meta_regions_relative<-RR_list_region_relative %>% 
#   filter(typepred == "original") %>% 
#   filter(predvar == 0.05 | predvar == 0.95 | RR == 1) %>% 
#   select(predvar, state, RR, LowRR, HighRR) %>% 
#   setNames(c("ptmean", "state", "RR", "LowRR", "HighRR"))
# all.equal(percentiles_meta_all_relative, percentiles_meta_regions_relative)
# percentile_per_state_relative<-merge(percentiles_meta_all_relative, percentiles_meta_regions_relative)
# ## A test to certify that 95th,5th for each state are the same

# Absolute
## 95th, 50th percentile
percentiles_meta_all_absolute_50<-RR_list_region_absolute %>% 
  filter(typepred == "original") %>% 
  left_join(quantiles_temp %>% select(q50, abbrev_state),
            by = c("state"="abbrev_state")) %>% 
  group_by(state) %>% 
  filter(abs(q50 - predvar) == min(abs(q50 - predvar))) %>% 
  select(q50, state, RR, LowRR, HighRR)

percentiles_meta_all_absolute_95<-RR_list_region_absolute %>% 
  filter(typepred == "original") %>% 
  left_join(quantiles_temp %>% select(q95, abbrev_state),
            by = c("state"="abbrev_state")) %>% 
  group_by(state) %>% 
  filter(abs(q95 - predvar) == min(abs(q95 - predvar))) %>% 
  select(q95, state, RR, LowRR, HighRR)

# 
# percentiles_meta_regions_absolute<-RR_list_region_absolute %>% 
#   filter(typepred == "original") %>% 
#   filter(predvar == quantile(predvar, 0.05) | predvar == quantile(predvar, 0.95) | RR == 1) %>% 
#   select(predvar, state, RR, LowRR, HighRR) %>% 
#   setNames(c("temp_mean", "state", "RR", "LowRR", "HighRR"))
# all.equal(percentiles_meta_all_absolute, percentiles_meta_regions_absolute)
# percentile_per_state_absolute<-merge(percentiles_meta_all_absolute, percentiles_meta_regions_absolute)
## A test to certify that 95th,5th for each state are the same

# # Relative
# percentile_per_region_relative<-res_region_relative %>% 
#   filter(ptmean == 0.05 | ptmean == 0.95 | RR == 1)%>%
#   mutate(state = region) %>% 
#   select(ptmean, state, RR, LowRR, HighRR)
# percentile_brasil_relative<-res_state_relative %>% 
#   filter(ptmean == 0.05 | ptmean == 0.95 | RR == 1) %>% 
#   mutate(state = "Brazil") %>% 
#   select(ptmean, state, RR, LowRR, HighRR)
# 
# # Absolute
# percentile_per_region_absolute<-vector("list", 5)
# for (i in 1:5) {
#   quantiles_region<-q05_q95_regions %>% 
#     filter(region == regions_names[i])
#   quantiles_region
#   res_in_region_absolute<-res_region_absolute %>% 
#     filter(region == regions_names[i])
#   res_in_region_absolute
#   
#   q05<-res_in_region_absolute %>% 
#     filter(temp_mean == quantiles_region$q05)
#   q05
#   q95<-res_in_region_absolute %>% 
#     filter(temp_mean == quantiles_region$q95)
#   q95
#   mht<-res_in_region_absolute %>% 
#     filter(RR == 1)
#   mht
#   
#   percentile_per_region_absolute[[i]]<- rbind(q05, q95, mht) %>% 
#     mutate(state = region) %>% 
#     select(temp_mean, state, RR, LowRR, HighRR)
# }
  


# Absolute
## 95th, 5th percentile
percentile_per_region_absolute_50<-res_region_absolute %>% 
  left_join(quantiles_temp %>% select(q50, abbrev_state),
            by = c("region"="abbrev_state")) %>% 
  rename(state=region) %>% 
  group_by(state) %>% 
  filter(abs(q50 - ptmean) == min(abs(q50 - ptmean))) %>% 
  select(q50, state, RR, LowRR, HighRR) 


percentile_per_region_absolute_95<-res_region_absolute %>% 
  left_join(quantiles_temp %>% select(q95, abbrev_state),
            by = c("region"="abbrev_state")) %>% 
  rename(state=region) %>% 
  group_by(state) %>% 
  filter(abs(q95 - ptmean) == min(abs(q95 - ptmean))) %>% 
  select(q95, state, RR, LowRR, HighRR) 

percentile_brasil_absolute_50<-res_state_absolute %>% 
  bind_cols(quantiles_temp %>% 
              filter(abbrev_state=="Brazil") %>% select(q50, abbrev_state)) %>% 
  filter(
    abs(q50 - temp_mean) == min(abs(q50 - temp_mean))) %>% 
  mutate(state = "Brazil") %>% 
  select(q50, state, RR, LowRR, HighRR)

percentile_brasil_absolute_95<-res_state_absolute %>% 
  bind_cols(quantiles_temp %>% 
              filter(abbrev_state=="Brazil") %>% select(q95, abbrev_state)) %>% 
  filter(
      abs(q95 - temp_mean) == min(abs(q95 - temp_mean))) %>% 
  mutate(state = "Brazil") %>% 
  select(q95, state, RR, LowRR, HighRR)

# 
# # Relative
# table_ready_percentile_relative<-rbind(percentiles_meta_all_relative, 
#                                        percentile_per_region_relative, 
#                                        percentile_brasil_relative) %>% 
#   mutate(state = factor(state, 
#                         levels = c(
#                                    "Brazil",
#                                    "North", "AC", "AM", "AP", "PA", "RO", "RR", "TO",
#                                    "Northeast", "AL","BA","CE", "MA", "PB", "PE",  "PI", "RN",  "SE",
#                                    "Center-West", "DF", "GO", "MS", "MT",  
#                                    "Southeast", "ES","MG", "SP", "RJ", 
#                                    "South","PR",  "RS", "SC"
#                                    )
#                         )
#          ) %>% 
#   arrange(state)

# Aboslute
table_ready_percentile_absolute<-
  bind_rows(
    percentiles_meta_all_absolute_50 %>% rename(temp_mean = q50) %>% mutate(model = "RR 50th"),
    percentiles_meta_all_absolute_95 %>% rename(temp_mean = q95) %>% mutate(model = "RR 95th"),
    
    percentile_per_region_absolute_50 %>% rename(temp_mean = q50) %>% mutate(model = "RR 50th"),
    percentile_per_region_absolute_95 %>% rename(temp_mean = q95) %>% mutate(model = "RR 95th"),
    
    percentile_brasil_absolute_50 %>% rename(temp_mean = q50) %>% mutate(model = "RR 50th"),
    percentile_brasil_absolute_95 %>% rename(temp_mean = q95) %>% mutate(model = "RR 95th")) %>% 
  mutate(state = factor(state, 
                        levels = c(
                          "Brazil",
                          "North", "AC", "AM", "AP", "PA", "RO", "RR", "TO",
                          "Northeast", "AL","BA","CE", "MA", "PB", "PE",  "PI", "RN",  "SE",
                          "Center-West", "DF", "GO", "MS", "MT",  
                          "Southeast", "ES","MG", "SP", "RJ", 
                          "South","PR",  "RS", "SC"
                        )
  )
  ) %>% 
  arrange(state)
# 
# # Relative
# table_ready_percentile_relative <-
#   table_ready_percentile_relative %>% 
#   filter(ptmean %in% c(0.05, 0.95)) %>% 
#   mutate(
#     estimate = paste0(
#       format(round(RR, 3), nsmall = 3),
#       " (",
#       format(round(LowRR, 3), nsmall = 3),
#       "-",
#       format(round(HighRR, 3), nsmall = 3),
#       ")"),
#     model    = ifelse(ptmean==0.05, "RR 5th", "RR 95th"),
#   ) %>% 
#   select(state, estimate, model)

# Absolute
table_ready_percentile_absolute <-
  table_ready_percentile_absolute %>% 
  mutate(
    estimate = paste0(
      format(round(RR, 3), nsmall = 3),
      " (",
      format(round(LowRR, 3), nsmall = 3),
      "-",
      format(round(HighRR, 3), nsmall = 3),
      ")"),
  ) %>% 
  select(state, estimate, model)
# 
# # Relative
# table_ready_percentile_relative <- 
# table_ready_percentile_relative %>% 
#   pivot_wider(names_from = model, 
#               values_from = estimate)
# Absolute
table_ready_percentile_absolute <- 
  table_ready_percentile_absolute %>% 
  pivot_wider(names_from = model, 
              values_from = estimate)

# Relative
# mht_ready_relative <-
#   bind_rows(
#     MHT_brasil_relative %>% select(ptmean) %>% mutate(state = "Brazil"),
#     MHT_meta_regions_relative,
#     MHT_per_region_relative %>% select(region, ptmean) %>% rename(state = region)
#   ) %>% 
#   rename(mht = ptmean)

# Aboslute
mht_ready_absolute <-
  bind_rows(
    MHT_brasil_absolute %>% select(temp_mean) %>% mutate(state = "Brazil"),
    MHT_meta_regions_absolute %>% rename(temp_mean = predvar),
    MHT_per_region_absolute %>% select(region, ptmean) %>% rename(state = region, temp_mean = ptmean)
  ) %>% 
  rename(mht = temp_mean)

# Relative
# table_rr_final_relative <-
#   table_ready_percentile_relative %>% left_join(mht_ready_relative, by = "state") %>% 
#   select(state, mht, `RR 5th`, `RR 95th`)
# table_rr_final_relative
# Absolute
table_rr_final_absolute <-
  table_ready_percentile_absolute %>% left_join(mht_ready_absolute, by = "state") %>% 
  select(state, mht, `RR 50th`, `RR 95th`) %>% 
  mutate(mht = format(round(mht, 1), nsmall = 1))
table_rr_final_absolute

table_rr_final_absolute_no_states <-
  table_ready_percentile_absolute %>% left_join(mht_ready_absolute, by = "state") %>% 
  select(state, mht, `RR 50th`, `RR 95th`) %>% 
  filter(state %in% c("Brazil", "North", "Northeast", "Center-West", "Southeast", "South")) %>% 
  mutate(mht = format(round(mht, 1), nsmall = 1))
table_rr_final_absolute_no_states

## Relative
# vroom_write(table_rr_final_relative,
#             file ="Outputs/Tables/table_1_mht_RR_relative.csv")
## Absoluto
vroom_write(table_rr_final_absolute,
            file ="Outputs/Tables/table_1_mht_RR_absolute.csv")
vroom_write(table_rr_final_absolute_no_states, 
            file = "Outputs/Tables/table_1_mht_RR_absolute_no_states.csv")


#