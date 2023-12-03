#### Forest plot with all analysis

library(tidyverse)
library(tidylog)
library(grid)
library(gridExtra)

main <- vroom::vroom('Outputs/Tables/New_run/table_1_mht_RR_absolute.csv') %>% janitor::clean_names()
s1   <- vroom::vroom('Outputs/Tables/Sensitivity_analysis/Newrun/SA1_table_1_mht_RR_absolute.csv') %>% janitor::clean_names()
s2   <- vroom::vroom('Outputs/Tables/Sensitivity_analysis/Newrun/SA2_table_1_mht_RR_absolute.csv') %>% janitor::clean_names()
s3   <- vroom::vroom('Outputs/Tables/adding_cases/table_1_mht_RR_absolute.csv') %>% janitor::clean_names()


db <-
  bind_rows(
    main %>% mutate(model = 'Main analysis'),
    s1   %>% mutate(model = 'Sensitivity 1'),
    s2   %>% mutate(model = 'Sensitivity 2'),
    s3   %>% mutate(model = 'Sensitivity 3')) %>% 
  mutate(
    rr_50 = as.numeric(str_sub(rr_50th, 1,5)),
    rr_95 = as.numeric(str_sub(rr_95th, 1,5)),
    low_50= as.numeric(str_sub(rr_50th, 8,12)),
    low_95= as.numeric(str_sub(rr_95th, 8,12)),
    hig_50= as.numeric(str_sub(rr_50th, 14,18)),
    hig_95= as.numeric(str_sub(rr_95th, 14,18))
  )
    

db <-
  db %>% 
  mutate(
    submodel = factor(state,
                      levels = c('Brazil', 'North', 'Northeast','Center-West','Southeast', 'South')))

## 50th

models_int_plot_50 <-
  db %>% 
  select(submodel, model, rr_50, low_50, hig_50,
         rr_50th, mht) %>% 
  arrange(submodel) %>% 
  mutate(order = 
           c(2:5,7:10,12:15,17:20,22:25, 27:30),
         model = paste0("     ", model),
         bold  = 1)

add_lines <-
  tibble(
    levels = c("Brazil", 'North', 'Northeast','Center-West','Southeast', 'South'),
    order = 
      c(1,5,11,16,21,26),
    bold = 2
  )

models_int_plot_50 <-
  bind_rows(models_int_plot_50, add_lines) %>% 
  arrange(order)

models_int_plot_50 <-
  models_int_plot_50 %>% 
  mutate(
    mht = case_when(
    levels == "Brazil" ~ "MHT*", TRUE ~ as.character(mht)),
    rr_50th = case_when(
      levels == "Brazil" ~ "     RR (95% CI)", TRUE ~ rr_50th))

models_int_plot_50$yval <- seq(nrow(models_int_plot_50), 1, by = -1)


p1 <- 
  models_int_plot_50 %>% 
  ggplot() + 
  theme_bw() + 
  aes(x = rr_50, xmin = low_50, xmax = hig_50, y = yval, col = model, fill = model) + 
  geom_errorbarh(
    # aes(color = submodel),
    height = 0.4, size = 1.0) + 
  geom_point(size = 3, shape = 15) + 
  geom_vline(xintercept = 1) + 
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.border = element_blank(),
    legend.position = 'none',
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12, colour = "black")
  )  +
  xlab("\n Relative Risk (95% CI)") +
  scale_x_log10(
    # trans="log",
    # breaks = c(0.75,1,2.5,5,10,15,20),
    # label = scales::breaks_log(accuracy = 1L),
    #limits = c(-0.05, 0.82)
  )
p1

# labels, could be extended to show more information
p2 <-
  models_int_plot_50 %>% 
  ggplot() + 
  theme_bw() + 
  aes(y = yval) + 
  geom_text(aes(label = levels, 
                x = 0,
                fontface = bold, hjust = 0)) + 
  geom_text(aes(label = model, 
                x = 0.02,
                fontface = bold, hjust = 0)) + 
  geom_text(aes(label = mht, 
                x = 0.5,
                fontface = bold, hjust = 0)) + 
  geom_text(aes(label = rr_50th, 
                x = 0.7,
                fontface = bold, hjust = 0)) + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank() 
  ) +
  xlim(0,1)
p2  



grid.draw(gridExtra::gtable_cbind(ggplotGrob(p2), ggplotGrob(p1), 
                                  size = "max"))


### 95th


models_int_plot_95 <-
  db %>% 
  select(submodel, model, rr_95, low_95, hig_95,
         rr_95th, mht) %>% 
  arrange(submodel) %>% 
  mutate(order = 
           c(2:5,7:10,12:15,17:20,22:25, 27:30),
         model = paste0("     ", model),
         bold  = 1)

add_lines <-
  tibble(
    levels = c("Brazil", 'North', 'Northeast','Center-West','Southeast', 'South'),
    order = 
      c(1,5,11,16,21,26),
    bold = 2
  )

models_int_plot_95 <-
  bind_rows(models_int_plot_95, add_lines) %>% 
  arrange(order)

models_int_plot_95 <-
  models_int_plot_95 %>% 
  mutate(
    mht = case_when(
      levels == "Brazil" ~ "MHT*", TRUE ~ as.character(mht)),
    rr_95th = case_when(
      levels == "Brazil" ~ "     RR (95% CI)", TRUE ~ rr_95th))

models_int_plot_95$yval <- seq(nrow(models_int_plot_95), 1, by = -1)


p3 <- 
  models_int_plot_95 %>% 
  ggplot() + 
  theme_bw() + 
  aes(x = rr_95, xmin = low_95, xmax = hig_95, y = yval, col = model, fill = model) + 
  geom_errorbarh(
    # aes(color = submodel),
    height = 0.4, size = 1.0) + 
  geom_point(size = 3, shape = 15) + 
  geom_vline(xintercept = 1) + 
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.border = element_blank(),
    legend.position = 'none',
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12, colour = "black")
  )  +
  xlab("\n Relative Risk (95% CI)") +
  scale_x_log10(
    # trans="log",
    # breaks = c(0.75,1,2.5,5,10,15,20),
    # label = scales::breaks_log(accuracy = 1L),
    #limits = c(-0.05, 0.82)
  )
p3

# labels, could be extended to show more information
p4 <-
  models_int_plot_95 %>% 
  ggplot() + 
  theme_bw() + 
  aes(y = yval) + 
  geom_text(aes(label = levels, 
                x = 0,
                fontface = bold, hjust = 0)) + 
  geom_text(aes(label = model, 
                x = 0.02,
                fontface = bold, hjust = 0)) + 
  geom_text(aes(label = mht, 
                x = 0.5,
                fontface = bold, hjust = 0)) + 
  geom_text(aes(label = rr_95th, 
                x = 0.7,
                fontface = bold, hjust = 0)) + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank() 
  ) +
  xlim(0,1)
p4  



grid.draw(gridExtra::gtable_cbind(ggplotGrob(p4), ggplotGrob(p3), 
                                  size = "max"))
