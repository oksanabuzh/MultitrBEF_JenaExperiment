# Fig 5 e-f 
# flows grouped by ecosystem functions, and AG vs BG
# stocks grouped by trophic groups, trophic levels and AG vs BG.

# Packages
library(tidyverse)
library(patchwork)

# Read the data -----------------------------------------------------------
df_main <- read_csv("Results/mod_main_text.csv") %>% 
    filter(predictor %in% c("FDis", "FDbranch"))

df_rootShoot <- read_csv("Results/mod_ShootRoot.csv")%>% # roots and shoots separately
mutate(predictor=case_when(predictor=="sum_bl" ~ "FDbranch",
                           .default=predictor)) %>% 
  filter(predictor %in% c("FDis", "FDbranch"))


group <- read_csv("Data/EF_grouped.csv")


# Prepare flows -----------------------------------------------------------
# combine data with group info
df_all_flow <- df_main %>%
  left_join(group, by = join_by(response)) %>%
  filter(Dimens == "flow") %>%
  select(
    -Tr_Group, -Trophic_level, -r2_part, -R2_model, -p,
    -estimate, -effect_size, -lambda, -formula, -response,
    -Dimens
  )

# Make the plot -----------------------------------------------------------

# combine column AG_BG and Ecos_Function into one for plotting and grouping
df_to_plot_flow <- df_all_flow %>%
  pivot_longer(c(AG_BG, Ecos_Function)) %>%
  select(-name) %>%
  dplyr::rename(Ecos_Function = value) %>%
  filter(Ecos_Function != "AG_BG")

# calculate means to determine color of the points
df_colors_flow <- df_to_plot_flow %>%
  group_by(Ecos_Function, predictor) %>%
  dplyr::summarize(mean = mean(effect_size_st)) %>%
  mutate(color = case_when(
    mean > 0 ~ "positive",
    mean < 0 ~ "negative"
  ))

# add color indication to the data to plot and put predictors and ES-Functions
# in the right order to plot
df_to_plot_flow <- df_to_plot_flow %>%
  left_join(df_colors_flow, by = c("Ecos_Function", "predictor")) %>%
  mutate(
    predictor = factor(predictor,
      levels = c("FDis", "FDbranch")
    ),
    Ecos_Function = case_match(Ecos_Function,
      "Carbon_uptake" ~ "Carbon uptake",
      "Detritus_production" ~ "Detritus production",
      .default = Ecos_Function
    )
  ) %>%
  mutate(Ecos_Function = factor(Ecos_Function, levels = c(
    "BG", "AG", "Carbon uptake", "Herbivory", "Decomposition",
    "Predation", "Detritus production", "Respiration"
  )))

# Add significance
df_to_plot_flow <- df_to_plot_flow %>%
  mutate(significance = case_when(
    Ecos_Function %in% c("AG", "BG") & predictor == "sowndiv" ~ "s",
    !(Ecos_Function %in% c("AG", "BG")) & predictor == "leg.ef" ~ "s",
    TRUE ~ "ns"
  ))

# Prepare stocks ----------------------------------------------------------

df_all_stocks <- df_main %>%
  left_join(group, by = join_by(response)) %>%
  filter(Dimens == "stock") %>%
  select(
    -Ecos_Function, -r2_part, -R2_model, -p,
    -estimate, -effect_size, -lambda, -formula,
    -Dimens, -response
  )

# combine column AG_BG, Tr_Group and Trophic_level into one for plotting and grouping
df_to_plot_stocks <- df_all_stocks %>%
  mutate(Trophic_level = as.character(Trophic_level)) %>%
  pivot_longer(c(AG_BG, Tr_Group, Trophic_level),
    names_to = "group",
    values_to = "group_value"
  ) %>%
  filter(group_value != "AG_BG")
# Add root and shoot biomass for AG and BG

# Add the root and shoot stock for AG and BG calculation
df_root_shoot <- df_rootShoot %>%
  mutate(group = "AG_BG") %>%
  mutate(group_value = case_when(
    str_detect(response, "root") ~ "BG",
    str_detect(response, "shoot") ~ "AG"
  )) %>%
  select(predictor, effect_size_st, group, group_value)

df_to_plot_stocks <- bind_rows(df_root_shoot, df_to_plot_stocks)

# calculate means to determine color of the points

df_colors_stocks <- df_to_plot_stocks %>%
  group_by(group_value, predictor) %>%
  dplyr::summarize(mean = mean(effect_size_st)) %>%
  mutate(color = case_when(
    mean > 0 ~ "positive",
    mean < 0 ~ "negative"
  ))

# add color indication to the data to plot and put predictors and ES-Functions
# in the right order to plot
df_to_plot_stocks <- df_to_plot_stocks %>%
  left_join(df_colors_stocks, by = c("group_value", "predictor")) %>%
  mutate(
    predictor = factor(predictor,
      levels = c("FDis", "FDbranch")
    )
  ) %>%
  mutate(group_value = factor(group_value, levels = c(
    "BG", "AG", "1", "2", "3", "Plants", "Detritus", "Herbivores", "Decomposers",
    "Omnivores", "Carnivores"
  )))

df_to_plot_stocks <- df_to_plot_stocks %>%
  mutate(significance = "ns")

# Make flow plot ----------------------------------------------------------

# create nice facet labels
predictor_label <- c(
  FDis = "FDis",
  FDbranch ="FDbranch"
)

predictor_label_flows <- c(
  FDis = "e",
  FDbranch = "f"
)

predictor_label_stocks <- c(
  FDis = "j",
  FDbranch = "h"
)

# Format x-axis so that 0 is printed as 0 and not as 0.00
plain <- function(x, ...) {
  format(x, ..., scientific = FALSE, drop0trailing = TRUE)
}

jitter_width <- 0.05
jitter_size <- 0.6
sizes_signif <- c(1, 2)
bar_size <- 0.5
mean_point_size <- 1.8

plot_flow <- df_to_plot_flow %>%
  ggplot(aes(y = Ecos_Function, x = effect_size_st, color = color)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey70", size = 0.5) +
  #geom_jitter(height = 0.1, width = 0, alpha = 0.2, size = jitter_size) +
  stat_summary(fun = "mean", geom = "point", size = mean_point_size) +
  stat_summary(
    fun.data = "mean_cl_normal", geom = "errorbar",
    fun.args = list(conf.int = 0.95), width = 0, size = bar_size
  ) +
  facet_wrap(~predictor, labeller = labeller(
    predictor = predictor_label_flows
  ), nrow = 1, scales="free_x") +
  geom_hline(yintercept = 2.5, color = "azure3") +
  labs(x = "Standardized effect sizes") +
  scale_x_continuous(breaks = c(-0.25, 0, 0.25), labels = plain)

# Make stocks plot --------------------------------------------------------
# make the plot
plot_stocks <- df_to_plot_stocks %>%
  ggplot(aes(y = group_value, x = effect_size_st, color = color)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey70", size = 0.5) +
  #geom_jitter(height = 0.3, width = 0, alpha = 0.2, size = jitter_size) +
  stat_summary(fun = "mean", geom = "point", size = mean_point_size) +
  stat_summary(
    fun.data = "mean_cl_normal", geom = "errorbar",
    fun.args = list(conf.int = 0.95), width = 0, size = bar_size
  ) +
  facet_wrap(~predictor, labeller = labeller(
    predictor = predictor_label_stocks
  ), nrow = 1, scale="free_x") +
  geom_hline(yintercept = 2.5, color = "azure3") +
  geom_hline(yintercept = 5.5, color = "azure3") +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2)) +
  labs(x = "Standardized effect size")


# Combine the plots into one ----------------------------------------------
plot <- (plot_flow + plot_stocks) &
  scale_color_manual(values = c("red", "royalblue")) &
  theme_bw(base_size = 14) &
  theme(
    legend.position = "none",
    panel.border = element_rect(fill = NA, colour = "grey30", linewidth = 0.8),
    axis.text.x = element_text(size = 10, colour = "black"),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold"),
    plot.margin = margin(t = 0, b = 20, l = 0, r = 0)
  )



plot

# Save the plot
# ggsave("Results/Fig4_5_compined.png", plot,
#  width = 20, height = 20, units = "cm")
