# Fig 3 barplots for the design variables of the Jena Experiment

library(tidyverse)
library(ggtext)

# Prepare data ------------------------------------------------------------
# Percentages of significant effects
# calculate percentages of significant effects
# Data

df_main <- read_csv("Results/mod_supp.csv") 
str(df_main)

group <- read_csv ("Data/EF_grouped.csv")
names(group)

dat_perc <- merge(df_main, group)
dat_perc
str(dat_perc)


# Create new columns needed for the further analysis
dat_perc <- dat_perc %>% 
  mutate(Signif=case_when(p < 0.050 ~ "Signif", p >= 0.050 ~ "Non-sign")) %>% # create column with significant vs non-significant effects
  mutate(Eff_sighn=case_when(effect_size_st < 0 ~ "negative", p >= 0 ~ "positive")) # create column with direction of effect (positive vs negative)


count <- dat_perc %>% 
  count(predictor, Dimens, AG_BG, Signif,  Eff_sighn) %>% 
  add_count(predictor, Dimens, wt=sum(n), name = "sum") %>% 
  filter(Signif=="Signif")
count

summarised <- count %>% 
  mutate(predictor=fct_relevel(predictor, c("sowndiv", "numfg",
                                            "leg.ef", "gr.ef",
                                            "sh.ef", "th.ef",
                                            "FDbranch", "FDis"))) %>% 
  pivot_wider(names_from = "AG_BG", values_from = "n") %>%
  mutate(AG = ifelse(is.na(AG), 0, AG),     
         AG_BG = ifelse(is.na(AG_BG), 0, AG_BG),
         BG = ifelse(is.na(BG ), 0, BG )) %>% 
  mutate(Total=AG+AG_BG+BG) %>% 
  mutate(AG_pr=round(AG*100/sum, digits = 2)) %>% 
  mutate(BG_pr=round(BG*100/sum, digits = 2)) %>% 
  mutate(Total_pr=round(Total*100/sum, digits = 2)) %>% 
  select(-Signif ) %>% 
  arrange(predictor, Dimens)

summarised


# write_csv (summarised, "Results/Summary_Effects_MainText.csv")


# Read data ------

dat <- summarised %>%
  mutate(Dimens = case_when(
    Dimens == "flow" ~ "Flows",
    Dimens == "stock" ~ "Stocks"
  )) %>%
  select(predictor, Dimens, Total_pr, AG_pr, BG_pr, Eff_sighn) %>%
  pivot_longer(!c(predictor, Dimens, Eff_sighn)) %>%
  mutate(value = case_match(
    Eff_sighn,
    "negative" ~ value * -1,
    "positive" ~ value
  )) %>%
  mutate(
    predictor = factor(predictor,
      levels = c("sowndiv", "numfg", "leg.ef", "gr.ef", "sh.ef", "th.ef",
                 "FDbranch", "FDis")
    )
  )

# Prepare plot ------------------------------------------------------------

# Modify factor labels

predictor_label <- c(
  gr.ef = "Presence of grasses",
  leg.ef = "Presence of legumes",
  numfg = "Functional group richness",
  sh.ef = "Presence of small herbs",
  sowndiv = "Species richness",
  th.ef = "Presence of tall herbs",
  FDbranch ="Branch length, FDbranch",
  FDis = "Functional dispersion, FDis")


# Make plot ---------------------------------------------------------------


barplot_effects <- dat %>%
  ggplot(aes(x = Dimens, y = value, fill = name)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_vline(xintercept = 1.5, color = "grey80") +
  geom_hline(yintercept = 0, size = 0.3) +
  labs(y = "Significant effects (%)<br>(<span style = 'color: red;'>negative</span>,
       <span style = 'color: blue;'>positive</span>)") +
  scale_y_continuous(
    breaks = seq(-10, 50, by = 10),
    limits = c(-10, 51),
    expand = c(0, 0),
    labels=c("10", "0", "10", "20", "30", "40", "50")
  ) +
  scale_x_discrete(expand = c(0.2, 0.2)) +
  scale_fill_manual(
    values = c("cyan4", "darkorange", "grey"),
    labels = c("Aboveground subnetwork", "Belowground subnetwork", "Entire network")
  ) +
  facet_wrap(~predictor, labeller = labeller(
    predictor = predictor_label
  ), nrow = 2) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_markdown(),
    axis.text.y = element_text(color = c("red", "black", rep("blue", 5))),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title = element_blank(),
    legend.position="bottom",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold")
  )+
  theme(panel.spacing.y = unit(2, "lines"))


barplot_effects

