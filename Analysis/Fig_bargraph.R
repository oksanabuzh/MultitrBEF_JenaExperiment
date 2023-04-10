library(tidyverse)
library(ggtext)

# Prepare data ------------------------------------------------------------

dat <- read_csv("Results/Summary_Effects_MainText.csv")

dat <- dat %>%
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
      levels = c("sowndiv", "numfg", "leg.ef", "gr.ef", "sh.ef", "th.ef")
    )
  )

# Prepare plot ------------------------------------------------------------

# Modify factor labels

predictor_label <- c(
  gr.ef = "Presence of grasses",
  leg.ef = "Presence of legumes",
  numfg = "Functional richness",
  sh.ef = "Presence of small herbs",
  sowndiv = "Species richness",
  th.ef = "Presence of tall herbs"
)

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
    expand = c(0, 0)
  ) +
  scale_x_discrete(expand = c(0.2, 0.2)) +
  scale_fill_manual(
    values = c("cyan4", "darkorange", "grey"),
    labels = c("Aboveground", "Belowground", "Total")
  ) +
  facet_wrap(~predictor, labeller = labeller(
    predictor = predictor_label
  )) +
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
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold")
  )

# Save plot ---------------------------------------------------------------

ggsave("Results/barplot_effects.png",
  width = 16, height = 8, units = "cm",
  scale = 1.3
)
