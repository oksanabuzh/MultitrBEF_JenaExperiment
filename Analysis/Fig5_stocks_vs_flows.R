# Do the plant diversity effects on EFs depend on the dimension of EFs 
# (whether it is measured as flux or stock)?

# ----------------------------------------------------

rm(list=ls(all=TRUE))
dev.off()
# Packages
library(tidyverse)

# Data 
df_main <- read_csv("Results/mod_main_text.csv")
str(df_main)

group <- read_csv ("Data/EF_grouped.csv")
names(group)

df_all <- df_main %>% 
  left_join(group, by = join_by(response)) 

str(df_all)

df_all %>%  pull(Dimens) %>% unique()
df_all %>%  pull(Ecos_Function) %>% unique()
df_all %>%  pull(Tr_Group) %>% unique()
df_all %>%  pull(Trophic_level) %>% unique()

                  


flux_dat <- df_all %>% 
  dplyr::select(predictor, Dimens, effect_size_st, flux_out_stock, flux_in_stock, AG_BG, Ecos_Function) %>% 
  filter(Dimens=="flow") %>% 
  rename(flow_effect=effect_size_st,
         AG_BG_flux=AG_BG) %>% 
  dplyr::select(-Dimens) %>% 
  pivot_longer(cols = starts_with("flux_"), names_to = "in_out_flux",
               values_drop_na = TRUE,  values_to = "stock_group") %>% 
  mutate(in_out_flux=case_when(in_out_flux=="flux_in_stock" ~ "inflow",
                               in_out_flux=="flux_out_stock" ~ "outflow"))
flux_dat

stock_dat <- df_all %>% 
  dplyr::select(predictor, Dimens, effect_size_st, flux_out_stock, flux_in_stock, AG_BG, Tr_Group, Trophic_level) %>% 
  filter(Dimens=="stock") %>% 
  rename(stock_effect=effect_size_st,
         AG_BG_stock=AG_BG,
         stock_group=flux_in_stock)%>% 
  dplyr::select(-Dimens, - flux_out_stock)

stock_dat


dat<- flux_dat %>% 
  left_join(stock_dat, by= c("predictor", "stock_group"))

# plot

ggplot(dat %>% 
         filter(predictor=="sowndiv"),
  aes(x = stock_effect, y = flow_effect, color = stock_group, shape=in_out_flux)) +
  geom_point() +
  scale_shape_manual(values=c(19, 21)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~ in_out_flux + AG_BG_stock, scales = "free") +
  theme_bw()

 ?facet_wrap()
