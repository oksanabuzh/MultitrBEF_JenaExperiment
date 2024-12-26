# Produce Fig. 5
#
# Comparison of plant diversity effects on stocks and flows for each trophic group 
# across aboveground and belowground subnetworks and given different 
# ecosystem functions represented by the energy flows and depending 
# on flow types (inflow vs outflow).
#

# -------------------------------------------------------------------------------#

rm(list=ls(all=TRUE))
dev.off()

# Packages ---------------------------------------------------------------------
library(tidyverse)
library(patchwork)

# (1) Single figure for each biodiversity facet  -------------------------------

# Data ----

df_main <- read_csv("Results/mod_main_text.csv") %>% 
  bind_rows(read_csv("Results/mod_ShootRoot.csv"))

str(df_main)

group <- read_csv ("Data/Shoot_Root_separately/EF_grouped_Shoot_Root.csv")
names(group)

df_all <- df_main %>% 
  left_join(group, by = join_by(response)) 

str(df_all)

df_all %>%  pull(Dimens) %>% unique()
df_all %>%  pull(Ecos_Function) %>% unique()
df_all %>%  pull(Tr_Group) %>% unique()
df_all %>%  pull(Trophic_level) %>% unique()
df_all %>%  pull(AG_BG,  response) 




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
  filter(Dimens=="stock") %>% 
  rename(stock_effect=effect_size_st,
         AG_BG_stock=AG_BG,
         stock_group=flux_in_stock)%>% 
  dplyr::select(-Dimens, - flux_out_stock)

stock_dat


stock_dat %>%  pull(stock_group) %>% unique()


## Data wrangling for plotting ----

dat<- flux_dat %>% 
  left_join(stock_dat, by= c("predictor", "stock_group")) %>% 
  mutate(stock_group_general=case_when(stock_group=="AG.Herbivores" ~ "Herbivores",
                                       stock_group=="BG.Herbivores" ~ "Herbivores",
                                       stock_group=="AG.Decomposers" ~ "Decomposers",
                                       stock_group=="BG.Decomposers" ~ "Decomposers",
                                       stock_group=="AG.Carnivores" ~ "Carnivores",
                                       stock_group=="BG.Carnivores" ~ "Carnivores",
                                       stock_group=="AG.Omnivores" ~ "Omnivores",
                                       stock_group=="BG.Omnivores" ~ "Omnivores",
                                       stock_group=="Soil.Microorganisms" ~ "Soil microorganisms",
                                       stock_group=="AG.Litter" ~ "Litter",
                                       stock_group=="AG.Plants" ~ "Plants",
                                       stock_group=="BG.Plants" ~ "Plants" , 
                                       .default = stock_group)) %>% 
  mutate(stock_group_general = fct_relevel(stock_group_general, 
                                           "Plants", "Litter", "SOM", 
                                           "Herbivores", "Decomposers", "Soil microorganisms", 
                                           "Omnivores", "Carnivores")) %>% 
  mutate(flux_group_dummy=case_when(Ecos_Function=="Carbon_uptake" ~ "Carbon uptake",
                                    Ecos_Function=="Decomposition" &  in_out_flux=="inflow" ~ "Feeding",
                                    Ecos_Function=="Predation"  &  in_out_flux=="inflow" ~ "Feeding",
                                    Ecos_Function=="Herbivory" &  in_out_flux=="inflow" ~ "Feeding",
                                    Ecos_Function=="Decomposition" &  in_out_flux=="outflow" ~ "Production",
                                    Ecos_Function=="Predation"  &  in_out_flux=="outflow" ~ "Production",
                                    Ecos_Function=="Herbivory"  &  in_out_flux=="outflow" ~ "Production",
                                    Ecos_Function=="Respiration"  ~ "Respiration",
                                    Ecos_Function=="Detritus_production" ~ "Detritus production", 
                                    .default = Ecos_Function)) %>% 
  mutate(flux_group_dummy=fct_relevel(flux_group_dummy, "Carbon uptake", 
                                      "Feeding", "Production", "Respiration","Detritus production"))


str(dat)
dat %>% pull(stock_group) %>% unique()
dat %>% pull(in_out_flux) %>% unique()
dat %>% pull(stock_group_general) %>% unique()
dat%>% pull(Ecos_Function) %>% unique()
dat%>% pull(flux_group_dummy) %>% unique()

factor(dat$stock_group_general)
dat%>% pull(AG_BG_stock) %>% unique()



## Plot----

MyShape2=c(21, 19)

SR_effects <- dat %>% 
  filter(predictor=="sowndiv") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, 
             color = stock_group_general, 
             # fill=dummy_fill, 
             shape=AG_BG_stock)) +
  geom_point(size=2, stroke=1.1 ) +
  scale_shape_manual(values=MyShape2) +
  scale_color_manual(values=myPalette)+
  # scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  # facet_wrap( ~  in_out_flux, scales = "free") +
  theme_bw() +  # guides(fill = "none") +
  labs(x="Effects on stocks", y= "Effects on flows", title="Species richness",
       shape="AG/BG subnetwork", col="Trophic group")

SR_effects


numfg_effects <- dat %>% 
  filter(predictor=="numfg") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             #   fill=dummy_fill, 
             shape=AG_BG_stock)) +
  geom_point(size=2, stroke=1.1 ) +
  scale_shape_manual(values=MyShape2) +
  scale_color_manual(values=myPalette)+
  #  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  #  facet_wrap( ~ in_out_flux, scales = "free") +
  theme_bw() +   
  labs(x="Effects on stocks", y= "Effects on flows", title="FG richness",
       shape="AG/BG subnetwork", col="Trophic group")


numfg_effects


leg.ef_effects <- dat %>% 
  filter(predictor=="leg.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             #   fill=dummy_fill, 
             shape=AG_BG_stock)) +
  geom_point(size=2, stroke=1.1) +
  scale_shape_manual(values=MyShape2) +
  scale_color_manual(values=myPalette)+
  # scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  # facet_wrap( ~  in_out_flux , scales = "free") +
  theme_bw() +   
  labs(x="Effects on stocks", y= "Effects on flows", title="Legumes",
       shape="AG/BG subnetwork", col="Trophic group")



leg.ef_effects


gr.ef_effects <- dat %>% 
  filter(predictor=="gr.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             #  fill=dummy_fill, 
             shape=AG_BG_stock)) +
  geom_point(size=2, stroke=1.1 ) +
  scale_shape_manual(values=MyShape2) +
  scale_color_manual(values=myPalette)+
  # scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  #  facet_wrap( ~  in_out_flux , scales = "free") +
  theme_bw() +   
  labs(x="Effects on stocks", y= "Effects on flows", title="Grasses",
       shape="AG/BG subnetwork", col="Trophic group")


gr.ef_effects



sh.ef_effects <- dat %>% 
  filter(predictor=="sh.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             #  fill=dummy_fill, 
             shape=AG_BG_stock)) +
  geom_point(size=2, stroke=1.1 ) +
  scale_shape_manual(values=MyShape2) +
  scale_color_manual(values=myPalette)+
  # scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  # facet_wrap( ~  in_out_flux , scales = "free") +
  theme_bw() +   
  labs(x="Effects on stocks", y= "Effects on flows", title="Small herbs",
       shape="AG/BG subnetwork", col="Trophic group")


sh.ef_effects



th.ef_effects <- dat %>% 
  filter(predictor=="th.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             #  fill=dummy_fill, 
             shape=AG_BG_stock)) +
  geom_point(size=2, stroke=1.1) +
  scale_shape_manual(values=MyShape2) +
  scale_color_manual(values=myPalette)+
  #  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  #  facet_wrap( ~  in_out_flux , scales = "free") +
  theme_bw() +   
  labs(x="Effects on stocks", y= "Effects on flows", title="Tall herbs",
       shape="AG/BG subnetwork", col="Trophic group")


th.ef_effects




SR_effects + numfg_effects + 
  leg.ef_effects + gr.ef_effects + 
  sh.ef_effects + th.ef_effects + 
  plot_annotation(tag_levels = 'a') + 
  plot_layout(guides = "collect", ncol=2) & # theme(legend.position = 'right') +
  # plot_layout(ncol=2) & # ylab(NULL) & 
  theme(legend.position = 'right',
        plot.margin = margin(10, 30, 6, 30), 
        plot.tag = element_text(size = 12, face = 'bold'), 
        plot.tag.position = c(0.08, 1.06))


# (2) Separate figures by different groups and fluxes---------------------------

## Data -------
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
df_all %>%  pull(AG_BG,  response) 


                  


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


## Data wrangling for plotting ---------

dat<- flux_dat %>% 
  left_join(stock_dat, by= c("predictor", "stock_group")) %>% 
  mutate(stock_group_general=case_when(stock_group=="AG.Herbivores" ~ "Herbivores",
                                       stock_group=="BG.Herbivores" ~ "Herbivores",
                                       stock_group=="AG.Decomposers" ~ "Decomposers",
                                       stock_group=="BG.Decomposers" ~ "Decomposers",
                                       stock_group=="AG.Carnivores" ~ "Carnivores",
                                       stock_group=="BG.Carnivores" ~ "Carnivores",
                                       stock_group=="AG.Omnivores" ~ "Omnivores",
                                       stock_group=="BG.Omnivores" ~ "Omnivores",
                                       stock_group=="Soil.Microorganisms" ~ "Soil microorganisms",
                                       stock_group=="AG.Litter" ~ "Litter",
                                       .default = stock_group)) %>% 
  mutate(stock_group_general = fct_relevel(stock_group_general, 
                                   "Plants", "Litter", "SOM", 
                                   "Herbivores", "Decomposers", "Soil microorganisms", 
                                   "Omnivores", "Carnivores")) %>% 
  mutate(flux_group_dummy=case_when(Ecos_Function=="Carbon_uptake" ~ "Carbon uptake",
                                    Ecos_Function=="Decomposition" &  in_out_flux=="inflow" ~ "Feeding",
                                    Ecos_Function=="Predation"  &  in_out_flux=="inflow" ~ "Feeding",
                                    Ecos_Function=="Herbivory" &  in_out_flux=="inflow" ~ "Feeding",
                                    Ecos_Function=="Decomposition" &  in_out_flux=="outflow" ~ "Production",
                                    Ecos_Function=="Predation"  &  in_out_flux=="outflow" ~ "Production",
                                    Ecos_Function=="Herbivory"  &  in_out_flux=="outflow" ~ "Production",
                                    Ecos_Function=="Respiration"  ~ "Respiration",
                                    Ecos_Function=="Detritus_production" ~ "Detritus production", 
                                    .default = Ecos_Function)) %>% 
  mutate(flux_group_dummy=fct_relevel(flux_group_dummy, "Carbon uptake", 
                                      "Feeding", "Production", "Respiration","Detritus production"))
  
  
str(dat)
dat %>% pull(stock_group) %>% unique()
dat %>% pull(in_out_flux) %>% unique()
dat %>% pull(stock_group_general) %>% unique()
dat%>% pull(Ecos_Function) %>% unique()
dat%>% pull(flux_group_dummy) %>% unique()

factor(dat$stock_group_general)

dat%>% pull(dummy_fill) %>% unique()


  
dat <- dat %>% 
  mutate(dummy_fill = case_when(in_out_flux == "inflow" ~ "inflow",
                           .default = as.character(stock_group_general))) %>% 
  mutate(dummy_fill=fct_relevel(dummy_fill, 
                                "Plants", "Litter",  "SOM", "Herbivores", "Decomposers", 
                                "Soil microorganisms", "Omnivores", "Carnivores", "inflow")) %>% 
  mutate(dummy_fill_2=mutate(dummy_AG_BG = case_when(stock_group=="AG.Herbivores" ~ "AG Consumers",
                                                     stock_group=="BG.Herbivores" ~ "BG Consumers",
                                                     stock_group=="AG.Decomposers" ~ "AG Consumers",
                                                     stock_group=="BG.Decomposers" ~ "BG Consumers",
                                                     stock_group=="AG.Carnivores" ~ "AG Consumers",
                                                     stock_group=="BG.Carnivores" ~ "BG Consumers",
                                                     stock_group=="AG.Omnivores" ~ "AG Consumers",
                                                     stock_group=="BG.Omnivores" ~ "BG Consumers",
                                                     stock_group=="Soil.Microorganisms" ~ "BG Consumers",
                                                     stock_group=="AG.Litter" ~ "Plants, detritus",
                                                     stock_group=="SOM" ~ "Plants, detritus",
                                                     stock_group=="Plants" ~ "Plants, detritus",
                                                     .default = as.character(stock_group)))


dat <- dat %>% mutate(dummy_AG_BG = case_when(stock_group=="AG.Herbivores" ~ "AG Consumers",
                                 stock_group=="BG.Herbivores" ~ "BG Consumers",
                                 stock_group=="AG.Decomposers" ~ "AG Consumers",
                                 stock_group=="BG.Decomposers" ~ "BG Consumers",
                                 stock_group=="AG.Carnivores" ~ "AG Consumers",
                                 stock_group=="BG.Carnivores" ~ "BG Consumers",
                                 stock_group=="AG.Omnivores" ~ "AG Consumers",
                                 stock_group=="BG.Omnivores" ~ "BG Consumers",
                                 stock_group=="Soil.Microorganisms" ~ "BG Consumers",
                                 stock_group=="AG.Litter" ~ "Plants, detritus",
                                 stock_group=="SOM" ~ "Plants, detritus",
                                 stock_group=="Plants" ~ "Plants, detritus",
                                .default = as.character(stock_group)))
  
  
dat %>% pull(dummy_AG_BG) %>% unique()


## Plots -----------

myPalette <- c("#32CD32", "#8B4513", "#46220A", "#008B8B", "#D2691E", "#EDAE83","#EE82EE", "#C71585")

myPalette_with_white <- c("#32CD32", "#8B4513", "#46220A", "#008B8B", "#D2691E", "#EDAE83","#EE82EE",
                          "#C71585", "white")

MyShape=c(13, 21, 22,  24, 23)


dat %>% pull(predictor) %>%  unique()


## facet_wrap( ~ AG_BG) ------------------------------------------------------

SR_effects <- dat %>% 
  filter(predictor=="sowndiv") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~ # in_out_flux + 
                dummy_AG_BG, scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of species richness on stocks", y= "Effects of species richness on flows", 
       col="Trophic group", shape="Ecosystem function")

SR_effects


numfg_effects <- dat %>% 
  filter(predictor=="numfg") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~ # in_out_flux + 
                dummy_AG_BG, scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of FG richness on stocks", y= "Effects of FG richness on flows", 
       col="Trophic group", shape="Ecosystem function")

numfg_effects


leg.ef_effects <- dat %>% 
  filter(predictor=="leg.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~ # in_out_flux + 
                dummy_AG_BG, scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of legumes on stocks", y= "Effects of legumes on flows", 
       col="Trophic group", shape="Ecosystem function")

leg.ef_effects


gr.ef_effects <- dat %>% 
  filter(predictor=="gr.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~ # in_out_flux + 
                dummy_AG_BG, scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of grasses on stocks", y= "Effects of grasses on flows", 
       col="Trophic group", shape="Ecosystem function")

gr.ef_effects



sh.ef_effects <- dat %>% 
  filter(predictor=="sh.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~ # in_out_flux + 
                dummy_AG_BG, scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of small herbs on stocks", y= "Effects of small herbs on flows", 
       col="Trophic group", shape="Ecosystem function")

sh.ef_effects



th.ef_effects <- dat %>% 
  filter(predictor=="th.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~ # in_out_flux + 
                dummy_AG_BG, scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of tall herbs on stocks", y= "Effects of tall herbs on flows", 
       col="Trophic group", shape="Ecosystem function")

th.ef_effects




SR_effects + numfg_effects + 
leg.ef_effects + gr.ef_effects + 
sh.ef_effects + th.ef_effects + 
  plot_annotation(tag_levels = 'a') + 
  plot_layout(guides = "collect", ncol=2) & # theme(legend.position = 'right') +
  # plot_layout(ncol=2) & # ylab(NULL) & 
  theme(legend.position = 'right',
        plot.margin = margin(10, 30, 6, 30), 
        plot.tag = element_text(size = 12, face = 'bold'), 
        plot.tag.position = c(0.08, 1.06))




## facet_wrap( ~ in_out_flux) -------------------------------------------------



SR_effects <- dat %>% 
  filter(predictor=="sowndiv") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~  in_out_flux, scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of species richness on stocks", y= "Effects of species richness on flows", 
       col="Trophic group", shape="Ecosystem function")

SR_effects


numfg_effects <- dat %>% 
  filter(predictor=="numfg") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~ in_out_flux, scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of FG richness on stocks", y= "Effects of FG richness on flows", 
       col="Trophic group", shape="Ecosystem function")

numfg_effects


leg.ef_effects <- dat %>% 
  filter(predictor=="leg.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~  in_out_flux , scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of legumes on stocks", y= "Effects of legumes on flows", 
       col="Trophic group", shape="Ecosystem function")

leg.ef_effects


gr.ef_effects <- dat %>% 
  filter(predictor=="gr.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~  in_out_flux , scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of grasses on stocks", y= "Effects of grasses on flows", 
       col="Trophic group", shape="Ecosystem function")

gr.ef_effects



sh.ef_effects <- dat %>% 
  filter(predictor=="sh.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~  in_out_flux , scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of small herbs on stocks", y= "Effects of small herbs on flows", 
       col="Trophic group", shape="Ecosystem function")

sh.ef_effects



th.ef_effects <- dat %>% 
  filter(predictor=="th.ef") %>% 
  ggplot(aes(x = stock_effect, y = flow_effect, color = stock_group_general, 
             fill=dummy_fill, 
             shape=flux_group_dummy)) +
  geom_point(size=2, stroke=1 ) +
  scale_shape_manual(values=MyShape) +
  scale_color_manual(values=myPalette)+
  scale_fill_manual(values=myPalette_with_white)+
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap( ~  in_out_flux , scales = "free") +
  theme_bw() +   guides(fill = "none") +
  labs(x="Effects of tall herbs on stocks", y= "Effects of tall herbs on flows", 
       col="Trophic group", shape="Ecosystem function")

th.ef_effects




SR_effects + numfg_effects + 
  leg.ef_effects + gr.ef_effects + 
  sh.ef_effects + th.ef_effects + 
  plot_annotation(tag_levels = 'a') + 
  plot_layout(guides = "collect", ncol=2) & # theme(legend.position = 'right') +
  # plot_layout(ncol=2) & # ylab(NULL) & 
  theme(legend.position = 'right',
        plot.margin = margin(10, 30, 6, 30), 
        plot.tag = element_text(size = 12, face = 'bold'), 
        plot.tag.position = c(0.08, 1.06))


