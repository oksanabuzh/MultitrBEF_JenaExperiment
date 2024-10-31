# Variance explained by each plant diversity facet for each energy fluxes (partial R2 from regression models)#
# and for the Total Network Energy Flow (sum of partial R2 for all fluxes across the tropic network).

rm(list=ls(all=TRUE))

# Packages
library(tidyverse)
library(ggplot2)


# Data 
df_main <- read_csv("Results/mod_main_text.csv")
str(df_main)

group <- read_csv ("Data/EF_grouped.csv")
names(group)



df_all <- df_main %>% 
  left_join(group, by = join_by(response)) %>% 
  filter(Dimens=="flow") %>% 
 # mutate(r2_part=round(r2_part, digits=2)) %>% 
  mutate(predictor=fct_relevel(predictor, c("sowndiv", "numfg",
                                            "leg.ef", "gr.ef",
                                            "sh.ef", "th.ef",
                                             "FDbranch", "FDis"))) %>% 
  mutate(predictor=fct_recode(predictor, 
                             "Species richness" = "sowndiv",
                              "FG richness" ="numfg",
                              "Legumes"="leg.ef",
                              "Grasses"="gr.ef",
                              "Small herbs"="sh.ef",
                              "Tall herbs"="th.ef"))%>%
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                  "Predation", "Detritus production", "Respiration")))

df_all %>% 
  filter(predictor=="Species richness",  Ecos_Function=="Predation", AG_BG=="AG")%>% 
  select(AG_BG, response, r2_part) #  %>%  summarise(r2_part=sum(r2_part))

grouped <- df_all %>% 
  group_by(Ecos_Function, AG_BG, predictor) %>% 
  summarise(r2_part=sum(r2_part)) %>% 
  unite("response", AG_BG:Ecos_Function, sep = " ", remove = FALSE) %>% 
  ungroup()
  
grouped


sum <- grouped %>% 
  group_by(predictor) %>% 
  summarise(r2_part=sum(r2_part)) %>% 
  mutate(response="Total Network Energy Flow")%>% 
  ungroup()
sum 

my.data <- rbind(grouped %>% 
                   select(response, predictor, r2_part),
                 sum)

my.data

levels(factor(my.data$response))


my.data <- my.data %>% 
  mutate(response=recode_factor(response, "AG_BG Respiration" = "Plant Respiration",
                                "AG Carbon uptake" = "Carbon uptake")) %>% 
    mutate(response=fct_relevel(response, c("Total Network Energy Flow",
                                                               "BG Predation",
                                                               "BG Herbivory", 
                                                               "BG Decomposition",
                                                               "BG Detritus production",
                                                               "BG Respiration",
                                                               "Plant Respiration",
                                                               "Carbon uptake",
                                                               "AG Respiration",
                                                               "AG Detritus production",
                                                               "AG Decomposition",
                                                               "AG Herbivory",
                                                               "AG Predation"))) 
  

max(my.data$r2_part)


# # Bubble chart showing partial R2

fig <- ggplot(my.data%>%
                filter(!response=="Total Network Energy Flow") %>% 
                mutate(r2_part = (ifelse( r2_part>0, r2_part, NA))),
              aes(x = predictor, 
                     y = response,
                     colour = predictor,
                     size = r2_part)) +
  geom_point() +
# geom_text(aes(label = r2_part),  colour = "black",  size = 4) +
  scale_x_discrete(position = "top") +
  scale_size_continuous(range = c(0, 17), breaks = c(0.1,  0.2, 0.3, 0.4)) + 
  scale_color_brewer(palette =  "Paired") +
  #  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom",
        legend.key = element_rect(fill = NA, color = NA),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey80"),
        axis.ticks = element_blank(),
        axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=15))+
  labs(x = NULL, y = NULL, 
       size=bquote("Variance explained, partial R"^"2")) +
  guides(colour=FALSE, size = guide_legend(override.aes = list(colour = "grey"))) +
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=rel(1.2)))

fig


ggsave(fig, file="Results/Fig_R2_Fluxes.png", width = 34,
       height = 25,
       units = "cm", dpi=600)
