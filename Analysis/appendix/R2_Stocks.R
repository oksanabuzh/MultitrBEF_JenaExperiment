# Variance explained by each plant diversity facet for each standing stock (partial R2 from regression models)
# and for the Total Network Stock (sum of partial R2 for all stocks across the tropic network).

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
  filter(Dimens=="stock") %>% 
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
  mutate(Tr_level=as_factor(Trophic_level)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus","Herbivores",
                                         "Decomposers","Omnivores","Carnivores"))) %>% 
  mutate(response=fct_relevel(response,c("BG.Carnivores.Stock",
                                         "BG.Omnivores.Stock",
                                         "BG.Herbivores.Stock",
                                         "BG.Decomposers.Stock",
                                         "Soil.Microorganisms.Stock",
                                         "SOM.Stock",
                                         "Plants.Stock",
                                         "AG.Litter.Stock",
                                         "AG.Decomposers.Stock",
                                         "AG.Herbivores.Stock",
                                         "AG.Omnivores.Stock",
                                         "AG.Carnivores.Stock"))) %>% 
  mutate(response=fct_recode(response, "BG Carnivores" ="BG.Carnivores.Stock",
                                        "BG Omnivores" = "BG.Omnivores.Stock",
                                        "BG Herbivores" = "BG.Herbivores.Stock",
                                        "BG Decomposers" = "BG.Decomposers.Stock",
                                        "Soil Microorganisms" = "Soil.Microorganisms.Stock",
                                        "SOM" = "SOM.Stock",
                                        "Plants" = "Plants.Stock",
                                        "AG Litter" = "AG.Litter.Stock",
                                        "AG Decomposers" = "AG.Decomposers.Stock",
                                        "AG Herbivores" = "AG.Herbivores.Stock",
                                        "AG Omnivores" = "AG.Omnivores.Stock",
                                        "AG Carnivores" = "AG.Carnivores.Stock"))


sum <- df_all %>% 
  group_by(predictor) %>% 
  summarise(r2_part=sum(r2_part)) %>% 
  mutate(response="Total Network Stock")



my.data <- rbind(df_all %>% 
                   select(response, predictor, r2_part),
                 sum)

levels(my.data$response)

my.data <- my.data %>% mutate(response=fct_relevel(response, c("Total Network Stock",
                                                               "BG Carnivores",
                                                               "BG Omnivores",
                                                               "BG Herbivores",
                                                               "BG Decomposers",
                                                               "Soil Microorganisms",
                                                               "SOM",
                                                               "Plants",
                                                               "AG Litter",
                                                               "AG Decomposers",
                                                               "AG Herbivores",
                                                               "AG Omnivores",
                                                               "AG Carnivores"))) 
  


  # # Bubble chart showing partial R2

fig <- ggplot(my.data%>%
                filter(!response=="Total Network Stock") %>% 
                mutate(r2_part = (ifelse( r2_part>0, r2_part, NA))),
              aes(x = predictor, 
                     y = response,
                     colour = predictor,
                     size = r2_part)) +
  geom_point() +
#  geom_text(aes(label = r2_part),  colour = "black",  size = 4) +
  scale_x_discrete(position = "top") +
  scale_size_continuous(range = c(2, 17)) + 
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
  theme(legend.title=element_text(size=14),
        legend.text=element_text(size=rel(1.2)))

fig


ggsave(fig, file="Results/Fig_R2_Stocks.png", width = 34,
       height = 25,
       units = "cm", dpi=600)
