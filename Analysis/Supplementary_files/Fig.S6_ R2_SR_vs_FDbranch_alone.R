# Tests for the effects of each diversity facet on each stock and flow across the food web
# Main analysis (in the main text) and supplementary analyses

library(tidyverse)
library(glue)



## Plot R2 for Stocks -----

group <- read_csv ("Data/EF_grouped.csv")
names(group)


df_all <- read_csv("Results/mod_main_text.csv") %>% 
  filter(predictor=="sowndiv_alone" | predictor=="FDbranch") %>% 
  left_join(group, by = join_by(response)) %>% 
  filter(Dimens=="stock") %>% 
  # mutate(r2_part=round(r2_part, digits=2)) %>% 
    mutate(predictor=fct_relevel(predictor, c("sowndiv_alone", "FDbranch"))) %>% 
  mutate(predictor=fct_recode(predictor, 
                              "Species richness" = "sowndiv_alone"))%>%
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


sjPlot::set_theme(base = theme_bw(),
          axis.textsize.x = 0.9, axis.textsize.y = 0.9, axis.textcolor = "black",
          axis.title.color = "black", axis.title.size = 1.2,
          geom.linetype = 1, legend.pos = "bottom") 


#### plot ----
dodge_width <- 0.7

factor((my.data%>%
         filter(!response=="Total Network Stock"))$predictor)

plot_Stocks <- ggplot(my.data%>%
                 filter(!response=="Total Network Stock"),
               aes(y =response, x = r2_part, 
                   color = reorder(predictor, desc(predictor))
                   )) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_point(position = position_dodge(width = dodge_width), size = 4) +
  geom_errorbarh(aes(xmin = 0, xmax = r2_part),
                 position = position_dodge(width = dodge_width), height = 0.1) +
  theme(legend.key=element_blank()) +
  # theme_bw()+
  theme(axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=11),
        axis.title=element_text(size=13, face="bold"), 
        legend.text = element_text(size=10)) +
  #  theme(axis.title.x=element_text(vjust=-0.1), axis.title.y=element_text(vjust=2))+
  labs(y=" ", color="Biodiversity",
       x= expression(paste("Variance explained, ", R^{2})))  
  #  scale_color_manual(values=c( "#F8766D", "#00BFC4"))



plot_Stocks


## Plot R2 for Flows -----


df_all_ <- read_csv("Results/mod_main_text.csv") %>% 
  filter(predictor=="sowndiv_alone" | predictor=="FDbranch") %>% 
  left_join(group, by = join_by(response)) %>% 
  filter(Dimens=="flow") %>% 
  # mutate(r2_part=round(r2_part, digits=2)) %>% 
  mutate(predictor=fct_relevel(predictor, c("sowndiv_alone", "FDbranch"))) %>% 
  mutate(predictor=fct_recode(predictor, 
                              "Species richness" = "sowndiv_alone"))%>%
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration")))

df_all_ %>% 
  filter(predictor=="Species richness",  Ecos_Function=="Predation", AG_BG=="AG")%>% 
  select(AG_BG, response, r2_part) #  %>%  summarise(r2_part=sum(r2_part))

grouped_ <- df_all_ %>% 
  group_by(Ecos_Function, AG_BG, predictor) %>% 
  summarise(r2_part=sum(r2_part)) %>% 
  unite("response", AG_BG:Ecos_Function, sep = " ", remove = FALSE) %>% 
  ungroup()

grouped_


sum_ <- grouped_ %>% 
  group_by(predictor) %>% 
  summarise(r2_part=sum(r2_part)) %>% 
  mutate(response="Total Network Energy Flow")%>% 
  ungroup()
sum_ 

my.data_ <- rbind(grouped_ %>% 
                   select(response, predictor, r2_part),
                 sum_)

my.data_

levels(factor(my.data_$response))


my.data_ <- my.data_ %>% 
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


max(my.data_$r2_part)



dodge_width <- 0.7

factor(my.data_$predictor)

plot_Flow <- ggplot(my.data_%>%
                      filter(!response=="Total Network Energy Flow"),
                      aes(y =response, x = r2_part, 
                          color = reorder(predictor, desc(predictor)))) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_point(position = position_dodge(width = dodge_width), size = 4) +
  geom_errorbarh(aes(xmin = 0, xmax = r2_part),
                 position = position_dodge(width = dodge_width), height = 0.1) +
  theme(legend.key=element_blank()) +
  # theme_bw()+
  theme(axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=11),
        axis.title=element_text(size=13, face="bold"), 
        legend.text = element_text(size=10)) +
  #  theme(axis.title.x=element_text(vjust=-0.1), axis.title.y=element_text(vjust=2))+
  labs(y=" ", color="Biodiversity",
       x= expression(paste("Variance explained, ", R^{2}))) 



plot_Flow

              
