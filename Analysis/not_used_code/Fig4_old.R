# Plots for the fluxes
# grouped by ecosystem functions, and AG vs BG

rm(list=ls(all=TRUE))
 dev.off()

# Packages
library(tidyverse)
library(rcompanion)
library(plyr)
library(reshape)
library(Rmisc)
library(forcats)

# Data main
df_main <- read_csv("Results/mod_main_text.csv")
str(df_main)

group <- read_csv ("Data/EF_grouped.csv")
names(group)

df_all <- df_main %>% 
  left_join(group, by = join_by(response)) %>% 
  select(-Tr_Group, -Trophic_level, -r2_part, -R2_model, -p, -estimate, -effect_size, -lambda, -formula) %>% 
  filter(Dimens=="flow") %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  pivot_wider(names_from = "predictor", values_from = "effect_size_st")

str(df_all)

factor(df_all$AG_BG)


# Get descriptive statistics----

## for Ecos_Function ----

names(df_all)
Ecos_Function<- df_all$Ecos_Function
tmp <- df_all[,5:10]
d <- ddply(tmp, c("Ecos_Function"),
           function(x) rbind(apply(x,2,sd),
                             apply(x,2,function(y)Rmisc::CI(y, ci = 0.95)),
                             apply(x,2,function(y)quantile(y,c(0.05,0.95)))
           ))
d



d$stat <- c(rep(c("sd","ci_upper","mean","ci_lower","qntl_lower", "qntl_upper")))

y <- melt(d,c("Ecos_Function","stat"))
stat_EF <-  cast(y, ... ~ variable+stat)

stat_EF <- stat_EF %>%  
  dplyr::rename(Group=Ecos_Function)

names(stat_EF)

# check result:
df_check <- df_all %>% 
  filter(Ecos_Function=="Herbivory")

Rmisc::CI(df_check$sowndiv, ci = 0.95)

# compare  to

stat_EF %>% 
  filter(Group=="Herbivory") %>% 
  select(sowndiv_ci_upper, sowndiv_mean, sowndiv_ci_lower)



## for AG vs BG compartment----

df_AG_BG <- df_all %>% 
  filter(!AG_BG=="AG_BG")

factor(df_AG_BG$AG_BG)

AG_BG<- factor(df_AG_BG$AG_BG)
tmp2 <- df_all[,5:10]
d_AGBG <- ddply(tmp2, c("AG_BG"),
                function(x) rbind(apply(x,2,sd),
                                  apply(x,2,function(y)Rmisc::CI(y, ci = 0.95)),
                                  apply(x,2,function(y)quantile(y,c(0.05,0.95)))
                ))
d_AGBG

d_AGBG$stat <- c(rep(c("sd","ci_upper","mean","ci_lower","qntl_lower", "qntl_upper")))

y_AGBG <- melt(d_AGBG,c("AG_BG","stat"))
stat_AG_BG <-  cast(y_AGBG, ... ~ variable+stat)

stat_AG_BG <- stat_AG_BG %>%  
  dplyr::rename(Group=AG_BG)

names(stat_AG_BG)

#-----
stat_EF
stat_AG_BG

# Add datasets vertically
my.data <- rbind(stat_EF, stat_AG_BG)

my.data$Group

my.data <- my.data %>% 
  mutate(Group = fct_relevel(Group, 
                             "BG", "AG",
                             "Carbon uptake", "Herbivory", "Decomposition", 
                             "Predation", "Detritus production", "Respiration"))

my.data
library(tidyverse)
write_csv(my.data, "Results/stats_for_Fig4.csv")


my.data %>% 
  summarise(max=min(sowndiv_ci_lower))

k<-my.data %>% 
  filter(!Group=="Carbon uptake") %>% 
summarise(min1 = min(sowndiv_ci_lower), max1 = max(sowndiv_ci_lower),
          min2 = min(numfg_ci_lower), max2 = max(numfg_ci_lower),
          min3 = min(leg.ef_ci_lower), max3 = max(leg.ef_ci_lower),
          min4 = min(gr.ef_ci_lower), max4 = max(gr.ef_ci_lower),
          min5 = min(sh.ef_ci_lower), max5 = max(sh.ef_ci_lower),
          min6 = min(th.ef_ci_lower), max6 = max(th.ef_ci_lower),
          min7 = min(sowndiv_ci_upper), max7 = max(sowndiv_ci_upper),
          min8 = min(numfg_ci_upper), max8 = max(numfg_ci_upper),
          min9 = min(leg.ef_ci_upper), max9 = max(leg.ef_ci_upper),
          min10 = min(gr.ef_ci_upper), max10 = max(gr.ef_ci_upper),
          min11 = min(sh.ef_ci_upper), max11 = max(sh.ef_ci_upper),
          min12 = min(th.ef_ci_upper), max12 = max(th.ef_ci_upper)) %>% 
  pivot_longer(cols = starts_with("m"),, names_to = "names", values_to = "count") %>% 
  summarise(min = min(count), max = max(count))
k

library(ggplot2)

# Plot of effects (slopes) - Diversity vs. Ecosystem Functions ---------- Fig. 4.


color1 <- c("royalblue","royalblue","royalblue","royalblue","royalblue","royalblue",
                       "royalblue","royalblue")
                       

plot1 <- my.data %>%
  #  ggplot( aes(x = Group, y = sowndiv_mean, ymin = sowndiv_qntl_lower, ymax = sowndiv_qntl_upper)) +
  ggplot( aes(x = Group, y = sowndiv_mean, ymin = sowndiv_ci_lower, ymax = sowndiv_ci_upper)) +
  geom_hline(yintercept = 0, colour="grey30", linetype="dotted", size=0.1)+
  geom_point( position = position_nudge(0), size=1.5,colour = color1) +
  geom_errorbar(position = position_nudge(0), width = 0, size=0.3,colour = color1) +
  coord_flip() +
  theme_bw()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
  theme(plot.margin = unit(c(1,0.35,1,1.1), "lines"),
        panel.border = element_rect(fill = NA, colour = "grey30", size=0.8))+
  theme(axis.text.x=element_text(size=8,colour = "black"),
        axis.text.y=element_text(size=10,colour = "black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10))+
  ylab("SR effects")+
  xlab("") +
  ylim(-0.44,0.44)

plot1




color2 <- c("red","red","royalblue","royalblue","red","red",
                 "red","red")
                 

plot2 <- my.data %>%
  ggplot( aes(x = Group, y = numfg_mean, ymin = numfg_ci_lower, ymax = numfg_ci_upper)) +
  geom_hline(yintercept = 0, colour="grey30", linetype="dotted", size=0.1)+
  geom_point( position = position_nudge(0), size=1.5,colour = color2) +
  geom_errorbar(position = position_nudge(0), width = 0, size=0.3,colour = color2) +
  coord_flip() +
  theme_bw()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
  theme(axis.text.y=element_blank())+
  theme(plot.margin = unit(c(1,0.35,1,0.1), "lines"),
        panel.border = element_rect(fill = NA, colour = "grey30", size=0.8))+
  theme(axis.text=element_text(colour = "black"),
        axis.text.x=element_text(size=8,colour = "black"),
        axis.title=element_text(size=10))+
  ylab("FG number effects")+
  xlab("")+
  ylim(-0.44,0.44)
plot2



color3 <- c("red","royalblue","royalblue","royalblue","royalblue","red",
                 "royalblue","royalblue")
                 

plot3 <- my.data %>%
  ggplot( aes(x = Group, y = leg.ef_mean, ymin = leg.ef_ci_lower, ymax = leg.ef_ci_upper)) +
  geom_hline(yintercept = 0, colour="grey30", linetype="dotted", size=0.1)+
  geom_point( position = position_nudge(0), size=1.5,colour = color3) +
  geom_errorbar(position = position_nudge(0), width = 0, size=0.3,colour = color3) +
  coord_flip() +
  theme_bw()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
  theme(axis.text.y=element_blank())+
  theme(plot.margin = unit(c(1,0.35,1,0.1), "lines"),
        panel.border = element_rect(fill = NA, colour = "grey30", size=0.8))+
  theme(axis.text=element_text(colour = "black"),
        axis.text.x=element_text(size=8,colour = "black"),
        axis.title=element_text(size=10))+
  ylab("Legume effects")+
  xlab("")+
  ylim(-0.44,0.44)
plot3



color4 <-  c("royalblue","red","royalblue","royalblue","red","royalblue",
                        "royalblue","red")
                        
plot4 <- my.data %>%
  ggplot( aes(x = Group, y = gr.ef_mean, ymin = gr.ef_ci_lower, ymax = gr.ef_ci_upper)) +
  geom_hline(yintercept = 0, colour="grey30", linetype="dotted", size=0.1)+
  geom_point( position = position_nudge(0), size=1.5,colour = color4) +
  geom_errorbar(position = position_nudge(0), width = 0, size=0.3,colour = color4) +
  coord_flip() +
  theme_bw()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
  theme(plot.margin = unit(c(1,0.35,1,1.1), "lines"),
        panel.border = element_rect(fill = NA, colour = "grey30", size=0.8))+
  theme(axis.text.x=element_text(size=8,colour = "black"),
        axis.text.y=element_text(size=10,colour = "black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10))+
  ylab("Grass effects")+
  xlab("")+
  ylim(-0.44,0.44)

plot4


color5 <-  c("royalblue","royalblue","royalblue","royalblue","red","royalblue",
                        "royalblue","red")
                        
plot5 <- my.data %>%
  ggplot( aes(x = Group, y = sh.ef_mean, ymin = sh.ef_ci_lower, ymax = sh.ef_ci_upper)) +
  geom_hline(yintercept = 0, colour="grey30", linetype="dotted", size=0.1)+
  geom_point( position = position_nudge(0), size=1.5,colour = color5) +
  geom_errorbar(position = position_nudge(0), width = 0, size=0.3,colour = color5) +
  coord_flip() +
  #scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
  theme(plot.margin = unit(c(1,0.35,1,0.1), "lines"),
        panel.border = element_rect(fill = NA, colour = "grey30", size=0.8))+
  theme(axis.text=element_text(colour = "black"),
        axis.text.x=element_text(size=8,colour = "black"),
        axis.title=element_text(size=10))+
  ylab("Small herb effects")+
  xlab("")+
  ylim(-0.44,0.44)
plot5


color6 <-  c("red","red","red","red","red","red",
                  "red","red")
                  
plot6 <- my.data %>%
  ggplot( aes(x = Group, y = th.ef_mean, ymin = th.ef_ci_lower, ymax = th.ef_ci_upper)) +
  geom_hline(yintercept = 0, colour="grey30", linetype="dotted", size=0.1)+
  geom_point( position = position_nudge(0), size=1.5,colour = color6) +
  geom_errorbar(position = position_nudge(0), width = 0, size=0.3,colour = color6) +
  coord_flip() +
  # scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
  theme(plot.margin = unit(c(1,0.35,1,0.1), "lines"),
        panel.border = element_rect(fill = NA, colour = "grey30", size=0.8))+
  theme(axis.text=element_text(colour = "black"),
        axis.text.x=element_text(size=8,colour = "black"),
        axis.title=element_text(size=10))+
  ylab("Tall herb effects")+
  xlab("")+
  ylim(-0.44,0.44)

plot6


library(grid) 
library(gridExtra)
library(officer)
library(devEMF)
library(magrittr)
library(ggplot2)

filename <- tempfile(fileext = ".emf")
emf(file = filename, width = 6, height = 7)
grid.arrange(plot1, plot2, plot3, plot4,  plot5, plot6, nrow = 2, widths=c(.57,.3,.3))


grid.text((paste("a")),
          x = unit(0.27, "npc"), y = unit(0.992, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))

grid.text((paste("b")),
          x = unit(0.52, "npc"), y = unit(0.992, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))

grid.text((paste("c")),
          x = unit(0.78, "npc"), y = unit(0.992, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))

grid.text((paste("d")),
          x = unit(0.27, "npc"), y = unit(0.493, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))

grid.text((paste("e")),
          x = unit(0.52, "npc"), y = unit(0.493, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))

grid.text((paste("f")),
          x = unit(0.78, "npc"), y = unit(0.493, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))


grid.lines(x = unit(c(0.08, 0.985), "npc"),
           y = unit(c(0.685, 0.685), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col="azure3",lwd=0.1, lty="solid"), draw = TRUE, vp = NULL)

grid.lines(x = unit(c(0.08, 0.985), "npc"),
           y = unit(c(0.185, 0.185), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col="azure3",lwd=0.1, lty="solid"), draw = TRUE, vp = NULL)



dev.off()
read_docx() %>% 
  body_add_img(src = filename, width = 6, height = 7)%>% 
  print(target = "Results/Fig_4.docx")

