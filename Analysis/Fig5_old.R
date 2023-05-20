# Plots for the stocks
# by trophic groups, trophic levels and AG vs BG.

rm(list=ls(all=TRUE))

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
  select(-Ecos_Function, -r2_part, -R2_model, -p, -estimate, -effect_size, -lambda, -formula) %>% 
  filter(Dimens=="stock") %>% 
  mutate(Tr_level=as_factor(Trophic_level)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus","Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  pivot_wider(names_from = "predictor", values_from = "effect_size_st")

str(df_all)

# Get descriptive statistics----

## for trophic group----

Rmisc::CI(df_all$sowndiv, ci = 0.95)

Tr_Group<- df_all$Tr_Group
tmp <- df_all[,7:12]
d <- ddply(tmp, c("Tr_Group"),
           function(x) rbind(apply(x,2,sd),
                             apply(x,2,function(y)Rmisc::CI(y, ci = 0.95)),
                             apply(x,2,function(y)quantile(y,c(0.05,0.95)))
           ))
d

d$stat <- c(rep(c("sd","ci_upper","mean","ci_lower","qntl_lower", "qntl_upper")))

y <- melt(d,c("Tr_Group","stat"))
stat_TrGroup <-  cast(y, ... ~ variable+stat)

stat_TrGroup <- stat_TrGroup %>%  
  dplyr::rename(Group=Tr_Group)

names(stat_TrGroup)

# check result:
df_check <- df_all %>% 
  filter(Tr_Group=="Carnivores")

df_check  

Rmisc::CI(df_check$sowndiv, ci = 0.95)

#   upper        mean       lower 
# 1.15440099 -0.07179485 -1.29799069 

stat_TrGroup%>% 
  filter(Group=="Carnivores")

## for trophic level-----

Tr_level<- df_all$Tr_level
tmp <- df_all[,7:12]
d_TL <- ddply(tmp, c("Tr_level"),
           function(x) rbind(apply(x,2,sd),
                             apply(x,2,function(y)Rmisc::CI(y, ci = 0.95)),
                             apply(x,2,function(y)quantile(y,c(0.05,0.95)))
           ))
d_TL

d_TL$stat <- c(rep(c("sd","ci_upper","mean","ci_lower","qntl_lower", "qntl_upper")))

y_TL <- melt(d_TL,c("Tr_level","stat"))
stat_Tr_level <-  cast(y_TL, ... ~ variable+stat)


stat_Tr_level <- stat_Tr_level %>%  
  dplyr::rename(Group=Tr_level)

names(stat_Tr_level)

## for AG vs BG compartment----
      #new data for AG _ BG analysis, where plant stock is separated among root and shoot
df_ShootRoot <- read_csv("Results/mod_ShootRoot.csv")
str(df_ShootRoot)

group_ShootRoot <- read_csv ("Data/Shoot_Root_separately/EF_grouped_Shoot_Root.csv")
str(group_ShootRoot)

df_AG_BG <- bind_rows(df_main, df_ShootRoot) %>% 
  left_join(group_ShootRoot, by = join_by(response)) %>% 
  filter(!response=="Plants.Stock") %>% 
  select(-Ecos_Function, -r2_part, -R2_model, -p, -estimate, -effect_size, -lambda, -formula) %>% 
  filter(Dimens=="stock") %>% 
  pivot_wider(names_from = "predictor", values_from = "effect_size_st")

str(df_AG_BG)


AG_BG<- factor(df_AG_BG$AG_BG)
tmp2 <- df_AG_BG[,6:11]
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

#s-----
stat_TrGroup
stat_Tr_level
stat_AG_BG

# Add datasets vertically
my.data <- rbind(stat_TrGroup, stat_Tr_level, stat_AG_BG)

my.data <- my.data %>% 
  mutate(Group = fct_relevel(Group, 
                           "BG", "AG",
                           "1", "2","3",
                           "Plants", "Detritus", 
                           "Herbivores", "Decomposers",
                           "Omnivores", "Carnivores"))

my.data

library(readr)
write_csv(my.data, "Results/stats_for_Fig5.csv")


k<-my.data %>% 
  filter(!Group=="Plants") %>% 
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
# Plot of effects (slopes) - Diversity vs. Ecosystem Functions ---------- Fig. 5.


color1 <- c("royalblue","royalblue","royalblue","royalblue","royalblue","red",
            "royalblue","royalblue", "royalblue",
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
    xlab("")+
  ylim(-2.3,2.3)

plot1


# Carnivores
# Decomposers
# Detritus
# Herbivores
# Omnivores
# Plants

color2 <- c("royalblue","royalblue","red","royalblue","royalblue","royalblue",
            "royalblue","royalblue", "royalblue",
            "royalblue","royalblue")

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
  ylim(-2.3,2.3)
plot2


color3 <- c("red","royalblue","red","royalblue","royalblue","royalblue",
            "royalblue","royalblue", "royalblue",
            "royalblue","red")
                 
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
  ylim(-2.3,2.3)
plot3

color4 <- c("royalblue","red","red","red","royalblue","royalblue",
            "red","red", "royalblue",
            "red","royalblue")

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
  ylim(-2.3,2.3)
  
plot4

color5 <- c("royalblue","royalblue","royalblue","royalblue","red","red",
            "royalblue","royalblue", "red",
            "royalblue","royalblue")

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
  ylim(-2.3,2.3)
plot5

color6 <- c("royalblue","royalblue","red","red","red","red",
            "royalblue","red", "red",
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
  ylim(-2.3,2.3)

plot6


library(grid) 
library(gridExtra)
library(officer)
library(devEMF)
library(magrittr)
library(ggplot2)

filename <- tempfile(fileext = ".emf")
emf(file = filename, width = 6, height = 7)
grid.arrange(plot1, plot2, plot3, plot4,  plot5, plot6, nrow = 2, widths=c(.5,.3,.3))


grid.text((paste("a")),
          x = unit(0.22, "npc"), y = unit(0.992, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))

grid.text((paste("b")),
          x = unit(0.5, "npc"), y = unit(0.992, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))

grid.text((paste("c")),
          x = unit(0.77, "npc"), y = unit(0.992, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))

grid.text((paste("d")),
          x = unit(0.23, "npc"), y = unit(0.493, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))

grid.text((paste("e")),
          x = unit(0.5, "npc"), y = unit(0.493, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))

grid.text((paste("f")),
          x = unit(0.77, "npc"), y = unit(0.493, "npc"), just = c("left", "top"), 
          gp = gpar(fontface = "bold", fontsize = 12, col = "black"))


grid.lines(x = unit(c(0.07, 0.985), "npc"),
           y = unit(c(0.758, 0.758), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col="azure3",lwd=0.1, lty="solid"), draw = TRUE, vp = NULL)



grid.lines(x = unit(c(0.07, 0.985), "npc"),
           y = unit(c(0.653, 0.653), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col="azure3",lwd=0.1, lty="solid"), draw = TRUE, vp = NULL)

grid.lines(x = unit(c(0.07, 0.985), "npc"),
           y = unit(c(0.258, 0.258), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col="azure3",lwd=0.1, lty="solid"), draw = TRUE, vp = NULL)

grid.lines(x = unit(c(0.07, 0.985), "npc"),
           y = unit(c(0.153, 0.153), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col="azure3",lwd=0.1, lty="solid"), draw = TRUE, vp = NULL)



dev.off()
read_docx() %>% 
  body_add_img(src = filename, width = 6, height = 7)%>% 
  print(target = "Results/Fig_5.docx")

