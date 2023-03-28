# Plots for the stocks
# by trophic groups, trophic levels and AG vs BG.

rm(list=ls(all=TRUE))

# Packages
library(tidyverse)
library(rcompanion)

library(plyr)
library(reshape)
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

Tr_Group<- df_all$Tr_Group
tmp <- df_all[,7:12]

d <- ddply(tmp, c("Tr_Group"),
                      function(x) rbind(apply(x,2,mean),
                                        apply(x,2,sd),
                                        apply(x,2,function(y)as.vector(t.test(y, conf.level = 0.95)$conf.int))
                                        ))

# line 39 does not work

d$stat <- c(rep(c("mean","sd","ci95.lower","ci95.upper")))

y <- melt(d,c("Tr_Group","stat"))
Stock.stat_Tr_Group <-  cast(y, ... ~ variable+stat)

# alternative:

Tr_Gr <- df_all %>% 
  group_by(Tr_Group) %>% 
  summarize_at(vars(sowndiv, numfg, leg.ef, gr.ef, sh.ef, th.ef), 
               list(mean = mean, 
                    sd = sd, 
                    CI_5 = as.vector(t.test(conf.level = 0.95)$conf.int)[1],
                    CI_25= as.vector(t.test(conf.level = 0.95)$conf.int)[2]))
# lines 56 and 57 do not work
 


Tr_Gr <- df_all %>% 
  group_by(Tr_Group) %>% 
  summarize(CI_5 = (t.test(sowndiv, conf.level = 0.95)$conf.int)[1],
            CI_25= (t.test(sowndiv, conf.level = 0.95)$conf.int)[2])

Tr_Gr
# does not give results for the separate groups



# End

#####################################

Tr_Gr <- df_all %>% 
  group_by(Tr_Group) %>% 
  summarize_at(vars(sowndiv, numfg, leg.ef, gr.ef, sh.ef, th.ef), 
               list(mean = mean, sd = sd))
                    
Tr_Gr

str(Tr_Gr)

as.vector(t.test(df_all$sowndiv, conf.level = 0.95)$conf.int)[2]

test2 <- t.test(df_all$sowndiv, conf.level = 0.95)
test2$conf.int




# --- Descriptive Statistical Analysis 

## for trophic group: 
Tr_Group<- Index3$Tr_Group
tmp <- Index3[,6:11]
d <- ddply(tmp,
           c("Tr_Group"),
           function(x) rbind(apply(x,2,mean),apply(x,2,sd),apply(x,2,function(y)quantile(y,c(0.05,0.95)))))

d$stat <- c(rep(c("mean","sd","ci95.lower","ci95.upper")))

# - - -
y <- melt(d,c("Tr_Group","stat"))
Stock.stat_Tr_Group <-  cast(y, ... ~ variable+stat)
write.csv(Stock.stat_Tr_Group, file = "Srock.stat_Tr_Group.csv")

## for trophic level:
Trophic_level<- factor(Index3$Trophic_level)
tmp <- Index3[,6:11]
d <- ddply(tmp,
           c("Trophic_level"),
           function(x) rbind(apply(x,2,mean),apply(x,2,sd),apply(x,2,function(y)quantile(y,c(0.05,0.95)))))

d$stat <- c(rep(c("mean","sd","ci95.lower","ci95.upper")))
#d$plot.no <-  sort(rep(1:24,4))
#to form the table
y <- melt(d,c("Trophic_level","stat"))
Stock.stat_TrLev <-  cast(y, ... ~ variable+stat)
write.csv(Stock.stat_TrLev, file = "Srock.stat_TrLev.csv")


## for AG vs BG compartment:
      #new data for AG _ BG analysis, where plant stock is separated among root and shoot
Slopes_ <- read.csv ( "Slopes_Stocks.csv", header=T)
Index2_ <- read.csv ("separately_root_shoot/stock_groups.csv", header=T)
names(Index2_)
## to merge slopes with the groupping information
Index3_ <- merge(Index2_, Slopes_[,2:8])
names(Index3_)

AG_BG<- factor(Index3_$AG_BG)
tmp <- Index3_[,6:11]
d <- ddply(tmp,
           c("AG_BG"),
           function(x) rbind(apply(x,2,mean),apply(x,2,sd),apply(x,2,function(y)quantile(y,c(0.05,0.95)))))

d$stat <- c(rep(c("mean","sd","ci95.lower","ci95.upper")))
#d$plot.no <-  sort(rep(1:24,4))
#to form the table
y <- melt(d,c("AG_BG","stat"))
Stock.stat_AG_BG <-  cast(y, ... ~ variable+stat)
write.csv(Stock.stat_AG_BG, file = "Stock.stat_AG_BG.csv")


Stock.stat_Tr_Group$stock_group <-Stock.stat_Tr_Group$Tr_Group
Stock.stat_TrLev$stock_group <-Stock.stat_TrLev$Trophic_level
Stock.stat_AG_BG$stock_group <-Stock.stat_AG_BG$AG_BG

names(Stock.stat_Tr_Group)
names(Stock.stat_TrLev)
names(Stock.stat_AG_BG)


# Add datasets vertically
my.data <- rbind(Stock.stat_Tr_Group[,2:26], Stock.stat_TrLev[,2:26], Stock.stat_AG_BG[,2:26])


#names(my.data)
#my.data[6,2:25]=NA

library(ggplot2)


# Plot of effects (slopes) - Diversity vs. Ecosystem Functions ---------- Fig. 1.
color_y <- c("black","black", 
             "black", "black","black",
             "#32CD32", "#8B4513",   "#008B8B", "#D2691E", "#EE82EE", "#C71585")


color1 <- c("red","royalblue","royalblue","royalblue","royalblue","royalblue",
            "royalblue","royalblue", "royalblue",
            "royalblue","royalblue")

plot1 <- my.data %>%
  mutate(stock_group = fct_relevel(stock_group, 
                                   "BG", "AG",
                                   "1", "2","3",
                                     "Plants", "Detritus", 
                                     "Herbivores", "Decomposers",
                                     "Omnivores", "Carnivores")) %>% # to arrange the order in "stock_group" on the plot:
  ggplot( aes(x = stock_group, y = sowndiv_mean, ymin = sowndiv_ci95.lower, ymax = sowndiv_ci95.upper)) +
  geom_hline(yintercept = 0, colour="grey30", linetype="dotted", size=0.1)+
  geom_point( position = position_nudge(0), size=1.5,colour = color1) +
  geom_errorbar(position = position_nudge(0), width = 0, size=0.3,colour = color1) +
  coord_flip() +
  theme_bw()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
  theme(plot.margin = unit(c(1,0.35,1,1.1), "lines"),
        panel.border = element_rect(fill = NA, colour = "grey30", size=0.8))+
  theme(axis.text.x=element_text(size=9,colour = "black"),
        axis.text.y=element_text(size=10,colour = color_y),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10))+
  ylab("SR effects")+
    xlab("")
# labs(x=expression(bold("Standing stock, g m "^-2)))
#plot1

# Carnivores
# Decomposers
# Detritus
# Herbivores
# Omnivores
# Plants

color2 <- c("royalblue","red","royalblue","red","royalblue","royalblue",
            "royalblue","red", "royalblue",
            "royalblue","royalblue")

plot2 <- my.data %>%
  mutate(stock_group = fct_relevel(stock_group, 
                                   "BG", "AG",
                                   "1", "2","3",
                                   "Plants", "Detritus", 
                                   "Herbivores", "Decomposers",
                                   "Omnivores", "Carnivores")) %>% # to arrange the order in "stock_group" on the plot:
  ggplot( aes(x = stock_group, y = numfg_mean, ymin = numfg_ci95.lower, ymax = numfg_ci95.upper)) +
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
        axis.text.x=element_text(size=9,colour = "black"),
        axis.title=element_text(size=10))+
  ylab("FG number effects")+
  xlab("")
#plot2


color3 <- c("royalblue","royalblue","royalblue","red","red","red",
            "royalblue","royalblue", "royalblue",
            "royalblue","royalblue")
                 
plot3 <- my.data %>%
  mutate(stock_group = fct_relevel(stock_group, 
                                   "BG", "AG",
                                   "1", "2","3",
                                   "Plants", "Detritus", 
                                   "Herbivores", "Decomposers",
                                   "Omnivores", "Carnivores")) %>% # to arrange the order in "stock_group" on the plot:
  ggplot( aes(x = stock_group, y = leg.ef_mean, ymin = leg.ef_ci95.lower, ymax = leg.ef_ci95.upper)) +
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
        axis.text.x=element_text(size=9,colour = "black"),
        axis.title=element_text(size=10))+
  ylab("Legume effects")+
  xlab("")
#plot3

color4 <- c("royalblue","red","red","red","royalblue","royalblue",
            "red","red", "royalblue",
            "red","red")

plot4 <- my.data %>%
  mutate(stock_group = fct_relevel(stock_group, 
                                   "BG", "AG",
                                   "1", "2","3",
                                   "Plants", "Detritus", 
                                   "Herbivores", "Decomposers",
                                   "Omnivores", "Carnivores")) %>% # to arrange the order in "stock_group" on the plot:
  ggplot( aes(x = stock_group, y = gr.ef_mean, ymin = gr.ef_ci95.lower, ymax = gr.ef_ci95.upper)) +
  geom_hline(yintercept = 0, colour="grey30", linetype="dotted", size=0.1)+
  geom_point( position = position_nudge(0), size=1.5,colour = color4) +
  geom_errorbar(position = position_nudge(0), width = 0, size=0.3,colour = color4) +
  coord_flip() +
  theme_bw()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
  theme(plot.margin = unit(c(1,0.35,1,1.1), "lines"),
        panel.border = element_rect(fill = NA, colour = "grey30", size=0.8))+
  theme(axis.text.x=element_text(size=9,colour = "black"),
        axis.text.y=element_text(size=10,colour = color_y),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10))+
  ylab("Grass effects")+
xlab("")
  #  labs(x=expression(bold("Standing stock, g m "^-2)))
#plot4

color5 <- c("red","royalblue","royalblue","royalblue","royalblue","royalblue",
            "royalblue","royalblue", "red",
            "royalblue","royalblue")

plot5 <- my.data %>%
  mutate(stock_group = fct_relevel(stock_group, 
                                   "BG", "AG",
                                   "1", "2","3",
                                   "Plants", "Detritus", 
                                   "Herbivores", "Decomposers",
                                   "Omnivores", "Carnivores")) %>% # to arrange the order in "stock_group" on the plot:
  ggplot( aes(x = stock_group, y = sh.ef_mean, ymin = sh.ef_ci95.lower, ymax = sh.ef_ci95.upper)) +
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
        axis.text.x=element_text(size=9,colour = "black"),
        axis.title=element_text(size=10))+
  ylab("Small herb effects")+
  xlab("")
#plot5

color6 <- c("red","red","royalblue","red","red","royalblue",
            "royalblue","red", "red",
            "royalblue","red")

plot6 <- my.data %>%
  mutate(stock_group = fct_relevel(stock_group, 
                                   "BG", "AG",
                                   "1", "2","3",
                                   "Plants", "Detritus", 
                                   "Herbivores", "Decomposers",
                                   "Omnivores", "Carnivores")) %>% # to arrange the order in "stock_group" on the plot:
  ggplot( aes(x = stock_group, y = th.ef_mean, ymin = th.ef_ci95.lower, ymax = th.ef_ci95.upper)) +
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
        axis.text.x=element_text(size=9,colour = "black"),
        axis.title=element_text(size=10))+
  ylab("Tall herb effects")+
  xlab("")

plot6
# Separate Ecosystem functions by AG and BG ecosystem parts and grouped by consumer and plant
# --- Descriptive Statistical Analysis --- (by model) --- for Ecosystem functions

## mean and CI95  within AG Consumers
library(grid) 
library(gridExtra)
library(officer)
library(devEMF)
library(magrittr)
library(ggplot2)

filename <- tempfile(fileext = ".emf")
emf(file = filename, width = 6, height = 7)
grid.arrange(plot1, plot2, plot3, plot4,  plot5, plot6,nrow = 2, widths=c(.5,.3,.3))


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
  print(target = "fig2.docx")

