library(readr)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggfortify)
# _____________________________________________________
library(showtext)
font_add_google("Oswald", "oswald")
font_add_google("Roboto", "Roboto")
font_add_google("Gochi Hand", "gochi")
font_add_google("Montserrat Alternates", "montse")
trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)
showtext_auto()
# _____________________________________________________

UNGrowth <- read_csv("UNGrowth.csv")

# Histograms
hi.we <- ggplot(UNGrowth, aes(x=gr)) + 
  geom_histogram(aes(y=..density..), color="black", fill="azure1", bins=30) +
  theme(text=element_text(size=12,  family="montse"),panel.background = element_blank())+
  labs(x="GDP growth rate", y="Density")

hi.webs <- ggplot(UNGrowth, aes(x=gr, color=region, fill=region)) + 
  geom_histogram(aes(y=..density..), bins=30) + 
  theme(panel.background = element_blank()) +
  theme(text=element_text(size=12,  family="montse"),legend.position =c(0.8,0.8), legend.key.size = unit(0.65, 'cm'))+
  labs(x="GDP growth rate", y="Density")

plot_grid(hi.we, hi.webs)

#-------------------------------------------------------------------------------------------------------------------------------------------------
# Scatterplots
xy.fg <- ggplot(data = UNGrowth, aes(x = fertility, y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(colour="lightgrey", size=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="Total fertility rate (num. of children/woman)", y="GDP growth rate")

xy.pg <- ggplot(data = UNGrowth, aes(x = ppgdp, y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(colour="lightgrey", size=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="Per capita GDP ($)", y="GDP growth rate")

xy.lg <- ggplot(data = UNGrowth, aes(x = lifeExpF, y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(colour="lightgrey", size=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="Female life expectancy (years)", y="GDP growth rate")

xy.ug <- ggplot(data = UNGrowth, aes(x = pctUrban, y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(colour="lightgrey", size=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="Percent of population in urban areas", y="GDP growth rate")

xy.ig <- ggplot(data = UNGrowth, aes(x = infantMortality, y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(colour="lightgrey", size=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="Infant deaths by age 1 year (per 1000 live births)", y="GDP growth rate")

grid.arrange(hi.webs, xy.fg, xy.pg, xy.lg, xy.ug, xy.ig, ncol = 3, nrow = 2)

#-------------------------------------------------------------------------------------------------------------------------------------------------
# Scatterplots (logarithm?)
xy.fg.log <- ggplot(data = UNGrowth, aes(x = log(fertility), y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(colour="lightgrey", size=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="log(Total fertility rate (num. of children/woman))", y="GDP growth rate")

xy.pg.log <- ggplot(data = UNGrowth, aes(x = log(ppgdp), y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(colour="lightgrey", size=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="log(Per capita GDP ($))", y="GDP growth rate")

xy.lg.log <- ggplot(data = UNGrowth, aes(x = log(lifeExpF), y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(colour="lightgrey", size=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="log(Female life expectancy (years))", y="GDP growth rate")

xy.ug.log <- ggplot(data = UNGrowth, aes(x = log(pctUrban), y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(colour="lightgrey", size=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="log(Percent of population in urban areas)", y="GDP growth rate")

xy.ig.log <- ggplot(data = UNGrowth, aes(x = log(infantMortality), y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(colour="lightgrey", size=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="log(Infant deaths by age 1 year (per 1000 live births))", y="GDP growth rate")

grid.arrange(hi.webs, xy.fg.log, xy.pg.log, 
             xy.lg.log, xy.ug.log, xy.ig.log, 
             ncol = 3, nrow = 2)

#-------------------------------------------------------------------------------------------------------------------------------------------------
# The model (not taking logarithm) (not deleting outliers)
lm_full <- lm(gr ~ region*(fertility + ppgdp + lifeExpF + pctUrban + infantMortality), data = UNGrowth)
anova(lm_full)
summary(lm_full)
autoplot(lm_full, which = c(1, 3, 5, 2), ncol = 2, label.size = 3, data=UNGrowth, 
         colour='region', label.alpha = 0.5, label.family = "montse") 

lm_r <- lm(gr ~ region+fertility+ppgdp+region:ppgdp, data = UNGrowth)
summary(lm_r)

#-------------------------------------------------------------------------------------------------------------------------------------------------
# The model (non-linear) (deleting outliers)
UNGrowth_do <- UNGrowth[c(-49,-88),]
lm.al.log <- lm(gr ~ region*(fertility + I(fertility^2)+ log(ppgdp) + lifeExpF + pctUrban + infantMortality), data = UNGrowth_do)
anova(lm.al.log)
summary(lm.al.log)
autoplot(lm.al.log, which = c(1, 3, 5, 2), ncol = 2, label.size = 3, data=UNGrowth_do, 
         colour='region', label.alpha = 0.5, label.family = "montse")

lm.al.log.r <- lm(gr ~ region+fertility+I(fertility^2)+log(ppgdp)+pctUrban+region:log(ppgdp), data = UNGrowth_do)
summary(lm.al.log.r)
