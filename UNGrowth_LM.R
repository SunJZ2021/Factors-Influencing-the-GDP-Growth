library(readr)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggfortify)
# _____________________________________________________________________
library(showtext)
font_add_google("Oswald", "oswald")
font_add_google("Roboto", "Roboto")
font_add_google("Gochi Hand", "gochi")
font_add_google("Montserrat Alternates", "montse")
trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)
showtext_auto()
# _____________________________________________________________________

UNGrowth <- read_csv("UNGrowth.csv")

# Histograms
hi.we <- ggplot(UNGrowth, aes(x=gr)) + 
  geom_histogram(aes(y=after_stat(density)), 
                 color="black", 
                 fill="azure1", 
                 bins=30) +
  theme(text=element_text(size=12,  family="montse"),
        panel.background = element_blank())+
  labs(x="GDP growth rate", y="Density")

hi.webs <- ggplot(UNGrowth, 
                  aes(x=gr, 
                      color=region, 
                      fill=region)) + 
  geom_histogram(aes(y=after_stat(density)), bins=30) + 
  theme(panel.background = element_blank()) +
  theme(text=element_text(size=12,  family="montse"),
        legend.position =c(0.8,0.8), 
        legend.key.size = unit(0.65, 'cm'))+
  labs(x="GDP growth rate", y="Density")

plot_grid(hi.we, hi.webs)

# _____________________________________________________________________
# Scatterplots
xy.fg <- ggplot(data = UNGrowth, 
                aes(x = fertility, y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, 
          colour = "black", 
          linetype=1), 
        legend.key = element_rect(
          fill = "transparent", 
          colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="Fertility", 
       y="GDP growth rate")

xy.pg <- ggplot(data = UNGrowth, 
                aes(x = ppgdp, y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, 
          colour = "black", 
          linetype=1), 
        legend.key = element_rect(
          fill = "transparent", 
          colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="Ppgdp", 
       y="GDP growth rate")

xy.lg <- ggplot(data = UNGrowth, 
                aes(x = lifeExpF, y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, 
          colour = "black", 
          linetype=1), 
        legend.key = element_rect(
          fill = "transparent", 
          colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="LifeExpF", 
       y="GDP growth rate")

xy.ug <- ggplot(data = UNGrowth, 
                aes(x = pctUrban, y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, 
          colour = "black", 
          linetype=1), 
        legend.key = element_rect(
          fill = "transparent", 
          colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="PctUrban", 
       y="GDP growth rate")

xy.ig <- ggplot(data = UNGrowth, 
                aes(x = infantMortality, y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, 
          colour = "black", 
          linetype=1), 
        legend.key = element_rect(
          fill = "transparent", 
          colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="InfantMortality", 
       y="GDP growth rate")

grid.arrange(hi.webs, xy.fg, xy.pg, 
             xy.lg, xy.ug, xy.ig, 
             ncol = 3, nrow = 2)

# _____________________________________________________________________
# Scatterplots (logarithm?)
xy.fg.log <- ggplot(data = UNGrowth, 
                    aes(x = log(fertility), y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, 
          colour = "black", 
          linetype=1), 
        legend.key = element_rect(
          fill = "transparent", 
          colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="log(Fertility)", 
       y="GDP growth rate")

xy.pg.log <- ggplot(data = UNGrowth, 
                    aes(x = log(ppgdp), y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, 
          colour = "black", 
          linetype=1), 
        legend.key = element_rect(
          fill = "transparent", 
          colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="log(Ppgdp)", 
       y="GDP growth rate")

xy.lg.log <- ggplot(data = UNGrowth, 
                    aes(x = log(lifeExpF), y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, 
          colour = "black", 
          linetype=1), 
        legend.key = element_rect(
          fill = "transparent", 
          colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="log(LifeExpF)", 
       y="GDP growth rate")

xy.ug.log <- ggplot(data = UNGrowth, 
                    aes(x = log(pctUrban), y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, 
          colour = "black", 
          linetype=1), 
        legend.key = element_rect(
          fill = "transparent", 
          colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="log(PctUrban)", 
       y="GDP growth rate")

xy.ig.log <- ggplot(data = UNGrowth, 
                    aes(x = log(infantMortality), y = gr)) +
  geom_point(aes(color = region), size = 2) +
  theme(text=element_text(size=10,  family="montse"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(4, 4, 4, 4),
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, 
          colour = "black", 
          linetype=1), 
        legend.key = element_rect(
          fill = "transparent", 
          colour = "transparent"), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(face = "bold", size=12)) +
  labs(x="log(InfantMortality)", 
       y="GDP growth rate")

grid.arrange(hi.webs, xy.fg.log, xy.pg.log, 
             xy.lg.log, xy.ug.log, xy.ig.log, 
             ncol = 3, nrow = 2)

#lm.al <- lm(gr~region*(fertility+ppgdp+lifeExpF
#                    +pctUrban+infantMortality),
#            data=UNGrowth[c(-49,-56,-88),])
#anova(lm.al)
#summary(lm.al)
#lm.al.r <- lm(gr ~  fertility+ppgdp+pctUrban+region+
#                fertility:region+ppgdp:region,
#              data=UNGrowth[c(-49,-56,-88),])
#anova(lm.al.r)
#summary(lm.al.r)
#lm.al.f <- lm(gr~(fertility+ppgdp+lifeExpF
#                       +pctUrban+infantMortality)*region,
#            data=UNGrowth[c(-49,-56,-88),])
#anova(lm.al.f)
#summary(lm.al)
# _____________________________________________________________________
# Full model
library(car)
f = gr ~ region*(fertility+log(ppgdp)+lifeExpF+pctUrban+infantMortality+
     I(fertility^2)+I((log(ppgdp))^2))
lm.f <- lm(f,data=UNGrowth)
anova(lm.f)
summary(lm.f)
vif(lm.f)

autoplot(lm.f, which = c(1, 3, 5, 2), 
         ncol = 2, label.size = 3, 
         data=UNGrowth, colour='region', 
         label.alpha = 0.5, label.family = "montse")

library(lmtest)
bptest(lm.f)

lm.r <- lm(gr ~ region+fertility+pctUrban+infantMortality+
             I(fertility^2)+I((log(ppgdp))^2)+region:log(ppgdp)+
             region:I((log(ppgdp))^2), data = UNGrowth)
summary(lm.r)
vif(lm.r)

# _____________________________________________________
# Dropping outliers
UNGrowth.drop <- UNGrowth[c(-49,-56,-88),]
lm.f.drop <- lm(f,data=UNGrowth.drop)
anova(lm.f.drop)
summary(lm.f.drop)
vif(lm.f.drop)

bptest(lm.f.drop)

autoplot(lm.f.drop, which = c(1, 3, 5, 2), 
         ncol = 2, label.size = 3, 
         data=UNGrowth.drop, colour='region', 
         label.alpha = 0.5, label.family = "montse")

#lm1 <- lm(gr ~ region*
#            (fertility+log(ppgdp)+lifeExpF+
#               pctUrban+infantMortality+
#               I(fertility^2)+I((log(ppgdp))^2))-lifeExpF,
#          data=UNGrowth.drop)
#anova(lm1,lm.f.drop)
#anova(lm1)
#
#lm2 <- lm(gr ~ region*
#            (fertility+log(ppgdp)+lifeExpF+
#               pctUrban+infantMortality+
#               I(fertility^2)+I((log(ppgdp))^2))-
#            lifeExpF-infantMortality ,
#          data=UNGrowth.drop)
#anova(lm2,lm.f.drop)
#anova(lm2)

lm.r.drop <- lm(gr ~ region+fertility+log(ppgdp)+pctUrban+
             I(fertility^2)+I((log(ppgdp))^2), data = UNGrowth.drop)

lm.a.drop <- lm(gr ~ region+fertility+log(ppgdp)+pctUrban+
                  I(fertility^2)+I((log(ppgdp))^2)+
                  region:I((log(ppgdp))^2), data = UNGrowth.drop)
anova(lm.r.drop,lm.a.drop)

summary(lm.r.drop)
vif(lm.r.drop)
anova(lm.r.drop)
anova(lm.r.drop,lm.f.drop)

# _____________________________________________________
# Deal with colinearity by assuming that it is caused by data itself
# normalize data
# ppgdp: take logarithm first and scale it

UNGrowth.scale <- data.frame(UNGrowth[1:2],
                             scale(UNGrowth$fertility),
                             scale(log(UNGrowth$ppgdp)),
                             scale(UNGrowth$lifeExpF),
                             scale(UNGrowth$pctUrban),
                             scale(UNGrowth$infantMortality),
                             scale(UNGrowth$gr))

f.scale = scale.UNGrowth.gr. ~ region*
  (scale.UNGrowth.fertility.+scale.log.UNGrowth.ppgdp..+
     scale.UNGrowth.lifeExpF.+scale.UNGrowth.pctUrban.+
     scale.UNGrowth.infantMortality.+
     I(scale.UNGrowth.fertility.^2)+
     I(scale.log.UNGrowth.ppgdp..^2))

lm.f.scale <- lm(f.scale, data = UNGrowth.scale)
anova(lm.f.scale)
summary(lm.f.scale)
vif(lm.f.scale)

autoplot(lm.f.scale, which = c(1, 3, 5, 2), 
         ncol = 2, label.size = 3, 
         data=UNGrowth.scale, colour='region', 
         label.alpha = 0.5, label.family = "montse")

lm.r.scale <- lm(scale.UNGrowth.gr. ~ 
                   region+scale.UNGrowth.fertility.+
                   scale.UNGrowth.pctUrban.+
                   scale.UNGrowth.infantMortality.+
                   I(scale.UNGrowth.fertility.^2)+
                   I(scale.log.UNGrowth.ppgdp..^2)+
                   region:scale.log.UNGrowth.ppgdp..+
                   region:I(scale.log.UNGrowth.ppgdp..^2), 
                 data = UNGrowth.scale)
summary(lm.r.scale)

# Drop outliers
UNGrowth.scale.drop <- UNGrowth.scale[c(-49,-56,-88),]
lm.f.scale.drop <- lm(f.scale, data = UNGrowth.scale.drop)
anova(lm.f.scale.drop)
summary(lm.f.scale.drop)
vif(lm.f.scale.drop)

lm.r.scale.drop <- lm(scale.UNGrowth.gr. ~ 
                        region+scale.UNGrowth.fertility.+
                        scale.log.UNGrowth.ppgdp..+
                        scale.UNGrowth.pctUrban.+
                        I(scale.UNGrowth.fertility.^2)+
                        I(scale.log.UNGrowth.ppgdp..^2),
                      data = UNGrowth.scale.drop)
summary(lm.r.scale.drop)
vif(lm.r.scale.drop)
