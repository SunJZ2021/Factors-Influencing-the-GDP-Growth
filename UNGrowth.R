library(readr)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggfortify)

UNGrowth <- read_csv("UNGrowth.csv")

# check which are factors
UNGrowth %>% select(where(is.factor)) %>%
  glimpse()

# sample sizes by region
UNGrowth %>% count(region, .drop = 'none')

# summaries
UNGrowth %>% 
  select(-c(Country)) %>% 
  tbl_summary(by = region,
              statistic = all_continuous() ~ "{mean} ({sd})",
              label = list(
                fertility ~ "Total fertility rate",
                ppgdp ~ "Per capita GDP ($)",
                lifeExpF ~ "Female life expectancy (years)",
                pctUrban ~ "Percent of urban population",
                infantMortality ~ "Infant deaths by age 1 year ( per 1000 live births)",
                gr ~ "GDP growth rate")) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("<div style='text-align: center; font-weight: bold; color: grey'> Table 1. National averages of GDP growth per capita and growth
determinants</div>") %>%
  bold_labels()

UNGrowth %>% 
  select(-c(Country)) %>% 
  tbl_summary(by = region, 
              type = all_continuous() ~ "continuous2", 
              statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
              label = list(
                fertility ~ "Total fertility rate",
                ppgdp ~ "Per capita GDP ($)",
                lifeExpF ~ "Female life expectancy (years)",
                pctUrban ~ "Percent of urban population",
                infantMortality ~ "Infant deaths by age 1 year ( per 1000 live births)",
                gr ~ "GDP growth rate")) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("<div style='text-align: center; font-weight: bold; color: grey'> Table 1. National averages of GDP growth per capita and growth
determinants</div>") %>%
  bold_labels()

#-------------------------------------------------------------------------------------------------------------------------------------------------
# Boxplots
dis.fert <- ggplot(UNGrowth, aes(x=region, y=fertility, fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", size=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(0, 7, 0.5)) +
  labs(x="", y="Total fertility rate (num. of children/woman")

dis.ppgdp <- ggplot(UNGrowth, aes(x=region, y=ppgdp, fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", size=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(0, 110000, 10000)) +
  labs(x="", y="Per capita GDP ($)")

dis.lifeExpF <- ggplot(UNGrowth, aes(x=region, y=lifeExpF, fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", size=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(40, 90, 5)) +
  labs(x="", y="Female life expectancy (years)")

dis.pctUrban <- ggplot(UNGrowth, aes(x=region, y=pctUrban, fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", size=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  labs(x="", y="Percent of population in urban areas")

dis.infantMortality <- ggplot(UNGrowth, aes(x=region, y=infantMortality, fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", size=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(0, 130, 10)) +
  labs(x="", y="Infant deaths by age 1 year (per 1000 live births)")

dis.gr <- ggplot(UNGrowth, aes(x=region, y=gr, fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", size=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(size = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(-5, 20, 1)) +
  labs(x="", y="GDP growth rate")


grid.arrange(dis.fert, dis.ppgdp, 
             dis.lifeExpF, dis.pctUrban, 
             dis.infantMortality, dis.gr, 
             ncol = 3, nrow = 2)

#-------------------------------------------------------------------------------------------------------------------------------------------------
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

plot_grid(hi.we, hi.webs) #??WTF??

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
  labs(x="Total fertility rate (num. of children/woman", y="GDP growth rate")

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
# The full model
lm_full <- lm(gr ~ region:(fertility + ppgdp + lifeExpF + pctUrban + infantMortality), data = UNGrowth)
summary(lm_full) # ??WTF??

autoplot(lm_full, which = c(1, 3, 5, 2), ncol = 2, label.size = 3, data=UNGrowth, 
         colour='region', label.alpha = 0.5, label.family = "montse") 

lm_alternative <- lm(gr ~ region:(fertility + log(ppgdp) + lifeExpF + pctUrban + infantMortality), data = UNGrowth)
summary(lm_alternative)

ggplot(data = UNGrowth, aes(x = log(ppgdp), y = gr)) +
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
  labs(x="ln(Per capita GDP) ($)", y="GDP growth rate")

autoplot(lm_alternative, which = c(1, 3, 5, 2), ncol = 2, label.size = 3, data=UNGrowth, 
         colour='region', label.alpha = 0.5, label.family = "montse")
