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

# check which are factors
UNGrowth %>% select(where(is.factor)) %>%
  glimpse()

# sample sizes by region
UNGrowth %>% count(region)

# summaries
UNGrowth %>% 
  select(-c(Country)) %>% 
  tbl_summary(by = region,
              statistic = all_continuous() ~ "{mean} ({sd})",
              label = list(
                fertility ~ "Fertility",
                ppgdp ~ "Ppgdp",
                lifeExpF ~ "LifeExpF",
                pctUrban ~ "PctUrban",
                infantMortality ~ "InfantMortality",
                gr ~ "Gr")) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("<div style='text-align: center; 
  font-weight: bold; color: grey'> 
  Table 1. National averages of GDP growth per capita 
                 and growth determinants</div>") %>%
  bold_labels()

UNGrowth %>% 
  select(-c(Country)) %>% 
  tbl_summary(by = region, 
              type = all_continuous() ~ "continuous2", 
              statistic = all_continuous() ~ 
                c("{median} ({p25}, {p75})", "{min}, {max}"),
              label = list(
                fertility ~ "Fertility",
                ppgdp ~ "Ppgdp",
                lifeExpF ~ "LifeExpF",
                pctUrban ~ "PctUrban",
                infantMortality ~ "InfantMortality",
                gr ~ "Gr")) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("<div style='text-align: center; 
  font-weight: bold; color: grey'> 
  Table 1. National averages of GDP growth per capita 
                 and growth determinants</div>") %>%
  bold_labels()

# _____________________________________________________
# Bar chart with sample sizes
ggplot(UNGrowth,aes(x=region, fill=region))+
  geom_bar(alpha=0.8)+
  theme_minimal(base_family="montse", base_size=14)+
  coord_flip()

# _____________________________________________________
# Boxplots
dis.fert <- ggplot(UNGrowth, 
                   aes(x=region, 
                       y=fertility, 
                       fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(0, 7, 0.5)) +
  labs(x="", y="Fertility")

dis.ppgdp <- ggplot(UNGrowth, 
                    aes(x=region, 
                        y=ppgdp, 
                        fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(0, 110000, 10000)) +
  labs(x="", y="Ppgdp")

dis.lifeExpF <- ggplot(UNGrowth, 
                       aes(x=region, 
                           y=lifeExpF, 
                           fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(40, 90, 5)) +
  labs(x="", y="LifeExpF")

dis.pctUrban <- ggplot(UNGrowth, 
                       aes(x=region, 
                           y=pctUrban, 
                           fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  labs(x="", y="PctUrban")

dis.infantMortality <- ggplot(UNGrowth, 
                              aes(x=region, 
                                  y=infantMortality, 
                                  fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(0, 130, 10)) +
  labs(x="", y="InfantMortality")

dis.gr <- ggplot(UNGrowth, 
                 aes(x=region, 
                     y=gr, 
                     fill=region)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none", 
        panel.grid.minor = element_line(
          colour="lightgrey", linewidth=0.35),
        panel.background = element_blank(), 
        axis.line = element_line(
          linewidth = 1, colour = "black", linetype=1)) + 
  scale_y_continuous(minor_breaks = seq(-5, 20, 1)) +
  labs(x="", y="Gr")


grid.arrange(dis.fert, dis.ppgdp, 
             dis.lifeExpF, dis.pctUrban, 
             dis.infantMortality, dis.gr, 
             ncol = 3, nrow = 2)

# _____________________________________________________
# Correlation
cor_gf <- UNGrowth %>%
  group_by(region) %>%
  summarise(Fertility=cor(gr, fertility))

cor_gp <- UNGrowth %>%
  group_by(region) %>%
  summarise(Ppgdp=cor(gr, ppgdp))

cor_gl <- UNGrowth %>%
  group_by(region) %>%
  summarise(LifeExpF=cor(gr, lifeExpF))

cor_gpU <- UNGrowth %>%
  group_by(region) %>%
  summarise(PctUrban=cor(gr, pctUrban))

cor_gi <- UNGrowth %>%
  group_by(region) %>%
  summarise(InfantMortality=cor(gr, infantMortality))

merge(cor_gf,
      merge(cor_gp,
            merge(cor_gl,
                  merge(cor_gpU,cor_gi,
                        by="region"),
                  by="region"),
            by="region"),
      by="region")  
# _____________________________________________________
# Correlation between factors
library(plyr)
corby <- function(D)
{
  return(data.frame(FertPpgdp = cor(D$fertility, D$ppgdp), 
                    FertLifeEx = cor(D$fertility, D$lifeExpF), 
                    FertPctU = cor(D$fertility, D$pctUrban),
                    FertInfMort = cor(D$fertility, D$infantMortality),
                    PpgdpLifeEx = cor(D$ppgdp, D$lifeExpF),
                    PpgdpPctU = cor(D$ppgdp, D$pctUrban),
                    PpgdpInfMort = cor(D$ppgdp, D$infantMortality),
                    LifeExPctU = cor(D$lifeExpF, D$pctUrban),
                    LifeExInfMort = cor(D$lifeExpF, D$infantMortality),
                    PctUInfMort = cor(D$pctUrban, D$infantMortality)))
}
Ccors <- ddply(UNGrowth, .(region), corby)
Ccors
detach("package:plyr")

# _____________________________________________________
# Normalize
UNGrowth.scale <- data.frame(UNGrowth[1:2],scale(UNGrowth[3:8]))

cor_gf <- UNGrowth.scale %>%
  group_by(region) %>%
  summarise(Fertility=cor(gr, fertility))

cor_gp <- UNGrowth.scale %>%
  group_by(region) %>%
  summarise(Ppgdp=cor(gr, ppgdp))

cor_gl <- UNGrowth.scale %>%
  group_by(region) %>%
  summarise(LifeExpF=cor(gr, lifeExpF))

cor_gpU <- UNGrowth.scale %>%
  group_by(region) %>%
  summarise(PctUrban=cor(gr, pctUrban))

cor_gi <- UNGrowth.scale %>%
  group_by(region) %>%
  summarise(InfantMortality=cor(gr, infantMortality))

merge(cor_gf,
      merge(cor_gp,
            merge(cor_gl,
                  merge(cor_gpU,cor_gi,
                        by="region"),
                  by="region"),
            by="region"),
      by="region")

library(plyr)
Ccors.scale <- ddply(UNGrowth.scale, .(region), corby)

Ccors.scale

# _____________________________________________________
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
