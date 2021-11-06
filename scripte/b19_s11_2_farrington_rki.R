# Setup ----
rm(list = ls())
library(data.table)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(scales)
require(here)
require(toolboxH)
require(lubridate)

# based on: https://github.com/holgerman/covid19_germanyByAge/blob/main/covid19_germanyByAge/app.R

# Read data ----
farrington  = read_excel2(here("data/farrington_rki.xlsx"))
farrington[,Kategorie:=paste0("Schutz vor\n", Kategorie)]
farrington[,Kategorie:= factor(Kategorie, levels = c( "Schutz vor\nInfektion"    ,    "Schutz vor\nHospitalisierung" ,"Schutz vor\nITS"         ,     "Schutz vor\nVersterben" ))]

farrington[,Wochen := paste0(`Woche Start`, "-", `Woche Ende`)]

p1=ggplot(farrington, aes(Wochen, Schutz, fill = Altersgruppe, col = Altersgruppe)) + geom_point(size = 2) + geom_line(alpha = 0.6,size = 1,aes(group = paste(Kategorie, Altersgruppe))) + facet_grid(.~Kategorie) + scale_y_continuous(limits = c(0,1), labels = label_percent(accuracy = 1), breaks = pretty_breaks(10)) + 
  theme_hc(base_size = 14) +
  theme(panel.spacing.x =unit(1, "lines"), 
        axis.text.x = element_text(angle = 90, hjust = 0.4, vjust = 0.5), 
        legend.position = "top") +
  ylab("Schutz nach Zweifachimpfung (Deutschland, RKI)") + 
  xlab("Kalenderwochen")+
  scale_color_wsj()

p1

# Saving ----

ggsave(plot = p1,
       filename = here("results/b19_s10_2_RKI_farrington.jpeg"),
       dpi = 150,
       quality=100,
       device = "jpeg",
       units = "in",
       height = 5,
       width = 7)
# 


# Session Info ----

sessionInfo()
