# Setup ----
rm(list = ls())
library(data.table)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(scales)
require(toolboxH)
require(here)
require(lubridate)
require(stringr)

# data: https://www.coronavirus.sachsen.de/infektionsfaelle-in-sachsen-4151.html



dat_pre <- read_excel2(here("data/sms_vaccination_data.xlsx")) 
dat_pre[,Datum:= as_date(Datum)]
setorder(dat_pre, -Datum,-Datenstand)
dat_pre[allDuplicatedEntries(Datum)]
dat_pre2 = dat_pre[is.na(non_vacc)==F][duplicated(Datum)==F]
dat_pre2
maxdat = max(dat_pre2$Datum)
maxdat

dat = melt(dat_pre2[Datum <= maxdat], id.vars = "Datum", measure.vars = c("vacc", "non_vacc", "incidence"), variable.name = "group", value.name = "count")
dat

dat[,group2 := ifelse(group=="vacc", "vollst. geimpft", 
                      ifelse(group=="non_vacc", "unvollst. geimpft", 
                             ifelse(group=="incidence", "Gesamtbevölkerung", group)))]
# quick preview
brk_vek = seq(max(dat$Datum), min(dat$Datum), -7)

maxi = dat[Datum %in% c(dat[,.(maxdat =max(Datum)), group]$maxdat)]


p1 = ggplot(dat, 
            aes(x = Datum,
                y = count,
                col = group2
            )
) +
  ylab("Neuinfektion / 100.000 Einwohner\nje  Impfgruppe")+
  # scale_y_continuous(breaks = pretty_breaks(10))+
  scale_y_log10(breaks = log_breaks(15))+
  scale_x_date(breaks = brk_vek, labels = label_date(format = "%d-%b-%y"), limits = c(min(dat$Datum), maxdat+9))+
  theme_pander(base_size = 16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0),
        legend.position = "top")+
  geom_line(lwd = 2, alpha=0.7) +
  xlab("") + 
  labs(color = "")+
  guides(col = guide_legend(nrow = 2,override.aes=list(size=5),keywidth = 2))+
  scale_color_manual(values = c( "black",hue_pal()(3)[2], hue_pal()(3)[3])) +
  geom_line(data = dat[group =="incidence"],lwd = 2, alpha=1, lty = 3, col = "black") +
  geom_ribbon(aes(xmin =  max(Datum)-7, xmax =  max(Datum)), fill = "grey55", alpha = 0.7, col = "grey55") +
  annotate(geom = 'text', x = as_date("2021/10/14"), y = 10, label = "Grau: Daten noch unvollständig,\nNachmeldungen erwartet", col = "grey33", fontface = "bold")+
  geom_text(data = maxi, aes(label = count ), size=4, alpha = 1,hjust= -0.2, show.legend=FALSE)



p1
# groups:
# vacc = vollständig geimpfte
# non_vacc = nicht (vollständig) geimpfte
# group = 7-Tage Inzidenz gesamt

# save

jpeg(here('results/b19_s10_5_vaccNonVaccPlotData.jpeg'), 5,5, quality = 100, res = 150,  units = "in")
p1
dev.off()


sessionInfo()
