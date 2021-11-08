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
# digitized using: https://apps.automeris.io/wpd/


dat_pre <- fread(here("data/wpd_datasets_211107"), header = F) # credits to https://automeris.io/WebPlotDigitizer/
maxdat = as_date("2021-10-07")
names(dat_pre) = c('nonvacc_x','nonvacc_y', 'all_x', 'all_y', 'vacc_x', 'vacc_y' )
dat_pre2  = dat_pre[-1:-2]
dat_pre3 = rbind(data.table(datum = dat_pre2[,1] %>% unlist(), testpositiv =dat_pre2[,2]%>% unlist(), group = "non_vacc"),
                 data.table(datum = dat_pre2[,3]%>% unlist(), testpositiv =dat_pre2[,4]%>% unlist(), group = "incidence"),
                 data.table(datum = dat_pre2[,5]%>% unlist(), testpositiv =dat_pre2[,6]%>% unlist(), group = "vacc")
                 )
dat_pre4 = dat_pre3[datum!=""]
dat_pre4[, date:= as_date(datum)]
dat_pre4[, count:= as.numeric(testpositiv)]
dat_pre4 = dat_pre4[count>0 & date> dat_pre4[group=='vacc',min(date)] & (group=="incidence" & count > dat_pre4[group!="incidence",max(count)])==F]

# CAve MEldeverzug! SAchen SMS : Hinweise: Die Auswertung der 7-Tage-Inzidenz (Zahl der SARS-CoV-2-Neuinfektionen je 100.000 Einwohner in den vergangenen 7 Tagen) der Geimpften und nicht (vollständig) Geimpften erfolgt aktuell noch unter Vorbehalt
# create date column
dat = dat_pre4[date <= maxdat]
# round data to nearest full Int
dat$count <- gsub(dat$count, pattern = ",", replacement = ".", fixed = T)
dat$count <- as.numeric(dat$count)
dat$count <- round(dat$count, 0)

dat[,group2 := ifelse(group=="vacc", "vollst. geimpft", 
                      ifelse(group=="non_vacc", "unvollst. geimpft", 
                             ifelse(group=="incidence", "Gesamtbevölkerung", group)))]
# quick preview
brk_vek = seq(max(dat$date), min(dat$date), -7)
p1 = ggplot(dat, 
            aes(x = date,
                y = count,
                col = group2
            )
) +
  ylab("Neuinfektion / 100.000 Einwohner\nje  Impfgruppe")+
  # scale_y_continuous(breaks = pretty_breaks(10))+
  scale_y_log10(breaks = log_breaks(15))+
  scale_x_date(breaks = brk_vek, labels = label_date(format = "%d-%b-%y"))+
  theme_pander(base_size = 16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0),
        legend.position = "top")+
  geom_line(lwd = 2, alpha=0.7) +
  xlab("") + 
  labs(color = "")+
  scale_color_manual(values = c( "black",hue_pal()(3)[2], hue_pal()(3)[3])) +
  geom_line(data = dat[group =="incidence"],lwd = 2, alpha=1, lty = 3, col = "black") +
  geom_ribbon(aes(xmin =  max(date)-7, xmax =  max(date)), fill = "grey55", alpha = 0.7, col = "grey55") +
  annotate(geom = 'text', x = as_date("2021/10/14"), y = 10, label = "Grau: Daten noch unvollständig,\nNachmeldungen erwartet", col = "grey33", fontface = "bold")



p1
# groups:
# vacc = vollständig geimpfte
# non_vacc = nicht (vollständig) geimpfte
# group = 7-Tage Inzidenz gesamt

# save

jpeg(here('results/b19_s10_4_vaccNonVaccPlotData.jpeg'), 5,5, quality = 100, res = 150,  units = "in")
p1
dev.off()


sessionInfo()
