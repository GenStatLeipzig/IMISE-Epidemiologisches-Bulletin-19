# Setup ----
rm(list = ls())
library(data.table)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(scales)
require(here)
require(lubridate)

# based on: https://github.com/holgerman/covid19_germanyByAge/blob/main/covid19_germanyByAge/app.R

# Read data ----

# read data of all age groups in germany and saxony
dat_age_ger <- fread(here("data/211014_population_germany.csv"), fill = TRUE, encoding = "UTF-8")
dat_age_sax <- fread(here("data/211014_population_saxony.csv"), fill = TRUE, encoding = "UTF-8")  # alterstratifizierter Bevoelkerungstand Sachsen bzw. bundeslandebene gibt es hier
# https://www-genesis.destatis.de/genesis//online?operation=table&code=12411-0012&bypass=true&levelindex=0&levelid=1634203440391#abreadcrumb

# alterstratifizierter Bevoelkerungstand Deutschland gibt es hier
# https://www-genesis.destatis.de/genesis//online?operation=table&code=12411-0006&bypass=true&levelindex=0&levelid=1634203557481#abreadcrumb

# read data of confirmed covid cases
# dat_cases_age =  fread(here('data/b19_206_2_dat_cases_age.txt'), encoding = "UTF-8") # "https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/by_age/truth_RKI-Incident%20Cases%20by%20Age_Germany.csv", encoding = "UTF-8") from 16.10.21

dat_cases_age =  fread("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/by_age/truth_RKI-Incident%20Cases%20by%20Age_Germany.csv", encoding = "UTF-8") 

# Clean data ----


dat_cases_age[location_name == "Germany"]
dat_cases_age[location_name == "Free State of Saxony"]

# I only need germany and saxony
dat_cases_age <- dat_cases_age[location_name %in% c("Germany", "Free State of Saxony")]


# add overall group----
dat_cases_all = dat_cases_age[, .(value = sum(value), age_group="alle"), .(date, location, location_name)]
toolboxH::showNA(dat_cases_all)

dat_cases_age = unique(rbind(dat_cases_age, dat_cases_all))

# exclude unknown age
dat_cases_age <- dat_cases_age[age_group != "unbekannt"]
# check
stopifnot(dat_cases_age[age_group=="unbekannt", .N == 0])

# clean values < 0
# dat_cases_age[value < 0, ][order(date)]
# 
# dat_cases_age[value >= 0, ] # TODO Negativ sollten Nachmeldungen sein, dass am vorigen Tag zu viel gemeldet wurde. Muessten eigentlich mit vorigem Tag verrechnet werden, i.e. vorigen Tag nach unten korrigieren. In schleife, fall dann voriger Tag negativ wird, bis keiner mehr negativ ist. Fuer aktuelle Darstellung im Bulletin 19 aber ausreichend 

# useless group
dat_cases_age$location <- NULL

# Create age groups ----

# create age group totals
all_groups <- unique(dat_cases_age$age_group)

# for german total data
dat_age_ger$age_group <- NULL
dat_age_ger[1:which(age == "4-Jährige"), age_group := all_groups[1]]
dat_age_ger[which(age == "5-Jährige"):which(age == "14-Jährige"), age_group := all_groups[2]]
dat_age_ger[which(age == "15-Jährige"):which(age == "34-Jährige"), age_group := all_groups[3]]
dat_age_ger[which(age == "35-Jährige"):which(age == "59-Jährige"), age_group := all_groups[4]]
dat_age_ger[which(age == "60-Jährige"):which(age == "79-Jährige"), age_group := all_groups[5]]
dat_age_ger[which(age == "80-Jährige"):.N, age_group := all_groups[6]]

# for saxony data
dat_age_sax$age_group <- NULL
dat_age_sax[1:which(age == "4-Jährige"), age_group := all_groups[1]]
dat_age_sax[which(age == "5-Jährige"):which(age == "14-Jährige"), age_group := all_groups[2]]
dat_age_sax[which(age == "15-Jährige"):which(age == "34-Jährige"), age_group := all_groups[3]]
dat_age_sax[which(age == "35-Jährige"):which(age == "59-Jährige"), age_group := all_groups[4]]
dat_age_sax[which(age == "60-Jährige"):which(age == "79-Jährige"), age_group := all_groups[5]]
dat_age_sax[which(age == "80-Jährige"):.N, age_group := all_groups[6]]

# summarize by age group
dat_age_ger_sum <- dat_age_ger[, .(pop_sum = sum(pop_total)), by = age_group]
dat_age_ger_sum[, location_name := "Germany"]
dat_age_sax_sum <- dat_age_sax[, .(pop_sum = sum(pop_sac)), by = age_group]
dat_age_sax_sum[, location_name := "Free State of Saxony"]

# merge
dat_age_sum_pre <- rbindlist(list(dat_age_ger_sum, dat_age_sax_sum))
dat_age_sum_all = dat_age_sum_pre[,.(age_group = "alle",pop_sum = sum(pop_sum)), .(location_name)]
dat_age_sum = rbind(dat_age_sum_pre, dat_age_sum_all)
dat_age_sum
toolboxH::WriteXLS_hk("dat_age_sum", here("results/b19_s06_3_numbers_per_age_group.xlsx"))
# Merge data ----

# match to dat_cases_age
m1 <- match(
  dat_cases_age[, paste0(location_name, age_group)],
  dat_age_sum[, paste0(location_name, age_group)]
)
dat_cases_age[, pop_group := dat_age_sum[m1, pop_sum]]

# Calculate incidence and rolling sum ----


# get incidence PER age group
dat_cases_age[, value_per_group := (value / pop_group) * 100000]


# numeric error?
# dat_cases_age[,  .((value / (pop_group / 100000)),((value / pop_group)* 100000))][V1 != V2, plot(V1~V2)]

# get the rolling mean over 7 days?
# dat_cases_age[,  val_mean := frollmean(n = 7, x = value_per_group, align = "right", na.rm = T), .(location_name, age_group)]

# for each age group
setorder(dat_cases_age, date, location_name, age_group)
dat_cases_age[,  val_sum_group := frollsum(
  n = 7, 
  x = value_per_group, 
  align = "right", 
  na.rm = T), .(location_name, age_group)]



# Plotting ----

# plot with 100k per age group
brk_vec = seq(max(dat_cases_age$date),min(dat_cases_age$date), -14)

dat_cases_age[, location_name := factor(location_name, levels = c("Germany", "Free State of Saxony"))]

mindat = as_date("2021-06-07")



maxi = dat_cases_age[date ==max(date)]
maxi[,val_sum_group_round := round(val_sum_group,0) %>% as.character()]
maxi[,val_sum_group_round_name := paste0(age_group,": ", val_sum_group_round)]
maxi

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

p1 <- ggplot(dat_cases_age[age_group!="alle" & date >= mindat, ],
             aes(date, 
                 val_sum_group, 
                 color = age_group )
) + 
  geom_line(size = 2, alpha= 0.5) +
  geom_line(data = dat_cases_age[age_group=="alle" & date >= mindat], color = "black", size = 1.5,lty = 3, alpha= 0.7) +
  geom_line(data = dat_cases_age[age_group=="alle" & date >= mindat], color = "black", size = 1.5,lty = 1, alpha= 0.5) +
  facet_wrap(~location_name,
             scales = "fixed",
             labeller = labeller(
               location_name = c(
                 "Germany" = "Deutschland",
                 "Free State of Saxony" = "Freistaat Sachsen"
               ))) + 
  scale_y_log10(
    breaks = log_breaks(14), 
    label = label_comma(accuracy = 1),
    sec.axis =dup_axis()
      
  ) +
  theme_pander(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.grid.major = element_line(linetype = "solid"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
  ) + 
  scale_x_date(breaks = brk_vec, date_labels = "%d-%b-%y"  , limits = c(mindat, max(dat_cases_age$date)+38) ) +
  ylab("") + 
  xlab("") + 
  scale_color_manual(values = c(gg_color_hue(6), "black"))+
  labs(color = "Altersgruppe") +
  ylab("7-Tage Inzidenz") +
  coord_cartesian(ylim = c(0.5, max(dat_cases_age[date >= mindat, val_sum_group], na.rm = T)))+
  ggrepel::geom_text_repel(data = maxi, aes(label = val_sum_group_round_name ), size=2.5, alpha = 0.8, show.legend=FALSE,   min.segment.length = 11, force = 0.001,  nudge_x = 6, direction = "y", hjust = "left")+
  guides(col = guide_legend(nrow = 2,override.aes=list(size=2),keywidth = 1.5))

p1

p2 = p1 + scale_y_continuous(breaks = pretty_breaks(12),sec.axis = dup_axis()) #+ theme(axis.title.y.right = element_blank())

p2
# Small check ----

# calculate incidences without age stratification
tmp <- dat_cases_age[, .(val = sum(value)), .(date, location_name)]
tmp[, val_rel := (val / 83155031)* 100000]
tmp[,  .(date, frollsum(
  n = 7, 
  x = val_rel, 
  align = "right", 
  na.rm = T)), .(location_name)][location_name=="Germany" & date >= "2021-10-10"]

# Saving ----

ggsave(plot = p1,
       filename = here("results/b19_s06_3_ageStratPlot.jpeg"),
       dpi = 300,
       device = "jpeg",
       units = "in",
       height = 5,
       width = 9)
# # 
# # # save data
# fwrite(x = dat_cases_age,
#        file = here("results/b19_s06_3_ageStratDat_211014.tsv"),
#        sep = "\t")

# Session Info ----

sessionInfo()
