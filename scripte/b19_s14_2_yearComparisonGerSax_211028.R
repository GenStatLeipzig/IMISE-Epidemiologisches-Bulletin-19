# Setup ----

library(data.table)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(scales)
library(lubridate)
require(toolboxH)
require(here)
# Data ----

# get current data from github (needs linux + wget), might work with windows+RStudio?
# system(paste("wget -P", paste0(getwd(), "/data"), 
#              "https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/by_age/truth_RKI-Incident%20Deaths%20by%20Age_Germany.csv")) 
# system(paste("wget -P", paste0(getwd(), "/data"), 
#              "https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/by_age/truth_RKI-Incident%20Cases%20by%20Age_Germany.csv")) 
# system(paste("wget -P", paste0(getwd(), "/data"), 
#              "https://diviexchange.blob.core.windows.net/%24web/zeitreihe-bundeslaender.csv")) 
# system(paste("wget -P", paste0(getwd(), "/data"), 
#              "https://diviexchange.blob.core.windows.net/%24web/zeitreihe-deutschland.csv")) 

# read data of all age groups in germany and saxony
dat_age_ger <- fread(here("data/211014_population_germany.csv"), fill = TRUE, encoding = "UTF-8")
dat_age_sax <- fread(here("data/211014_population_saxony.csv"), fill = TRUE, encoding = "UTF-8")

# incidences
dat_case <- fread("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/by_age/truth_RKI-Incident%20Cases%20by%20Age_Germany.csv", encoding = "UTF-8")#"data/truth_RKI-Incident Cases by Age_Germany.csv")

# death
dat_death <- fread("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/by_age/truth_RKI-Incident%20Deaths%20by%20Age_Germany.csv", encoding = "UTF-8")#"data/truth_RKI-Incident Deaths by Age_Germany.csv")

# icu
dat_icu_ger <- fread(  "https://diviexchange.blob.core.windows.net/%24web/zeitreihe-deutschland.csv", encoding = "UTF-8")
dat_icu_sax <- fread("https://diviexchange.blob.core.windows.net/%24web/zeitreihe-bundeslaender.csv", encoding = "UTF-8")
dat_icu <- fread("https://diviexchange.blob.core.windows.net/%24web/bund-covid-altersstruktur-zeitreihe_ab-2021-04-29.csv", encoding = "UTF-8")

# Data cleaning & merging ----

# Clean ICU data ####

# I only need saxony & germany data
dat_case <- dat_case[location_name %in% c("Germany", "Free State of Saxony")]
dat_death <- dat_death[location_name %in% c("Germany", "Free State of Saxony")]

# summarize to all ages
dat_icu_ger$Behandlungsgruppe %>% table
dat_icu_sax$Behandlungsgruppe %>% table

# clean germany data
dat_icu_ger <- dat_icu_ger[Behandlungsgruppe == "ERWACHSENE", .(date = Datum, 
                                                                area = Bundesland, 
                                                                cases_its = Aktuelle_COVID_Faelle_ITS)]

# clean saxony data
dat_icu_sax <- dat_icu_sax[Bundesland == "SACHSEN", .(date = Datum, 
                                                      area = Bundesland, 
                                                      cases_its = Aktuelle_COVID_Faelle_ITS)]


# remove time from date entry
dat_icu_ger$date <- format(dat_icu_ger$date, format='%Y/%m/%d')
dat_icu_sax$date <- format(dat_icu_sax$date, format='%Y/%m/%d')

# reformate area entry
dat_icu_ger$area <- "Germany"
dat_icu_sax$area <- "Free State of Saxony"

# change to per 100 000 
pop_ger18 <- dat_age_ger[which(age == "18-Jährige"): .N, sum(pop_total)]
dat_icu_ger[, cases100k := (cases_its / pop_ger18) * 100000]
dat_icu_ger[, type := "ICU"]

pop_sax18 <- dat_age_sax[which(age == "18-Jährige"): .N, sum(pop_sac)]
dat_icu_sax[, cases100k := (cases_its / pop_sax18) * 100000]
dat_icu_sax[, type := "ICU"]

# merge data
dat_icu2 <- rbindlist(
  list(
    dat_icu_ger[ , .(date, area, type, cases=cases_its,cases100k)],
    dat_icu_sax[ , .(date, area, type, cases=cases_its,cases100k)]
  )
)

# Clean Case and Death data ####

# clean case data
# sum values of each date and location
# dat_case <- dat_case[age_group != "unbekannt"]

# dat_case <- dat_case[value >= 0, ]
dat_case2 <- dat_case[, .(cases = sum(value)), by = .(date, location_name)]
dat_case2[, date:=as_date(date)]

qlist1 = venn2(dat_case2$date %>% as.character(), (min(dat_case2$date):max(dat_case2$date)) %>%as_date() %>%  as.character())
qlist1$q3
dat_case3 = rbind(dat_case2, data.table(date = as_date("2021-10-26"), 
                                        location_name = c('Germany', 'Free State of Saxony'),
                                        cases = c(10473, 494)))
dat_case3[, type := "Case"]
# calculate per 100 000 incidence
pop_ger <- dat_age_ger[, sum(pop_total)]
pop_sax <- dat_age_sax[, sum(pop_sac)]
dat_case3[, pop := ifelse(location_name=="Germany", pop_ger, pop_sax)]
dat_case3[, cases100k := (cases / pop) * 100000 ]

# clean death data
# dat_death <- dat_death[age_group != "unbekannt"]
# dat_death <- dat_death[value >= 0, ]
dat_death2 <- dat_death[, .(cases = sum(value)), by = .(date, location_name)]
qlist2 = venn2(dat_death2$date %>% as.character(), (min(dat_death2$date):max(dat_death2$date)) %>%as_date() %>%  as.character())
qlist2$q3
dat_death2[, date:=as_date(date)]
dat_death3 = rbind(dat_death2, data.table(date = as_date("2021-10-26"), 
                                        location_name = c('Germany', 'Free State of Saxony'),
                                        cases = c(95245-95117, 10286-10284)))
dat_death3[, type := "Death"]

# calculate per 100 000 incidence
dat_death3[, pop := ifelse(location_name=="Germany", pop_ger, pop_sax)]
dat_death3[, cases100k := (cases / pop) * 100000 ]

# merge data
dat_case_death <- rbindlist(
  list(
    dat_case3[ , .(date, area = location_name, type,cases, cases100k)],
    dat_death3[ , .(date, area = location_name, type, cases,cases100k)]
  )
)

# format date
dat_case_death$date <- format(as.POSIXct(dat_case_death$date), format = "%Y-%m-%d")
dat_icu2$date <- format(as.POSIXct(dat_icu2$date), format = "%Y-%m-%d")

# Joint data ----

# bind all data

dat <- rbindlist(
  list(
    dat_icu2,
    dat_case_death
  )
)

# get rolling mean

# for each age group
setorder(dat, area, type, date)
dat[,  cases100k7d := frollsum(
  n = 7, 
  x = cases100k, 
  align = "right", 
  na.rm = T), .(area, type)]

dat[,cases2plot := ifelse(type=="ICU", cases100k, cases100k7d)]
# create date comparison groups
dat$date <- as.Date(dat$date, format="%Y-%m-%d")
dat[, year := year(date) %>% as.numeric()]
maxdate = max(dat[year ==2021, date])
maxdate


# add a year to enable plotting using date scale, otherwise: chaos, madness, death...
dat[year == "2020", date := date + years(1)]
# dat$date <- format(dat$date, format="%m-%d")

# remove data not in time window
dat <- dat[!is.na(year)]
dat[,year:=factor(year,levels = c(2021, 2020))]
dat %>% str

# Plotting ----
brk_vec = c(seq(max(dat$date),maxdate +30, -30), seq(maxdate,  min(dat$date), -30))
plotdates = dat[,.(maxdat =max(date) %>% unique()), .(year,type)]$maxdat %>% unique()
maxi = dat[date %in% plotdates]
maxi[,cases2plotround := ifelse(type=="Death", round(cases2plot,2) %>% as.character(),round(cases2plot,1)%>% as.character())]
setorder(maxi, -date)
maxi[allDuplicatedEntries(paste(area, type, year))]
# maxi = maxi[duplicated(paste(area, type, year))==F]
maxi
p1 <- ggplot(
  dat, 
  aes(
    x = date,
    y = cases2plot,
    col = year,
    lty = year,
    lwd = year
  )
) +
  facet_wrap(
    area~type,
    scales = "free_y",
    labeller = labeller(
      area = c(
        "Germany" = "Deutschland",
        "Free State of Saxony" = "Sachsen"
      ),
      type = c("Case" = "Testpositiv / 7-Tage",
               "Death" = "Verstorben / 7-Tage",
               "ICU" = "tägl. Bettenbelegung ITS")
    )
  ) +
  geom_line(alpha=0.5) +
  scale_x_date(breaks = brk_vec, date_labels = "%d-%b") +
  ylab("Fälle / 100.000 Einwohner") + 
  xlab("") + 
  theme_pander() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.grid.major = element_line(linetype = "solid"),
        panel.grid.minor = element_blank(), 
        legend.position = "top",
        legend.justification = "center"
  ) +
  labs(color = "Jahr",
       lty = "Jahr") +
  scale_size_manual(values = c(1.2, 0.9)) +
  ggrepel::geom_text_repel(data = maxi, aes(label = cases2plotround ), size=3, alpha = 1, show.legend=FALSE, min.segment.length = 0, direction = "y")+
  
  # ggrepel::geom_text_repel(data = maxi, aes(label = cases2plotround ), size=3, alpha = 0.9, show.legend=FALSE, min.segment.length = 0, col = "black")+
  guides(lwd = "none",col = guide_legend(nrow = 1,override.aes=list(size=2),keywidth = 5))
p1

jpeg(here("results/b19_S14_2_year2year_comparison.jpeg"), 10,4,units = "in", quality  = 100, res = 150)
p1
dev.off()


p2   = p1+scale_y_log10(breaks  = log_breaks(6))
p2

jpeg(here("results/b19_S14_2_year2year_comparison_log.jpeg"), 10,4,units = "in", quality  = 100, res = 150)
p2
dev.off()

finalizeSkript()

