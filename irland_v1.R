require(toolboxH)
require(ggplot2)
require(scales)
require(stringr)
require(ggthemes)
require(lubridate)

irland = fread("C:/Users/IMISE/Nextcloud/saxocov/ireland/COVID-19_HPSC_Detailed_Statistics_Profile.csv") # https://covid-19.geohive.ie/datasets/d8eb52d56273413b84b0187a4e9117be_0/about
irland[,unique(paste(X,Y))]
irlandm = melt(irland, id.vars = c("X", "Y", "Date", "StatisticsProfileDate"))
unique(irlandm$variable)
irlandm[grep("ged", variable), unique(variable)]

irlandm2 = irlandm[grep("ged", variable)]
irlandm2[,type := ifelse(grepl("Hospital", variable), "Hospitalized", "testpositive")]
irlandm2[,agegroup := str_replace_all(variable, "HospitalisedAged|Aged", "")]
irlandm2[,unique(agegroup)] %>% dput

irlandm2[,agegroup2 := ifelse(agegroup=="5", "1to4", agegroup)]
irlandm2[,unique(agegroup2)] %>% dput
irlandm2[,agegroup2 := factor(agegroup2, levels = c("1to4", "5to14", "15to24", "25to34", "35to44", "45to54", "55to64", 
                                                    "65to74", "75to84", "85up"))]


irlandm3 = irlandm2[,.(value = sum(value, na.rm = T)), .(Date = as_date(Date), type, agegroup2)]

ggplot(irlandm3, aes(Date, value, col = agegroup2)) + geom_point() + facet_wrap(~agegroup2) + scale_y_log10() + theme()
p2 = ggplot(irlandm3, aes(Date, value, col = agegroup2 ,lty = type)) + geom_line() + scale_y_log10() + theme()
require(plotly)
ggplotly(p2)

qlist1 = venn2((min(irlandm3$Date):max(irlandm3$Date)) %>% as_date() %>% as.character, irlandm3$Date %>% as.character)
str(qlist1)

irlandm4 = irlandm3[Date>=as_date("2020-03-03")]
qlist2 = venn2((min(irlandm4$Date):max(irlandm4$Date)) %>% as_date() %>% as.character, irlandm4$Date %>% as.character)
str(qlist2)


setorder(irlandm4, type, agegroup2, Date)

irlandm4[,newvalue :=  value-c(0, value[1:(.N-1)]), .(type, agegroup2) ]
irlandm4[,newvalue7 :=  frollmean(x = newvalue, n = 14, align = "center"), .(type, agegroup2) ]


p3 = ggplot(irlandm4, aes(Date, newvalue7, col = agegroup2 ,lty = type)) + geom_line() + scale_y_log10() +  facet_wrap(~agegroup2) + theme()
p3


irlandm4[,newvalue7v2 := ifelse(type =="Hospitalized", newvalue7 *10, newvalue7)]

p4 = ggplot(irlandm4, aes(Date, newvalue7v2, col = type)) + geom_line() + scale_y_log10(limits = c(0.5, 1100)) +  facet_wrap(~agegroup2, scales = "free") + theme()
p4

p5 = ggplot(irlandm4[Date>as_date("2020-08-01")], aes(Date, newvalue7v2, col =  type)) + geom_line() +  facet_wrap(~agegroup2, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0),
        legend.position = "top",
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))  + 
  scale_x_date(breaks =  date_breaks(width = '1 months'), date_labels = "%Y-%b") + scale_y_continuous(sec.axis = sec_axis(~ . / 10, name = "N Hospitalized (centered 14-day-mean)"), name = "N Testpositive (centered 14-day-mean)")

p5
  
irland5 = dcast.data.table(irlandm4[is.na(newvalue7v2)==F],Date +agegroup2 ~ type, value.var = c("newvalue7v2"))
irland5[,ratio := Hospitalized/testpositive]


irlandm6 = melt(irland5, id.vars = c("Date", "agegroup2"))

p6 = ggplot(irlandm6[Date>as_date("2020-08-01")], aes(Date, value, col =  variable)) + geom_line() +  facet_wrap(~agegroup2, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0),
        legend.position = "top",
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))   +
  # scale_x_date(breaks =  date_breaks(width = '1 months'), date_labels = "%Y-%b") + 
  scale_y_log10(limits = c(0.01, 1100), labels = label_comma(accuracy = 0.01) , breaks= log_breaks(6))

p6
  

irlandm6[ variable =="ratio",ratio14 := frollmean(x = value, n = 14, align = "center"), .( agegroup2) ]
p7 = ggplot(irlandm6[Date>as_date("2020-08-01") & variable =="ratio"], aes(Date, value, col =  variable)) + geom_line() +  facet_wrap(~agegroup2, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0),
        legend.position = "top",
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))   +
  scale_x_date(breaks =  date_breaks(width = '1 months'), date_labels = "%Y-%b") +
  geom_smooth() +ylab('Ratio hospitalized/Testpositive')
  # scale_y_log10(limits = c(0.01, 1100), labels = label_comma(accuracy = 0.01) , breaks= log_breaks(6))

p7
