# # Initialize ----
rm(list = ls())
set.seed(192)

knitr::stitch_rhtml(here::here("scripte/b19_s06_3_ageStratIncidencePlot_211014.R"))
knitr::stitch_rhtml(here::here("scripte/b19_s10_4_vaccNonVaccPlotData.R"))
knitr::stitch_rhtml(here::here("scripte/b19_s11_2_farrington_rki.R"))
knitr::stitch_rhtml(here::here("scripte/b19_s14_2_yearComparisonGerSax_211028.R"))

# # finalize -----
sessionInfo() # verwendete packages und software fuer dokumentationszwecke
