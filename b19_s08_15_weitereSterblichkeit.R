# # Initialize ----
rm(list = ls())
require(ggthemes)
require(scales)
require(OurTools) # von Dirk Hasenclever, fuer Darstellun NiceHist
require(triangle)
require(toolboxH)
require(ggplot2)
require(here)

set.seed(192)


# # Parameter ----

calcNEutod <- function(erhoehung_impf_ue60=0, showAllHist = F, zusatzschutzfaktorImpfung3_min = 1, zusatzschutzfaktorImpfung3_max = 1, impfschutz_80plus_beta = T,   impfschutz_80plus_min = 0.55, impfschutz_80plus_max = 0.85,   impfschutz_60_80_min = 0.80, impfschutz_60_80_max = 0.90,  betroffen_ungeimpft_min = 0.3, betroffen_ungeimpft_max = 0.6 , betroffen_geimpft_infiziert_min = 0.3, betroffen_geimpft_infiziert_max = 0.6) {
  # impfschutz_80plus_beta = T;   impfschutz_80plus_min = 0.55; impfschutz_80plus_max = 0.85
  par(mfrow = c(1,1))
  Nsim=50000
  
  dunkelziffer_60_80 = 1.5 + (7-1.5)*rbeta(Nsim, 2, 6)
  if(showAllHist==T) NiceHist(dunkelziffer_60_80)  # Interpretation HK von MUCOS Analyse 26.10.21 vgl. auch # Berits Report https://www.medrxiv.org/content/10.1101/2021.05.04.21256597v2.full.pdf  und RKI Report Bulletin https://www.rki.de/DE/Content/Infekt/EpidBull/Archiv/2021/Ausgaben/37_21.html
  
  dunkelziffer_80plus =  1.5 + (4.5-1.5)*rbeta(Nsim, 2, 3)
  if(showAllHist==T) NiceHist(dunkelziffer_80plus)   # Interpretation HK von MUCOS Analyse 26.10.21  
  
  # Case fatality rates
  cfr_80plus_ungeimpft_min = 0.16 # basierenden auf RKI github "https://media.githubusercontent.com/media/robert-koch-institut/SARS-CoV-2_Infektionen_in_Deutschland/master/Aktuell_Deutschland_SarsCov2_Infektionen.csv" bis 15.1.21, i.e. vor Impfung
  cfr_80plus_ungeimpft_max = 0.24
  
  cfr_80plus_ungeimpft = rtriangle(n = Nsim, a = cfr_80plus_ungeimpft_min, b = cfr_80plus_ungeimpft_max)
  if(showAllHist==T) NiceHist(cfr_80plus_ungeimpft)
  
  cfr_60_80_ungeimpft_min = 0.04
  cfr_60_80_ungeimpft_max = 0.06
  cfr_60_80_ungeimpft = rtriangle(n = Nsim, a = cfr_60_80_ungeimpft_min, b = cfr_60_80_ungeimpft_max)
  if(showAllHist==T) NiceHist(cfr_60_80_ungeimpft)
  
  
  
  
  if(impfschutz_80plus_beta ==T) {
    message("using rbeta 3,2  ",impfschutz_80plus_min, "-",impfschutz_80plus_max," distribution for 80plus")
    impfschutz_80plus= (impfschutz_80plus_min + (impfschutz_80plus_max-impfschutz_80plus_min)*rbeta(Nsim, 3, 2)) 
    
  } else {
    message("using uniform ",impfschutz_80plus_min, "-",impfschutz_80plus_max," distribution for 80plus")
    impfschutz_80plus=    runif(n = Nsim, min = impfschutz_80plus_min, max = impfschutz_80plus_max) 
    
  }
  if(showAllHist==T) NiceHist(impfschutz_80plus)
  range(impfschutz_80plus)
  
  cfr_80plus_geimpft_ohneBooster = cfr_80plus_ungeimpft * (1-impfschutz_80plus)
  
  zusatzschutzfaktorImpfung3 = runif(Nsim,zusatzschutzfaktorImpfung3_min, zusatzschutzfaktorImpfung3_max)
  cfr_80plus_geimpft = cfr_80plus_geimpft_ohneBooster/zusatzschutzfaktorImpfung3
  
  
  
  if(showAllHist==T) NiceHist(cfr_80plus_geimpft)
  if(showAllHist==T) NiceHist(cfr_80plus_geimpft/dunkelziffer_80plus)
  
  
  
  impfschutz_60_80=    runif(n = Nsim, min = impfschutz_60_80_min, max = impfschutz_60_80_max) 
  if(showAllHist==T) NiceHist(impfschutz_60_80)
  range(impfschutz_60_80)
  
  cfr_60_80_geimpft_ohneBooster = cfr_60_80_ungeimpft * (1-impfschutz_60_80)
  cfr_60_80_geimpft = cfr_60_80_geimpft_ohneBooster/zusatzschutzfaktorImpfung3
  
  if(showAllHist==T) NiceHist(cfr_60_80_geimpft)
  if(showAllHist==T) NiceHist(cfr_60_80_geimpft/dunkelziffer_60_80)
  # wieviel der Ue60 werden ueberhaupt sich in den naechsten Jahren anstecken
  # # Grippe (Influenza): Verbreitung leider nur wenig gefunden, e.g. Die Weltgesundheitorganisation WHO geht davon aus, dass auf der Nordhalbkugel jährlich etwa 5 bis 15 Prozent der Bevölkerung an Grippe erkranken. In Deutschland wurden dem Robert Koch-Institut (RKI) für die Grippe-Saison 2014/2015 70.247 Fälle gemeldet.04.08.2016 # www.lungeninformationsdienst.de › virale Infekte
  # Verwende daher Informationen von EBV Durchseuchung mit grosserer Unsicherheit: #EBV https://www.gesundheitsforschung-bmbf.de/de/epstein-barr-virus-von-harmlos-bis-folgenschwer-7238.php gr 90%
  
  betroffen_ungeimpft = rtriangle(n = Nsim, a = betroffen_ungeimpft_min, b = betroffen_ungeimpft_max)
  if(showAllHist==T) NiceHist(betroffen_ungeimpft)
  
  betroffen_geimpft_infiziert = rtriangle(n = Nsim, a = betroffen_geimpft_infiziert_min, b = betroffen_geimpft_infiziert_max)
  if(showAllHist==T) NiceHist(betroffen_geimpft_infiziert)
  
  
  
  
  # Einwohner Sachsen nach Alter 
  N60_80 =1011120
  N80plus = 361987  # https://www-genesis.destatis.de/genesis/online?operation=previous&levelindex=2&step=2&titel=Ergebnis&levelid=1634026654759&acceptscookies=false#abreadcrumb
  
  
  impfquote_60_80_min = 0.778  +erhoehung_impf_ue60 # Impfquote von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Impfquoten-Tab.html 
  
  impfquote_60_80_max =  impfquote_60_80_min + 0.05# COVIMO  meint, dass darueber hinaus die Impfquoten generell wegen Untererfassung hoeher sind https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/COVIMO_Reports/covimo_studie_bericht_7.pdf?__blob=publicationFile 
  
  impfquote_60_80_pre = rtriangle(n = Nsim, a = impfquote_60_80_min, b = impfquote_60_80_max, c = impfquote_60_80_max)
  if(showAllHist==T) NiceHist(impfquote_60_80_pre)
  
  erhoehung_impf_80 =   runif(n = Nsim, min = 0.02, max = 0.08) # in IPSOS Sachsen und Vgl. https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/Jun_2021/2021-06-02-de.pdf?__blob=publicationFile Sonderauswertung Impfquoten Impfquote nicht gleich in ue60, sondern in ue80 hoeher
  
  korrektur_ungleichverteilungImpfung_60_80 = erhoehung_impf_80*N80plus/N60_80 #  minuskorrektur_ungleichverteilungImpfung_60_80  wegen Annahme Impfquote bei 80plus Jaehrigen ca 5-10% hoeher als 60-80, muss dann ja bei den juengeren ausgeglichen werden. 
  summary(korrektur_ungleichverteilungImpfung_60_80)
  
  impfquote_60_80 = impfquote_60_80_pre- korrektur_ungleichverteilungImpfung_60_80
  impfquote_60_80 = ifelse(impfquote_60_80>1, 1, impfquote_60_80)
  if(showAllHist==T) NiceHist(impfquote_60_80)
  summary(impfquote_60_80)
  
  impfquote_80plus_min = 0.778  + erhoehung_impf_ue60# https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Impfquoten-Tab.html s.o. generell hoeher in aelteren  
  
  impfquote_80plus_max =  impfquote_80plus_min + 0.05# COVIMO  berichtet von wahrscheinlich hoeherer Impfquote https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/COVIMO_Reports/covimo_studie_bericht_7.pdf?__blob=publicationFile 
  
  impfquote_80plus_pre = rtriangle(n = Nsim, a = impfquote_80plus_min, b = impfquote_80plus_max, c = impfquote_80plus_max)
  if(showAllHist==T) NiceHist(impfquote_80plus_pre)
  
  impfquote_80plus = impfquote_80plus_pre +erhoehung_impf_80
  impfquote_80plus = ifelse(impfquote_80plus>1, 1, impfquote_80plus)
  if(showAllHist==T) NiceHist(impfquote_80plus)
  summary(impfquote_80plus)
  
  # Lassen sich irgendwann Infizierte genauso haeufig impfen wie alle anderen? 
  # Leider keine Daten, daher groesserer bereich5, nahm als wahrscheinlichsten Wert Impfquote an
  
  ueberlapp_infiziertUNDgeimpft_60_80_min = mean(impfquote_60_80) -0.15
  ueberlapp_infiziertUNDgeimpft_60_80_max = mean(impfquote_60_80) +0.15
  if(ueberlapp_infiziertUNDgeimpft_60_80_max>1) ueberlapp_infiziertUNDgeimpft_60_80_max=1
  ueberlapp_infiziertUNDgeimpft_60_80 = rtriangle(n = Nsim, a = ueberlapp_infiziertUNDgeimpft_60_80_min, b = ueberlapp_infiziertUNDgeimpft_60_80_max, c=mean(impfquote_60_80)) # 0.8 ca. saechsische impfquote 60+
  if(showAllHist==T) NiceHist(ueberlapp_infiziertUNDgeimpft_60_80)
  
  
  ueberlapp_infiziertUNDgeimpft_80plus_min = mean(impfquote_80plus) -0.15
  ueberlapp_infiziertUNDgeimpft_80plus_max = mean(impfquote_80plus) +0.15
  if(ueberlapp_infiziertUNDgeimpft_80plus_max>1) ueberlapp_infiziertUNDgeimpft_80plus_max=1
  ueberlapp_infiziertUNDgeimpft_80plus = rtriangle(n = Nsim, a = ueberlapp_infiziertUNDgeimpft_80plus_min, b = ueberlapp_infiziertUNDgeimpft_80plus_max, c=mean(impfquote_80plus)) # 0.8 ca. saechsische impfquote 60+
  if(showAllHist==T) NiceHist(ueberlapp_infiziertUNDgeimpft_80plus)
  
  # Infektionsgeschehen in Sachsen 
  # https://media.githubusercontent.com/media/robert-koch-institut/SARS-CoV-2_Infektionen_in_Deutschland/master/Aktuell_Deutschland_SarsCov2_Infektionen.csv
  
  verstorben_60_80  = 2936  # 12.10.21  
  verstorben_80plus  = 6993 # 12.10.21  
  
  testpositiv_60_80  = 57303          # 12.10.21                 
  testpositiv_80plus  = 35026         # 12.10.21    
  
  
  # # Abgeleitete Zahlen----
  
  # Kumulativ Testpositive und infizierte ----
  testpositiv_lebend_60_80  = testpositiv_60_80-  verstorben_60_80            
  testpositiv_lebend_60_80
  testpositiv_lebend_80plus  = testpositiv_80plus  - verstorben_80plus
  testpositiv_lebend_80plus
  
  infiziert_lebend_60_80 = testpositiv_lebend_60_80*dunkelziffer_60_80
  infiziert_lebend_80plus = testpositiv_lebend_80plus*dunkelziffer_80plus
  
  # Geschuetzt/Ungeschuetzte ----
  
  geimpft_60_80 =  impfquote_60_80*N60_80 
  
  infiziert_ungeimpft_60_80 = infiziert_lebend_60_80 - infiziert_lebend_60_80*ueberlapp_infiziertUNDgeimpft_60_80 # die geschuetzten, i.e. geimpfte plus genesene abzueglich genesene, die schon bei den geimpften mit gezaehlt wurden
  
  infiziert_ungeimpft_60_80 = ifelse(infiziert_ungeimpft_60_80 > (N60_80 - verstorben_60_80),(N60_80 - verstorben_60_80), infiziert_ungeimpft_60_80 ) # es koennen nicht mehr infiziert sein, als da sind                                                              
  
  ungeschuetzt_60_80 = N60_80 - verstorben_60_80 - geimpft_60_80 - infiziert_ungeimpft_60_80 # alle anderen ungeschuetzt
  ungeschuetzt_60_80 = ifelse(ungeschuetzt_60_80 <0, 0, ungeschuetzt_60_80)
  par(mfrow = c(3,1))
  if(showAllHist==T) NiceHist(geimpft_60_80)
  if(showAllHist==T) NiceHist(infiziert_ungeimpft_60_80)
  if(showAllHist==T) NiceHist(ungeschuetzt_60_80)
  
  
  # nun noch 80plus
  
  geimpft_80plus =  impfquote_80plus*N80plus 
  
  infiziert_ungeimpft_80plus = infiziert_lebend_80plus - infiziert_lebend_80plus*ueberlapp_infiziertUNDgeimpft_80plus # die geschuetzten, i.e. geimpfte plus genesene abzueglich genesene, die schon bei den geimpften mit gezaehlt wurden
  
  infiziert_ungeimpft_80plus = ifelse(infiziert_ungeimpft_80plus > (N80plus - verstorben_80plus),(N80plus - verstorben_80plus), infiziert_ungeimpft_80plus ) # es koennen nicht mehr infiziert sein, als da sind                                                              
  
  ungeschuetzt_80plus = N80plus - verstorben_80plus - geimpft_80plus - infiziert_ungeimpft_80plus # alle anderen ungeschuetzt
  ungeschuetzt_80plus = ifelse(ungeschuetzt_80plus <0, 0, ungeschuetzt_80plus)
  par(mfrow = c(3,1))
  if(showAllHist==T) NiceHist(geimpft_80plus)
  if(showAllHist==T) NiceHist(infiziert_ungeimpft_80plus)
  if(showAllHist==T) NiceHist(ungeschuetzt_80plus)
  
  # Erwartete Todesfaelle  ----
  neutod_geimpft_infiziert_60_80 = geimpft_60_80* betroffen_geimpft_infiziert * cfr_60_80_geimpft/dunkelziffer_60_80 + 
    infiziert_ungeimpft_60_80* betroffen_geimpft_infiziert * cfr_60_80_geimpft_ohneBooster/dunkelziffer_60_80
  
  neutod_ungeschuezt_60_80 = ungeschuetzt_60_80* betroffen_ungeimpft * cfr_60_80_ungeimpft/dunkelziffer_60_80 
  neutod_60_80 = neutod_ungeschuezt_60_80 + neutod_geimpft_infiziert_60_80
  
  if(showAllHist==T) NiceHist(neutod_ungeschuezt_60_80)
  if(showAllHist==T) NiceHist(neutod_geimpft_infiziert_60_80)
  
  par(mfrow = c(1,1))
  if(showAllHist==T) NiceHist(neutod_60_80)
  
  
  
  
  # neutod_geimpft_infiziert_80plus = geimpft_infiziert_80plus* betroffen_geimpft_infiziert * cfr_80plus_geimpft/dunkelziffer_80plus 
  
  neutod_geimpft_infiziert_80plus = geimpft_80plus* betroffen_geimpft_infiziert * cfr_80plus_geimpft/dunkelziffer_80plus + 
    infiziert_ungeimpft_80plus* betroffen_geimpft_infiziert * cfr_80plus_geimpft_ohneBooster/dunkelziffer_80plus 
    
    neutod_ungeschuezt_80plus = ungeschuetzt_80plus* betroffen_ungeimpft * cfr_80plus_ungeimpft/dunkelziffer_80plus 
  neutod_80plus = neutod_ungeschuezt_80plus + neutod_geimpft_infiziert_80plus
  
  if(showAllHist==T) NiceHist(neutod_ungeschuezt_80plus)
  if(showAllHist==T) NiceHist(neutod_geimpft_infiziert_80plus)
  
  par(mfrow = c(1,1))
  if(showAllHist==T) NiceHist(neutod_80plus)
  neutod_ue60 = neutod_60_80 + neutod_80plus
  neutod_ue60_ungeschuetzt = neutod_ungeschuezt_60_80 + neutod_ungeschuezt_80plus
  neutod_geimpft_infiziert = neutod_geimpft_infiziert_60_80 + neutod_geimpft_infiziert_80plus
  NiceHist(neutod_ue60)
  quantile = quantile(neutod_ue60, probs = c(0.025, 0.5, 0.975))
  
  quantile_schutzdet = data.table(gruppe =factor(c( "neutod_ue60", "neutod_ue60_ungeschuetzt", "neutod_geimpft_infiziert" ),levels = c( "neutod_ue60", "neutod_ue60_ungeschuetzt", "neutod_geimpft_infiziert" ), labels = c("Gesamt","Ungeschützt","Geimpft/Genesen")),
                            rbind(c(min = min(neutod_ue60), quantile(neutod_ue60, probs = c(0.025, 0.5, 0.975)), max = max(neutod_ue60)),
                                  c(min = min(neutod_ue60_ungeschuetzt), quantile(neutod_ue60_ungeschuetzt, probs = c(0.025, 0.5, 0.975)), max = max(neutod_ue60_ungeschuetzt)),
                                  c(min = min(neutod_geimpft_infiziert), quantile(neutod_geimpft_infiziert, probs = c(0.025, 0.5, 0.975)), max = max(neutod_geimpft_infiziert))) %>% round())
  
  
  quantile_ageANDschutzdet = data.table(gruppe =factor(x = c( "neutod_ue60", "neutod_ungeschuezt_60_80","neutod_ungeschuezt_80plus", "neutod_geimpft_infiziert_60_80", "neutod_geimpft_infiziert_80plus" ),
                                               levels =c( "neutod_ue60", "neutod_ungeschuezt_60_80","neutod_ungeschuezt_80plus", "neutod_geimpft_infiziert_60_80", "neutod_geimpft_infiziert_80plus" ),
                                               labels = c("Gesamt","Ungeschützt 60-80","Ungeschützt 80+","Geimpft/Genesen 60-80", "Geimpft/Genesen 80+")),
                                rbind(c(min = min(neutod_ue60), quantile(neutod_ue60, probs = c(0.025, 0.5, 0.975)), max = max(neutod_ue60)),
                                      c(min = min(neutod_ungeschuezt_60_80), quantile(neutod_ungeschuezt_60_80, probs = c(0.025, 0.5, 0.975)), max = max(neutod_ungeschuezt_60_80)),
                                      c(min = min(neutod_ungeschuezt_80plus), quantile(neutod_ungeschuezt_80plus, probs = c(0.025, 0.5, 0.975)), max = max(neutod_ungeschuezt_80plus)),
                                      c(min = min(neutod_geimpft_infiziert_60_80), quantile(neutod_geimpft_infiziert_60_80, probs = c(0.025, 0.5, 0.975)), max = max(neutod_geimpft_infiziert_60_80)),
                                      c(min = min(neutod_geimpft_infiziert_80plus), quantile(neutod_geimpft_infiziert_80plus, probs = c(0.025, 0.5, 0.975)), max = max(neutod_geimpft_infiziert_80plus))) %>% round())
  
  quantile_agedet = data.table(gruppe =factor(x = c( "neutod_ue60", "neutod_60_80","neutod_80plus" ),
                                               levels =c( "neutod_ue60", "neutod_60_80","neutod_80plus"),
                                               labels = c("Gesamt 60+","60-80","80+")),
                               rbind(c(min = min(neutod_ue60), quantile(neutod_ue60, probs = c(0.025, 0.5, 0.975)), max = max(neutod_ue60)),
                                     c(min = min(neutod_60_80), quantile(neutod_60_80, probs = c(0.025, 0.5, 0.975)), max = max(neutod_60_80)),
                                     c(min = min(neutod_80plus), quantile(neutod_80plus, probs = c(0.025, 0.5, 0.975)), max = max(neutod_80plus))
                               ) %>% round())
  
  tabelle = data.table(neutod_ue60, neutod_80plus, neutod_ungeschuezt_80plus ,ungeschuetzt_80plus,  betroffen_ungeimpft, cfr_80plus_ungeimpft, dunkelziffer_60_80,dunkelziffer_80plus, N80plus , verstorben_80plus ,geimpft_80plus, infiziert_ungeimpft_80plus,impfquote_80plus,  infiziert_lebend_80plus ,ueberlapp_infiziertUNDgeimpft_80plus,testpositiv_lebend_80plus,        neutod_geimpft_infiziert_80plus,neutod_60_80, neutod_ungeschuezt_60_80 , neutod_geimpft_infiziert_60_80)
  tabelle
  
  res = list(neutod_ue60,  quantile,  quantile_schutzdet, quantile_agedet  ,quantile_ageANDschutzdet, tabelle)
  names(res ) = c('neutod_ue60',  'quantile',"quantile_schutzdet","quantile_agedet","quantile_ageANDschutzdet", "tabelle")
  res
}



# alle Zusammen -----

scenario_aktuell = calcNEutod(erhoehung_impf_ue60 = 0) 

scenario_10prozUe60mehr = calcNEutod(erhoehung_impf_ue60 = 0.1)

scenario_PlusDrittimpfung = calcNEutod(erhoehung_impf_ue60 = 0, zusatzschutzfaktorImpfung3_min = 2, zusatzschutzfaktorImpfung3_max =  4)

scenario_10prozUe60mehrPlusDrittimpfung = calcNEutod(erhoehung_impf_ue60 = 0.1,  zusatzschutzfaktorImpfung3_min = 2, zusatzschutzfaktorImpfung3_max =  4)

par(mfrow = c(2,2))
NiceHist(scenario_aktuell$neutod_ue60,MAIN =  "scenario_aktuell")
NiceHist(scenario_10prozUe60mehr$neutod_ue60,MAIN = "scenario_10prozUe60mehr")
NiceHist(scenario_PlusDrittimpfung$neutod_ue60, MAIN = "scenario_PlusDrittimpfung")
NiceHist(scenario_10prozUe60mehrPlusDrittimpfung$neutod_ue60, MAIN = "scenario_10prozUe60mehrPlusDrittimpfung" )


allscenarien_schutzdet = rbind(scenario_aktuell$quantile_schutzdet,
                     scenario_10prozUe60mehr$quantile_schutzdet,
                     scenario_PlusDrittimpfung$quantile_schutzdet,
                     scenario_10prozUe60mehrPlusDrittimpfung$quantile_schutzdet) %>% data.table

allscenarien_schutzdet$scenario = rep(c("Scenario 1:\nIst-Stand", "Scenario 2:\nImpfquote +10%", "Scenario 3:\n+Drittimpfung", "Scenario 4:\nImpfquote +10%\n+Drittimpfung"), 3) %>% sort()


allscenarien_schutzdet
ccc(allscenarien_schutzdet)


p_scenarien = ggplot(allscenarien_schutzdet[gruppe == 'Gesamt' ], aes(scenario, `50%`))  + geom_point(size  = 44, alpha = 0.8, pch ="-")  +
  scale_y_continuous(labels = label_comma(accuracy = 1), breaks = pretty_breaks(15))+
  
  geom_errorbar(aes(ymin =`2.5%`, ymax = `97.5%`), alpha = 0.7, lwd  = 2, width = 0.2)+ 
  
  theme_pander(base_size = 11) + ylab("Abschätzung zukünftige COVID-19 Verstorbene 60+\n(Median mit 95% Wahrscheinlichkeitsbereich)") + xlab("")

p_scenarien


p_scenarien_schutzdet = ggplot(allscenarien_schutzdet, aes(gruppe, `50%`, col  = gruppe))  + geom_point(size  = 44, alpha = 0.8, pch ="-")  +
  scale_y_continuous(labels = label_comma(accuracy = 1), breaks = pretty_breaks(15))+
  facet_grid(.~scenario, scales = "free", )+
  geom_errorbar(aes(ymin =`2.5%`, ymax = `97.5%`), alpha = 0.7, lwd  = 2, width = 0.2)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        panel.spacing = unit(2, "lines"))+
  guides(col = "none") + 
  scale_color_manual(values = c("black", "purple", "steelblue"))+
  
  theme_pander(base_size = 11) + ylab("Abschätzung zukünftige COVID-19 Verstorbene 60+\n(Median mit 95% Wahrscheinlichkeitsbereich)") + xlab("")

p_scenarien_schutzdet


# agedetails ----

allscenarien_agedet = rbind(scenario_aktuell$quantile_agedet,
                               scenario_10prozUe60mehr$quantile_agedet,
                               scenario_PlusDrittimpfung$quantile_agedet,
                               scenario_10prozUe60mehrPlusDrittimpfung$quantile_agedet) %>% data.table

allscenarien_agedet$scenario = rep(c("Scenario 1:\nIst-Stand", "Scenario 2:\nImpfquote +10%", "Scenario 3:\n+Drittimpfung", "Scenario 4:\nImpfquote +10%\n+Drittimpfung"), 3) %>% sort()


allscenarien_agedet
ccc(allscenarien_agedet)


p_scenarien_agedet = ggplot(allscenarien_agedet, aes(gruppe, `50%`, col  = gruppe))  + geom_point(size  = 44, alpha = 0.8, pch ="-")  +
  scale_y_continuous(labels = label_comma(accuracy = 1), breaks = pretty_breaks(15))+
  facet_grid(.~scenario, scales = "free", )+
  geom_errorbar(aes(ymin =`2.5%`, ymax = `97.5%`), alpha = 0.7, lwd  = 2, width = 0.2)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        panel.spacing = unit(2, "lines"))+
  guides(col = "none") + 
  scale_color_manual(values = c("black", "steelblue", "purple"))+
  
  theme_pander(base_size = 11) + ylab("Abschätzung zukünftige COVID-19 Verstorbene 60+\n(Median mit 95% Wahrscheinlichkeitsbereich)") + xlab("")

p_scenarien_agedet





allscenarien_ageANDschutzdet = rbind(scenario_aktuell$quantile_ageANDschutzdet,
                             scenario_10prozUe60mehr$quantile_ageANDschutzdet,
                             scenario_PlusDrittimpfung$quantile_ageANDschutzdet,
                             scenario_10prozUe60mehrPlusDrittimpfung$quantile_ageANDschutzdet) %>% data.table

allscenarien_ageANDschutzdet$scenario = rep(c("Scenario 1:\nIst-Stand", "Scenario 2:\nImpfquote +10%", "Scenario 3:\n+Drittimpfung", "Scenario 4:\nImpfquote +10%\n+Drittimpfung"), 5) %>% sort()


allscenarien_ageANDschutzdet
ccc(allscenarien_ageANDschutzdet)

p_scenarien_ageANDschutzdet = ggplot(allscenarien_ageANDschutzdet, aes(gruppe, `50%`, col  = gruppe))  + geom_point(size  = 22, alpha = 0.8, pch ="-")  +
  scale_y_continuous(labels = label_comma(accuracy = 1), breaks = pretty_breaks(15))+
  facet_grid(.~scenario, scales = "free", )+
  geom_errorbar(aes(ymin =`2.5%`, ymax = `97.5%`), alpha = 0.7, lwd  = 2, width = 0.2)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        panel.spacing = unit(2, "lines"))+
  guides(col = "none") + 
  scale_color_manual(values = c("black", "purple","purple4", "steelblue", "steelblue4"))+
  
  theme_pander(base_size = 11) + ylab("Abschätzung zukünftige COVID-19 Verstorbene 60+\n(Median mit 95% Wahrscheinlichkeitsbereich)") + xlab("")

p_scenarien_ageANDschutzdet



jpeg(here('results/b19_s08_15_szenarienV3_Verstorbene.jpeg'), 4.4,4.4, quality = 100, res = 150,  units = "in")
p_scenarien
dev.off()
# 
jpeg(here('results/b19_s08_15_szenarienV3_Verstorbene_schutzdetails.jpeg'), width =7.4,6.4, quality = 100, res = 150,  units = "in")
p_scenarien_schutzdet
dev.off()

jpeg(here('results/b19_s08_15_szenarienV3_Verstorbene_agedetails.jpeg'), width =7.4,6.4, quality = 100, res = 150,  units = "in")
p_scenarien_agedet
dev.off()


jpeg(here('results/b19_s08_15_szenarienV3_Verstorbene__ageANDschutzdet.jpeg'), width =9.9,5.4, quality = 100, res = 150,  units = "in")
p_scenarien_ageANDschutzdet
dev.off()

jpeg(here('results/b19_s08_15_szenarienV3_Verstorbene_histogramme.jpeg'), width =9,9, quality = 100, res = 150,  units = "in")

par(mfrow = c(2,2))
NiceHist(scenario_aktuell$neutod_ue60,MAIN =  "scenario_aktuell")
NiceHist(scenario_10prozUe60mehr$neutod_ue60,MAIN = "scenario_10prozUe60mehr")
NiceHist(scenario_PlusDrittimpfung$neutod_ue60, MAIN = "scenario_PlusDrittimpfung")
NiceHist(scenario_10prozUe60mehrPlusDrittimpfung$neutod_ue60, MAIN = "scenario_10prozUe60mehrPlusDrittimpfung" )
dev.off()

# # finalize -----
sessionInfo() # verwendete packages und software fuer dokumentationszwecke
