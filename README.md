# IMISE Epidemiologisches Bulletin 19

Analysecode und Aktualisierungen von Grafiken des Epidemiologischen Bulletins 19 des IMISE der Universität Leipzig. Das Bulletin 19 ist [hier](https://github.com/GenStatLeipzig/IMISE-Epidemiologisches-Bulletin-19/raw/main/IMISE_Bulletin_19_2021_11_01.pdf) verfügbar.

### Aktualisierung Abb. 1: Verlauf der COVID-19 Testpositiven in Deutschland und Sachsen nach Altersgruppe.

<img src="results/b19_s06_3_ageStratPlot.jpeg" title="Infektionsgeschehen in Sachsen und Deutschland" width="671"/>

*Verlauf der COVID-19 Testpositiven in Deutschland und Sachsen nach Altersgruppe. Die schwarze Linie entspricht dem Durchschnitt der Gesamtbevölkerung. In Sachsen ist das langanhaltende exponentielle Wachstum (i.e. stetige Verdoppelung der Fallzahlen in der gleichen Zeiteinheit) deutlich als linearer Anstieg in der hier gewählten logarithmischen Skaleneinteilung sichtbar. (Datenquelle: RKI nach Meldedatum, aggregiert vom KIT - Karlsruher Institut für Technologie*

### Aktualisierung Abb. 2: Vergleich der berichteten COVID-19 Testpositiven nach Impfstatus in Sachsen

<img src="results/b19_s10_4_vaccNonVaccPlotData.jpeg" title="Abb. 2: Vergleich der berichteten COVID-19 Testpositiven nach Impfstatus in Sachsen. Quelle: SMS" width="360"/>

Quelle: SMS

### Aktualisierung Abb.3: Vergleich Infektionsgeschehen 2021 vs. 2020

![](results/b19_S14_2_year2year_comparison.jpeg "vergleich infektionsgeschehen 2021 vs 2020")

*Vergleich der Testpositiven, Verstorbenen und der ITS-Bettenbelegung 2021 vs. 2021. Das Infektionsgeschehen ist in all diesen drei Bereichen in 2021 bisher stärker als in 2020. Datenquelle: DIVI, RKI nach Meldedatum*

Gleiche Daten in logarithmischer Skaleneinteilung:

![](results/b19_S14_2_year2year_comparison_log.jpeg "logarithmische darstellung infektionsgeschehen 2021 vs 2020")

### Aktualisierung Abb. 7: Aktuelle Entwicklung des Impfschutzes in Deutschland

<img src="results/b19_s10_2_RKI_farrington.jpeg" title="Abb. 7: Aktuelle Entwicklung des Impfschutzes in Deutschland:" width="564"/>

*Aktuelle Entwicklung des Impfschutzes in Deutschland: Dargestellt sind die Werte für die angegebenen Kalenderwochen. Die meisten Impfungen erfolgten in Deutschland bereits ab der 13. Kalenderwoche (RKI). Etwa 20 Wochen danach beginnt der Schutz vor Infektion zu sinken. Datenquelle: RKI Wochenberichte*

### Code für Abb. 10: Szenarien zur Abschätzung der zu erwartenden weiteren COVID-19 Verstorbenen in Sachsen.

Der Analysecode ist unter *scripte\\b19_s08_18_weitereSterblichkeit.R verfügbar,* weitere zu Grunde liegende Annahmen sind im [Bulletin 19](https://github.com/GenStatLeipzig/IMISE-Epidemiologisches-Bulletin-19/raw/main/IMISE_Bulletin_19_2021_11_01.pdf) aufgeführt.

<img src="results/b19_s08_16_szenarienV3_Verstorbene_agedetails.jpeg" title="abb10 szenarien" width="569"/>

*Verstorbenen in Sachsen. Die vertikalen Linien kennzeichnen den 95% Unsicherheitsbereich der jeweiligen Schätzung, der Querbalken den medianen Schätzwert zukünftig Verstorbener (Wert in der Mitte). Gezeigt ist sowohl die gesamte Abschätzung der über 60-Jährigen als auch für die 60-79-Jährigen bzw. über 80-Jährigen separat.*
