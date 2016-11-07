# Control Data


## Sources
Population Density: Genesis Data Table 12411-0050

Population: Genesis Data Table 12211-0005

State area: Genesis Data Table 11111-0001

Unemployment Rates: Genesis Data Table 13211-0007

State GDP: Genesis Data Table 82111-0001

Beer Tax: Genesis Data Table 71211-0007

Youth Unemployment: Statista (based on Bundesagentur für Arbeit)

Education: Genesis Table ???

## Variables

Variable |Name Eng | Name GER
-----|-------------------- | --------------------
`STATE`| German State | Bundesland
`YEAR`| Year 1999-2014 | Jahr
`PD` | Population Density per qkm | Bevölkerungsdichte pro km^3
`UTOTAL` | Registered unemployed | 
`UR.LF`|Unemployment as percent. of civilian labour force| Arbeitslosenquote aller zivilen Erwerbspersonen 
`UR`| Rate of registered unemployed| Arbeitslosenquote der abhängigen zivilen Erwerbspersonen
`VAC`| Registered vacancies| 
`PP` | Population per State in 1000 | Bevölkerung Bundesland in 1000
`PP.EM` | Persons in employment in 1000 | Erwerbstätige in 1000
`PP.UE`| Unemployed persons in 1000 | Erwerbslose in 1000
`PP.EA`| Economically active population in 1000 | Erwerbspersonen in 1000
`PP.EIA`| Economically inactive population in 1000 | Nichterwerbspersonen in 1000
`SA`| Size of territory (state) |  Gebietsfläche: Bundesländer
`GDP`| GDP at market prices (current prices) (EUR mn)| VGR der Länder (Entstehungsrechnung) - Bruttoinlandsprodukt zu Marktpreisen (nominal)
`BTAX`| Tax revenue beer tax | Steueraufkommen Biersteuer
`YUR`| Youth unemployment rate 15-25 year-olds | Jugendarbeitslosenquote (15 bis unter 25 Jahre)
`EDU_TOTAL` | School Diploma Total | Schulabschlüsse Insgesamt
`EDU_NO` | x | ohne Hauptschulabschluss
`EDU_HS` | x| Hauptschulabschluss
`EDU_RS` | x| Realschulabschluss
`EDU_FH` | x| Fachhochschulreife
`EDU_AH`| x | Allgemeine Hochschulreife


### Arbeitslosenquote aller zivilen Erwerbspersonen 
Die Arbeitslosenquote bezogen auf alle zivilen Erwerbspersonen wird von der Bundesagentur für Arbeit seit 1982 errechnet.Die Arbeitslosenquote zeigt die relative Unterauslastung des Arbeitskräfteangebots an, indem sie die registrierten Arbeitslosen (AL) zu den zivilen Erwerbspersonen (EP = zivile Erwerbstätige + registrierte Arbeitslose) in Beziehung setzt (AL/EP*100). Die zivilen Erwerbspersonen ermittelt die BA wie folgt: 

Sozialversicherungspflichtig Beschäftigte zum 30.6. des Vorjahres 
+ ausschließlich geringfügig Beschäftigte zum 30.6. des Vorjahres 
+ Beamte ohne Soldaten (Personalstandstatistik) zum Juni des Vorvorjahres 
+ Personen in Arbeitsgelegenheiten zum Juni des Vorjahres 
+ auspendelnde Grenzarbeitnehmer (Ergebnisse VGR, Landkreis Waldshut und luxemburgische Sozialversicherungsanstalt)
+ Selbständige/mithelfende Familienangehörige (Mikrozensus)
= zivile Erwerbstätige
+ registrierte Arbeitslose zum 30.6. des Vorjahres
= Zivile Erwerbspersonen.

Alle Komponenten der Bezugsgröße werden wohnortbezogen 
aufbereitet. Die Bezugsgrößen werden einmal jährlich - 
üblicherweise ab Berichtsmonat Mai - aktualisiert.

### Arbeitslosenquote der abhängigen zivilen Erwerbspersonen

Die Arbeitslosenquote bezogen auf die abhängigen zivilen Erwerbspersonen wird von der Bundesagentur für Arbeit seit 1950 errechnet. Die Arbeitslosenquote zeigt die relative Unterauslastung des Arbeitskräfteangebots an, indem sie die registrierten Arbeitslosen (AL) zu den abhängigen zivilen Erwerbspersonen (EP = abhängige zivile Erwerbstätige + registrierte Arbeitslose) in Beziehung setzt (AL/EP*100). Die abhängigen zivilen Erwerbspersonen ermittelt die BA wie folgt:

Sozialversicherungspflichtig Beschäftigte zum 30.6. des Vorjahres
+ ausschließlich geringfügig Beschäftigte zum 30.6. des Vorjahres 
+ Beamte ohne Soldaten (Personalstandstatistik) zum Juni des Vorvorjahres
+ Personen in Arbeitsgelegenheiten zum Juni des Vorjahres
+ auspendelnde Grenzarbeitnehmer (Ergebnisse VGR, Landkreis Waldshut und luxemburgische Sozialversicherungsanstalt)
= abhängige zivile Erwerbstätige
+ registrierte Arbeitslose zum 30.6. des Vorjahres
= Abhängig zivile Erwerbspersonen.

Alle Komponenten der Bezugsgröße werden wohnortbezogen aufbereitet. Die Bezugsgrößen werden einmal jährlich - üblicherweise ab Berichtsmonat Mai - aktualisiert.

© Statistisches Bundesamt, Wiesbaden 2012

