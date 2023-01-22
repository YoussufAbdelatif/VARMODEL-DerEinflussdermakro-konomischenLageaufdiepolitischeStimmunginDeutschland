setwd("C:/Users/Yussuf Schwarz/OneDrive/Desktop/UniWiwi/Seminar Statistik & Ökonometrie/Daten")



################################PACKAGES##############################################

#install.packages("rvest")
#install.packages("dplyr")
#install.packages("janitor")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tidyverse")
#install.packages("xts")
#install.packages("tibble")
#install.packages("writexl")
#install.packages("ggplot2")
#install.packages("zoo")
#install.packages("readxl")
#install.packages("Hmisc")
#install.packages("corrplot")
#install.packages("lattice")
#install.packages("Formula")
#install.packages("survival")
#install.packages("ggcorrplot")
#install.packages("gridExtra")
#install.packages("gridExtra")
#install.packages("readxl")
#install.packages("tseries")
#install.packages("urca")
#install.packages("ggfortify")
#install.packages("tseries")
#install.packages("aTSA")
#install.packages("forecast")
#install.packages("vars")
#install.packages("MLmetrics")
#install.packages("lpir")
#install.packages("magick")
#install.packages("webshot")
#webshot::install_phantomjs()
#install.packages("kableExtra")
#install.packages("mFilter")
#install.packages("SciViews")
#install.packages("cowplot")
#install.packages("strucchange")
#install.packages("gtsummary")
#install.packages("magrittr")
#install.packages("stargazer")
#install.packages("memisc")
#install.packages("gtable")
#install.packages("xfun")
#install.packages("Rtools")
#install.packages("knitr")
#install.packages("forecast")
#install.packages("FIAR")
#install.packages("sparsevar")


library("sparsevar")
library("FIAR")
library("forecast")
library(knitr)
library("xfun")
library(gtable)
library(memisc)
library(stargazer)
library(magrittr)
library(gtsummary)
library("strucchange")
library(cowplot)
library("SciViews")
library(gridExtra)
library("ggcorrplot")
library("lattice")
library("Formula")
library("survival")
library(corrplot)
library("Hmisc")
library("readxl")
library("rvest")
library("dplyr")
library("janitor")
library(dplyr)
library(stringr)
library(tidyverse)
library(xts)
library(tibble)
library("writexl")
library(ggplot2)
library("zoo")
library("kableExtra")
library("gridExtra")
library("readxl")
library("tseries")
library("urca")
library("ggfortify")
library("ggplot2")
library("tseries")
library("aTSA")
library("forecast")
library("vars")
library("MLmetrics")
library("mFilter")
library("magick")
library("webshot")
library("lmtest")


##########################################################################################

##############################DATEN LADEN#################################################

#Daten: Sonntagsfrage / Wahlumfragen 1998-heute. Werden nicht zum Download zur Verfügung gestellt
#--> müssen zunächst von der Webseite extrahiert werden. Die extrahierten Daten werden zwischenge-
#gespeichert. 

#Daten 1998-2012 ; NOTE: Data2013 enthält 2009-2012!
webindex1 = c(1998:2008,"2013")
dataname=c()
for (k in 1:length(webindex1)){
  dataname[k] =  paste("data", webindex1[k],sep="")
}
for (i in 1:length(webindex1)){
  linkname =  paste("https://www.wahlrecht.de/umfragen/forsa/", webindex1[i],".htm" ,sep="")
  page = read_html(linkname)
  data = page %>% html_elements(".wilko") %>% html_table()
  data = do.call(rbind.data.frame, data)
  assign(dataname[i],data,envir=globalenv())
}


#2013-aktuell

linkname ="https://www.wahlrecht.de/umfragen/forsa.htm"
page = read_html(linkname)
data13ak = page %>% html_elements(".wilko") %>% html_table()
data13ak = do.call(rbind.data.frame, data13ak)
#Daten manuell eingeben, an denen Sonstige nach Prozenten aufgeschlüsselt wurde -> damit nachher Umwandlung in as.numeric() möglich ist.


##############################AUFBEREITUNG der extrahierten Daten#################################################

#Aufbereitung Daten 1998-2002
for (b in dataname[1:4]){
  get(b,envir = .GlobalEnv)[-2,-2]
  assign(b,get(b,envir = .GlobalEnv)[-c(1,2),-2],envir=globalenv())
}


#Aufbereitung Daten 2002-2013
for (b in dataname[5:(length(dataname)-1)]){
  get(b,envir = .GlobalEnv)[-2,-c(2,9,10,11)]
  assign(b,get(b,envir = .GlobalEnv)[-c(1,2),-c(2,9,10,11)],envir=globalenv())
}
data2002=data2002[-13,]
data2005=data2005[-16,]
data2013=data2013[-c(1:3),-c(2,9,11:13)] 


#Aufbereitung Daten 2013-aktuell
data13ak=data13ak[-c(1:3),-c(2,10:13)]

data13ak$Sonstige[30] = "8.7%"
data13ak$Sonstige[31] = "8%"
data13ak$Sonstige[32] = "8%"
data13ak$Sonstige[33] = "9%"
data13ak$Sonstige[447]="8%"
data13ak$Sonstige[456]="6.2%"

colnames(data1998)[6] = "Linke/PDS"
colnames(data1999)[6]= "Linke/PDS"
colnames(data2000)[6]= "Linke/PDS"
colnames(data2001)[6]= "Linke/PDS"
colnames(data2002)[6]= "Linke/PDS"
colnames(data2003)[6]= "Linke/PDS"
colnames(data2004)[6]= "Linke/PDS"
colnames(data2005)[6]= "Linke/PDS"
colnames(data2006)[6]= "Linke/PDS"
colnames(data2007)[6]= "Linke/PDS"
colnames(data2008)[6]= "Linke/PDS"
colnames(data2013)[6]= "Linke/PDS"
colnames(data13ak)[6]= "Linke/PDS"

data = rbind(data1999,data2000,data2001,data2002,data2003,data2004,data2005,data2006,data2007,data2008)
data=add_column(data,.before = "Sonstige",Piraten=rep(0,times=nrow(data)))
data=add_column(data,.before = "Sonstige",AfD=rep(0,times=nrow(data)))

data2013= add_column(data2013,.before = "Sonstige",AfD=rep(0,times=nrow(data2013)))
data13ak= add_column(data13ak,.before = "AfD",Piraten=rep(0,times=nrow(data13ak)))

colnames(data)
colnames(data2013)
colnames(data13ak)

colnames(data2013) = colnames(data)

colnames(data13ak) = colnames(data)


datacom = rbind(data,data2013,data13ak)
datacom = as.data.frame(apply(datacom,2, str_remove_all, " "))
datacom1=datacom
#datacom=datacom[-81,]
#datacom=datacom[-45,]

datacom$`CDU/CSU` = gsub("%","",x=datacom$`CDU/CSU`)
datacom$SPD = gsub("%","",x=datacom$SPD)
datacom$GRÜNE = gsub("%","",x=datacom$GRÜNE)
datacom$FDP = gsub("%","",x=datacom$FDP)
datacom$`Linke/PDS` = gsub("%","",x=datacom$`Linke/PDS`)
datacom$Piraten = gsub("%","",x=datacom$Piraten)
datacom$AfD = gsub("%","",x=datacom$AfD)
datacom$Sonstige = gsub("%","",x=datacom$Sonstige)

datacom$`CDU/CSU` = gsub(",",".",x=datacom$`CDU/CSU`)
datacom$SPD = gsub(",",".",x=datacom$SPD)
datacom$GRÜNE = gsub(",",".",x=datacom$GRÜNE)
datacom$FDP = gsub(",",".",x=datacom$FDP)
datacom$`Linke/PDS` = gsub(",",".",x=datacom$`Linke/PDS`)
datacom$Piraten = gsub(",",".",x=datacom$Piraten)
datacom$AfD = gsub(",",".",x=datacom$AfD)
datacom$Sonstige = gsub(",",".",x=datacom$Sonstige)
sub=datacom[627,7]
datacom$Piraten = gsub(sub,0,x=datacom$Piraten)

for (n in 2:ncol(datacom)) {
  datacom[,n]= as.numeric(datacom[,n])
}

datacom$V1 = as.Date(datacom$V1, format = "%d.%m.%Y")
colnames(datacom)[1] = "Datum"
datacom = datacom[order(datacom$Datum),]

datacom1 = datacom #Sicherheitskopie

datacom$month <- as.numeric(format(datacom$Datum, '%m'))
datacom$year <- as.numeric(format(datacom$Datum, '%Y'))

datacom = aggregate(datacom,by = list(datacom$month,datacom$year),FUN=mean)
#Für jeden Monat wurden die Umfrageergebnisse, die in diesem Monat erhoben wurden, aggregiert und
#der monatliche Durchschnitt dieser Umfragen für jede Partei errechnet (s. Winker Finanzmarkt-Ökonometrie)

write_xlsx(x = datacom,path = "Datensicherungdatacomplete.xlsx") #Extrahierte und aufbereitete Daten zwischenspeichern.
#--> mit gespeicherter xlsx weiterarbeiten.

############################################################################################
############################################################################################
############################################################################################


###########################DATEN LADEN#################################################################

#1. Umfragewerte laden: 

datacom = read_xlsx("Datensicherungdatacomplete.xlsx")
datacom = datacom[-c(279:281),]

#Monatliche Umfragewerte nach der Partien aufsummieren, um drei Parteiengruppen zu erhalten:
#Mitte-rechts Parteien, Mitte-links Parteien und Randparteien
#Das Ergebnis wird als datacomag Data FRame abgespeichert.

datacomag = datacom
datacomag$Piraten[is.na(datacomag$Piraten)==TRUE]=0
datacomaggg = data.frame(cbind(datacomag$`CDU/CSU`+datacomag$FDP,datacomag$GRÜNE+datacomag$SPD+datacomag$Piraten,datacomag$`Linke/PDS`+datacomag$AfD))
colnames(datacomaggg) = c("Mitte-rechts Parteien","Mitte-links Parteien","Randparteien")


#2. Makroökonomische Indikatoren laden:

VPIEnergy = read.csv("Consumer Price Index Energy.csv",sep=",")
VPIEnergy = VPIEnergy[-c(1:7),-3]
colnames(VPIEnergy) = c("Datum","VPI Energy")
VPIEnergy$Datum = as.yearmon(VPIEnergy$Datum)
VPIEnergy$Datum = as.Date(VPIEnergy$Datum,format = "%Y-%m")
VPIEnergy$`VPI Energy` = as.numeric(VPIEnergy$`VPI Energy`)
#in absoluten Zahlen --> Wachstumsrate der absoluten Zahl:
VPIEnergydiff = round((diff(VPIEnergy$`VPI Energy`,differences = 1)/VPIEnergy$`VPI Energy`[-375])*100,2)
VPIEnergy = cbind(VPIEnergy,c(VPIEnergydiff,NA))
VPIEnergy = VPIEnergy[97:374,]

VPIFood = read.csv("Consumer Price Index Food.csv",sep=",")
VPIFood = VPIFood[-c(1:7),-3]
colnames(VPIFood) = c("Datum","VPI Food")
VPIFood$`VPI Food` = as.numeric(VPIFood$`VPI Food`)
VPIFood$Datum = as.yearmon(VPIFood$Datum)
VPIFood$Datum = as.Date(VPIFood$Datum,format = "%Y-%m")
#in absoluten Zahlen --> Wachstumsrate der absoluten Zahl:
VPIFooddiff = round((diff(VPIFood$`VPI Food`,differences = 1)/VPIFood$`VPI Food`[-375])*100,2)
VPIFood = cbind(VPIFood,c(VPIFooddiff,NA))
VPIFood = VPIFood[97:374,]

VPI = read.csv("Consumer Price Index.csv",sep=",")
VPI = VPI[-c(1:8),-3]
colnames(VPI) = c("Datum","VPI")
VPI$`VPI` = as.numeric(VPI$`VPI`)
VPI$Datum = as.yearmon(VPI$Datum)
VPI$Datum = as.Date(VPI$Datum,format = "%Y-%m")
VPI=VPI[97:375,]
#in absoluten Zahlen --> Wachstumsrate der absoluten Zahl:
VPIdiff = round((diff(VPI$VPI,differences = 1)/VPI$VPI[-279])*100,2)
VPI= cbind(VPI,c(VPIdiff,NA))
VPI=VPI[1:278,]

PI = read.csv("Produktionsindex.csv",sep=",")
PI = PI[-c(1:8),-3]
colnames(PI) = c("Datum","PI")
PI$`PI` = as.numeric(PI$`PI`)
PI$Datum = as.yearmon(PI$Datum)
PI$Datum = as.Date(PI$Datum,format = "%Y-%m")
PI=PI[97:375,]
#in absoluten Zahlen --> Wachstumsrate der absoluten Zahl:
PIdiff = round((diff(PI$PI,differences = 1)/PI$PI[-279])*100,2)
PI = cbind(PI,c(PIdiff,NA))
PI = PI[1:278,]

EPU = read_excel("Europa Policy Uncertainty.xlsx")
EPU=EPU[145:422,3]

#Alle Daten liegen in monatlicher Frequenz für den ZEITRAUM 01-1999-02-2022 vor.

#Daten der makroökonomischen Indikatoren in Niveaus für den betrachteten Zeitraum abspeichern:

EPUL = EPU$European_News_Index
PIL = PI$PI
VPIL = VPI$VPI 
VPIEL=VPIEnergy$`VPI Energy`
VPIFL =VPIFood$`VPI Food`

############################################################################################
############################################################################################
############################################################################################


##############################GRAPHISCHE DARSTELLUNGEN DER DATEN#################################################


#Graphische Darstellung der Wahlumfragen in Niveaus, einzeln und unterschiedlich aggregiert 


#1. Einzeln über den gesamten Zeitraum --> Abbildung 6 in der Seminararbeit

plot1 = ggplot(datacom, aes(x = Datum)) +
  annotate("rect",xmin = datacom$Datum[1], xmax = datacom$Datum[82], ymin = 60, ymax = 65, fill = "red3", alpha = 0.5)+
  annotate("rect",xmin = datacom$Datum[83], xmax = datacom$Datum[274], ymin = 60, ymax = 65, fill = "gray12", alpha = 0.5)+
  annotate("rect",xmin = datacom$Datum[275], xmax = datacom$Datum[278], ymin = 60, ymax = 65, fill = "red3", alpha = 0.5)+
  geom_line(aes(y = `CDU/CSU`, colour = "black"), size = 0.8) +
  geom_line(aes(y = SPD, colour = "red"), size = 0.8) +
  geom_line(aes(y = GRÜNE, col = "green"), size = 0.8) +
  geom_line(aes(y = FDP, col = "gold1"), size = 0.8) +
  geom_line(aes(y = `Linke/PDS`, col = "darkred"),size = 0.8) +
  geom_line(aes(y = Piraten, col = "tan1"), 
            data = datacom[151:177,], size = 0.8) +
  geom_line(aes(y = AfD, col = "blue"),
            data = datacom[175:278,], size = 0.8) +
  geom_line(aes(y = Sonstige, col = "snow4"), size = 0.8) +
  geom_vline(data = datacom[c(273, 225, 177, 129, 81, 46),],
             aes(xintercept = Datum, color = "purple"), linetype = "dashed", 
             size = 0.5, key_glyph = draw_key_path)+
  scale_color_identity(name = NULL, 
                       labels = c(black = "CDU/CSU", red = "SPD",
                                  green = "Die Grünen", gold1 = "FDP",
                                  darkred = "Die Linke/PDS",
                                  tan1 = "Piraten", blue = "AfD",
                                  snow4 = "Sonstige",
                                  purple = "Bundestagswahlen",red3="SPD-geführte Regierung",gray12="CDU-geführte Regierung"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Forsa-Sonntagsfrage Bundestagswahl in %",
       y     = "Prozent",
       x     = "Jahre",subtitle="Rot=SPD-geführte-,Schwarz=CDU-geführte Regierung")

plot1


ggsave("Abbildung6.png",plot = plot1,scale = 1,width = 14,height = 8,device='png', dpi=500)


#2.Zeit ab 2010 markiert --> nicht in der Seminararbeit
#--> Verlauf kleiner Parteien genauer erkennen

plot1b =plot1 +
        annotate("rect",xmin = datacom$Datum[150], xmax = datacom$Datum[278], ymin = 0, ymax = 30, fill = "blue", alpha = 0.1)

ggsave("Plot1b.png",plot = plot1b,scale = 1,width = 14,height = 7,device='png', dpi=500)


#3.Detaillierter Blick auf markierte Zeit ab 2010 --> nicht in der Seminararbeit
#--> Verlauf kleiner Parteien genauer erkennen

datacomab2010=datacom[133:278,]
plot2 = ggplot(datacomab2010, aes(x = Datum)) +
  geom_line(aes(y = `CDU/CSU`, colour = "black"), size = 0.8) +
  geom_line(aes(y = SPD, colour = "red"), size = 0.8) +
  geom_line(aes(y = GRÜNE, col = "green"), size = 0.8) +
  geom_line(aes(y = FDP, col = "gold1"), size = 0.8) +
  geom_line(aes(y = `Linke/PDS`, col = "darkred"),size = 0.8) +
  geom_line(aes(y = Piraten, col = "tan1"), 
            data = datacom[151:177,], size = 1) +
  geom_line(aes(y = AfD, col = "blue"),
            data = datacom[175:278,], size = 0.8) +
  geom_line(aes(y = Sonstige, col = "snow4"), size = 0.9) +
  geom_vline(data = datacom[c(273, 225, 177),],
             aes(xintercept = Datum, color = "purple"), linetype = "longdash", 
             size = 0.5, key_glyph = draw_key_path)+
  scale_color_identity(name = NULL, 
                       labels = c(black = "CDU/CSU", red = "SPD",
                                  green = "Die Grünen", gold1 = "FDP",
                                  darkred = "Die Linke/PDS",
                                  tan1 = "Piraten", blue = "AfD",
                                  snow4 = "Sonstige",
                                  purple = "Bundestagswahlen"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Forsa-Sonntagsfrage Bundestagswahl in %",
       y     = "Prozent",
       x     = "Jahre")

plot2

ggsave("Plot2.png",plot = plot2,scale = 1,width = 12,height = 5,device='png', dpi=500)


#4. Nach ideologischer Grundausrichtung über den gesamten Zeitraum --> Abbildung 7 in der Seminararbeit


plot3 = ggplot(datacomaggg, aes(x = datacom$Datum)) +
  geom_line(aes(y = datacomaggg$`Mitte-rechts Parteien`, colour = "black"), size = 0.8) +
  geom_line(aes(y = datacomaggg$`Mitte-links Parteien`, colour = "red"), size = 0.8) +
  geom_line(aes(y = datacomaggg$Randparteien, col = "snow4"), size = 0.8) +
  geom_vline(data = datacom[c(273, 225, 177, 129, 81, 46),],
             aes(xintercept = Datum, color = "purple"), linetype = "longdash", 
             size = 0.5, key_glyph = draw_key_path)+
  scale_color_identity(name = NULL, 
                       labels = c(black = "Mitte-rechts Parteien = CDU + FDP", red = "Mitte-links Parteien = SPD + Die Grünen + Piraten",
                                  snow4 = "Randparteien = LINKE/PDS + AfD",
                                  purple = "Bundestagswahlen"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Forsa-Sonntagsfrage Bundestagswahl in %", subtitle = "Aggregiert nach ideologischer Ausrichtung: Mitte-rechts, Mitte-links, Randparteien",
       y     = "Prozent",
       x     = "Jahre")

plot3

ggsave("Abbildung7.png",plot = plot3,scale = 1,width = 14,height = 8,device='png', dpi=500)


#5.Noch gröbere Aggregation als in Abbildung 7, nur in Mitteparteien und Randparteien --> nicht in der Seminararbeit

datenmira = data.frame(cbind(datacomaggg$`Mitte-rechts Parteien`+datacomaggg$`Mitte-links Parteien`,datacomaggg$Randparteien))
colnames(datenmira) = c("Mitte","Rand")

plot4 = ggplot(datenmira, aes(x = datacom$Datum)) +
  geom_line(aes(y = datenmira$Mitte, colour = "black"), size = 0.8) +
  geom_line(aes(y = datenmira$Rand, col = "snow4"), size = 0.8) +
  geom_vline(data = datacom[c(273, 225, 177, 129, 81, 46),],
             aes(xintercept = Datum, color = "purple"), linetype = "longdash", 
             size = 0.5, key_glyph = draw_key_path)+
  scale_color_identity(name = NULL, 
                       labels = c(black = "Mitte = FDP + CDU + SPD+Piraten+Die Grünen",
                                  snow4 = "Randparteien = LINKE/PDS + AfD",
                                  purple = "Bundestagswahlen"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Forsa-Sonntagsfrage Bundestagswahl in %", subtitle = "Aggregiert nach ideologischer Ausrichtung: Mitte, Randparteien",
       y     = "Prozent",
       x     = "Jahre")

plot4

ggsave("Plot4.png",plot = plot4,scale = 1,width = 12,height = 5,device='tiff', dpi=500)


#Graphische Darstellung der makroökonomischen Indikatoren: 


#1. Preisentwicklung --> Abbildung 3 in der Seminararbeit:

VPIge = data.frame(data.frame(VPI$VPI,VPIEnergy$`VPI Energy`,VPIFood$`VPI Food`,VPI$Datum))
colnames(VPIge) =c("VPI","VPIEnergy","VPIFood","Datum")

plot5 = ggplot(VPIge, aes(x = Datum)) +
  geom_line(aes(y = VPI, colour = "black"), size = 0.8) +
  geom_line(aes(y = VPIEnergy, colour = "orange"), size = 0.8) +
  geom_line(aes(y = VPIFood, col = "green"), size = 0.8) +
  geom_hline(aes(yintercept=100,color="red"))+
  scale_color_identity(name = NULL, 
                       labels = c(black = "VPI", orange = "VPI Energie",
                                  green = "VPI Lebensmittel",red="2015=100"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Preisentwicklung", subtitle="VPI; VPI Energie; VPI Lebensmittel; 2015 = 100",
       y     = "Preisniveau",
       x     = "Jahre")

plot5

ggsave("Abbildung3.png",plot = plot5,scale = 1,width = 12,height = 5,device='png', dpi=500)


#2. Produktionsindex --> Abbildung 4 in der Seminararbeit:

PIge = data.frame(data.frame(PI$PI,VPI$Datum))
colnames(PIge) = c("PI","Datum")

plot6 = ggplot(PIge, aes(x = Datum)) +
  geom_line(aes(y = PI, colour = "black"), size = 0.8) +
  geom_hline(aes(yintercept=100,color="red"))+
  scale_color_identity(name = NULL, 
                       labels = c(black = "Produktionsindex",red="2015=100"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Produktionsindex", subtitle="2015 = 100",
       y     = "Produktionsindex",
       x     = "Jahre")


plot6

ggsave("Abbildung4.png",plot = plot6,scale = 1,width = 12,height = 5,device='png', dpi=500)


#3. Unsicherheitsindikator --> Abbildung 5 in der Seminararbeit:

EPUge = data.frame(VPIge$Datum,EPU)
colnames(EPUge) = c("Datum","EPU")

plot7 = ggplot(EPUge, aes(x = Datum)) +
  geom_line(aes(y = EPU, colour = "darkmagenta"), size = 0.8) +
  scale_color_identity(name = NULL, 
                       labels = c(darkmagenta = "Ökonomischer Unsicherheitsindikator"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Ökonomischer Unsicherheitsindikator", subtitle="European News Index",
       y     = "Indexwert",
       x     = "Jahre")

plot7

ggsave("Abbildung5.png",plot = plot7,scale = 1,width = 12,height = 5,device='png', dpi=500)

############################################################################################
############################################################################################
############################################################################################


#############################################Einfache Korrelationen:######################################################

corrmatdata1=cbind(datacom$`CDU/CSU`,datacom$SPD,datacom$GRÜNE,datacom$FDP,datacom$AfD,datacom$`Linke/PDS`,datacom$Sonstige,PIL,VPIL,VPIEL,VPIFL,EPUL)
colnames(corrmatdata1)=c("CDU/CSU","SPD","Die Grünen","FDP","AfD","Die Linke","Sonstige","Produktionsindex","Vebraucherpreisindex","VPI Lebensmittel","VPI Energie","Ökonom. Unsicherheit")
cormat1 = cor(corrmatdata1)

ggcorrplot1 = ggcorrplot(cormat1[7:1,12:8], hc.order = FALSE,lab = F,colors = c("red3","white","royalblue4"),lab_col = "black",type="full",show.diag=FALSE)+
  ggtitle("Korrelationsmatrix zwischen makroökonomischen \nIndikatoren und der politischen Stimmung")

ggsave("Anhang1.png",plot = ggcorrplot1,scale = 1,width = 12,height = 5,device='png', dpi=500)

xtable(cormat1[8:12,1:7]) #Abbildung1

############################################################################################
############################################################################################
############################################################################################


###########################################KERNDICHTESCHÄTZUNG###############################################################################

den1 = data.frame(VPIEL,VPIFL,PIL,EPUL)

#1. Kerndichteschätzung der makroökonomischen Variablen --> Anhang 2 in der Seminararbeit:

dichte1 = ggplot(data=den1) +
  stat_density(aes(x=VPIEL,fill="black"),adjust=1.5, alpha=.3)+
  stat_density(aes(x=VPIFL,fill="cyan3"),adjust=1.5, alpha=.4)+  
  stat_density(aes(x=PIL,fill="red"),adjust=1.5, alpha=.2) +
  stat_density(aes(x=EPUL,fill="orange"),adjust=1.5, alpha=.4) +
  scale_fill_identity(name = NULL, 
                      labels = c(black = "VPI Energie", cyan3 = "VPI Lebensmittel",
                                 red = "Produktionsindex",orange="Unsicherheitsindikator"
                      ),
                      guide = "legend")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Kerndichteschätzer: Makroökonomische Indikatoren",
       y     = "Dichtefunktion",
       x     = "x")

dichte1

ggsave("Anhang2.png",plot = dichte1,scale = 1,width = 12,height = 5,device='png', dpi=500)


#2. Kerndichteschätzung der Umfragewerte --> Anhang 3 in der Seminararbeit:

den2 = data.frame(datacomaggg$`Mitte-rechts Parteien`,datacomaggg$`Mitte-links Parteien`,datacomaggg$Randparteien)
colnames(den2) = c("MR","ML","RP")

dichte2 = ggplot(data=den2) +
  stat_density(aes(x=MR,fill="red"),adjust=1.5, alpha=.4)+  
  stat_density(aes(x=ML,fill="brown"),adjust=1.5, alpha=.4) +
  stat_density(aes(x=RP,fill="blue"),adjust=1.5, alpha=.4) +
  scale_fill_identity(name = NULL, 
                      labels = c(red = "Mitte-rechts Parteien",
                                 brown = "Mitte-links Parteien",blue="Randparteien"
                      ),
                      guide = "legend")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Kerndichteschätzer: Umfragewerte",
       y     = "Dichtefunktion",
       x     = "x")

dichte2

ggsave("Anhang3.png",plot = dichte2,scale = 1,width = 12,height = 5,device='png', dpi=500)

############################################################################################
############################################################################################
############################################################################################


##############################PRÜFEN DER DATEN AUF SAISONALE MUSTER##########################################################################


#1. Schritt: Monatliche Saisondummies erstellen:

M1=as.numeric(ifelse(datacom$Group.1==1, 1, 0))
M2=as.numeric(ifelse(datacom$Group.1==2, 1, 0))
M3=as.numeric(ifelse(datacom$Group.1==3, 1, 0))
M4=as.numeric(ifelse(datacom$Group.1==4, 1, 0))
M5=as.numeric(ifelse(datacom$Group.1==5, 1, 0))
M6=as.numeric(ifelse(datacom$Group.1==6, 1, 0))
M7=as.numeric(ifelse(datacom$Group.1==7, 1, 0))
M8=as.numeric(ifelse(datacom$Group.1==8, 1, 0))
M9=as.numeric(ifelse(datacom$Group.1==9, 1, 0))
M10=as.numeric(ifelse(datacom$Group.1==10, 1, 0))
M11=as.numeric(ifelse(datacom$Group.1==11, 1, 0))
M12=as.numeric(ifelse(datacom$Group.1==12, 1, 0))
M = cbind(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12)


#2. Schritt: Makroökonomische Variablen mittels LR-Test auf monatliche Saisonstruktur testen:

EPUBE=lm(EPUL ~ VPIge$Datum + M -1)
V1 = lm(EPUL ~ VPIge$Datum -1)
T1 = lrtest(EPUBE,V1)

PIBE=lm(PIL ~ VPIge$Datum+M -1)
V2 = lm(PIL ~ VPIge$Datum -1)
T2 = lrtest(PIBE,V2)

VPIBE =lm(VPIEL ~ VPIge$Datum+M -1)
V3 = lm(VPIEL ~ VPIge$Datum -1)
T3 = lrtest(VPIBE,V3)

VPIFOODBE =lm(VPIFL ~ VPIge$Datum+M -1)
V4 =lm(VPIFL ~ VPIge$Datum -1)
T4 = lrtest(VPIFOODBE,V4)


#3. Schritt: Umfragewerte mittels LR-Test auf Saisonstruktur nach Jones, Nielsen und Popiel (2014) testen:
#Anmerkung: Randparteien werden nicht berücksichtigt.

#3.1. Saisondummies erstellen:
dummys = read_xlsx("Dummys.xlsx")
dummys = dummys[-c(279:281),]

#3.2. Umfragewerte mittels LR-Test auf Saisonalität testen:
MRBE = lm(datacomaggg$`Mitte-rechts Parteien`~ datacom$Datum+ dummys$Time+dummys$SPDReg+dummys$CDUReg+dummys$CDUAmt:dummys$CDUReg+dummys$SPDAmt:dummys$SPDReg-1)
V1 = lm(datacomaggg$`Mitte-rechts Parteien`~datacom$Datum)
T10 = lrtest(MRBE,V1) 

MLBE = lm(datacomaggg$`Mitte-links Parteien`~ datacom$Datum+ dummys$Time +dummys$SPDReg+dummys$CDUReg+dummys$SPDAmt:dummys$SPDReg+dummys$CDUAmt:dummys$CDUReg-1)
V2 = lm(datacomaggg$`Mitte-links Parteien`~datacom$Datum)
T11 = lrtest(MLBE,V2) 


#4. Schritt: P-Werte der LR-Tests zusammenfassen --> Anhang 4 in der Seminararbeit

Modell <- c("Ökonomische Unsicherheit","Produktionsindex","VPI Energie","VPI Lebensmittel","Mitte-rechts Parteien","Mitte-links Parteien")
BG1 <- c(round(T1$`Pr(>Chisq)`[2],14),round(T2$`Pr(>Chisq)`[2],90),round(T3$`Pr(>Chisq)`[2],4),round(T4$`Pr(>Chisq)`[2],59),round(T10$`Pr(>Chisq)`[2],13),round(T11$`Pr(>Chisq)`[2],5))
Entscheidung = c("Ablehnen","Ablehnen","Nicht Ablehnen","Ablehnen","Ablehnen","Ablehnen")

dt <- data.frame(cbind(Modell,BG1,Entscheidung))
dt %>%
  kbl(caption = "Übersicht: LR-Test: Makroökonomische Indikatoren und Umfragewerte",col.names = c("Abhängige Variable","P-Wert des LR-Tests","Testentscheidung")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:6, align = "c") %>% footnote( symbol = c("Signifikanzniveau = 5%") )  %>% 
  save_kable(file = "Anhang4.png",zoom=4)


############################################################################################
############################################################################################
############################################################################################


#######################################TESTEN DER ZEITREIHEN AUF STATIONARITÄT###############################################################################

#KPSS-Test und ADF-Test werden verwendet. Erst auf die Zeitreihen in Niveaus, dann je nach Ergebnis in Wachstumsratne.

#1. Schritt: Zeitreihen in Niveaus:
#Anmerkungen: Alle Zeitreihen mit Drift, Preisindizes aber mit Trend, da graphische Darstellung 
#einen linearen Trend nahelegt.

summary(ur.df(datacomaggg$`Mitte-rechts Parteien`,  lags = 15, type = "drift"))
summary(ur.kpss(datacomaggg$`Mitte-rechts Parteien`, type = "mu", lags = "long")) 

summary(ur.df(datacomaggg$`Mitte-links Parteien`,  lags = 15, type = "drift"))
summary(ur.kpss(datacomaggg$`Mitte-links Parteien`, type = "mu", lags = "long")) 

summary(ur.df(datacomaggg$Randparteien,  lags = 15, type = "drift"))
summary(ur.kpss(datacomaggg$Randparteien, type = "mu", lags = "long")) 

summary(ur.df(PIL, lags = 15, type = "drift"))
summary(ur.kpss(PIL, type = "mu", lags = "long")) 

summary(ur.df(VPIEL,  lags = 15, type = "trend"))
summary(ur.kpss(VPIEL, type = "mu", lags = "long")) 

summary(ur.df(VPIFL,  lags = 15, type = "trend"))
summary(ur.kpss(VPIFL, type = "mu", lags = "long")) 

summary(ur.df(EPUL,  lags = 15, type = "drift"))
summary(ur.kpss(EPUL, type = "mu", lags = "long")) 

#Ergebnis: Alle Zeitreihen sind nicht stationär --> Wachstumsraten bilden und Tests wiederholen.


#2. Schritt: Zeitreihen in Wachstumsraten:

summary(ur.df(diff(datacomaggg$`Mitte-rechts Parteien`)/datacomaggg$`Mitte-rechts Parteien`[-278],  lags = 15, type = "drift"))
summary(ur.kpss(diff(datacomaggg$`Mitte-rechts Parteien`)/datacomaggg$`Mitte-rechts Parteien`[-278], type = "mu", lags = "long")) 

summary(ur.df(diff(datacomaggg$`Mitte-links Parteien`)/datacomaggg$`Mitte-links Parteien`[-278],  lags = 15, type = "drift"))
summary(ur.kpss(diff(datacomaggg$`Mitte-links Parteien`)/datacomaggg$`Mitte-links Parteien`[-278], type = "mu", lags = "long")) 

summary(ur.df(diff(datacomaggg$Randparteien)/datacomaggg$Randparteien[-278],  lags = 15, type = "drift"))
summary(ur.kpss(diff(datacomaggg$Randparteien)/datacomaggg$Randparteien[-278], type = "mu", lags = "long")) 

summary(ur.df(diff(VPIEL)/VPIEL[-278],  lags = 15, type = "trend"))
summary(ur.kpss(diff(VPIEL)/VPIEL[-278], type = "mu", lags = "long")) 

summary(ur.df(diff(VPIFL)/VPIFL[-278],  lags = 15, type = "trend"))
summary(ur.kpss(diff(VPIFL)/VPIFL[-278], type = "mu", lags = "long")) 

summary(ur.df(diff(PIL)/PIL[-278],  lags = 15, type = "drift"))
summary(ur.kpss(diff(PIL)/PIL[-278], type = "mu", lags = "long")) 

summary(ur.df(diff(EPUL)/EPUL[-278],  lags = 15, type = "drift"))
summary(ur.kpss(diff(EPUL)/EPUL[-278], type = "mu", lags = "long")) 

#ADF und KPSS Test --> alle Variablen in Niveaus sind nicht stationär, in Differenzen aber stationär --> Zeitreihen sind I(1)


#3. Schritt Ergebnisse darstellen --> Anhang 5 in der Seminararbeit:

Modell <- c("Mitte-rechts Parteien","Mitte-links Parteien","Randparteien","Produktionsindex","VPI Energie","VPI Lebensmittel","Ökonomische Unsicherheit")
BG1 <- c("2.13","2.87","1.08","2.17","2.47","2.11","1.56")
BG2 <- c("0.91","0.57","1.46","1.39","1.57","1.81","1.41")
Entscheidung = c("I(1)","I(1)","I(1)","I(1)","I(1)","I(1)","I(1)")

dt <- data.frame(cbind(Modell,BG2,BG1,Entscheidung))
dt %>%
  kbl(caption = "Übersicht des Betrags der Teststatistiken des ADF- und KPSS-Tests",col.names = c("Variable","ADF-Test","KPSS-Test","Integrationsgrad")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:7, align = "c") %>% footnote( symbol = c("Testentscheidung zum 5%-Signifikanzniveau ; Tests berücksichtigt 15 Lags") )  %>% 
  save_kable(file = "Anhang5.png",zoom=4)


############################################################################################
############################################################################################
############################################################################################

#######################################TESTEN DER ZEITREIHEN AUF KOINTEGRATION###############################################################################

#Vorgehen nach Johansen(1988) --> Schätzen eines VEC-Modells

#1. Schritt: Y-Matrix:
vecmdata = cbind(VPIEL,VPIFL,PIL,EPU$European_News_Index,datacomaggg$`Mitte-rechts Parteien`,datacomaggg$`Mitte-links Parteien`,datacomaggg$Randparteien) 
colnames(vecmdata) = c("VPI Energie","VPI Lebensmittel","Produktionsindex","Ökonomische Unsicherheit","Mitte-rechts Parteien","Mitte-links Parteien","Randparteien")


#2. Schritt: Optimale Lag-Länge der VAR-Darstellung des Modells
VARselect(vecmdata,lag.max = 13,season=12) #Informationskriterien ergeben eine optimale Lag-Länge von einem Lag
stargazer(VARselect(vecmdata,lag.max = 13,season=12),title = "Ergebnis der optimalen Lag-Wahl") #Darstellung der Ergebnisse --> Anhang 6 in der Seminararbeit
#Anmerkung: Abweichen --> 2 Lags verwenden in VAR-Darstellung, da sonst Fehlerterme nicht Annahmen erfüllen.

#3. Schritt: VEC-Darstellung des VAR-Modells schätzen --> Aus 2 VAR-Lags wird 1 VEC-Lag
VECM1 = VECM(vecmdata,lag = 1,exogen=M[,1:11],estim = "ML",include = "const")

#4. Schritt: Rang testen --> Eigenvalue- und Trace-Test, um Anzahl der Kointegrationsbeziehungen festzustellen
summ = summary(rank.test(VECM1,type = c("eigen","trace"))) #r=2

#Ergebnis: Rang = 2 --> 2 Kointegrationsbeziehungen

#5.Schritt: Ergebnisse darstellen --> Anhang 7 der Seminararbeit
rang= c(summ$r)
trace = c(summ$trace_pval)
eigen = c(summ$eigen_pval)
dt <- data.frame(cbind(rang,trace,eigen))
dt %>%
  kbl(caption = "Ergebnisse des Kointegrationstest",col.names = c("Rang","P-Wert Trace","P-Wert Eigenvalue")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:7, align = "c") %>% 
  save_kable(file = "Anhang7.png",zoom=4)

############################################################################################
############################################################################################
############################################################################################

#######################################SCHÄTZEN DES MODELLS###############################################################################

#VEC-Modell ist voll spezifiziert --> Y-Vektor, Anzahl Kointegrationsbeziehungen + Lag-Länge ist festgelegt, aber 
#Langfristiges stabiles Gleichgewicht kann nur durch weitere Annahmen eindeutig identifiziert werden --> mit VAR-Darstellung weiterarbeiten

VECM1 = VECM(vecmdata,lag = 1,exogen = M[,1:11],estim = "ML",r=2,include = "const")
summary(VECM1)

#--> Schätzen des VAR-Modells:

Var0 = VAR((vecmdata),p=1,type="const",season=12)
Var1 = VAR((vecmdata),p=2,type="const",season=12)

############################################################################################
############################################################################################
############################################################################################

#######################################RESIDUENANALYSE###############################################################################

#1. Auf Autokorrelation : H0 = keine Autokorrelation bis Lag...
serial.test(Var0, lags.bg = 10, type = "BG")
serial.test(Var1, lags.bg = 10, type = "BG")
#kann nur nicht verworfen werden, wenn 2 Lags berücksichtigt werden. 


#2.Test auf Normalverteilung: H0 = Normalverteilung liegt vor 
normality.test(Var0)
normality.test(Var1)
#in beiden Modellen wird H0 verworfen --> keine Normalverteilung --> Zentraler Grenzwertsatz


#3. Test auf Homoskedastie: H0 = Homoskedastie
Homosked0 <- arch.test(Var0,lags.multi = 10)
Homosked0
Homosked1 <- arch.test(Var1,lags.multi = 10)
Homosked1
#Für beide Modellspezifikationen nicht verworfen --> Homoskedastie 

#4. Ergebnisse darstellen --> Anhang 8 in der Seminararbeit
rang= c("0.07","<0.01","0.99")
trace = c("<0.01","<0.01","0.99")
eigen = c("Breusch-Godfrey LM-Test","Jarque-Bera-Test","ARCH-Test")
dt <- data.frame(cbind(eigen,trace,rang))
dt %>%
  kbl(caption = "Analyse der Residuen einer VAR(1) und VAR(2) Schätzung",col.names = c("Test","P-Wert VAR(1)","P- Wert VAR(2)")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:3, align = "c") %>% 
  save_kable(file = "Anhang8.png",zoom=4)

#Fazit: VAR-Modell mit zwei Lags erfüllt alle relevanten Annahmen.


############################################################################################
############################################################################################
############################################################################################

#######################################INTERPRETATION DES VAR(2)-MODELLS###############################################################################

#1. Impuls-Antwort-Funktionen

#1.1. Ist eine Orthogonalisierung notwendig oder gibt es zeitgleiche Effekte? --> LM-Test nach Schröder (2012)
corr1 <- (summary(Var1)$corres) 
lmtest <- (corr1[upper.tri(corr1)])^2
sum(lmtest)*278
#Fazit: Residuen sind nicht unabhängig --> es bestehen zeitgleiche Effekte --> Orthogonalisierung notwendig
#Notwendigkeit der Orthogonalisierung wurde bereits beim Festlegen der Reihenfolge der Daten berücksichtigt

#1.2. Ergebnisse darstellen --> Anhang 9 in der Seminararbeit
stargazer(corr1,type = "html",out="Anhang9.html",title = "Korrelationsmatrix der Residuen des VAR(2)-Modells")

#1.3. Berechnen und plotten der Impuls-Antwort-Funktionen:

#1.3.1. Response of Mitte-rechts Parteien --> Anhang 10 in der Seminararbeit

irf1a<- irf(Var1,seed=23 ,n.ahead = 20, ortho = T,impulse = "Produktionsindex",response = "Mitte.rechts.Parteien")
irf1b<- irf(Var1,seed=26 ,n.ahead = 20, ortho = T,impulse="VPI.Energie",response = "Mitte.rechts.Parteien")
irf1c <- irf(Var1,seed=23,n.ahead = 20, ortho = T,impulse="VPI.Lebensmittel",response = "Mitte.rechts.Parteien")
irf1d <- irf(Var1,seed=23,n.ahead = 20, ortho = T,impulse="Ökonomische.Unsicherheit",response = "Mitte.rechts.Parteien")

png("irf1a.png",width = 1500,height = 1000,res=200)
plot(irf1a,main = "Auswirkung eines Schocks des Produktionsindexes",xlab ="Perioden",ylab="Umfragewerte Mitte-rechts Parteien")
dev.off()

png("irf1b.png",width = 1500,height = 1000,res=200)
plot(irf1b,main = "Auswirkung eines Schocks des VPI für Energie",xlab ="Perioden",ylab="Umfragewerte Mitte-rechts Parteien")
dev.off()

png("irf1c.png",width = 1500,height = 1000,res=200)
plot(irf1c,main = "Auswirkung eines Schocks des VPI für Lebensmittel",xlab ="Perioden",ylab="Umfragewerte Mitte-rechts Parteien")
dev.off()

png("irf1d.png",width = 1500,height = 1000,res=200)
plot(irf1d,main = "Auswirkung eines Schocks der ökonomischen Unsicherheit",xlab ="Perioden",ylab="Umfragewerte Mitte-rechts Parteien")
dev.off()

#1.3.2. Response of Mitte-links Parteien --> Anhang 11 in der Seminararbeit

irf2a<- irf(Var1,seed=23, n.ahead = 20, ortho = T,impulse = "Produktionsindex",response = "Mitte.links.Parteien")
irf2b<- irf(Var1,seed=45, n.ahead = 20, ortho = T,impulse="VPI.Energie",response = "Mitte.links.Parteien")
irf2c <- irf(Var1, n.ahead = 20, ortho = T,impulse="VPI.Lebensmittel",response = "Mitte.links.Parteien")
irf2d <- irf(Var1,seed=300, n.ahead = 20, ortho = T,impulse="Ökonomische.Unsicherheit",response = "Mitte.links.Parteien")

png("irf2a.png",width = 1500,height = 1000,res=200)
plot(irf2a,main = "Auswirkung eines Schocks des Produktionsindexes",xlab ="Perioden",ylab="Umfragewerte Mitte-links Parteien")
dev.off()

png("irf2b.png",width = 1500,height = 1000,res=200)
plot(irf2b,main = "Auswirkung eines Schocks des VPI für Energie",xlab ="Perioden",ylab="Umfragewerte Mitte-links Parteien")
dev.off()

png("irf2c.png",width = 1500,height = 1000,res=200)
plot(irf2c,main = "Auswirkung eines Schocks des VPI für Lebensmittel",xlab ="Perioden",ylab="Umfragewerte Mitte-links Parteien")
dev.off()

png("irf2d.png",width = 1500,height = 1000,res=200)
plot(irf2d,main = "Auswirkung eines Schocks der ökonomischen Unsicherheit",xlab ="Perioden",ylab="Umfragewerte Mitte-links Parteien")
dev.off()

#1.3.3. Response of Randparteien --> Anhang 12 in der Seminararbeit

irf3a<- irf(Var1,seed=30, n.ahead = 20, ortho = T,impulse = "Produktionsindex",response = "Randparteien")
irf3b<- irf(Var1,seed=30, n.ahead = 20, ortho = T,impulse="VPI.Energie",response = "Randparteien")
irf3c <- irf(Var1,seed=48, n.ahead = 20, ortho = T,impulse="VPI.Lebensmittel",response = "Randparteien")
irf3d <- irf(Var1,seed=96 ,n.ahead = 20, ortho = T,impulse="Ökonomische.Unsicherheit",response = "Randparteien")

png("irf3a.png",width = 1500,height = 1000,res=200)
plot(irf3a,main = "Auswirkung eines Schocks des Produktionsindexes",xlab ="Perioden",ylab="Umfragewerte Randparteien")
dev.off()

png("irf3b.png",width = 1500,height = 1000,res=200)
plot(irf3b,main = "Auswirkung eines Schocks des VPI für Energie",xlab ="Perioden",ylab="Umfragewerte Randparteien")
dev.off()

png("irf3c.png",width = 1500,height = 1000,res=200)
plot(irf3c,main = "Auswirkung eines Schocks des VPI für Lebensmittel",xlab ="Perioden",ylab="Umfragewerte Randparteien")
dev.off()

png("irf3d.png",width = 1500,height = 1000,res=200)
plot(irf3d,main = "Auswirkung eines Schocks der ökonomischen Unsicherheit",xlab ="Perioden",ylab="Umfragewerte Randparteien")
dev.off()

#1.3.4. Response of makroökonomische Variablen nach einem Schock der Mitte-rechts Parteien --> Anhang 14 in der Seminararbeit

irf11a<- irf(Var1,seed=23 ,n.ahead = 20, ortho = T,response = "Produktionsindex",impulse = "Mitte.rechts.Parteien")
irf11b<- irf(Var1,seed=26 ,n.ahead = 20, ortho = T,response="VPI.Energie",impulse = "Mitte.rechts.Parteien")
irf11c <- irf(Var1,seed=23,n.ahead = 20, ortho = T,response="VPI.Lebensmittel",impulse = "Mitte.rechts.Parteien")
irf11d <- irf(Var1,seed=23,n.ahead = 20, ortho = T,response="Ökonomische.Unsicherheit",impulse = "Mitte.rechts.Parteien")

png("irf11a.png",width = 1600,height = 1000,res=200)
plot(irf11a,main = "Auswirkung eines Schocks der Umfragewerte der Mitte-rechts Parteien",xlab ="Perioden",ylab="Produktionsindex")
dev.off()

png("irf11b.png",width = 1600,height = 1000,res=200)
plot(irf11b,main = "Auswirkung eines Schocks der Umfragewerte der Mitte-rechts Parteien",xlab ="Perioden",ylab="VPI Energie")
dev.off()

png("irf11c.png",width = 1600,height = 1000,res=200)
plot(irf11c,main = "Auswirkung eines Schocks der Umfragewerte der Mitte-rechts Parteien",xlab ="Perioden",ylab="VPI Lebensmittel")
dev.off()

png("irf11d.png",width = 1600,height = 1000,res=200)
plot(irf11d,main = "Auswirkung eines Schocks der Umfragewerte der Mitte-rechts Parteien",xlab ="Perioden",ylab="Ökonomische Unsicherheit")
dev.off()

#1.3.5. Response of makroökonomische Variablen nach einem Schock der Mitte-links Parteien --> Anhang 15 in der Seminararbeit

irf22a<- irf(Var1,seed=23, n.ahead = 20, ortho = T,response = "Produktionsindex",impulse = "Mitte.links.Parteien")
irf22b<- irf(Var1,seed=45, n.ahead = 20, ortho = T,response="VPI.Energie",impulse = "Mitte.links.Parteien")
irf22c <- irf(Var1, n.ahead = 20, ortho = T,response="VPI.Lebensmittel",impulse = "Mitte.links.Parteien")
irf22d <- irf(Var1,seed=300, n.ahead = 20, ortho = T,response="Ökonomische.Unsicherheit",impulse = "Mitte.links.Parteien")

png("irf22a.png",width = 1600,height = 1000,res=200)
plot(irf22a,main = "Auswirkung eines Schocks der Umfragewerte der Mitte-links Parteien",xlab ="Perioden",ylab="Produktionsindex")
dev.off()

png("irf22b.png",width = 1600,height = 1000,res=200)
plot(irf22b,main = "Auswirkung eines Schocks der Umfragewerte der Mitte-links Parteien",xlab ="Perioden",ylab="VPI Energie")
dev.off()

png("irf22c.png",width = 1600,height = 1000,res=200)
plot(irf22c,main = "Auswirkung eines Schocks der Umfragewerte der Mitte-links Parteien",xlab ="Perioden",ylab="VPI Lebensmittel")
dev.off()

png("irf22d.png",width = 1600,height = 1000,res=200)
plot(irf22d,main = "Auswirkung eines Schocks der Umfragewerte der Mitte-links Parteien",xlab ="Perioden",ylab="Ökonomische Unsicherheit")
dev.off()

#1.3.6. Response of makroökonomische Variablen nach einem Schock der Randparteien --> Anhang 16 in der Seminararbeit

irf33a<- irf(Var1,seed=30, n.ahead = 20, ortho = T,response = "Produktionsindex",impulse = "Randparteien")
irf33b<- irf(Var1,seed=30, n.ahead = 20, ortho = T,response="VPI.Energie",impulse = "Randparteien")
irf33c <- irf(Var1,seed=48, n.ahead = 20, ortho = T,response="VPI.Lebensmittel",impulse = "Randparteien")
irf33d <- irf(Var1,seed=96 ,n.ahead = 20, ortho = T,response="Ökonomische.Unsicherheit",impulse = "Randparteien")

png("irf33a.png",width = 1600,height = 1000,res=200)
plot(irf33a,main = "Auswirkung eines Schocks der Umfragewerte der Randparteien",xlab ="Perioden",ylab="Produktionsindex")
dev.off()

png("irf33b.png",width = 1600,height = 1000,res=200)
plot(irf33b,main = "Auswirkung eines Schocks der Umfragewerte der Randparteien",xlab ="Perioden",ylab="VPI Energie")
dev.off()

png("irf33c.png",width = 1600,height = 1000,res=200)
plot(irf33c,main = "Auswirkung eines Schocks der Umfragewerte der Randparteien",xlab ="Perioden",ylab="VPI Lebensmittel")
dev.off()

png("irf33d.png",width = 1600,height = 1000,res=200)
plot(irf33d,main = "Auswirkung eines Schocks der Umfragewerte der Randparteien",xlab ="Perioden",ylab="Ökonomische Unsicherheit")
dev.off()


#2. GRANGER-Kausalitätstest

set.seed(2000)

#2.1. Makroökonomische Variablen granger-kausal für Umfragewerte? --> Anhang 17 in der Seminarabeit
granger_causality(Var1,var.y = c("VPI.Energie","VPI.Lebensmittel","Produktionsindex","Ökonomische.Unsicherheit"),var.x = c("Mitte.rechts.Parteien","Mitte.links.Parteien","Randparteien"))

#2.2. Umfragewerte granger-kausal für makroökonomische Variablen? --> Anhang 18 in der Seminarabeit
granger_causality(Var1,var.x = c("VPI.Energie","VPI.Lebensmittel","Produktionsindex","Ökonomische.Unsicherheit"),var.y = c("Mitte.rechts.Parteien","Mitte.links.Parteien","Randparteien"))


############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################