install.packages("yhat")
install.packages("heplots")
install.packages("lme4")
install.packages("lmerTest")
install.packages ("plotly")
install.packages('zip') 
install.packages('directlabels')

## Tidyverse packages
library(tidyverse)
library(dplyr)
library(kableExtra)
library(readxl)
library(flextable)


## Data analysis
library ("NbClust")
library("psych")
library("car")
library("sampling")
library("splitstackshape")
library("sjstats")
library("yhat")
library("heplots")
library(effectsize)

## Multilevel analyses
library(lme4)
library(lmerTest)

## Data visualisation
library(corrplot)
library(ggcorrplot)
library ("ggplot2")
library(plotly)
library (directlabels)


library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(stopwords)
library(SnowballC)
library(DT)

# Data opladen

Data0<- read.csv("Eerste semester anoniem2.csv", header = T, sep = ";", dec=".", stringsAsFactors = FALSE)
Data <- Data0%>%
  filter(Departement %in% c("GW", "MC", "OT", "WT"))
View(Data)


# Data cleanen


# 1. Spreiding variabelen
## 1.1 Achtergrondvariabelen

### Departement
Spreiding_Departement <- Data0%>%
  count(`Departement`)
Spreiding_Departement$Departement <- as.factor(Spreiding_Departement$Departement)

ggplot(data=Spreiding_Departement, aes(x=Departement, y=n)) +
  geom_bar( stat="identity", fill = "#b8040c", color = "black")+
  labs(title = "Spreiding van deelname over Departementen", x = "Departement", y = "Aantal studenten")+
  geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  theme_bw()   


### Opleiding met meer dan 20 respondenten
Spreiding_Departement_2 <- Spreiding_Departement %>%
  filter (n>=20)


ggplot(data=Spreiding_Departement_2, aes(x=Departement, y=n, fill = Departement)) +
  geom_bar( stat="identity")+
  labs(title = "Departementen waarvan meer dan 20 studenten deelnamen aan AP-vaardig", x = "Departement", y = "Aantal studenten")+
  geom_text(aes(label=n), vjust=1, size=4, color="White")+
  scale_fill_manual(values=c("#50A0AD", "#DE9700", "#950174", "#62985B"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position="none")


### Opleiding
Spreiding_Opleiding <- Data0 %>%
  group_by(Opleidingstype, Departement)%>%
  count(`Opleiding`)
Spreiding_Opleiding$Opleiding <- as.factor(Spreiding_Opleiding$Opleiding)

ggplot(data=Spreiding_Opleiding, aes(x=Opleiding, y=n)) +
  geom_bar( stat="identity", fill = "#b8040c", color = "black")+
  labs(title = "Spreiding van deelname over Opleidingen", x = "Opleiding", y = "Aantal studenten")+
  geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

### Opleidingen met meer dan 20 respondenten
Spreiding_Opleiding_2 <- Spreiding_Opleiding %>%
  filter (n>=20)%>%
  group_by(Opleidingstype, Departement)

ggplot(data=Spreiding_Opleiding_M, aes(x=Opleiding, y=n, fill = Departement)) +
  geom_bar( stat="identity")+
  labs(title = "Opleidingen met meer dan 20 deelnemers geordend per type en departement", x = "Opleiding", y = "Aantal studenten")+
  geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  scale_fill_manual(values=c("#50A0AD", "#DE9700", "#950174", "#62985B"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#### MC

Spreiding_Opleiding_MC <- filter (Spreiding_Opleiding_2, Departement == "MC")

ggplot(data=Spreiding_Opleiding_MC, aes(x=Opleiding, y=n)) +
  geom_bar( stat="identity", fill="#DE9700")+
  labs(title = "Opleidingen uit MC met meer dan 20 deelnemers", x = "Opleiding", y = "Aantal studenten")+
  coord_flip()+
  geom_text(aes(label=n), hjust =2, size=3.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

Spreiding_Opleiding_MC <- filter (Spreiding_Opleiding_2, Departement == "MC")


#### GW

Spreiding_Opleiding_GW <- filter (Spreiding_Opleiding_2, Departement == "GW")

ggplot(data=Spreiding_Opleiding_GW, aes(x=Opleiding, y=n)) +
  geom_bar( stat="identity", fill="#50A0AD")+
  labs(title = "Opleidingen uit GW met meer dan 20 deelnemers", x = "Opleiding", y = "Aantal studenten")+
  coord_flip()+
  geom_text(aes(label=n), hjust =2, size=3.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#### OT
Spreiding_Opleiding_OT <- filter (Spreiding_Opleiding_2, Departement == "OT")

ggplot(data=Spreiding_Opleiding_OT, aes(x=Opleiding, y=n)) +
  geom_bar( stat="identity", fill="#950174")+
  labs(title = "Opleidingen uit OT met meer dan 20 deelnemers", x = "Opleiding", y = "Aantal studenten")+
  coord_flip()+
  geom_text(aes(label=n), hjust =2, size=3.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#### WT

Spreiding_Opleiding_WT <- filter (Spreiding_Opleiding_2, Departement == "WT")

ggplot(data=Spreiding_Opleiding_WT, aes(x=Opleiding, y=n)) +
  geom_bar( stat="identity", fill="#62985B")+
  labs(title = "Opleidingen uit WT met meer dan 20 deelnemers", x = "Opleiding", y = "Aantal studenten")+
  coord_flip()+
  geom_text(aes(label=n), hjust =2, size=3.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

Data_clean_W <- Data %>%
  select(Woordenschat, GPA)

Data_clean_2 <- Data_clean_W[complete.cases(Data_clean_W), ]
cor(Data_clean_2$Woordenschat, Data_clean_2$GPA)

## 1.2 Studiepunten & GPA

### Studiepunten
Spreiding_Studiepunten <- Data %>%
  count(`Aantal_studiepunten`)
Spreiding_Studiepunten$Aantal_studiepunten <- as.factor(Spreiding_Studiepunten$Aantal_studiepunten)


#### spreiding studiepunten
ggplot(data=Spreiding_Studiepunten, aes(x= Aantal_studiepunten, y=n)) +
  geom_bar( stat="identity")+
  labs(title = "Spreiding van aantal opgenomen studiepunten", x = "Studiepunten", y = "Aantal studenten")+
  geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


### GPA
round(Data$GPA,0)
Spreiding_GPA <- Data%>%
  count(`GPA`)
Spreiding_GPA$GPA <- as.factor(Spreiding_GPA$GPA)

Spreiding_GPA_1 <- Data%>%
  filter(Departement %in% c("GW", "MC", "OT", "WT"))%>%
  group_by(Departement)%>%
  count(`GPA`)
Spreiding_GPA_1$GPA <- as.factor(Spreiding_GPA_1$GPA)

Spreiding_GPA_2 <- Data%>%
  filter(Departement %in% c("GW", "MC", "OT", "WT"))%>%
  group_by(Opleidingstype, Departement)%>%
  count(`GPA`)
Spreiding_GPA_2$GPA <- as.factor(Spreiding_GPA_2$GPA)

#### spreiding GPA
ggplot(data=Spreiding_GPA, aes(x= GPA, y=n)) +
  geom_bar( stat="identity")+
  labs(title = "Spreiding van GPA", x = "GPA", y = "Aantal studenten")+
  geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#### Spreiding GPA per departement
ggplot() +
  geom_bar( data=Spreiding_GPA_1, aes(x= GPA, y=n, fill = Departement), stat="identity", position ="dodge")+
  labs( x = "GPA", y = "Aantal studenten")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  scale_fill_manual(values=c("#50A0AD", "#DE9700", "#950174", "#62985B"))+
  scale_y_continuous(breaks=seq(0,240,20))


#### Proportionele spreiding per departement
ggplot(data=Spreiding_GPA_2) +
  geom_bar(mapping=aes(x=GPA, fill=Departement), position ="fill")+
  labs(title = " Propertionele spreiding van GPA per departement", x = "GPA", y = "Aantal studenten")+
  scale_fill_manual(values=c("#50A0AD", "#DE9700", "#950174", "#62985B"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#### significantie spreiding
Data_2 <- Data%>%
  filter(Departement %in% c("GW", "MC", "OT", "WT"))%>%
  select (Departement, GPA)
Data_3 <- Data_2[complete.cases(Data_2), ]

Anova_GPA <- lm(GPA  ~ Departement, data = Data_3)
summary(Anova_GPA)
etasq(Anova_GPA, anova = TRUE, partial = TRUE)
effect.size(Anova_GPA)


# 2. Beschrijvende statistieken

## 2.1 Rekenen


### Cijfergegevens
Gemiddelde_Rekenen<- Data%>%
  summarise(Rekenen= round(mean(Rekenen, na.rm = TRUE),2),
            Getallen= round(mean(Getallen, na.rm = TRUE),2),
            Vergelijkingen1= round(mean(Vergelijkingen1, na.rm = TRUE),2),
            Vergelijkingen2= round(mean(Vergelijkingen2, na.rm = TRUE),2),
            Verhoudingen= round(mean(Verhoudingen, na.rm = TRUE),2),
            Goniometrie= round(mean(Goniometrie, na.rm = TRUE),2),
            Totaal_rekenvaardigheid=round(mean(Totaal_rekenvaardigheid, na.rm = TRUE),2))
            
Mediaan_Rekenen<- Data%>%
  summarise(Rekenen= round(median(Rekenen, na.rm = TRUE),2),
            Getallen= round(median(Getallen, na.rm = TRUE),2),
            Vergelijkingen1= round(median(Vergelijkingen1, na.rm = TRUE),2),
            Vergelijkingen2= round(median(Vergelijkingen2, na.rm = TRUE),2),
            Verhoudingen= round(median(Verhoudingen, na.rm = TRUE),2),
            Goniometrie= round(median(Goniometrie, na.rm = TRUE),2),
            Totaal_rekenvaardigheid=round(median(Totaal_rekenvaardigheid, na.rm = TRUE),2))

Standaardeviatie_Rekenen<- Data%>%
  summarise(Rekenen= round(sd(Rekenen, na.rm = TRUE),2),
            Getallen= round(sd(Getallen, na.rm = TRUE),2),
            Vergelijkingen1= round(sd(Vergelijkingen1, na.rm = TRUE),2),
            Vergelijkingen2= round(sd(Vergelijkingen2, na.rm = TRUE),2),
            Verhoudingen= round(sd(Verhoudingen, na.rm = TRUE),2),
            Goniometrie= round(sd(Goniometrie, na.rm = TRUE),2),
            Totaal_rekenvaardigheid=round(sd(Totaal_rekenvaardigheid, na.rm = TRUE),2))

Mad_Rekenen<- Data%>%
  summarise(Rekenen= round(mad(Rekenen, na.rm = TRUE),2),
            Getallen= round(mad(Getallen, na.rm = TRUE),2),
            Vergelijkingen1= round(mad(Vergelijkingen1, na.rm = TRUE),2),
            Vergelijkingen2= round(mad(Vergelijkingen2, na.rm = TRUE),2),
            Verhoudingen= round(mad(Verhoudingen, na.rm = TRUE),2),
            Goniometrie= round(mad(Goniometrie, na.rm = TRUE),2),
            Totaal_rekenvaardigheid=round(mad(Totaal_rekenvaardigheid, na.rm = TRUE),2))


Volledige_antwoorden_rekenen <- (sum(!is.na(Data$Totaal_rekenvaardigheid)))


Maten <-  c("Gemiddelde", "Mediaan", "Standaardeviatie", "Absolute Mediaan Afwijking")
descriptives_Rekenen <- bind_rows(Gemiddelde_Rekenen, Mediaan_Rekenen, Standaardeviatie_Rekenen, Mad_Rekenen)
descriptives_Rekenen <- bind_cols(Maten, descriptives_Rekenen)
rename (descriptives_Rekenen,  Maten = ...1 )
datatable(descriptives_Rekenen)



### Boxplot
Rekenen_long <- Data%>%
  select(ID, Departement, Rekenen:Totaal_rekenvaardigheid)%>%
  pivot_longer(c(Rekenen:Totaal_rekenvaardigheid), names_to = "Onderdeel", values_to = "Score")

ggplot (data = Rekenen_long, mapping = aes(x = Onderdeel, y = Score )) +
  geom_boxplot(show.legend = F, fill = "#F69B00") +
  stat_summary(fun=mean, geom="point", color="black", size=2, show.legend = F)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  labs(x = "Schalen rekenvaardigheid in AP-Vaardig", y = "Score (0-100)")+
  scale_x_discrete(limits=c("Rekenen", "Getallen", "Verhoudingen", "Vergelijkingen1", "Vergelijkingen2", "Goniometrie", "Totaal_rekenvaardigheid"))

Deductief_redeneren= round(mean(Deductief_redeneren, na.rm = TRUE),2)

## 2.2 Taalvaardigheid

### Cijfergegevens
Gemiddelde_Taal<- Data%>%
  summarise(Woordenschat= round(mean(Woordenschat, na.rm = TRUE),2),
            Lidwoorden= round(mean(Lidwoorden, na.rm = TRUE),2),
            Werkwoordspelling= round(mean(Werkwoordspelling, na.rm = TRUE),2),
            Algemene_spelling= round(mean(Algemene_spelling, na.rm = TRUE),2),
            Woordvorming= round(mean(Woordvorming, na.rm = TRUE),2),
            Tekstanalyse=round(mean(Tekstanalyse, na.rm = TRUE),2),
            Correcte_taal=round(mean(Correcte_taal, na.rm = TRUE),2),
            Email=round(mean(Email, na.rm = TRUE),2),
            Totaal_taalvaardigheid=round(mean(Totaal_taalvaardigheid, na.rm = TRUE),2)
            )

Mediaan_Taal<- Data%>%
  summarise(Woordenschat= round(median(Woordenschat, na.rm = TRUE),2),
            Lidwoorden= round(mean(Lidwoorden, na.rm = TRUE),2),
            Werkwoordspelling= round(median(Werkwoordspelling, na.rm = TRUE),2),
            Algemene_spelling= round(median(Algemene_spelling, na.rm = TRUE),2),
            Woordvorming= round(median(Woordvorming, na.rm = TRUE),2),
            Tekstanalyse=round(median(Tekstanalyse, na.rm = TRUE),2),
            Correcte_taal=round(median(Correcte_taal, na.rm = TRUE),2),
            Email=round(median(Email, na.rm = TRUE),2),
            Totaal_taalvaardigheid=round(median(Totaal_taalvaardigheid, na.rm = TRUE),2)
  )

Standaarddeviatie_Taal<- Data%>%
  summarise(Woordenschat= round(sd(Woordenschat, na.rm = TRUE),2),
            Lidwoorden= round(sd(Lidwoorden, na.rm = TRUE),2),
            Werkwoordspelling= round(sd(Werkwoordspelling, na.rm = TRUE),2),
            Algemene_spelling= round(sd(Algemene_spelling, na.rm = TRUE),2),
            Woordvorming= round(sd(Woordvorming, na.rm = TRUE),2),
            Tekstanalyse=round(sd(Tekstanalyse, na.rm = TRUE),2),
            Correcte_taal=round(sd(Correcte_taal, na.rm = TRUE),2),
            Email=round(sd(Email, na.rm = TRUE),2),
            Totaal_taalvaardigheid=round(sd(Totaal_taalvaardigheid, na.rm = TRUE),2)
  )


Mad_Taal<- Data%>%
  summarise(Woordenschat= round(mad(Woordenschat, na.rm = TRUE),2),
            Lidwoorden= round(mad(Lidwoorden, na.rm = TRUE),2),
            Werkwoordspelling= round(mad(Werkwoordspelling, na.rm = TRUE),2),
            Algemene_spelling= round(mad(Algemene_spelling, na.rm = TRUE),2),
            Woordvorming= round(mad(Woordvorming, na.rm = TRUE),2),
            Tekstanalyse=round(mad(Tekstanalyse, na.rm = TRUE),2),
            Correcte_taal=round(mad(Correcte_taal, na.rm = TRUE),2),
            Email=round(mad(Email, na.rm = TRUE),2),
            Totaal_taalvaardigheid=round(mad(Totaal_taalvaardigheid, na.rm = TRUE),2)
  )



descriptives_Taal <- bind_rows(Gemiddelde_Taal, Mediaan_Taal, Standaarddeviatie_Taal, Mad_Taal)
descriptives_Taal <- bind_cols(Maten, descriptives_Taal)
rename (descriptives_Taal,  Maten = ...1 )


### Boxplot
Taal_long <- Data%>%
  select(ID, Departement, Woordenschat:Totaal_taalvaardigheid)%>%
  pivot_longer(c(Woordenschat:Totaal_taalvaardigheid), names_to = "Onderdeel", values_to = "Score")



ggplot (data = Taal_long, mapping = aes(x = Onderdeel, y = Score)) +
  geom_boxplot(show.legend = F, fill = "#F69B00") +
  stat_summary(fun=mean, geom="point", color="black", size=2, show.legend = F)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  scale_x_discrete(limits=c("Woordenschat", "Lidwoorden", "Werkwoordspelling", "Algemene_spelling", "Woordvorming", "Tekstanalyse", "Correcte_taal", "Email", "Totaal_taalvaardigheid"))





## 2.3 Studiemotivatie


#### Deelname Lemo
Data$Lemo_A <- Recode(Data$Lemo_A, "1='2'; 0='0'", as.numeric=T)

Data$Lemo_totaal<-Data$Lemo_T+Data$Lemo_A
Data$Lemo_totaal <- as.factor(Data$Lemo_totaal)

Lemo_freq <- Data%>%
  count(`Lemo_totaal`)

colnames(Lemo_freq) <- c("Bevraging", "Aantal")
Lemo_freq$Bevraging<- recode(Lemo_freq$Bevraging, "0 = 'Niet deelgenomen'; 1 = 'Enkel terugblikversie'; 2 ='Enkel actuele versie'; 3= 'Beide versies'")
flextable(Lemo_freq)

### Dubbels eruit
Data$Datum2 <- as.Date(Data$Datum2, "%d-%m-%Y")
Data$Datum1 <- as.Date(Data$Datum1, "%d-%m-%Y")

Data<- Data %>%
  mutate(Datum3 = as.character(as.Date(Datum2, "%d-%m-%Y"),"%Y-%m-%d"))

Lemo_long <- Data%>%
  filter(Datum2 > as.Date("2020-11-01") & Datum1 < as.Date("2020-10-30"))%>%
  filter (Lemo_totaal == "3")%>%
  select(Departement, KritiS_T:Lemo_T, StuuR_A:Datum3)
 

Lemo_long_2 <- Lemo_long%>%
  group_by(Departement)%>%
  count(Departement)



flextable(Lemo_long_2)

ungroup()

Lemo_test <- Lemo_long%>%
  select (Lemo_T, Lemo_A, AutoM_T, AutoM_A)%>%
  pivot_longer(c(Lemo_T, Lemo_A), names_to = "Schaal", values_to = "Score")


### Beschrijvende statistieken terugblikversie

  Gemiddelde_Motivatie_T<- Data%>%
  summarise(Willen_studeren= round(mean(AutoM_T, na.rm = TRUE),2),
            Moeten_studeren= round(mean(Cont_M, na.rm = TRUE),2),
            Demotivatie= round(mean(AM_T, na.rm = TRUE),2),
            Bekwaamheidsgevoel= round(mean(ZelfE_T, na.rm = TRUE),2)
           
  )

Mediaan_Motivatie_T<- Data%>%
  summarise(Willen_studeren= round(median(AutoM_T, na.rm = TRUE),2),
            Moeten_studeren= round(median(Cont_M, na.rm = TRUE),2),
            Demotivatie= round(median(AM_T, na.rm = TRUE),2),
            Bekwaamheidsgevoel= round(median(ZelfE_T, na.rm = TRUE),2)
            
  )

Standaarddeviatie_Motivatie_T<- Data%>%
  summarise(Willen_studeren= round(sd(AutoM_T, na.rm = TRUE),2),
            Moeten_studeren= round(sd(Cont_M, na.rm = TRUE),2),
            Demotivatie= round(sd(AM_T, na.rm = TRUE),2),
            Bekwaamheidsgevoel= round(sd(ZelfE_T, na.rm = TRUE),2)
            
  )


Mad_Motivatie_T<- Data%>%
  summarise(Willen_studeren= round(mad(AutoM_T, na.rm = TRUE),2),
            Moeten_studeren= round(mad(Cont_M, na.rm = TRUE),2),
            Demotivatie= round(mad(AM_T, na.rm = TRUE),2),
            Bekwaamheidsgevoel= round(mad(ZelfE_T, na.rm = TRUE),2)
  )

descriptives_Motivatie_T <- bind_rows(Gemiddelde_Motivatie_T, Mediaan_Motivatie_T, Standaarddeviatie_Motivatie_T, Mad_Motivatie_T)
descriptives_Motivatie_T <- bind_cols(Maten, descriptives_Motivatie_T)
descriptives_Motivatie_T <- rename (descriptives_Motivatie_T,  Maten = ...1 )
flextable (descriptives_Motivatie_T)

### Boxplot terugblikversie
Motivatie_long_T <- Data%>%
  select(ID, AutoM_T:AM_T, ZelfE_T)%>%
  pivot_longer(-ID, names_to = "Onderdeel", values_to = "Score")

ggplot (data = Motivatie_long_T, mapping = aes(x = Onderdeel, y = Score, fill = Onderdeel)) +
  geom_boxplot(show.legend = F, fill = "#F69B00") +
  stat_summary(fun=mean, geom="point", color="black", size=2, show.legend = F)+
  labs(x = "Schalen terugblikversie Studiemotivatie in AP-Vaardig", y = "Score (0-4)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  scale_x_discrete(limits=c("AutoM_T", "Cont_M", "AM_T", "ZelfE_T"), labels=c("AutoM_T" = "Willen studeren", "Cont_M" = "Moeten studeren",
                                                                            "AM_T" = "Demotivatie", "ZelfE_T" = "Bekwaamheidsgevoel"))


### Beschrijvende statistieken actuele versie

Gemiddelde_Motivatie<- Data%>%
  summarise(Willen_studeren= round(mean(AutoM_A, na.rm = TRUE),2),
            Moeten_studeren= round(mean(ContM_A, na.rm = TRUE),2),
            Demotivatie= round(mean(AM_A, na.rm = TRUE),2),
            Bekwaamheidsgevoel= round(mean(ZelfE_A, na.rm = TRUE),2)
            
  )

Mediaan_Motivatie<- Data%>%
  summarise(Willen_studeren= round(median(AutoM_A, na.rm = TRUE),2),
            Moeten_studeren= round(median(ContM_A, na.rm = TRUE),2),
            Demotivatie= round(median(AM_A, na.rm = TRUE),2),
            Bekwaamheidsgevoel= round(median(ZelfE_A, na.rm = TRUE),2)
            
  )

Standaarddeviatie_Motivatie<- Data%>%
  summarise(Willen_studeren= round(sd(AutoM_A, na.rm = TRUE),2),
            Moeten_studeren= round(sd(ContM_A, na.rm = TRUE),2),
            Demotivatie= round(sd(AM_A, na.rm = TRUE),2),
            Bekwaamheidsgevoel= round(sd(ZelfE_A, na.rm = TRUE),2)
            
  )


Mad_Motivatie<- Data%>%
  summarise(Willen_studeren= round(mad(AutoM_A, na.rm = TRUE),2),
            Moeten_studeren= round(mad(ContM_A, na.rm = TRUE),2),
            Demotivatie= round(mad(AM_A, na.rm = TRUE),2),
            Bekwaamheidsgevoel= round(mad(ZelfE_A, na.rm = TRUE),2)
  )

descriptives_Motivatie_A <- bind_rows(Gemiddelde_Motivatie_A, Mediaan_Motivatie_A, Standaarddeviatie_Motivatie_A, Mad_Motivatie_A)
descriptives_Motivatie_A <- bind_cols(Maten, descriptives_Motivatie_A)
descriptives_Motivatie_A <-  rename (descriptives_Motivatie_A,  Maten = ...1 )

### Boxplot  actuele versie
Motivatie_long_A <- Data%>%
  select(ID,Departement, AutoM_A:AM_A, ZelfE_A)%>%
  pivot_longer(c(AutoM_A:AM_A, ZelfE_A), names_to = "Onderdeel", values_to = "Score")

ggplot (data = Motivatie_long_A, mapping = aes(x = Onderdeel, y = Score, fill = Onderdeel)) +
  geom_boxplot(show.legend = F, fill = "#F69B00") +
  stat_summary(fun=mean, geom="point", color="black", size=2, show.legend = F)+
  labs(x = "Schalen actuele versie Studiemotivatie in AP-Vaardig", y = "Score (0-4)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  scale_x_discrete(limits=c("AutoM_A", "ContM_A", "AM_A", "ZelfE_A"), labels=c("AutoM_A" = "Willen studeren", "ContM_A" = "Moeten studeren",
                                                                              "AM_A" = "Demotivatie", "ZelfE_A" = "Bekwaamheidsgevoel"))

### Longitudinale analyses


AutoM_gem<- Lemo_long%>%
  summarise(Tijdstip_1= round(mean(AutoM_T, na.rm = TRUE),2),
            Tijdstip_2= round(mean(AutoM_A, na.rm = TRUE),2))
ContM_gem<- Lemo_long%>%
  summarise(Tijdstip_1= round(mean(Cont_M, na.rm = TRUE),2),
            Tijdstip_2= round(mean(ContM_A, na.rm = TRUE),2))

AM_gem<- Lemo_long%>%
  summarise(Tijdstip_1= round(mean(AM_T, na.rm = TRUE),2),
            Tijdstip_2= round(mean(AM_A, na.rm = TRUE),2))

ZelfE_gem<- Lemo_long%>%
  summarise(Tijdstip_1= round(mean(ZelfE_T, na.rm = TRUE),2),
            Tijdstip_2= round(mean(ZelfE_A, na.rm = TRUE),2))

Gemiddelden_Motivatie <- bind_rows(AutoM_gem, ContM_gem, AM_gem, ZelfE_gem)

Schalen <-c("Willen studeren", "Moeten studeren", "Demotivatie", "Bekwaamheidsgevoel")

Gemiddelden_Motivatie <- bind_cols(Schalen, Gemiddelden_Motivatie)
Gemiddelden_Motivatie <- rename (Gemiddelden_Motivatie,  Schalen = ...1 )
flextable (Gemiddelden_Motivatie)



#### Lijndiagram groei motivatie

Gemiddelden_Motivatie <- as.data.frame(Gemiddelden_Motivatie)

Motivatie_gem_long <- Gemiddelden_Motivatie%>%
  pivot_longer(c(Tijdstip_1,Tijdstip_2), names_to = "moment", values_to = "Score")
Motivatie_gem_long<-rename (Auto_gem_long,  Schalen = ...1 )
          
  ggplot(data = Motivatie_gem_long, mapping = aes(x=moment, y=Score, group=Schalen) )+
  geom_point (aes (x = moment, y = Score, group=Schalen),  stat="identity", size = 2, color=c("deepskyblue3", "deepskyblue3", "darkorange2", "darkorange2", "darkorange2", "darkorange2", "deepskyblue3", "deepskyblue3"), show.legend = F)+
    scale_y_continuous(breaks = seq(0,4,0.2), limits = c(0,4))+
  geom_line (mapping = aes(x=moment, y=Score,  group=Schalen), size = 0.5, color=c("deepskyblue", "deepskyblue", "darkorange", "darkorange", "darkorange", "darkorange", "deepskyblue", "deepskyblue"), show.legend = F )+
    geom_dl(aes(label = Schalen), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1), color=c("deepskyblue3", "deepskyblue3", "darkorange2", "darkorange2", "darkorange2", "darkorange2", "deepskyblue3", "deepskyblue3"))+
    labs(x= "Tijdstippen",
         y= "Gemiddelde score op schalen studiemotivatie (0-4)")
          
 

## 2.4 Verwerkingsstrategieën


### Beschrijvende statistieken terugblikversie
  
Gemiddelde_Verwerking_T<- Data%>%
  summarise(Relateren_structureren= round(mean(RelaS_T, na.rm = TRUE),2),
            Kritisch_verwerken= round(mean(KritiS_T, na.rm = TRUE),2),
            Toepassen= round(mean(ToepS_T, na.rm = TRUE),2),
            Samen_leren= round(mean(SameL_T, na.rm = TRUE),2),
            Analyseren= round(mean(AnalS_T, na.rm = TRUE),2),
            Memoriseren= round(mean(MemoS_T, na.rm = TRUE),2))

Mediaan_Verwerking_T<- Data%>%
  summarise(Relateren_structureren= round(median(RelaS_T, na.rm = TRUE),2),
            Kritisch_verwerken= round(median(KritiS_T, na.rm = TRUE),2),
            Toepassen= round(median(ToepS_T, na.rm = TRUE),2),
            Samen_leren= round(median(SameL_T, na.rm = TRUE),2),
            Analyseren= round(median(AnalS_T, na.rm = TRUE),2),
            Memoriseren= round(median(MemoS_T, na.rm = TRUE),2)  )

Standaarddeviatie_Verwerking_T<- Data%>%
  summarise(Relateren_structureren= round(sd(RelaS_T, na.rm = TRUE),2),
            Kritisch_verwerken= round(sd(KritiS_T, na.rm = TRUE),2),
            Toepassen= round(sd(ToepS_T, na.rm = TRUE),2),
            Samen_leren= round(sd(SameL_T, na.rm = TRUE),2),
            Analyseren= round(sd(AnalS_T, na.rm = TRUE),2),
            Memoriseren= round(sd(MemoS_T, na.rm = TRUE),2))


Mad_Verwerking_T<- Data%>%
  summarise(Relateren_structureren= round(mad(RelaS_T, na.rm = TRUE),2),
            Kritisch_verwerken= round(mad(KritiS_T, na.rm = TRUE),2),
            Toepassen= round(mad(ToepS_T, na.rm = TRUE),2),
            Samen_leren= round(mad(SameL_T, na.rm = TRUE),2),
            Analyseren= round(mad(AnalS_T, na.rm = TRUE),2),
            Memoriseren= round(mad(MemoS_T, na.rm = TRUE),2)
            
  )

descriptives_Verwerking_T <- bind_rows(Gemiddelde_Verwerking_T, Mediaan_Verwerking_T, Standaarddeviatie_Verwerking_T, Mad_Verwerking_T)
descriptives_Verwerking_T <- bind_cols(Maten, descriptives_Verwerking_T)
descriptives_Verwerking_T <-  rename (descriptives_Verwerking_T,  Maten = ...1 )
flextable(descriptives_Verwerking_T)


### Boxplot verwerkingsstrategieën terugblikversie

Verwerking_long_T <- Data%>%
  select(ID, Departement, KritiS_T:SameL_T, RelaS_T:MemoS_T)%>%
  pivot_longer(c(KritiS_T:SameL_T, RelaS_T:MemoS_T), names_to = "Onderdeel", values_to = "Score")

ggplot (data = Verwerking_long_T, mapping = aes(x = Onderdeel, y = Score, fill = Onderdeel)) +
  geom_boxplot(show.legend = F, fill = "#F69B00") +
  stat_summary(fun=mean, geom="point", color="black", size=2, show.legend = F)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  scale_x_discrete(limits=c("RelaS_T", "KritiS_T", "ToepS_T", "SameL_T", "AnalS_T", "MemoS_T"), labels=c("RelaS_T" = "Relateren en structureren", "KritiS_T" = "Kritisch verwerken", "ToepS_T" = "Toepassen", "SameL_T" = "Samen leren", "AnalS_T" = "Analyseren", "MemoS_T" = "Memoriseren"))



### Beschrijvende statistieken Actuele versie

Data_A <- Data%>%
  filter(Datum2 > as.Date("2020-11-01"))



Gemiddelde_Verwerking_A<- Data_A%>%
  summarise(Relateren_structureren= round(mean(RelaS_A, na.rm = TRUE),2),
            Kritisch_verwerken= round(mean(KritiS_A, na.rm = TRUE),2),
            Toepassen= round(mean(ToepS_A, na.rm = TRUE),2),
            Samen_leren= round(mean(SameL_A, na.rm = TRUE),2),
            Analyseren= round(mean(AnalS_A, na.rm = TRUE),2),
            Memoriseren= round(mean(MemoS_A, na.rm = TRUE),2))

Mediaan_Verwerking_A<- Data_A%>%
  summarise(Relateren_structureren= round(median(RelaS_A, na.rm = TRUE),2),
            Kritisch_verwerken= round(median(KritiS_A, na.rm = TRUE),2),
            Toepassen= round(median(ToepS_A, na.rm = TRUE),2),
            Samen_leren= round(median(SameL_A, na.rm = TRUE),2),
            Analyseren= round(median(AnalS_A, na.rm = TRUE),2),
            Memoriseren= round(median(MemoS_A, na.rm = TRUE),2)  )

Standaarddeviatie_Verwerking_A<- Data_A%>%
  summarise(Relateren_structureren= round(sd(RelaS_A, na.rm = TRUE),2),
            Kritisch_verwerken= round(sd(KritiS_A, na.rm = TRUE),2),
            Toepassen= round(sd(ToepS_A, na.rm = TRUE),2),
            Samen_leren= round(sd(SameL_A, na.rm = TRUE),2),
            Analyseren= round(sd(AnalS_A, na.rm = TRUE),2),
            Memoriseren= round(sd(MemoS_A, na.rm = TRUE),2))


Mad_Verwerking_A<- Data_A%>%
  summarise(Relateren_structureren= round(mad(RelaS_A, na.rm = TRUE),2),
            Kritisch_verwerken= round(mad(KritiS_A, na.rm = TRUE),2),
            Toepassen= round(mad(ToepS_A, na.rm = TRUE),2),
            Samen_leren= round(mad(SameL_A, na.rm = TRUE),2),
            Analyseren= round(mad(AnalS_A, na.rm = TRUE),2),
            Memoriseren= round(mad(MemoS_A, na.rm = TRUE),2)
            
  )

descriptives_Verwerking_A <- bind_rows(Gemiddelde_Verwerking_A, Mediaan_Verwerking_A, Standaarddeviatie_Verwerking_A, Mad_Verwerking_A)
descriptives_Verwerking_A <- bind_cols(Maten, descriptives_Verwerking_A)
descriptives_Verwerking_A <-  rename (descriptives_Verwerking_A,  Maten = ...1 )
flextable(descriptives_Verwerking_A)

### Boxplots verwerkingsstrategieën actuele versie

Verwerking_long_A<- Data_A%>%
  select(ID, Departement, StuuR_A:SameL_A, ToepS_A)%>%
  pivot_longer(c(StuuR_A:ToepS_A), names_to = "Onderdeel", values_to = "Score")

ggplot (data = Verwerking_long_A, mapping = aes(x = Onderdeel, y = Score, fill = Onderdeel)) +
  geom_boxplot(show.legend = F, fill = "#F69B00") +
  stat_summary(fun=mean, geom="point", color="black", size=2, show.legend = F)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  scale_x_discrete(limits=c("RelaS_A", "KritiS_A", "ToepS_A", "SameL_A", "AnalS_A", "MemoS_A"), labels=c("RelaS_A" = "Relateren en structureren", "KritiS_A" = "Kritisch verwerken", "ToepS_A" = "Toepassen", "SameL_A" = "Samen leren", "AnalS_A" = "Analyseren", "MemoS_A" = "Memoriseren"))





## 2.5 Sturingsstrategieën

Gemiddelde_Sturing<- Data_clean%>%
  summarise(Zelfsturing= round(mean(Zelfsturing, na.rm = TRUE),2),
            Planning= round(mean(Planning, na.rm = TRUE),2),
            Stuurloos_leergedrag= round(mean(Stuurloos_leergedrag, na.rm = TRUE),2)

            
  )

Mediaan_Sturing<- Data_clean%>%
  summarise(Zelfsturing= round(median(Zelfsturing, na.rm = TRUE),2),
            Planning= round(median(Planning, na.rm = TRUE),2),
            Stuurloos_leergedrag= round(median(Stuurloos_leergedrag, na.rm = TRUE),2)
            
            
  )
  

Standaarddeviatie_Sturing<- Data_clean%>%
  summarise(Zelfsturing= round(sd(Zelfsturing, na.rm = TRUE),2),
            Planning= round(sd(Planning, na.rm = TRUE),2),
            Stuurloos_leergedrag= round(sd(Stuurloos_leergedrag, na.rm = TRUE),2)
            
            
  )


Mad_Sturing<- Data_clean%>%
  summarise(Zelfsturing= round(mad(Zelfsturing, na.rm = TRUE),2),
            Planning= round(mad(Planning, na.rm = TRUE),2),
            Stuurloos_leergedrag= round(mad(Stuurloos_leergedrag, na.rm = TRUE),2)
            
            
  )

descriptives_Sturing <- bind_rows(Gemiddelde_Sturing, Mediaan_Sturing, Standaarddeviatie_Sturing, Mad_Sturing)
descriptives_Sturing <- bind_cols(Maten, descriptives_Sturing)
rename (descriptives_Sturing,  Maten = ...1 )

### Boxplot
Sturing_long <- Data_clean%>%
  select(ID, Zelfsturing:Stuurloos_leergedrag)%>%
  pivot_longer(-ID, names_to = "Onderdeel", values_to = "Score")

ggplot (data = Sturing_long, mapping = aes(x = Onderdeel, y = Score, fill = Onderdeel)) +
  geom_boxplot(show.legend = F) +
  stat_summary(fun=mean, geom="point", color="black", size=2, show.legend = F)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_discrete(limits=c("Zelfsturing", "Planning", "Stuurloos_leergedrag"))



## 2.6 Verschillen tussen opleidingen

### Rekenen

#### Regressies
Data_rekenen<- Data_clean%>%
  select(ID, Departement, Opleiding, Deductief_redeneren, Rekenen, Getallen, Vergelijkingen, Verhoudingen, Totaal_rekenen, GPA)
Data_rekenen <- Data_rekenen[complete.cases(Data_rekenen), ]

Manova_rekenen<- lm(cbind(Deductief_redeneren, Rekenen, Getallen, Vergelijkingen, Verhoudingen)  ~ Departement, data = Data_rekenen)
Anova_redeneren <- lm(Deductief_redeneren  ~ Departement, data = Data_rekenen)
Anova_rekenen<- lm(Rekenen  ~ Departement, data = Data_rekenen)
Anova_vergelijkingen <- lm(Vergelijkingen  ~ Departement, data = Data_rekenen)
Anova_verhoudingen <- lm(Verhoudingen  ~ Departement, data = Data_rekenen)

#### Multivariate testen
Multi_rekenen <- Anova(Manova_rekenen)
summary(Multi_rekenen)
etasq(Multi_rekenen, anova = TRUE, partial = TRUE)

#### Anova's
summary(Manova_rekenen)

etasq(Anova_redeneren, anova = TRUE, partial = TRUE)
effect.size(Anova_redeneren)

etasq(Anova_rekenen, anova = TRUE, partial = TRUE)
effect.size(Anova_rekenen)

etasq(Anova_vergelijkingen, anova = TRUE, partial = TRUE)
effect.size(Anova_vergelijkingen)

etasq(Anova_verhoudingen, anova = TRUE, partial = TRUE)
effect.size(Anova_verhoudingen)

etasq(Anova_verhoudingen, anova = TRUE, partial = TRUE)
effect.size(Anova_verhoudingen)

#### Visualisatie in boxplots
ggplot (data = Rekenen_long, mapping = aes(x = Onderdeel, y = Score, fill = Departement), position = "dodge") +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  scale_x_discrete(limits=c("Deductief_redeneren", "Rekenen", "Getallen", "Verhoudingen", "Vergelijkingen", "Totaal_rekenen"))
  
  ggplot (data = Rekenen_long, mapping = aes(x = Onderdeel, y = Score, fill = Onderdeel)) +
    geom_boxplot(show.legend = F) +
    stat_summary(fun=mean, geom="point", color="black", size=2, show.legend = F)+
    facet_grid(.~Departement)+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    scale_x_discrete(limits=c("Deductief_redeneren", "Rekenen", "Getallen", "Verhoudingen", "Vergelijkingen", "Totaal_rekenen"))
  
  
  ggplot (data = Rekenen_long, mapping = aes(x = Departement, y = Score, fill = Onderdeel)) +
    geom_boxplot(show.legend = F) +
    stat_summary(fun=mean, geom="point", color="black",  size=2, show.legend = F)+
    facet_grid(.~Onderdeel)+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    scale_x_discrete(limits=c("GW", "MC", "WT"))
  

#### Anova Totaalscore

Anova_Rekenen_totaal <- lm(Totaal_rekenen  ~ Departement, data = Data_rekenen)
summary (Anova_Rekenen_totaal)

etasq(Anova_Rekenen_totaal, anova = TRUE, partial = TRUE)
effect.size(Anova_Rekenen_totaal)

####  multilevel analyse



# 3. Evolutie in mediagebruik

## 3.1 digitap

## 3.2 Teams

## 3.3 Panopto

## 3.4 Geïntegreerd model

## 3.5 verschillen tussen opleidingen



# 4. Directe en indirecte voorspellers

## 4.1 Directe voorspeling AP-vaardig

### 4.1.1 Multi-level voorspelling vanuit Rekenen

#### Nulmodel
Data_rekenen_beperkt <- Data_rekenen %>%
  filter (Opleiding == "GRAD-AA"| Opleiding =="PBA-BM"|Opleiding =="PBA-BL"|Opleiding =="PBA-CH"|Opleiding =="PBA-EA"|Opleiding =="PBA-IV"|Opleiding =="PBA-TI"|Opleiding =="PBA-TP"|Opleiding =="PBA-VD"|Opleiding =="PBA-VK4"|Opleiding =="PBA-VR")

Data_rekenen_beperkt <- Data_rekenen_beperkt[complete.cases(Data_rekenen_beperkt), ]

Nulmodel_rekenen<- lmer(GPA~1+(1|Opleiding),
                                data=Data_rekenen_beperkt, REML= FALSE)
summary(Nulmodel_rekenen)
rand(Nulmodel_rekenen)
confint(Nulmodel_rekenen)
performance::icc(Nulmodel_rekenen)

#### Vanuit toaalscore
ggplot(data    = Data_rekenen_beperkt, mapping = aes(x= Totaal_rekenen, y = GPA, col= Opleiding, group = Opleiding))+
  geom_smooth(method = lm, se     = FALSE, size   = 1, alpha  = .8)+
  geom_hline(yintercept=10, color="black", linetype = "dashed", size   = 1, alpha  = .8)+#to add the colours for different classes
  geom_point(mapping = aes(x= Totaal_rekenen, y = GPA, color = Opleiding),  size     = 1.2, alpha    = .8, position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  scale_x_continuous(breaks = seq(0,100,5))+
  labs(title    = "Relatie totaalscore rekenen en GPA",
       subtitle = "opgedeeld per opleiding",
       x= "Totaalscore op rekenen in AP-Vaardig",
       y= "Gemiddelde score examens januari")


#### Random intercept model

RI_rekenen_totaal<- lmer(GPA~Totaal_rekenen+(1|Opleiding),
                        data=Data_rekenen_beperkt, REML= FALSE)

summary(RI_rekenen_totaal)
rand(RI_rekenen_totaal)
confint(RI_rekenen_totaal)
performance::icc(RI_rekenen_totaal)

anova(RI_rekenen_totaal,Nulmodel_rekenen)

#### Random slope model

RS_rekenen_totaal<- lmer(GPA~Totaal_rekenen+(1+Totaal_rekenen|Opleiding),
                         data=Data_rekenen_beperkt, REML= FALSE)

summary(RS_rekenen_totaal)
rand(RS_rekenen_totaal)
confint(RS_rekenen_totaal)
performance::icc(RS_rekenen_totaal)

anova(RS_rekenen_totaal,RI_rekenen_totaal)


#### Vanuit aparte voorspeller

#### Random intercept

RI_rekenen_schalen<- lmer(GPA~Deductief_redeneren + Rekenen + Getallen + Verhoudingen + Vergelijkingen+(1|Opleiding),
                         data=Data_rekenen_beperkt, REML= FALSE)

summary(RI_rekenen_schalen)
rand(RI_rekenen_schalen)
confint(RI_rekenen_schalen)
performance::icc(RI_rekenen_schalen)

anova(RI_rekenen_schalen,Nulmodel_rekenen)

RI_rekenen_schalen_2<- lmer(GPA~Deductief_redeneren + Getallen + Verhoudingen + Vergelijkingen+(1|Opleiding),
                          data=Data_rekenen_beperkt, REML= FALSE)

summary(RI_rekenen_schalen_2)
rand(RI_rekenen_schalen_2)
confint(RI_rekenen_schalen_2)
performance::icc(RI_rekenen_schalen_2)

anova(RI_rekenen_schalen,RI_rekenen_schalen_2)



#### Random slope model

RS_rekenen_schalen<- lmer(GPA~Deductief_redeneren + Getallen + Verhoudingen + Vergelijkingen+(1 + Deductief_redeneren + Getallen + Verhoudingen + Vergelijkingen|Opleiding),
                          data=Data_rekenen_beperkt, REML= FALSE)

summary(RS_rekenen_schalen)
rand(RS_rekenen_schalen)
confint(RS_rekenen_schalen)


anova(RI_rekenen_schalen_2,RS_rekenen_schalen)



#Enkel getallen als voorspeller

RI_getallen<- lmer(GPA~ Getallen +(1|Opleiding),
                          data=Data_rekenen_beperkt, REML= FALSE)

summary(RI_getallen)
rand(RI_getallen)
confint(RI_getallen)
performance::icc(RI_getallen)

anova(RI_getallen,Nulmodel_rekenen)

RS_getallen<- lmer(GPA~ Getallen +(1+ Getallen|Opleiding),
data=Data_rekenen_beperkt, REML= FALSE)

summary(RS_getallen)
rand(RS_getallen)
confint(RS_getallen)

anova(RI_getallen,RS_getallen)

ggplot(data    = Data_rekenen_beperkt, mapping = aes(x= Getallen, y = GPA, col= Opleiding, group = Opleiding))+
  geom_smooth(method = lm, se     = FALSE, size   = 1, alpha  = .8)+
  geom_hline(yintercept=10, color="black", linetype = "dashed", size   = 1, alpha  = .8)+#to add the colours for different classes
  geom_point(mapping = aes(x= Totaal_rekenen, y = GPA, color = Opleiding),  size     = 1.2, alpha    = .8, position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  scale_x_continuous(breaks = seq(0,100,5))+
  labs(title    = "Relatie totaalscore rekenen en GPA",
       subtitle = "opgedeeld per opleiding",
       x= "Totaalscore op onderdeel Getallen in AP-Vaardig",
       y= "Gemiddelde score examens januari")

## 4.2 Directe voorspelling mediagebruik

## 4.3 complexe modellen via SEM

Gebruik_Digitap <- Data_clean_2%>%
  select(Digitap_sep:Digitap_dec)	




Clusters_Digitap<-NbClust(Gebruik_Digitap, diss= NULL, distance="euclidean", min.nc=2, max.nc=10, 
               method="complete", index="all", alphaBeale=0.1)

Data_Digitap<- cbind(Gebruik_Digitap, clusterNum = Clusters_Digitap$Best.partition)
Data_Digitap$clusterNum <- as.factor(Data_Digitap$clusterNum)

Gemiddelden_Clusters_Digitap <- Data_Digitap %>%
  group_by (clusterNum) %>%
  summarise (September= round(mean(Digitap_sep, na.rm = TRUE),2),
            Oktober = round(mean(Digitap_okt, na.rm = TRUE),2),
            November = round(mean(Digitap_nov, na.rm = TRUE),2),
            December = round(mean(Digitap_dec, na.rm = TRUE),2))

library (DT)
datatable (Gemiddelden_Clusters_Digitap)

GPA_Digitap <- cbind(Data_clean_2$GPA, Data_Digitap$clusterNum)
GPA_Digitap <- as.data.frame(GPA_Digitap)
colnames(GPA_Digitap) <- c("GPA", "Cluster")
Lm_Digitap <- lm(GPA~Cluster, data = GPA_Digitap)
summary(Lm_Digitap)
etasq(Lm_Digitap, anova = TRUE, partial = TRUE)
