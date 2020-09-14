library(stringr)
library(rvest)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tm)
library(stm)
library(wordcloud)
library(ggwordcloud)
library(tidytext)
library(plyr)
library(lubridate)

#####INITITALISATION
#téléchargez le rapport de votre recherche au format .csv depuis Gallica
#nommez le rapport et rangez le dans votre répertoire de travail
setwd("C:/Users/Benjamin/Downloads/presse") #inscrivez ici votre répertoire de travail

rapport<-read.csv("rapport.csv", sep = ";", encoding = "UTF-8") #Lecture du rapport de recherche
rapport<-cbind(rapport[,1],rapport[,3],rapport[,6],rapport[,7]) #Extraction des colonnes contenant les critères d'intérêt
colnames(rapport)<-c("lien","titre","lieu","date")
rapport<-as.data.frame(rapport)
rapport<-rapport[str_count(rapport$date,"-")==2,] #On ne conserve que les numéros présentant une date au format (AAAA/MM/JJ)
rapport$date<-as.numeric(str_remove_all(rapport$date,"-")) #On transforme la date au format numérique
rapport$lieu<-str_extract(rapport$lieu,"([:alnum:]+[:alnum:])") #Nettoyage du nom de la première ville de publication

#Nettoyage des titres de presse et restriction du titre à 30 caractères
rapport$titre<-str_remove_all(rapport$titre,"  ")
rapport$titre<-str_remove_all(rapport$titre,"\n")
rapport$titre<-str_remove_all(rapport$titre,"\\[")
rapport$titre[nchar(rapport$titre)>30]<-str_extract(rapport$titre[nchar(rapport$titre)>30],"..............................") 

#####CHOIX DE LA PERIODE D'ETUDE : RENTREZ ICI LES BORNES CHRONOLOGIQUES SOUHAITEES (YYYYMMDD)
rapport<-rapport[(rapport$date>18831219 & rapport$date<19680531) ,]
rapport<-rapport[order(rapport$date),]

#####COMPTAGE PAR TITRE DE PRESSE
presse<-as.data.frame(unique(rapport$titre))
colnames(presse)<-c("titre")
presse$titre<-as.character(presse$titre)
presse$count<-NA
for (i in 1:length(presse$titre)) 
{
  presse$count[i]<-sum(str_count(rapport$titre,presse$titre[i]))
}
rapport$titre<-as.factor(rapport$titre)
rapport$lieu<-as.factor(rapport$lieu)
rapport$date<-ymd(rapport$date)

#####DETERMINATION DES PRINCIPAUX TITRES DE PRESSE
top_titres<-top_n(presse,10)

rapport$principaux_titres<-"Autre"
for (i in 1:length(rapport$titre)) 
{
  if(sum(as.numeric(rapport$titre[i]==top_titres$titre))==1)
  {
    rapport$principaux_titres[i]<-as.character(rapport$titre[i])
  }
  #else {rapport$principaux_titres[i]<-as.character("Autre")}
}
rapport$principaux_titres<-as.factor(rapport$principaux_titres)

#####AFFICHAGE DU GRAPHIQUE DE NOTORIETE DISTINGUANT LES PRINCIPAUX TITRES DE PRESSE
#L'histogramme a une résolution annuelle
#Modifiez les paramètres si dessous pour changer le titre, les légendes et la résolution du graphique
ggplot(rapport,aes(rapport$date,color=principaux_titres,fill=principaux_titres))+
  geom_histogram(binwidth=365)+
  scale_x_date(date_breaks = "2 year",limits = c(ymd("1900-01-01"),ymd("1950-01-01")),date_labels = "%Y")+
  xlab("Années (1900-1950)")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française selon le journal d'origine")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  ggsave("Histogramme.png",scale=2)

#####AFFICHAGE DU GRAPHIQUE REVELANT L'ORIGINE DE LA NOTORIETE DU PERSONNAGE ETUDIE (STRUCTURE DE LA BASE DE DONNEES)
top_titres2<-top_n(presse,50)
autres<-sum(presse$count)-sum(top_titres2$count)
top_titres2<-as.data.frame(rbind(top_titres2,c("Autres titres",autres)))
top_titres2$count<-as.integer(top_titres2$count)

top_titres2%>%mutate() %>%ggplot(aes(count,reorder(titre,count)))+
  geom_col()+
  #theme(axis.text.x = element_text(angle=90))+
  xlab("Nombre de numéros mentionnant Abel Bonnard") + ylab("Journal")+
  ggtitle("Titres de la presse française mentionnant le plus Abel Bonnard (1883-1968)")+
  ggsave("Histogramme_journaux.png",scale=2)

#####PREPARATION DE LA COURBE D'EVOLUTION DE LA NOTORIETE DU PERSONNAGE ETUDIE
#Deux résolutions au choix : annuelle/mensuelle

rapport$date_m<-as.character(rapport$date)
rapport$date_m<-str_remove_all(rapport$date_m,"-")
rapport$date_m<-str_extract(rapport$date_m,"......")
rapport$date_m<-str_c(rapport$date_m,"01")
rapport$date_m<-ymd(rapport$date_m)

rapport$date_y<-as.character(rapport$date)
rapport$date_y<-str_remove_all(rapport$date_y,"-")
rapport$date_y<-str_extract(rapport$date_y,"....")
rapport$date_y<-as.integer(rapport$date_y)

notoriete_m<-as.data.frame(unique(rapport$date_m))
colnames(notoriete_m)<-c("date_m")
notoriete_m$date_m<-as.character(notoriete_m$date_m)
notoriete_m$count<-NA
for (i in 1:length(notoriete_m$date_m)) 
{
  notoriete_m$count[i]<-sum(str_count(rapport$date_m,notoriete_m$date_m[i]))
}
notoriete_m$date_m<-ymd(notoriete_m$date)

notoriete_y<-as.data.frame(unique(rapport$date_y))
colnames(notoriete_y)<-c("date_y")
notoriete_y$date_y<-as.character(notoriete_y$date_y)
notoriete_y$count<-NA
for (i in 1:length(notoriete_y$date_y)) 
{
  notoriete_y$count[i]<-sum(str_count(rapport$date_y,notoriete_y$date_y[i]))
}
notoriete_y$date_y<-as.integer(notoriete_y$date_y)
#####AFFICHAGE DE LA COURBE D'EVOLUTION DE LA NOTORIETE DU PERSONNAGE ETUDIE
#Deux résolutions au choix : annuelle/mensuelle

ggplot(notoriete_y,aes(date_y,count))+geom_line(size=1)+
  scale_x_continuous(breaks=seq(1900,1950,2),limits = c(1900,1950))+
  xlab("Années (1900-1950)")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française")+
  theme(axis.text.x = element_text(angle=20))+
  ggsave("Notoriete_y.png",scale=2)

ggplot(notoriete_m,aes(date_m,count))+geom_line(size=1)+
  scale_x_date(date_breaks = "1 month",limits = c(ymd("1940-01-01"),ymd("1945-01-01")),date_labels = "%Y-%m")+
  xlab("Années (1940-1945)")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française")+
  theme(axis.text.x = element_text(angle=45))+
  ggsave("Notoriete_m.png",scale=3)
