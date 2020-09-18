library(stringr)
library(rvest)
library(tidyverse)
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
#####GALLICAPRESSE
#Gallicapresse révèle la structure des données utilisées dans l'analyse de notoriété réalisée par GALLICAGRAM
#Les affichages effectués grâce à cet outil révèlent la structure des données selon :
  #Les titres de presse les plus représentés
  #L'origine géographique des mentions (ville de publication de ces titres de presse)
#Cet outil affiche des analyses en termes absolus et relatifs.
#Deux résolutions d'affichage sont disponibles : à l'année et au mois

#####EXTRACTION D'UN RAPPORT DE RECHERCHE
#La fonction d'extraction de rapport de recherche depuis gallica fonctionnant mal, nous reprenons ici une partie de l'outil gargallica qui exécute parfaitement cette tâche

setwd("C:/Users/Benjamin/Downloads/presse") #inscrivez ici votre répertoire de travail
#####GARGALLICA###############
i = 1

# Indiquez la question (la requête CQL visible dans l'URL query = () )
# Il faut recopier la question posée sur gallica.bnf.fr

question <- '(%20text%20all%20"bonnard"%20%20prox/unit=word/distance=1%20"abel"))%20and%20(dc.type%20all%20"fascicule")%20sortby%20dc.date/sort.ascending&suggest=10&keywords='

page <- function(i)xml2::read_xml(paste0('http://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&query=(', question,')&collapsing=false&maximumRecords=50&startRecord=', i))


# Première 50 réponses (initialiser la structure xml avec un premier coup)
tot <- page(1)
# récupérer le nombre total de réponses
te <- xml2::as_list(tot)
nmax <- as.integer(unlist(te$searchRetrieveResponse$numberOfRecords))
# nmax <- 7853

# Boucle sur la suite, 50 par 50
# Ajouter au document xml tot les réponses des autres pages
for (j in seq(51, nmax, by = 50)){
  temp <- page(j)
  for (l in xml2::xml_children(temp)){
    xml2::xml_add_child(tot, l)
  }
}

xml2::write_xml(tot, 'results.xml')

xml_to_df <- function(doc, ns = xml_ns(doc)) {
  library(xml2)
  library(purrr)
  split_by <- function(.x, .f, ...) {
    vals <- map(.x, .f, ...)
    split(.x, simplify_all(transpose(vals)))
  }
  node_to_df <- function(node) {
    # Filter the attributes for ones that aren't namespaces
    # x <- list(.index = 0, .name = xml_name(node, ns))
    x <- list(.name = xml_name(node, ns))
    # Attributes as column headers, and their values in the first row
    attrs <- xml_attrs(node)
    if (length(attrs) > 0) {attrs <- attrs[!grepl("xmlns", names(attrs))]}
    if (length(attrs) > 0) {x <- c(x, attrs)}
    # Build data frame manually, to avoid as.data.frame's good intentions
    children <- xml_children(node)
    if (length(children) >= 1) {
      x <- 
        children %>%
        # Recurse here
        map(node_to_df) %>%
        split_by(".name") %>%
        map(bind_rows) %>%
        map(list) %>%
        {c(x, .)}
      attr(x, "row.names") <- 1L
      class(x) <- c("tbl_df", "data.frame")
    } else {
      x$.value <- xml_text(node)
    }
    x
  }
  node_to_df(doc)
}

# u <- xml_to_df(xml2::xml_find_all(tot, ".//srw:records"))
x = 1:3
parse_gallica <- function(x){
  xml2::xml_find_all(tot, ".//srw:recordData")[x] %>% 
    xml_to_df() %>% 
    select(-.name) %>% 
    .$`oai_dc:dc` %>% 
    .[[1]] %>% 
    mutate(recordId = 1:nrow(.)) %>% 
    #    tidyr::unnest() %>% 
    tidyr::gather(var, val, - recordId) %>% 
    group_by(recordId, var) %>% 
    mutate(value = purrr::map(val, '.value') %>% purrr::flatten_chr() %>% paste0( collapse = " -- ")) %>% 
    select(recordId, var, value) %>% 
    ungroup() %>% 
    mutate(var = stringr::str_remove(var, 'dc:')) %>% 
    tidyr::spread(var, value) %>% 
    select(-.name)
}

tot <- xml2::read_xml('results.xml')

tot_df <- 1:nmax %>% 
  parse_gallica %>% 
  bind_rows()

write.csv(tot_df,"rapport.csv")
##############################
#rapport<-read.csv("rapport.csv") #Lecture du rapport de recherche
rapport<-tot_df
rapport<-cbind(rapport$identifier,rapport$title,rapport$publisher,rapport$date) #Extraction des colonnes contenant les critères d'intérêt
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

#####COMPTAGE PAR VILLE D'EDITION
rapport<- rapport[is.na(rapport$lieu)==FALSE,]
presse_l<-as.data.frame(unique(rapport$lieu))
colnames(presse_l)<-c("lieu")
presse_l$lieu<-as.character(presse_l$lieu)
presse_l$count<-NA
for (i in 1:length(presse_l$lieu)) 
{
  presse_l$count[i]<-sum(str_count(rapport$lieu,presse_l$lieu[i]))
}


#####DETERMINATION DES PRINCIPAUX LIEUX D'EDITION
top_lieux<-top_n(presse_l,10)

rapport$principaux_lieux<-"Autre"
for (i in 1:length(rapport$lieu)) 
{
  if(sum(as.numeric(rapport$lieu[i]==top_lieux$lieu))==1)
  {
    rapport$principaux_lieux[i]<-as.character(rapport$lieu[i])
  }
  #else {rapport$principaux_titres[i]<-as.character("Autre")}
}
rapport$principaux_lieux<-as.factor(rapport$principaux_lieux)

#####STRUCTURE DES DONNEES SELON LE JOURNAL
  #Résolution annuelle
    #Modifiez les paramètres si dessous pour changer le titre, les légendes et la résolution du graphique

      #Sur le graphe suivant : 
        #Structure=journal
        #Valeurs=absolues
        #Résolution=annuelle
ggplot(rapport,aes(date,color=principaux_titres,fill=principaux_titres))+
  geom_histogram(binwidth=365)+
  scale_x_date(date_breaks = "2 year",limits = c(ymd("1900-01-01"),ymd("1950-01-01")),date_labels = "%Y")+
  xlab("1900-1950")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française selon le journal d'origine")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  ggsave("Histogramme.png",scale=2.5)


      #Sur le graphe suivant : 
        #Structure=journal
        #Valeurs=relatives
        #Résolution=annuelle
ggplot(rapport,aes(date,color=principaux_titres,fill=principaux_titres))+
  geom_histogram(position = "fill",binwidth=365)+
  scale_x_date(date_breaks = "2 year",limits = c(ymd("1900-01-01"),ymd("1950-01-01")),date_labels = "%Y")+
  xlab("1900-1950")+
  ylab("Part de chaque journal dans la répartition annuelle")+
  ggtitle("Répartition des mentions d'Abel Bonnard dans la presse française selon le journal d'origine")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  ggsave("Histogramme_100.png",scale=2)


      #Sur le graphe suivant : 
        #Structure=journal
        #Valeurs=absolues et individualisées
        #Résolution=annuelle
ggplot(rapport, aes(date,color=principaux_titres)) +
  geom_freqpoly(binwidth = 365, size=1)+
  scale_x_date(date_breaks = "2 year",limits = c(ymd("1900-01-01"),ymd("1950-01-01")),date_labels = "%Y")+
  xlab("1900-1950")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française selon le journal d'origine")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  ggsave("Courbe.png",scale=2)


    #Résolution au mois
      #Sur le graphe suivant : 
        #Structure=journal
        #Valeurs=absolues
        #Résolution=mensuelle
ggplot(rapport,aes(date,color=principaux_titres,fill=principaux_titres))+
  geom_histogram(binwidth=30)+
  scale_x_date(date_breaks = "2 month",limits = c(ymd("1940-01-01"),ymd("1945-01-01")),date_labels = "%b-%Y")+
  xlab("1940-1945")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française selon le journal d'origine")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  theme(axis.text.x = element_text(angle=45))+
  ggsave("Histogramme_1940-1945.png",scale=2)


      #Sur le graphe suivant : 
        #Structure=journal
        #Valeurs=relatives
        #Résolution=mensuelle
ggplot(rapport,aes(date,color=principaux_titres,fill=principaux_titres))+
  geom_histogram(position = "fill",binwidth=30)+
  scale_x_date(date_breaks = "2 month",limits = c(ymd("1940-01-01"),ymd("1945-01-01")),date_labels = "%b-%Y")+
  xlab("1940-1945")+
  ylab("Part de chaque journal dans la répartition annuelle")+
  ggtitle("Répartition des mentions d'Abel Bonnard dans la presse française selon le journal d'origine")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  theme(axis.text.x = element_text(angle=45))+
  ggsave("Histogramme_100_1940-1945.png",scale=2)


      #Sur le graphe suivant : 
        #Structure=journal
        #Valeurs=absolues et individualisées
        #Résolution=mensuelle
ggplot(rapport, aes(date,color=principaux_titres)) +
  geom_freqpoly(binwidth = 30, size=1)+
  scale_x_date(date_breaks = "2 month",limits = c(ymd("1940-01-01"),ymd("1945-01-01")),date_labels = "%b-%Y")+
  xlab("1940-1945)")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française selon le journal d'origine")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  theme(axis.text.x = element_text(angle=45))+
  ggsave("Courbe_1940-1945.png",scale=2)

#####STRUCTURE DES DONNEES SELON LA VILLE DE PUBLICATION
  #Résolution annuelle

      #Sur le graphe suivant : 
        #Structure=lieu
        #Valeurs=absolues
        #Résolution=annuelle
ggplot(rapport,aes(date,color=principaux_lieux,fill=principaux_lieux))+
  geom_histogram(binwidth=365)+
  scale_x_date(date_breaks = "2 year",limits = c(ymd("1900-01-01"),ymd("1950-01-01")),date_labels = "%Y")+
  xlab("1900-1950")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française selon la ville d'édition")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  ggsave("Histogramme_lieux.png",scale=2)


      #Sur le graphe suivant : 
        #Structure=lieu
        #Valeurs=relatives
        #Résolution=annuelle
ggplot(rapport,aes(date,color=principaux_lieux,fill=principaux_lieux))+
  geom_histogram(position = "fill",binwidth=365)+
  scale_x_date(date_breaks = "2 year",limits = c(ymd("1900-01-01"),ymd("1950-01-01")),date_labels = "%Y")+
  xlab("1900-1950")+
  ylab("Part de chaque journal dans la répartition annuelle")+
  ggtitle("Répartition des mentions d'Abel Bonnard dans la presse française selon la ville d'édition")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  ggsave("Histogramme_lieux_100.png",scale=2)


      #Sur le graphe suivant : 
        #Structure=lieu
        #Valeurs=absolues et individualisées
        #Résolution=annuelle
ggplot(rapport, aes(date,color=principaux_lieux)) +
  geom_freqpoly(binwidth = 365, size=1)+
  scale_x_date(date_breaks = "2 year",limits = c(ymd("1900-01-01"),ymd("1950-01-01")),date_labels = "%Y")+
  xlab("1900-1950")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française selon la ville d'édition")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  ggsave("Courbe_lieux.png",scale=2)


    #Résolution au mois
      #Sur le graphe suivant : 
        #Structure=lieu
        #Valeurs=absolues
        #Résolution=mensuelle
ggplot(rapport,aes(date,color=principaux_lieux,fill=principaux_lieux))+
  geom_histogram(binwidth=30)+
  scale_x_date(date_breaks = "2 month",limits = c(ymd("1940-01-01"),ymd("1945-01-01")),date_labels = "%b-%Y")+
  xlab("1940-1945")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française selon la ville d'édition")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  theme(axis.text.x = element_text(angle=45))+
  ggsave("Histogramme_lieux_1940-1945.png",scale=2)


      #Sur le graphe suivant : 
        #Structure=lieu
        #Valeurs=relatives
        #Résolution=mensuelle
ggplot(rapport,aes(date,color=principaux_lieux,fill=principaux_lieux))+
  geom_histogram(position = "fill",binwidth=30)+
  scale_x_date(date_breaks = "2 month",limits = c(ymd("1940-01-01"),ymd("1945-01-01")),date_labels = "%b-%Y")+
  xlab("1940-1945")+
  ylab("Part de chaque journal dans la répartition annuelle")+
  ggtitle("Répartition des mentions d'Abel Bonnard dans la presse française selon la ville de publication")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  theme(axis.text.x = element_text(angle=45))+
  ggsave("Histogramme_100_lieux_1940-1945.png",scale=2)


      #Sur le graphe suivant : 
        #Structure=lieu
        #Valeurs=absolues et individualisées
        #Résolution=mensuelle
ggplot(rapport, aes(date,color=principaux_lieux)) +
  geom_freqpoly(binwidth = 30, size=1)+
  scale_x_date(date_breaks = "2 month",limits = c(ymd("1940-01-01"),ymd("1945-01-01")),date_labels = "%b-%Y")+
  xlab("1940-1945)")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française selon la vile de publication")+
  theme(legend.position="top")+
  scale_color_brewer(type = 'seq', palette = "Paired")+
  scale_fill_brewer(type = 'seq', palette = "Paired")+
  theme(axis.text.x = element_text(angle=45))+
  ggsave("Courbe_lieux_1940-1945.png",scale=2)



#####STRUCTURE DE LA BASE DE DONNEES : HISTOGRAMME HORIZONTAL ATEMPOREL
  #####JOURNAUX D'ORIGINE DES MENTIONS
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

  #####ORIGINE GEOGRAPHIQUE DES MENTIONS
top_lieux2<-top_n(presse_l,20)
autres<-sum(presse_l$count)-sum(top_lieux$count)
top_lieux2<-as.data.frame(rbind(top_lieux2,c("Autres villes",autres)))
top_lieux2$count<-as.integer(top_lieux2$count)

top_lieux2%>%mutate() %>%ggplot(aes(count,reorder(lieu,count)))+
  geom_col()+
  #theme(axis.text.x = element_text(angle=90))+
  xlab("Nombre de numéros mentionnant Abel Bonnard") + ylab("Journal")+
  ggtitle("Villes d'édition des numéros de presse mentionnant Abel Bonnard (1883-1968)")+
  ggsave("Histogramme_villes.png",scale=2)

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
  xlab("1900-1950")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française")+
  theme(axis.text.x = element_text(angle=20))+
  ggsave("Notoriete_y.png",scale=2)

ggplot(notoriete_m,aes(date_m,count))+geom_line(size=1)+
  scale_x_date(date_breaks = "1 month",limits = c(ymd("1940-01-01"),ymd("1945-01-01")),date_labels = "%Y-%m")+
  xlab("1940-1945")+
  ylab("Nombre de mentions dans la presse française (Gallica)")+
  ggtitle("Evolution des mentions d'Abel Bonnard dans la presse française")+
  theme(axis.text.x = element_text(angle=45))+
  ggsave("Notoriete_m.png",scale=3)
