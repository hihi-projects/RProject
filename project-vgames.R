df=read.csv("C:/Users/xel-h/Desktop/RR/vgameDataset.csv",sep=",")


# ******************************************************Our dataset from 1980 to 2015
head(df)
str(df)
df$Year
summary(df)
df$Platform
#********************************** Import some libs *******************

library(ggplot2)
library(tidyverse)
install.packages("gridExtra")
library(plyr)
library(stringr)
library(grid)
library(gridExtra)
install.packages("remotes")
remotes::install_github("Displayr/flipTime")
library(flipTime)



#************************************** Dataset' analysing and visualization *************************************


#--------------Plot 1 : Top 10 des jeux vidéo les plus vendus :


?aggregate

dfAll <- aggregate(Global_Sales ~ Name, df, sum)

classementAll <- arrange(dfAll, desc(Global_Sales)) 
top10 <- subset(classementAll, Global_Sales %in% Global_Sales[1:10])
?rainbow
#Create some colores 
dd.col <- rainbow(length(top10$Name))
names(dd.col) <- top10$Name

reorder(df$Name, df$Global_Sales)

ggplot(data = top10, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + 
  geom_bar(stat="identity", show.legend = FALSE) + 
  coord_flip() + xlab("Jeux Vidéo") + 
  ylab("Nombre de ventes (M)") + 
  scale_fill_manual("Legend", values = dd.col)
  #+ ggtitle("Top 10 des jeux les plus vendus !") 

#---Plot 2 :  Best Publisher of games (Millieur editeur)

favoris <- ddply(df, c("Publisher"), function(x){sum(x$Global_Sales)})

classement_favoris <- arrange(favoris, desc(V1))

classement_favoris5 <- subset(classement_favoris, V1 %in% V1[1:5])

#Pour éviter les noms qui se chevauchent 
classement_favoris5$Publisher = str_wrap(classement_favoris5$Publisher, width = 10)

#Plot
dd.col <- rainbow(length(classement_favoris5$Publisher))
names(dd.col)  <- classement_favoris5$Publisher

classement_favoris5 %>% 
          ggplot(aes(x= reorder(Publisher, desc(V1)), y=V1, fill = Publisher)) + 
            geom_bar(stat="identity", show.legend = FALSE) + 
            ylab("Nombre de ventes (M)") +
            xlab("Editeurs") + 
            scale_fill_manual("Legend", values = dd.col) 
            #ggtitle("Les 5 Publishers de jeux vidéo favoris") + 

#-----Plot 3 : Best Platform (plus de ventes)------------------------------------



favoris <- ddply(df, c("Platform"), function(x){sum(x$Global_Sales)})

classement_favoris <- arrange(favoris, desc(V1))

classement_favoris10 <- subset(classement_favoris, V1 %in% V1[1:10])


classement_favoris10$Platform = str_wrap(classement_favoris10$Platform, width = 10)

#Plot
dd.col <- rainbow(length(classement_favoris10$Platform))
names(dd.col)  <- classement_favoris10$Platform

 classement_favoris10 %>%
        ggplot(aes(x= reorder(Platform, desc(V1)), y=V1, color = Platform)) + 
          geom_boxplot() + 
          ylab("Nombre de ventes (M)") +
          xlab("Editeurs") + 
          scale_fill_manual("Legend", values = dd.col)
          #ggtitle("Les 5 Platforms de jeux vidéo favoris") + 


#------Plot 4 : L'evolution des ventes pour chaque année TO-DO / FIRST PLOT IN REPORT---------------------------------
 years = subset(df, Year != "N/A")
 years$Year
 
years %>%
  ggplot(aes(x =  Year, fill=Year)) +
  geom_bar(position="dodge", show.legend = FALSE) 

#-------[Plot v: Ventes pour chaque genre---------------------------------------------- 

df %>%
  ggplot(aes(y = Genre, fill=Genre)) +
  geom_bar(position="dodge", show.legend = FALSE) 





