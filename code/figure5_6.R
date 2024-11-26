library(knitr)
library(lattice)
#library(tidyverse)
library(dplyr)
library(likert)
library(MASS)
library(psych)
library(viridis)
library(ggplot2)
library(here)
library(haven)
library(readxl)
library(purrr)

library("readxl")

belgium<-read_excel("../ref/jongeren-de-coronacrisis-ruwe-data-18-05-20-gesloten-vragen.xlsx",sheet="Jongeren - Ruwe data")
belgium$`Wat is je leeftijd?`
new_belgium<-belgium %>% filter(`Wat is je leeftijd?`>13 & `Wat is je leeftijd?`<20)



new_belgium_sel<-new_belgium[,c(5,18:25,56:61)]
new_belgium_sel= na.omit(new_belgium_sel)
col_name<-c("age","calmer","lonelier","sadder","stress","happier","angry more often","tired more often","more anxious","stress due to my schoolwork",
            "someone to help","go back to school","have all the material","too busy in the house","follow with the schoolwork")
colnames(new_belgium_sel)<-col_name

new_belgium_sel$"calmer"<-ifelse(new_belgium_sel$"calmer"=="Checked",2,1)
new_belgium_sel$"lonelier"<-ifelse(new_belgium_sel$"lonelier"=="Checked",2,1)
new_belgium_sel$"sadder"<-ifelse(new_belgium_sel$"sadder"=="Checked",2,1)
new_belgium_sel$"stress"<-ifelse(new_belgium_sel$"stress"=="Checked",2,1)
new_belgium_sel$"happier"<-ifelse(new_belgium_sel$"happier"=="Checked",2,1)
new_belgium_sel$"angry more often"<-ifelse(new_belgium_sel$"angry more often"=="Checked",2,1)
new_belgium_sel$"tired more often"<-ifelse(new_belgium_sel$"tired more often"=="Checked",2,1)
new_belgium_sel$"more anxious"<-ifelse(new_belgium_sel$"more anxious"=="Checked",2,1)

new_belgium_sel$"stress due to my schoolwork"<-ifelse(new_belgium_sel$"stress due to my schoolwork"=="Ja",2,1)
new_belgium_sel$"someone to help"<-ifelse(new_belgium_sel$"someone to help"=="Ja",2,1)
new_belgium_sel$"go back to school"<-ifelse(new_belgium_sel$"go back to school"=="Ja",2,1)
new_belgium_sel$"have all the material"<-ifelse(new_belgium_sel$"have all the material"=="Ja",2,1)
new_belgium_sel$"too busy in the house"<-ifelse(new_belgium_sel$"too busy in the house"=="Ja",2,1)
new_belgium_sel$"follow with the schoolwork"<-ifelse(new_belgium_sel$"follow with the schoolwork"=="Ja",2,1)
new_belgium_sel<-new_belgium_sel[,-c(1,2)]

cronbach.alpha(new_belgium_sel, CI=TRUE)

cornew_belgium_sel <- cor(new_belgium_sel,method='spearman')
library(corrplot)
tiff(filename = "fig5.tiff",width = 3000, height = 2100,res=300)
corrplot(cornew_belgium_sel, order = "hclust", tl.cex = 1.2)
dev.off()

clrs2 <- c("#00BF7D", "#C99800")
clrs3 <- c("#00FF33", "#CCFFCC")
new_belgium_sel <- new_belgium_sel %>%
  dplyr::mutate(`follow with the schoolwork` = factor(`follow with the schoolwork`, levels = 1:2, labels = c("Not following", "Following")))


gdat <- new_belgium_sel %>%
  dplyr::group_by(`follow with the schoolwork`, `stress due to my schoolwork`) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(`stress due to my schoolwork` = factor(`stress due to my schoolwork`, levels = 1:2, labels = c("Not stressed", "Stressed"))) 

tiff(filename = "fig6_a.tif",width = 3500, height = 3700,res=500)
gdat %>%
  ggplot(aes(`follow with the schoolwork`, Percent, 
             fill = `stress due to my schoolwork`, 
             color = `stress due to my schoolwork`))+
  labs(x = "Following schoolwork") +
  geom_bar(stat="identity" ,position = position_stack(reverse = TRUE)) +
  geom_line() +
  # define colors
  scale_fill_manual(values=clrs2) +
  scale_color_manual(values=clrs2) +
  # add text and define color
  theme_bw()

dev.off()
new_belgium_sel <- new_belgium_sel %>%
  dplyr::mutate(`someone to help` = factor(`someone to help`, levels = 1:2, labels = c("No", "Yes")))


gdat <- new_belgium_sel %>%
  dplyr::group_by(`someone to help`,stress) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(stress = factor(stress, levels = 1:2, labels = c("Not stressed in general", "Stressed in general"))) 

tiff(filename = "fig6_b.tif",width = 3500, height = 3700,res=500)
gdat %>%
  ggplot(aes(`someone to help`, Percent, 
             fill = stress, 
             color = stress))+
  labs(x = "Someone to help") +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  geom_line() +
  # define colors
  scale_fill_manual(values=clrs2) +
  scale_color_manual(values=clrs2) +
  theme_bw() 
dev.off()