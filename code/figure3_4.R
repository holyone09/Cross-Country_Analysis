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

data<-read.csv(file = "./ref_data/EcuadorPublicUse_sel.csv",header = T)
dataclean <- data %>%
  filter(answersurvey == 1)

use <- dataclean %>%
  dplyr::select(totaltimeuse20) 


library(corrplot)

connections <- data.frame(use)

#mentalhealth questions + mental health index
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(Q01))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(Q02))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(Q03))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(Q04))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(Q05))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(MHI5))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(depression))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(happiness))

#change name - mental health
colnames(connections)[which(names(connections) == "Q01")] <- "HappinessLevel"
colnames(connections)[which(names(connections) == "Q02")] <- "PeacefulnessLevel"
colnames(connections)[which(names(connections) == "Q03")] <- "NervousnessLevel"
colnames(connections)[which(names(connections) == "Q04")] <- "SadnessLevel"
colnames(connections)[which(names(connections) == "Q05")] <- "DepressionLevel"
colnames(connections)[which(names(connections) == "MHI5")] <- "MentalHealthIndex"




#mental health
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(mainstressor))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(schoolmainproblem))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(socialmainproblem))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(financesmainproblem))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(stresslevelPAP))

#timeuse 
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse1))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse2))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse3))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse4))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse5))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse6))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse7))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse8))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse9))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse10))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse11))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse12))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse13))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse14))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse15))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse16))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse17))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse18))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse19))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse21))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse22))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse23))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(totaltimeuse24))

#change name - time use 
colnames(connections)[which(names(connections) == "totaltimeuse1")] <- "Sleep"
colnames(connections)[which(names(connections) == "totaltimeuse2")] <- "PersonalCare"
colnames(connections)[which(names(connections) == "totaltimeuse3")] <- "WatchTV"
colnames(connections)[which(names(connections) == "totaltimeuse4")] <- "WorkFamilyBus"
colnames(connections)[which(names(connections) == "totaltimeuse5")] <- "WorkOtherJob"
colnames(connections)[which(names(connections) == "totaltimeuse6")] <- "LookForWork"
colnames(connections)[which(names(connections) == "totaltimeuse7")] <- "PrepareMeals"
colnames(connections)[which(names(connections) == "totaltimeuse8")] <- "Eat"
colnames(connections)[which(names(connections) == "totaltimeuse9")] <- "CleanKitchen"
colnames(connections)[which(names(connections) == "totaltimeuse10")] <- "Laundry"
colnames(connections)[which(names(connections) == "totaltimeuse11")] <- "BuyFood"
colnames(connections)[which(names(connections) == "totaltimeuse12")] <- "Religion"
colnames(connections)[which(names(connections) == "totaltimeuse13")] <- "HelpSiblings"
colnames(connections)[which(names(connections) == "totaltimeuse14")] <- "Read"
colnames(connections)[which(names(connections) == "totaltimeuse15")] <- "WatchListenEduc"
colnames(connections)[which(names(connections) == "totaltimeuse16")] <- "LearnUniversityOptions"
colnames(connections)[which(names(connections) == "totaltimeuse17")] <- "SchoolWork"
colnames(connections)[which(names(connections) == "totaltimeuse18")] <- "BuisnessIdea"
colnames(connections)[which(names(connections) == "totaltimeuse19")] <- "MusicInstrument"
colnames(connections)[which(names(connections) == "totaltimeuse20")] <- "HobbyOrSport"
colnames(connections)[which(names(connections) == "totaltimeuse21")] <- "GoOutWithFriends"
colnames(connections)[which(names(connections) == "totaltimeuse22")] <- "OnlineWithFriends"
colnames(connections)[which(names(connections) == "totaltimeuse23")] <- "PlayOnline"
colnames(connections)[which(names(connections) == "totaltimeuse24")] <- "Transport"

#learning situation
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(anylearning))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(teachersonlineclass))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(teacherssentmaterials))
connections <-  cbind(connections, dataclean %>%
                        dplyr::select(teachersethomework))

connections10 = connections %>% dplyr::select(c(NervousnessLevel, DepressionLevel, SadnessLevel, MentalHealthIndex, happiness, anylearning,OnlineWithFriends,GoOutWithFriends))
connections10<-na.omit(connections10)

library(ltm)
cronbach.alpha(connections10, CI=TRUE)

corconnect_10 <- cor(connections10,method='spearman')
tiff(filename = "fig3.tiff",width = 3500, height = 3000,res=300)
corrplot(corconnect_10, order = "hclust", tl.cex = 1.5)
dev.off()   

clrs5 <- c("#00BF7D", "#00BA38", "#6BB100", "#A3A500", "#C99800")
clrs6 <- c("#00FF33", "#CCFFCC", "#6BB100", "#A3A500", "#C99800")

connections10  <- connections10  %>%
  mutate(anylearning = factor(anylearning, levels = 0:1, labels = c("No", "Yes [Any Online or Tele-Educative Learning]")))

gdat <- connections10 %>%
  dplyr::group_by(anylearning,DepressionLevel) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(DepressionLevel = factor(DepressionLevel, levels = 1:5, labels = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time")))

tiff(filename = "fig4_a.tif",width = 3500, height = 3700,res=500)
gdat %>%
  ggplot(aes(anylearning, Percent, 
             fill = DepressionLevel, 
             color = DepressionLevel))+
  labs(x = "Type of Learning") +
  geom_bar(stat="identity") +
  geom_line() +
  # define colors
  scale_fill_manual(values=clrs6) +
  scale_color_manual(values=clrs6) +
  theme_bw()

dev.off()
connections11 = connections %>% dplyr::select(c(schoolmainproblem, SchoolWork, anylearning, teacherssentmaterials, teachersethomework, HappinessLevel))
connections11  <- connections11  %>%
  mutate(anylearning = factor(anylearning, levels = 0:1, labels = c("No", "Yes [Any Online or Tele-Educative Learning]")))

connections11<-na.omit(connections11)

gdat2 <- connections11 %>%
  dplyr::group_by(anylearning,HappinessLevel) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(HappinessLevel = factor(HappinessLevel, levels = 1:5, labels = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time")))

tiff(filename = "fig4_b.tif",width = 3500, height = 3700,res=500)
gdat2 %>%
  ggplot(aes(anylearning, Percent, 
             fill = HappinessLevel, 
             color = HappinessLevel))+
  labs(x = "Type of Learning") +
  geom_bar(stat="identity") +
  geom_line() +
  # define colors
  scale_fill_manual(values=clrs6) +
  scale_color_manual(values=clrs6) +
  theme_bw()

dev.off()