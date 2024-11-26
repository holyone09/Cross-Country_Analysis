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
library(flextable)
library(devtools)
library(haven)
library(readxl)
library(purrr)

data <- read_sav('ref_data/Adolescent_WaveData.sav')
new_data = data[,4:70]
map_int(new_data, function(.x) sum(is.na(.x)))
new_data = new_data %>% dplyr::select(everything(), - c(any_virtual,remote_duration))

##test1 data
new_data_test1<-na.omit(new_data)
library(ltm)
#calculate Cronbach's Alpha with 95% confidence interval
cronbach.alpha(new_data_test1, CI=TRUE)

corMatMy <- cor(new_data_test1,method='spearman')
library(corrplot)
tiff(filename = "fig1.tiff",width = 3500, height = 3000,res=300)
corrplot(corMatMy, order = "hclust", tl.cex = 0.8)
dev.off()

##subset
new_data_test2 = new_data %>% dplyr::select(c(hours_online,think_ofothers,together_nottogether,think_ofme,meaningful_work,
                                              school_satisfaction,school_feelings,interesting_classes,more_hangouts,happy_with_inperson,
                                              hours_inperson,hours_extracurricular,school_type,depressed,troubled_sleep))
new_data_test2<-na.omit(new_data_test2)
cronbach.alpha(new_data_test2, CI=TRUE)

##figure2
#new_data_test2  <- new_data_test2  %>%
#  mutate(school_type = factor(school_type, levels = 1:5, labels = c("In Person", "Hybrid", "Online", "Homeschool/Different version", "No school")))
new_data_test2  <- new_data_test2  %>%
  mutate(school_type = factor(school_type, levels = 1:5, labels = c("a public school", "a private school", "Homeschool", "No school","Other")))

new_data_test2  <- new_data_test2  %>%
  mutate(troubled_sleep = factor(troubled_sleep, levels = 1:4, labels = c("Not at all", "Several days", "More than half of the days", "Nearly every day")))

new_data_test2  <- new_data_test2  %>%
  mutate(hours_online = factor(hours_online, levels = 1:8, labels = c("None", "< 30min.", "31-59min.", "1-2h","3-4h",
                                                                      "5-6h","7-8h","> 8h")))

clrs5 <- c("#00BF7D", "#00BA38", "#6BB100", "#A3A500", "#C99800") ##select colors for clear separation and identification
clrs6 <- c("#00FF33", "#CCFFCC", "#6BB100", "#A3A500", "#C99800") 
clrs8 <- c("#00BF7D", "#00BA38", "#6BB100", "#A3A500", "#C99800", "#FFBF00", "#FF7F50", "#DE3163")
clrs9 <- c("#00FF33", "#CCFFCC", "#6BB100", "#A3A500", "#C99800", "#FFBF00", "#FF7F50", "#DE3163")

gdat <- new_data_test2 %>%
  #dplyr::group_by(school_type,happy_with_inperson) %>%
  dplyr::group_by(school_type,hours_online) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) 

tiff(filename = "fig2_a.tiff",width = 3500, height = 3700,res=500)
gdat %>%
  ggplot(aes(school_type, Percent, 
             fill = hours_online, 
             color = hours_online))+
  labs(x = "School type") +
  
  geom_bar(stat="identity",position = position_stack(reverse = TRUE)) +
  geom_line() +
  # define colors
  scale_fill_manual(values=clrs9) +
  scale_color_manual(values=clrs9) +
  # add text and define color
  theme_bw() 
dev.off()

gdat_dp <- new_data_test2 %>%
  dplyr::group_by(troubled_sleep,depressed) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(depressed = factor(depressed, levels = 1:4, labels = c("Not at all", "Several days", "More than half of the days", "Nearly every day")))

tiff(filename = "fig2_b.tiff",width = 3500, height = 3700,res=500)
gdat_dp %>%
  ggplot(aes(troubled_sleep, Percent, 
             fill = depressed, 
             color = depressed))+
  labs(x = "Troubled sleep") +
  geom_bar(stat="identity",position = position_stack(reverse = TRUE)) +
  geom_line() +
  # define colors
  scale_fill_manual(values=clrs6) +
  scale_color_manual(values=clrs6) +
  # add text and define color
  theme_bw()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) 

dev.off()
