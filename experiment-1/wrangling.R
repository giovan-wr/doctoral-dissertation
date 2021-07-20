rm(list=ls())

library(tidyverse)

setwd("C:/Users/Giovan WR/projects/doctoral-dissertation/experiment-1/raw-data")


create_list <- function(x){
  a <- read.csv(x,sep = ';') %>%
    mutate(Speed=1/Chlat*1000,Trl=1:length(Trl))
  a$Block<-factor(a$Block, levels=unique(a$Block))
  a
}

files <- dir(pattern="ADRevOTM")
adrev_group <- lapply(files,create_list)
names(adrev_group) <- files
adrev_group <- bind_rows(adrev_group,.id = "Participant")

files <- dir(pattern="ContOTM")
control_group <- lapply(files,create_list)
names(control_group) <- files
control_group <- bind_rows(control_group,.id = "Participant")

complete_data <- bind_rows(adrev_group, control_group, .id = "Group")
complete_data <- mutate(complete_data,Group=case_when(
  complete_data$Group==1~'ADr',
  complete_data$Group==2~'Controle',
  TRUE~'Error'
))
complete_data <- complete_data %>% mutate(hit_stm = case_when(
  .$HitKey==1 ~ .$ChKey1,
  .$HitKey==2 ~ .$ChKey2,
  .$HitKey==3 ~ .$ChKey3,
  .$HitKey==4 ~ .$ChKey4,
  TRUE ~ "Error"
))

setwd("C:/Users/Giovan WR/projects/doctoral-dissertation/experiment-1")

save(complete_data,file="complete_data.RData")
