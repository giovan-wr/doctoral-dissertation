rm(list=ls())

library(tidyverse)

setwd("C:/Users/Giovan WR/projects/doctoral-dissertation/experiment-1")

load("complete_data.RData")

#Anova
test_data <- complete_data %>% filter(Block%in%c('eq1','reorg1','maint1'))
test_data <- test_data %>%
  mutate(Block=case_when(.$Block=='maint1'~'reorg1',TRUE~as.character(.$Block)))
test_data <- test_data %>%
  group_by(Group,Participant, Block) %>% summarise(Speed=mean(Speed))

mixed.anova <- aov(Speed ~ Group*Block + Error(Participant/Block),
                   data=test_data)
summary(mixed.anova)

#Simple effects
sme.eq <- test_data %>% filter(Block=='eq1')
anova.sme.eq <-aov(Speed ~ Group,data = sme.eq)
summary(anova.sme.eq)

sme.reorg <- test_data %>% filter(Block=='reorg1')
anova.sme.reorg <-aov(Speed ~ Group,data = sme.reorg)
summary(anova.sme.reorg)

sme.adr <- test_data %>% filter(Group=='ADr')
anova.sme.adr <-aov(Speed ~ Block + Error(Participant/Block),data = sme.adr)
summary(anova.sme.adr)

sme.con <- test_data %>% filter(Group=='Controle')
anova.sme.con <-aov(Speed ~ Block + Error(Participant/Block),data = sme.con)
summary(anova.sme.con)
