setwd("C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Resultados/Experimento 2/R")
rm(list=ls())

library(ggplot2)
library(tidyverse)
library(officer)
library(rvg)
library(lme4)
library(emmeans)
library(ggpubr)

#### Wrangling#####
create_list <- function(x){
  a <- read.csv(x,sep = ';') %>%
    mutate(Speed=1/Chlat*1000,Trl=1:length(Trl))
  a$Block<-factor(a$Block, levels=unique(a$Block))
  a
}

files <- dir(pattern="StF")
stf_group <- lapply(files,create_list)
names(stf_group) <- files
stf_group <- bind_rows(stf_group,.id = "Participant")

files <- dir(pattern="StR")
str_group <- lapply(files,create_list)
names(str_group) <- files
str_group <- bind_rows(str_group,.id = "Participant")

setwd("C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Resultados/Experimento 2/Follow-up/R")

files <- dir(pattern="StF")
stf_groupf <- lapply(files,create_list)
names(stf_groupf) <- files
stf_groupf <- bind_rows(stf_groupf,.id = "Participant")

files <- dir(pattern="StR")
str_groupf <- lapply(files,create_list)
names(str_groupf) <- files
str_groupf <- bind_rows(str_groupf,.id = "Participant")


stf_data <- bind_rows(stf_group, stf_groupf, .id = "Phase")
stf_data <- mutate(stf_data,Phase=case_when(
  stf_data$Phase==1~'1stSession',
  stf_data$Phase==2~'Follow-up',
  TRUE~'Error'
))

str_data <- bind_rows(str_group, str_groupf, .id = "Phase")
str_data <- mutate(str_data,Phase=case_when(
  str_data$Phase==1~'1stSession',
  str_data$Phase==2~'Follow-up',
  TRUE~'Error'
))

complete_data <- bind_rows(stf_data, str_data, .id = "Group")
complete_data <- mutate(complete_data,Group=case_when(
  complete_data$Group==1~'StF',
  complete_data$Group==2~'StR',
  TRUE~'Error'
))

complete_data <- mutate(complete_data,Block=case_when(
  complete_data$Phase=='Follow-up' & complete_data$Block=='reorg1' ~ 'maint1',
  complete_data$Phase=='Follow-up' & complete_data$Block=='reorg2' ~ 'maint2',
  complete_data$Phase=='Follow-up' & complete_data$Block=='reorg3' ~ 'maint3',
  TRUE~as.character(complete_data$Block)
))

complete_data <- complete_data %>% mutate(hit_stm = case_when(
  .$HitKey==1 ~ .$ChKey1,
  .$HitKey==2 ~ .$ChKey2,
  .$HitKey==3 ~ .$ChKey3,
  .$HitKey==4 ~ .$ChKey4,
  TRUE ~ "Error"
))

complete_data <- complete_data %>% mutate(category = case_when(
  #Ambas
  .$Samp %in% c('B1.jpg','C1.jpg') & .$hit_stm %in% c('B1.jpg','C1.jpg') ~ 'Ambas',
  .$Samp %in% c('B2.jpg','C2.jpg') & .$hit_stm %in% c('B2.jpg','C2.jpg') ~ 'Ambas',
  .$Samp %in% c('B3.jpg','C3.jpg') & .$hit_stm %in% c('B3.jpg','C3.jpg') ~ 'Ambas',
  
  #B1
  .$Samp == 'B1.jpg' & .$hit_stm == 'D1.jpg' ~ 'Formação',
  .$Samp == 'B1.jpg' & .$hit_stm == 'D2.jpg' ~ 'Reversão',
  .$Samp == 'B1.jpg' & .$hit_stm %in% c('C2.jpg','C3.jpg','D3.jpg') ~ 'Inconsistente',
  
  #B2
  .$Samp == 'B2.jpg' & .$hit_stm == 'D2.jpg' ~ 'Formação',
  .$Samp == 'B2.jpg' & .$hit_stm == 'D3.jpg' ~ 'Reversão',
  .$Samp == 'B2.jpg' & .$hit_stm %in% c('C3.jpg','C1.jpg','D1.jpg') ~ 'Inconsistente',
  
  #B3
  .$Samp == 'B3.jpg' & .$hit_stm == 'D3.jpg' ~ 'Formação',
  .$Samp == 'B3.jpg' & .$hit_stm == 'D1.jpg' ~ 'Reversão',
  .$Samp == 'B3.jpg' & .$hit_stm %in% c('C1.jpg','C2.jpg','D2.jpg') ~ 'Inconsistente',
  
  #C1
  .$Samp == 'C1.jpg' & .$hit_stm == 'D1.jpg' ~ 'Formação',
  .$Samp == 'C1.jpg' & .$hit_stm == 'D2.jpg' ~ 'Reversão',
  .$Samp == 'C1.jpg' & .$hit_stm %in% c('B2.jpg','B3.jpg','D3.jpg') ~ 'Inconsistente',
  
  #C2
  .$Samp == 'C2.jpg' & .$hit_stm == 'D2.jpg' ~ 'Formação',
  .$Samp == 'C2.jpg' & .$hit_stm == 'D3.jpg' ~ 'Reversão',
  .$Samp == 'C2.jpg' & .$hit_stm %in% c('B3.jpg','B1.jpg','D1.jpg') ~ 'Inconsistente',
  
  #C3
  .$Samp == 'C3.jpg' & .$hit_stm == 'D3.jpg' ~ 'Formação',
  .$Samp == 'C3.jpg' & .$hit_stm == 'D1.jpg' ~ 'Reversão',
  .$Samp == 'C3.jpg' & .$hit_stm %in% c('B1.jpg','B2.jpg','D2.jpg') ~ 'Inconsistente',
  
  #D1
  .$Samp == 'D1.jpg' & .$hit_stm %in% c('B1.jpg','C1.jpg') ~ 'Formação',
  .$Samp == 'D1.jpg' & .$hit_stm %in% c('B3.jpg','C3.jpg') ~ 'Reversão',
  .$Samp == 'D1.jpg' & .$hit_stm %in% c('B2.jpg','C2.jpg') ~ 'Inconsistente',
  
  #D2
  .$Samp == 'D2.jpg' & .$hit_stm %in% c('B2.jpg','C2.jpg') ~ 'Formação',
  .$Samp == 'D2.jpg' & .$hit_stm %in% c('B1.jpg','C1.jpg') ~ 'Reversão',
  .$Samp == 'D2.jpg' & .$hit_stm %in% c('B3.jpg','C3.jpg') ~ 'Inconsistente',  
  
  #D3
  .$Samp == 'D3.jpg' & .$hit_stm %in% c('B3.jpg','C3.jpg') ~ 'Formação',
  .$Samp == 'D3.jpg' & .$hit_stm %in% c('B2.jpg','C2.jpg') ~ 'Reversão',
  .$Samp == 'D3.jpg' & .$hit_stm %in% c('B1.jpg','C1.jpg') ~ 'Inconsistente',
  TRUE ~ "Error"
))

complete_data <- complete_data %>% mutate(Follow_up = case_when(
  .$Group=="StF" & .$Participant %in% c("StFOTM_P01 - Lucas.csv",
                                    "StFOTM_P02 - André.csv",
                                    "StFOTM_P03 - Julia.csv",
                                    "StFOTM_P04 - Sharon.csv") ~ 30,
  .$Group=="StF" & .$Participant %in% c("StFOTM_P06 - Guilherme.csv",
                                    "StFOTM_P07 - Eduarda.csv",
                                    "StFOTM_P08 - Lucas.csv",
                                    "StFOTM_P09 - Ana Laura.csv") ~ 14,
  .$Group=="StR" & .$Participant %in% c("StROTM_P01 - Kaique.csv",
                                    "StROTM_P02 - Thamires.csv",
                                    "StROTM_P03 - Thiago.csv",
                                    "StROTM_P04 - Lydia.csv",
                                    "StROTM_P05 - Pedro.csv") ~ 30,
  .$Group=="StR" & .$Participant %in% c("StROTM_P06 - Matheus.csv",
                                    "StROTM_P07 - Abner.csv",
                                    "StROTM_P08 - Isadora.csv",
                                    "StROTM_P09 - Daniel.csv") ~ 14,
  TRUE ~ 0
))


#### GLzM ####
complete_data <- complete_data %>%
  mutate(trial_type = paste(str_sub(Samp,1,1),str_sub(hit_stm,1,1),sep = ''))
test_data <- complete_data %>% filter(Block%in%c('eq1','reorg1','maint1'))
test_data <- test_data %>% mutate(trial_type = case_when(
  .$trial_type=='CB' ~ 'BC',
  .$trial_type=='DB' ~ 'BD',
  .$trial_type=='DC' ~ 'CD',
  TRUE ~ as.character(.$trial_type)
))
test_data$Group <- factor(test_data$Group, levels = c("StF", "StR"))
test_data$Block <- factor(test_data$Block, levels = c("eq1", "reorg1", "maint1"))
test_data$Follow_up <- as.factor(test_data$Follow_up)

model<-test_data %>% glmer(formula = Speed ~ Group*Block + (1|Participant), family = Gamma)
summary(model) #modelo

#### Contraste  entre blocos por grupo transformado para a escala original ####
regrid(emmeans(model,~Group*Block)) %>%
  contrast("revpairwise", by="Group",adjust="tukey") %>% summary()

regrid(emmeans(model,~Group*Block)) %>%
  contrast("revpairwise",adjust="tukey")

grp.blk.contrast <- regrid(emmeans(model,~Group*Block)) %>%
  contrast("revpairwise", by="Group",adjust="tukey") %>% summary() %>%
  as.data.frame() %>% separate(contrast,into = c('group1','group2'), sep = " - ") %>%
  filter(p.value<0.05) %>% mutate(asterisk=ifelse(p.value<0.01,'**','*'))

grp.blk.contrast

model_emmean <- regrid(emmeans(model,~Group*Block)) %>% summary()
ggplot(model_emmean,aes(x=Block,y=response,group=Group))+
  geom_line()+geom_point(size=2.5)+geom_errorbar(aes(ymin=response-SE,ymax=response+SE),width=0.25)+
  stat_pvalue_manual(grp.blk.contrast,label = "asterisk",y.position = 0.47,step.increase = 0.1)+
  scale_x_discrete(labels = c("Eq","Reorg","Manut"))+xlab(label = "")+ylab(label = 'Velocidade')+
  theme_classic()+theme(axis.text=element_text(size=11),
                        axis.text.x = element_text(color='black'),
                        axis.title.y=element_text(size=11),
                        axis.text.y = element_text(color='black'),
                        strip.text.x = element_text(size = 11))+
  facet_wrap(~Group)

ggsave('Contraste 1.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
       width = 10, height = 6, units = "cm")




#### Contraste  entre grupos por bloco transformado para a escala original ####
regrid(emmeans(model,~Group*Block)) %>%
  contrast("revpairwise", by="Block",adjust="tukey") %>% summary()

blk.grp.contrast <- regrid(emmeans(model,~Group*Block)) %>%
  contrast("revpairwise", by="Block",adjust="tukey") %>% summary() %>%
  as.data.frame() %>% separate(contrast,into = c('group1','group2'), sep = " - ") %>%
  filter(p.value<0.05) %>% mutate(asterisk=ifelse(p.value<0.01,'**','*'))

blk.grp.contrast

model_emmean <- regrid(emmeans(model,~Group*Block)) %>% summary()
lab<-as_labeller(c(`eq1`='Eq',`reorg1`='Reorg',`maint1`='Manut'))
ggplot(model_emmean,aes(x=Group,y=response,group=Block))+
  geom_line()+geom_point(size=2.5)+geom_errorbar(aes(ymin=response-SE,ymax=response+SE),width=0.25)+
  stat_pvalue_manual(blk.grp.contrast,label = "asterisk",y.position = 0.47)+
  scale_x_discrete(labels = c("StF","StR"))+xlab(label = "")+ylab(label = 'Velocidade')+
  theme_classic()+theme(axis.text=element_text(size=11),
                        axis.text.x = element_text(color='black'),
                        axis.title.y=element_text(size=11),
                        axis.text.y = element_text(color='black'),
                        strip.text.x = element_text(size = 11))+
  facet_wrap(~Block,labeller = lab)

ggsave('Contraste 2.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
       width = 10, height = 6, units = "cm")
#### GLzM para a Manutenção ####

complete_data <- complete_data %>%
  mutate(trial_type = paste(str_sub(Samp,1,1),str_sub(hit_stm,1,1),sep = ''))
test_data <- complete_data %>% filter(Block%in%c('eq1','reorg1','maint1'))
test_data <- test_data %>% mutate(trial_type = case_when(
  .$trial_type=='CB' ~ 'BC',
  .$trial_type=='DB' ~ 'BD',
  .$trial_type=='DC' ~ 'CD',
  TRUE ~ as.character(.$trial_type)
))
test_data$Group <- factor(test_data$Group, levels = c("StF", "StR"))
test_data$Block <- factor(test_data$Block, levels = c("eq1", "reorg1", "maint1"))
test_data$Follow_up <- as.factor(test_data$Follow_up)

test_data %>% filter(Block=='maint1') %>%
  glmer(formula = Speed ~Group*Follow_up+ (1|Participant), family = Gamma) %>%
  summary()

#### Análise de erros ####

treino <- unique(grep("ab|ac|ad|mix|st",complete_data$Block,value = T))
treino_fase1 <- grep("rev|str", treino, value = T, invert = T)
treino_fase2 <- treino[!(treino%in%treino_fase1)]
teste <- unique(grep("ab|ac|ad|mix",complete_data$Block,value = T, invert = T))
teste_fase1 <- grep("eq",teste,value=T)
teste_fase2 <- grep("reorg",teste,value=T)

errors_fase1 <- complete_data %>% filter(Block%in%treino_fase1) %>% group_by(Participant) %>%
  summarise(Error=sum(Result==0),Speed= mean(Speed)) 
errors_fase2 <- complete_data %>% filter(Block%in%treino_fase2) %>% group_by(Participant) %>%
  summarise(Error=sum(Result==0),Speed= mean(Speed))
#### Anova mista ####

test_data <- complete_data %>% filter(Block%in%c('eq1','reorg1','maint1'),
                         Participant!='StFOTM_P05 - Fernanda.csv',
                         Participant!='StFOTM_P10 - Ana Beatriz.csv') %>%
  group_by(Group,Participant, Block) %>% summarise(Speed=mean(Speed))
test_data$Block <- factor(test_data$Block, levels = c("eq1", "reorg1", "maint1"))

mixed.anova <- aov(Speed ~ Group*Block + Error(Participant/Block),
                   data=test_data)
summary(mixed.anova)
TukeyHSD(mixed.anova)

#explorando o efeito principal do bloco
pairwise.t.test(test_data$Speed, test_data$Block,paired = TRUE,
                p.adjust.method = "bonferroni")

#efeitos simples principais

sme.eq <- test_data %>% filter(Block=='eq1')
anova.sme.eq <-aov(Speed ~ Group,data = sme.eq)
summary(anova.sme.eq)

sme.reorg <- test_data %>% filter(Block=='reorg1')
anova.sme.reorg <-aov(Speed ~ Group,data = sme.reorg)
summary(anova.sme.reorg)

sme.maint <- test_data %>% filter(Block=='maint1')
anova.sme.maint <-aov(Speed ~ Group,data = sme.maint)
summary(anova.sme.maint)

sme.stf <- test_data %>% filter(Group=='StF')
anova.sme.stf <-aov(Speed ~ Block + Error(Participant/Block),data = sme.stf)
summary(anova.sme.stf)

sme.str <- test_data %>% filter(Group=='StR')
anova.sme.str <-aov(Speed ~ Block + Error(Participant/Block),data = sme.str)
summary(anova.sme.str)
str.data <- test_data %>% filter(Group=='StR')
pairwise.t.test(str.data$Speed, str.data$Block,paired = TRUE,
                p.adjust.method = "bonferroni")




#### Figura da interação ####
sm.data <- complete_data %>% filter(Block%in%c('eq1','reorg1','maint1')) %>%
  group_by(Group,Block) %>% summarise(Speed=mean(Speed))

sm.data$Block <- factor(sm.data$Block, levels = c("eq1", "reorg1", "maint1"))

sm.data %>%  
  ggplot(aes(x=Block,y=Speed,group=Group,shape=Group,fill=Group)) +
  geom_line() + geom_point(size=4) +
  scale_shape_manual(values = c(21,21),labels=c('Grupo StF','Grupo StR')) +
  scale_fill_manual(values = c('black','white'),labels=c('Grupo StF','Grupo StR')) +
  xlab('') + ylab('Velocidade média (r/s)') +
  scale_x_discrete(label=c('Equivalência','Reorg.','Manut.'))+
  scale_y_continuous(limits = c(0.31,0.46),breaks = seq(0.31,0.46, by = 0.05),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title = element_text(size=12, color = 'black'),
        axis.text = element_text(size = 12,color = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 12,color = 'black'))

ggsave('Interação.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 2",
       width = 15, height = 8, units = "cm")
