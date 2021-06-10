setwd("C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Resultados/Experimento 1/R")
rm(list=ls())

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

#### GLzM ####
test_data<-complete_data %>% filter(Block%in%c('eq1','reorg1','maint1')) %>%
  mutate(Block=case_when(.$Block=='maint1'~'reorg1',TRUE~as.character(.$Block))) %>%
  filter(Block%in%c('eq1','reorg1')) %>%
  mutate(trial_type = paste(str_sub(Samp,1,1),str_sub(hit_stm,1,1),sep = ''))
test_data <- test_data %>% mutate(trial_type = case_when(
  .$trial_type=='CB' ~ 'BC',
  .$trial_type=='DB' ~ 'BD',
  .$trial_type=='DC' ~ 'CD',
  TRUE ~ as.character(.$trial_type)
))
test_data$Group <- factor(test_data$Group, levels = c("Controle","ADr"))
test_data$Block <- factor(test_data$Block, levels = c("eq1", "reorg1"))

model<-test_data %>% glmer(formula = Speed ~ Group*Block + (1|Participant),
                           family = Gamma('identity'))
summary(model) #modelo

#### Contraste  entre blocos por grupo transformado para a escala original ####

emmeans(model,specs = pairwise~Group:Block,adjust='tukey') %>%
  .$contrasts %>% as.data.frame()

grp.blk.contrast <- emmeans(model,~Group*Block) %>%
  contrast("revpairwise", by="Group",adjust="bonferroni") %>% summary() %>%
  as.data.frame() %>% separate(contrast,into = c('group1','group2'), sep = " - ") %>%
  filter(p.value<0.05) %>% mutate(asterisk=ifelse(p.value<0.01,'**','*'))

grp.blk.contrast

model_emmean <- emmeans(model,~Group*Block) %>% summary()
ggplot(model_emmean,aes(x=Block,y=emmean,group=Group))+
  geom_line()+geom_point(size=2.5)+geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE),width=0.25)+
  stat_pvalue_manual(grp.blk.contrast,label = "asterisk",y.position = 0.58,step.increase = 0.1)+
  scale_x_discrete(labels = c("Eq","Reorg"))+xlab(label = "")+ylab(label = 'Velocidade')+
  theme_classic()+theme(axis.text=element_text(size=11),
                        axis.text.x = element_text(color='black'),
                        axis.title.y=element_text(size=11),
                        axis.text.y = element_text(color='black'),
                        strip.text.x = element_text(size = 11))+
  facet_wrap(~Group)

ggplot(model_emmean,aes(x=Block,y=emmean,group=Group,color=Group))+
  geom_line()+geom_point(size=2.5)+
  stat_pvalue_manual(grp.blk.contrast,label = "asterisk",y.position = 0.58,step.increase = 0.1)+
  scale_x_discrete(labels = c("Eq","Reorg"))+xlab(label = "")+ylab(label = 'Velocidade')+
  theme_classic()+theme(axis.text=element_text(size=11),
                        axis.text.x = element_text(color='black'),
                        axis.title.y=element_text(size=11),
                        axis.text.y = element_text(color='black'),
                        strip.text.x = element_text(size = 11))

ggsave('Contraste exp1_1.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
       width = 10, height = 6, units = "cm")


#### Contraste  entre grupos por bloco transformado para a escala original ####
emmeans(model,specs = pairwise~Group:Block, adjust='tukey') %>%
  .$contrasts %>% as.data.frame()

blk.grp.contrast <- emmeans(model,~Group*Block) %>%
  contrast("revpairwise", by="Block",adjust="tukey") %>% summary() %>%
  as.data.frame() %>% separate(contrast,into = c('group1','group2'), sep = " - ") %>%
  filter(p.value<0.05) %>% mutate(asterisk=ifelse(p.value<0.01,'**','*'))

blk.grp.contrast

model_emmean <- emmeans(model,~Group*Block) %>% summary()
lab<-as_labeller(c(`eq1`='Eq',`reorg1`='Reorg/Manut'))
ggplot(model_emmean,aes(x=Group,y=emmean,group=Block))+
  geom_line()+geom_point(size=2.5)+geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE),width=0.25)+
  #stat_pvalue_manual(blk.grp.contrast,label = "asterisk",y.position = 0.47)+
  scale_x_discrete(labels = c("ADr","Control"))+xlab(label = "")+ylab(label = 'Velocidade')+
  theme_classic()+theme(axis.text=element_text(size=11),
                        axis.text.x = element_text(color='black'),
                        axis.title.y=element_text(size=11),
                        axis.text.y = element_text(color='black'),
                        strip.text.x = element_text(size = 11))+
  facet_wrap(~Block,labeller = lab)

ggsave('Contraste exp1_2.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
       width = 10, height = 6, units = "cm")



#### Testes ####
test_data <- complete_data %>% filter(Block%in%c('eq1','reorg1','maint1'))
test_data <- test_data %>%
  mutate(Block=case_when(.$Block=='maint1'~'reorg1',TRUE~as.character(.$Block)))
test_data <- test_data %>%
  group_by(Group,Participant, Block) %>% summarise(Speed=mean(Speed))

mixed.anova <- aov(Speed ~ Group*Block + Error(Participant/Block),
                   data=test_data)
summary(mixed.anova)

#efeitos simples principais
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

#### Figura da interação ####
complete_data %>% filter(Block%in%c('eq1','reorg1','maint1')) %>%
  mutate(Block=case_when(.$Block=='maint1'~'reorg1',TRUE~as.character(.$Block))) %>%
  group_by(Group,Block) %>% summarise(Speed=mean(Speed)) %>%
  
  ggplot(aes(x=Block,y=Speed,group=Group,color=Group,shape=Group,
             fill=Group,linetype=Group)) +
  geom_line() + geom_point(size=2.5) +
  scale_shape_manual(values = c(21,22)) +
  scale_fill_manual(values = c('red','blue')) +
  scale_color_manual(values = c('red','blue')) +
  xlab('Bloco') + ylab('Velocidade média (r/s)') +
  scale_x_discrete(label=c('Equivalência','Reorg./Manut.'))+
  scale_y_continuous(limits = c(0.36,0.52),expand = c(0,0)) +
  theme_classic() +
  theme(axis.title = element_text(size=12, color = 'black'),
        axis.text = element_text(size = 12,color = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 12,color = 'black'))

ggsave('Interação.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 1",
       width = 13, height = 8, units = "cm")
