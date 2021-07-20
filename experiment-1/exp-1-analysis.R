rm(list=ls())

library(tidyverse)
library(lme4)
library(emmeans)
library(ggpubr)

setwd("C:/Users/Giovan WR/projects/doctoral-dissertation/experiment-1")

load("complete_data.RData")

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
test_data <- test_data %>% mutate(Group = case_when(
  .$Group=='ADr' ~ 'Revers?o',
  TRUE ~ as.character(.$Group)
))
test_data$Group <- factor(test_data$Group, levels = c("Controle","Revers?o"))
test_data$Block <- factor(test_data$Block, levels = c("eq1", "reorg1"))

model<-test_data %>% glmer(formula = Speed ~ Group*Block + (1|Participant),
                           family = Gamma('identity'))
summary(model) #model

#### Contrast between blocks by group transformed to the original scale ####

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


ggsave('Contraste exp1_1.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualifica??o",
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
  scale_x_discrete(labels = c("Controle","Revers?o"))+xlab(label = "")+ylab(label = 'Velocidade')+
  theme_classic()+theme(axis.text=element_text(size=11),
                        axis.text.x = element_text(color='black'),
                        axis.title.y=element_text(size=11),
                        axis.text.y = element_text(color='black'),
                        strip.text.x = element_text(size = 11))+
  facet_wrap(~Block,labeller = lab)

ggsave('Contraste exp1_2.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualifica??o",
       width = 10, height = 6, units = "cm")

