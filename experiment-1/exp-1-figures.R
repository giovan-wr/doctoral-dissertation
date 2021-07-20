setwd("C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Resultados/Experimento 1/R")
rm(list=ls())

library(tidyverse)
library(officer)
library(rvg)

#### Wrangling#####
create_list <- function(x){
  a <- read.csv(x,sep = ';') %>%
    mutate(Speed=1/Chlat*1000,Trl=1:length(Trl))
  a$Block<-factor(a$Block, levels=unique(a$Block))
  a
}

files <- dir(pattern="ADRevOTM")
adrev_group <- lapply(files,create_list)
adrev_group <- bind_rows(adrev_group,.id = "Participant")

files <- dir(pattern="ContOTM")
control_group <- lapply(files,create_list)
control_group <- bind_rows(control_group,.id = "Participant")

complete_data <- bind_rows(adrev_group, control_group, .id = "Group")
complete_data <- mutate(complete_data,Group=case_when(
  complete_data$Group==1~'ADr',
  complete_data$Group==2~'Controle',
  TRUE~'Error'
))


summ_data_part <- complete_data %>% group_by(Group, Participant, Block) %>%
  summarise(Result=mean(Result)*100,Speed=mean(Speed))

summ_data <- complete_data %>% group_by(Group, Block) %>%
  summarise(Result=mean(Result)*100,Speed=mean(Speed))

summ_data_fst_blk <- summ_data %>%
  filter(Block %in% c('treinoab1','treinoac1','treinoad1','mix1','eq1','adrev1',
                      'yz1','mixrev1','mixcontr1','reorg1','maint1'))

font_size <- 11


#### Duplo eixo ####
summ_data_fst_blk <- mutate(complete_data,Block=case_when(
  complete_data$Block=='yz1'~'adrev1',
  complete_data$Block=='mixcontr1'~'mixrev1',
  complete_data$Block=='maint1'~'reorg1',
  TRUE~as.character(complete_data$Block)
)) %>%
  group_by(Group, Block) %>%
  summarise(Result=mean(Result)*100,Speed=mean(Speed)) %>%
  filter((Block %in% c('treinoab1','treinoac1','treinoad1','mix1','eq1','adrev1',
                       'mixrev1','reorg1')))

summ_data_fst_blk$Block <- factor(summ_data_fst_blk$Block,
                                 levels=c('treinoab1','treinoac1','treinoad1',
                                          'mix1','eq1','adrev1','mixrev1','reorg1'))

p <- summ_data_fst_blk %>% ggplot(aes(x=Block,y=Result,fill=Group)) +
  geom_col(color='black', position = 'dodge', width = 0.8)

p <- p + geom_line(aes(x=Block,y=Speed/0.008, group=Group),size=1) +
  geom_point(aes(x=Block,y=Speed/0.008,shape=Group),size=3,fill='white',stroke =1) +
  xlab('') + ylab('Acurácia (%)') +
  scale_y_continuous(expand = c(0, 0),limits=c(0, 100), sec.axis=sec_axis(~.*0.008, name = 'Velocidade (r/s)'))

p <- p + scale_x_discrete(labels = c('Treino AB','Treino AC','Treino AD','Revisão','Equivalência','Rev. AD / Tr. YZ','Revisão','Reorg. / Manut.')) +
  scale_fill_manual(values=c("grey90","grey60"),name='',labels=c('Acurácia Reversão','Acurácia Controle')) +
  scale_shape_manual(values = c(21,24),name='',labels = c('Vel. Reversão', 'Vel. Controle')) +
  theme_classic() + theme(axis.text=element_text(size=font_size),
                          axis.text.x = element_text(color='black',angle = 45,hjust = 1),
                          axis.ticks.x = element_blank(),axis.title.y=element_text(size=font_size),
                          axis.text.y = element_text(color='black'),legend.position = "bottom",
                          legend.text = element_text(size = font_size))

ggsave('Eixo duplo.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 1",
       width = 16, height = 12, units = "cm")




#### Duplo eixo por participante ####

individual_second_axis <-function(participant,group){
  
  ifelse(group=='ADr',nome_blocos <- c('Treino AB','Treino AC','Treino AD','Revisão','Equivalência','Rev. AD','Revisão','Reorganização'),
                        nome_blocos <- c('Treino AB','Treino AC','Treino AD','Revisão','Equivalência','Treino YZ','Revisão','Manutenção'))
  
  summ_data_part %>%
    filter(Participant==participant,Group==group, Block %in% c('treinoab1','treinoac1',
                                                               'treinoad1','mix1','eq1',
                                                               'adrev1','yz1','mixrev1',
                                                               'mixcontr1','reorg1','maint1')) %>%
    ggplot(aes(x=Block,y=Result)) +
    geom_col(color='black', fill='grey90', position = 'dodge') +
    geom_line(aes(x=Block,y=Speed/ajuste_eixo,group = 'Block')) +
    geom_point(aes(x=Block,y=Speed/ajuste_eixo),size=3,fill='white',shape =21) +
    xlab('') + ylab('Acurácia (%)') +
    scale_y_continuous(expand = c(0, 0),limits=c(0, 100),
                       sec.axis=sec_axis(~.*ajuste_eixo, name = 'Velocidade (r/s)'))+
    scale_x_discrete(labels = nome_blocos) +
    theme_classic() + theme(axis.text=element_text(size=font_size),
                            axis.text.x = element_text(color='black',angle = 45,hjust = 1),
                            axis.ticks.x = element_blank(),axis.title.y=element_text(size=font_size),
                            axis.text.y = element_text(color='black'))+
    if (group=="ADr"){
      ggtitle(paste('R.',participant,sep = '')) 
    } else {
      ggtitle(paste('C.',participant,sep = '')) 
    }
  
  ggsave(paste('eixoduplo_',group,'P',participant,'.png',sep = ''),device = 'png',
         path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 1",
         width = 15, height = 10, units = "cm")
}

ajuste_eixo <- 0.008
dir(pattern="AD")
lapply(1:5,individual_second_axis,group='ADr')



#### Figura da interação ####
complete_data %>% filter(Block%in%c('eq1','reorg1','maint1')) %>%
  mutate(Block=case_when(.$Block=='maint1'~'reorg1',TRUE~as.character(.$Block))) %>%
  group_by(Group,Block) %>% summarise(Speed=mean(Speed)) %>%
  
  ggplot(aes(x=Block,y=Speed,group=Group,shape=Group, fill=Group)) +
  geom_line() + geom_point(size=4) +
  scale_shape_manual(values = c(21,21),labels=c('Grupo ADr','Grupo Controle')) +
  scale_fill_manual(values = c('black','white'),labels=c('Grupo ADr','Grupo Controle')) +
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
       width = 15, height = 8, units = "cm")


#### Boxplot ####
complete_data %>% mutate(Block=case_when(
  .$Block=='maint1'~'reorg1',
  TRUE~as.character(.$Block)
)) %>%
  filter(Block %in% c('eq1','reorg1')) %>%
  ggplot(aes(Block,Speed,fill=Group))+geom_boxplot()+
  scale_fill_manual(values=c("grey90","grey60"),name='')+
  scale_x_discrete(labels = c('Equivalência','Reorg. / Manut.'))+
  xlab('') + ylab('Velocidade (r/s)') + theme_classic() +
  theme(axis.text=element_text(size=font_size,colour = 'black'),
        axis.title=element_text(size=font_size),
        legend.text = element_text(size = font_size))

ggsave('Boxplot.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 1",
       width = 15, height = 12, units = "cm")

#### Densidade ####

complete_data %>% mutate(Block=case_when(
  .$Block=='maint1'~'reorg1',
  TRUE~as.character(.$Block)
)) %>%
filter(Block %in% c('eq1','reorg1')) %>%
  mutate(Block = case_when(
    .$Block == 'eq1' ~ "Equivalência",
    .$Block == 'reorg1' ~ "Reorg./Manut.",
    TRUE~'Erro')) %>%
  ggplot(aes(x=Speed,fill=Group))+geom_density(outline.type = 'full',alpha=0.3)+
  facet_grid(Block~.) +
  xlab('Densidade') + ylab('Velocidade (r/s)') +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  scale_fill_discrete(name='',labels=c('ADr','Controle')) + theme_classic()  + 
  theme(axis.text=element_text(size=font_size,colour = 'black'),
        axis.title=element_text(size=font_size),
        legend.text = element_text(size = font_size),
        strip.text.x = element_text(size = font_size))

ggsave('Densidade.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 1",
       width = 15, height = 10, units = "cm")

#### Revisão com reversão AD  ####
complete_data <- complete_data %>% mutate(hit_stm = case_when(
  .$HitKey==1 ~ .$ChKey1,
  .$HitKey==2 ~ .$ChKey2,
  .$HitKey==3 ~ .$ChKey3,
  .$HitKey==4 ~ .$ChKey4,
  TRUE ~ "Error"
))

mixrev <- complete_data %>% filter(Block=='mixrev1') %>%
  mutate(trial_type = str_sub(hit_stm,1,1)) %>%
  mutate(trial_type = sub( '(?<=.{0})', 'A', trial_type, perl=TRUE ))

AB <- mixrev %>% filter(trial_type=='AB') %>%
  group_by(BlkTrl) %>% summarise(Result=sum(Result)) %>%
  mutate(Tentativas=1:9, Relação=rep('AB',9))
AC <- mixrev %>% filter(trial_type=='AC') %>%
  group_by(BlkTrl) %>% summarise(Result=sum(Result)) %>%
  mutate(Tentativas=1:9, Relação=rep('AC',9))
AD <- mixrev %>% filter(trial_type=='AD') %>%
  group_by(BlkTrl) %>% summarise(Result=sum(Result)) %>%
  mutate(Tentativas=1:9, Relação=rep('ADr',9))

bind_rows(AB,AC,AD) %>%
  ggplot(aes(Tentativas,Result,group=Relação,shape=Relação,
             linetype=Relação, fill=Relação)) +
  geom_line()+ scale_linetype_manual(values=c('solid','longdash','dotted'))+
  geom_point(aes(size=Relação),stroke=1,fill='white') +
  scale_shape_manual(values = c(21,24,22)) +
  scale_size_manual(values = c(4,3,3)) +
  coord_cartesian(c(1,9),c(0,5),clip = 'off')+
  scale_y_continuous(breaks=0:5,expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(breaks=c(1:9),expand = expansion(mult = c(.03, 0))) +
  ylab('N. de participantes') + theme_classic() +
  theme(legend.title = element_blank(),
        legend.text = element_text(color = 'black', size=font_size),
        axis.title = element_text(color = 'black', size=font_size),
        axis.text = element_text(color = 'black', size=font_size))

ggsave('adrev.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 1",
       width = 13, height = 8, units = "cm")

#### Gráficos individuais (Velocidade por tentativa)#############

individual_plots <- function(participant,group){
  
  blocks_position <- complete_data %>% filter(Participant==participant,
                                              Group==group,BlkTrl==1,
                                              grepl('1',Block)) %>% .$Trl
  blocks_names <- complete_data %>% filter(Participant==participant,
                                           Group==group,BlkTrl==1,
                                           grepl('1',Block)) %>% .$Block
  positions<-data.frame(x=blocks_position,y=rep(3,length(blocks_position)),label=blocks_names)
  
  positions <- positions %>%
    mutate(label = case_when(
      .$label %in% 'treinoab1' ~ "AB",
      .$label %in% 'treinoac1' ~ "AC",
      .$label %in% 'treinoad1' ~ "AD",
      .$label %in% 'mix1' ~ "Rev",
      .$label %in% 'stf1' ~ "StF",
      .$label %in% 'eq1' ~ "Eq",
      .$label %in% 'adrev1' ~ "ADr",
      .$label %in% 'mixrev1' ~ "Rev",
      .$label %in% 'str1' ~ "StR",
      .$label %in% 'reorg1' ~ "Reorg",
      .$label %in% 'yz1' ~ "YZ",
      .$label %in% 'mixcontr1' ~ "Rev",
      .$label %in% 'maint1' ~ "Manut",
      TRUE ~ "Error")
    )
  
  p <- complete_data %>% filter(Participant==participant,Group==group) %>%
    ggplot(aes(x=Trl,y=Speed))+
    geom_line(color='blue')+
    theme_classic() +
    geom_vline(xintercept = blocks_position)+
    geom_point(aes(color=as.factor(Result)))+scale_color_manual(values=c("red",NA))+
    theme(legend.position = "none")+scale_x_continuous(expand = c(0, 0))
  
  p+geom_text(data=positions,aes(x=x,y=y,label=label),nudge_x = 15, nudge_y = -1) +
    xlab('Tentativa') + ylab('Velocidade') +
    theme(axis.text = element_text(colour = 'black',size=font_size))+
    if (group=="ADr"){
      ggtitle(paste('R.',participant,sep = '')) 
    } else {
      ggtitle(paste('C.',participant,sep = '')) 
    } 
  
  ggsave(paste('velocidade',group,'P',participant,'.png',sep = ''),device = 'png',
         path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 1",
         width = 26, height = 7, units = "cm")
}

dir(pattern="AD")
lapply(1:4,individual_plots,group='Controle')

#### Wrangling matrix de respostas#######################################
library(xlsx)


create_list <- function(x){
  a <- read.csv(x,sep = ';') %>%
    mutate(Speed=1/Chlat*1000,Trl=1:length(Trl))
  a$Block<-factor(a$Block, levels=unique(a$Block))
  a
}

files <- dir(pattern="ADRevOTM")
adr_group <- lapply(files,create_list)
adr_group <- bind_rows(adr_group,.id = "Participant")

files <- dir(pattern="ContOTM")
cont_group <- lapply(files,create_list)
cont_group <- bind_rows(cont_group,.id = "Participant")

complete_data <- bind_rows(adr_group, cont_group, .id = "Group")

complete_data <- complete_data %>% mutate(hit_stm = case_when(
  .$HitKey==1 ~ .$ChKey1,
  .$HitKey==2 ~ .$ChKey2,
  .$HitKey==3 ~ .$ChKey3,
  .$HitKey==4 ~ .$ChKey4,
  TRUE ~ "Error"
))

complete_data <- complete_data %>% mutate(Group = case_when(
  .$Group==1 ~ 'ADr',
  .$Group==2 ~ 'Controle',
  TRUE ~ "Error"
))

r_matrix<- function(x,y,wb){
  sheet <- createSheet(wb, paste(y,'P',x,sep = ''))
  mixrev_data <- complete_data %>%
    filter(Group==y,Participant==x) %>%
    mutate(Block=case_when(
      .$Block=='mixcontr1' ~ 'mixrev1',
      .$Block=='mixcontr2' ~ 'mixrev2',
      .$Block=='mixcontr3' ~ 'mixrev3',
      .$Block=='mixcontr4' ~ 'mixrev4',
      .$Block=='mixcontr5' ~ 'mixrev5',
      .$Block=='mixcontr6' ~ 'mixrev6',
      TRUE ~ as.character(.$Block)
    ))
  blocks <- unique(grep('mixrev',mixrev_data$Block,value = TRUE))
  environment(pre_matrix) <- environment()
  lapply(blocks, pre_matrix)
}

pre_matrix <- function(z){
  mixrev_data <- mixrev_data %>%
    filter(Block %in% z)
  mixrev_data <- lapply(mixrev_data, gsub, pattern='.jpg', replacement='')
  df_table <- table(mixrev_data$Samp, mixrev_data$hit_stm)
  
  style <- CellStyle(wb) +
    Font(wb,name = 'Arial') +
    Alignment(horizontal="ALIGN_CENTER")
  
  addDataFrame(as.data.frame.matrix(df_table), sheet=sheet,
               startRow = 7*as.numeric(gsub("[^0-9]",'',z))-5,
               colnamesStyle = style,
               rownamesStyle = style,
               colStyle = list('1'=style,'2'=style,'3'=style,
                               '4'=style,'5'=style,'6'=style,
                               '7'=style,'8'=style,'9'=style,
                               '10'=style))
  rows <- createRow(sheet,rowIndex=7*as.numeric(gsub("[^0-9]",'',z))-6)
  sheetTitle <- createCell(rows, colIndex=1)
  setCellValue (sheetTitle[[1,1]], z)
  
  saveWorkbook (wb, "Matrizes de resposta.xlsx")
}

setwd("C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 1")
wb = createWorkbook()
lapply(1:5, r_matrix,y='Controle',wb=wb)

