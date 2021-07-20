setwd("C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Resultados/Experimento 2/R")
rm(list=ls())

library(ggplot2)
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

complete_data <- complete_data %>% mutate(Id=substr(Participant,9,10))

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
)) %>%
  mutate(category=as.factor(category))

complete_data$category <- factor(complete_data$category,
                                 levels=c('Formação','Reversão','Inconsistente','Ambas'))

sum_category <- complete_data %>% group_by(Group,Participant,category) %>%
  count(category)

complete_data <- complete_data %>% mutate(Follow_up = case_when(
  .$Participant %in% c("StFOTM_P01 - Lucas.csv","StFOTM_P02 - André.csv",
                       "StFOTM_P03 - Julia.csv","StFOTM_P04 - Sharon.csv",
                       "StROTM_P01 - Kaique.csv","StROTM_P02 - Thamires.csv",
                       "StROTM_P03 - Thiago.csv","StROTM_P04 - Lydia.csv",
                       "StROTM_P05 - Pedro.csv") ~ 30,
  .$Participant %in% c("StFOTM_P06 - Guilherme.csv","StFOTM_P07 - Eduarda.csv",
                       "StFOTM_P08 - Lucas.csv","StFOTM_P09 - Ana Laura.csv",
                       "StROTM_P06 - Matheus.csv","StROTM_P07 - Abner.csv",
                       "StROTM_P08 - Isadora.csv","StROTM_P09 - Daniel.csv") ~ 14,
  TRUE ~ 0
))

complete_data$Block <- factor(complete_data$Block,
                                  levels=c('treinoab1',"treinoab2","treinoab3","treinoab4","treinoab5","treinoab6",
                                           'treinoac1','treinoac2','treinoac3','treinoac4','treinoac5','treinoac6',
                                           'treinoad1','treinoad2','treinoad3','treinoad4','treinoad5','treinoad6',
                                           'mix1','mix2','mix3','mix4','mix5','mix6',
                                           'eq1','eq2','eq3',
                                           'adrev1','adrev2','adrev3','adrev4','adrev5','adrev6',
                                           'mixrev1','mixrev2','mixrev3','mixrev4','mixrev5','mixrev6',
                                           'reorg1','reorg2','reorg3',
                                           'maint1','maint2','maint3'))

summ_data_part <- complete_data %>% group_by(Group, Participant, Block) %>%
  summarise(Result=mean(Result)*100,Speed=mean(Speed))

summ_data <- complete_data %>% group_by(Group, Block) %>%
  summarise(Result=mean(Result)*100,Speed=mean(Speed))
summ_data_fst_blk <- summ_data %>%
  filter(Block %in% c('treinoab1','treinoac1','treinoad1','mix1','eq1','adrev1',
                      'mixrev1','reorg1'))
summ_data_fst_blk$Block <- factor(summ_data_fst_blk$Block,
                                  levels=c('treinoab1','treinoac1','treinoad1',
                                           'mix1','eq1','adrev1','mixrev1',
                                           'reorg1'))

font_size <- 11


#### Duplo eixo ####################

p <- summ_data_fst_blk %>% ggplot(aes(x=Block,y=Result,fill=Group)) +
  geom_col(color='black', position = 'dodge')

p <- p + geom_line(aes(x=Block,y=Speed/0.008, group=Group)) +
  geom_point(aes(x=Block,y=Speed/0.008,shape=Group),size=3,fill='white') +
  xlab('') + ylab('Acurácia (%)') +
  scale_y_continuous(expand = c(0, 0),limits=c(0, 100), sec.axis=sec_axis(~.*0.008, name = 'Velocidade (r/s)'))
  
p <- p + scale_x_discrete(labels = c('Treino AB','Treino AC','Treino AD','Revisão','Equivalência','Rev. AD','Revisão','Reorganização', 'Manutenção')) +
  scale_fill_manual(values=c("grey90","grey60"),name='',labels=c('Acurácia StF','Acurácia StR')) +
  scale_shape_manual(values = c(21,24),name='',labels = c('Velocidade StF', 'Velocidade StR')) +
  theme_classic() + theme(axis.text=element_text(size=font_size),
        axis.text.x = element_text(color='black',angle = 45,hjust = 1),
        axis.ticks.x = element_blank(),axis.title.y=element_text(size=font_size),
        axis.text.y = element_text(color='black'),legend.position = "bottom",
        legend.text = element_text(size = font_size))

ggsave('Eixo duplo.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
       width = 16, height = 12, units = "cm")

#### Duplo eixo por participante ####

individual_second_axis <-function(participant){
  summ_data_part %>%
    filter(Participant==participant, Block %in% c('treinoab1','treinoac1',
                                                 'treinoad1','mix1','eq1',
                                                 'adrev1','mixrev1','reorg1')) %>%
    ggplot(aes(x=Block,y=Result)) +
    geom_col(color='black', fill='grey90', position = 'dodge') +
    geom_line(aes(x=Block,y=Speed/ajuste_eixo,group = 'Block')) +
    geom_point(aes(x=Block,y=Speed/ajuste_eixo),size=3,fill='white',shape =21) +
    xlab('') + ylab('Acurácia (%)') +
    scale_y_continuous(expand = c(0, 0),limits=c(0, 100),
                       sec.axis=sec_axis(~.*ajuste_eixo, name = 'Velocidade (r/s)'))+
    scale_x_discrete(labels = c('Treino AB','Treino AC','Treino AD','Revisão','Equivalência','Rev. AD','Revisão','Reorganização')) +
    theme_classic() + theme(axis.text=element_text(size=font_size),
                            axis.text.x = element_text(color='black',angle = 45,hjust = 1),
                            axis.ticks.x = element_blank(),axis.title.y=element_text(size=font_size),
                            axis.text.y = element_text(color='black'))+
    ggtitle(paste(substr(participant,1,3),'.',substr(participant,9,10),sep = ''))
  ggsave(paste('eixoduplo_',participant,'.png',sep = ''),device = 'png',
         path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
         width = 15, height = 10, units = "cm")
}

ajuste_eixo <- 0.008
lapply(dir(pattern="OTM_"),individual_second_axis)

ajuste_eixo <- 0.01
individual_second_axis('StFOTM_P04 - Sharon.csv')

ajuste_eixo <- 0.011
individual_second_axis('StROTM_P04 - Lydia.csv')

ajuste_eixo <- 0.009
individual_second_axis('StROTM_P05 - Pedro.csv')

ajuste_eixo <- 0.01
individual_second_axis('StROTM_P07 - Abner.csv')

#### Boxplot ####
complete_data %>% filter(Block %in% c('eq1','reorg1')) %>%
  ggplot(aes(Block,Speed,fill=Group))+geom_boxplot()+
  scale_fill_manual(values=c("grey90","grey60"),name='')+
  scale_x_discrete(labels = c('Equivalência','Reorganização'))+
  xlab('') + ylab('Velocidade (r/s)') + theme_classic() +
  theme(axis.text=element_text(size=font_size,colour = 'black'),
        axis.title=element_text(size=font_size),
        legend.text = element_text(size = font_size))

ggsave('Boxplot.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
       width = 15, height = 12, units = "cm")

#### Densidade ####
complete_data %>% filter(Block %in% c('eq1','reorg1')) %>%
  mutate(Block = case_when(
  .$Block == 'eq1' ~ "Equivalência",
  .$Block == 'reorg1' ~ "Reorganização",
  TRUE~'Erro')) %>%
  ggplot(aes(x=Speed,fill=Group))+geom_density(outline.type = 'full',alpha=0.3)+
  facet_grid(Block~.) +
  xlab('Densidade') + ylab('Velocidade (r/s)') +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  scale_fill_discrete(name='') + theme_classic()  + 
  theme(axis.text=element_text(size=font_size,colour = 'black'),
        axis.title=element_text(size=font_size),
        legend.text = element_text(size = font_size),
        strip.text.x = element_text(size = font_size))

ggsave('Densidade.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
       width = 15, height = 10, units = "cm")


#### Gráficos individuais (Velocidade por tentativa)#############

individual_plots <- function(participant){
  
  complete_data <- complete_data %>% filter(!(Block %in% c('maint1','maint2','maint3')))
  
  blocks_position <- complete_data %>% filter(Participant==participant, BlkTrl==1,
                                              grepl('1',Block)) %>% .$Trl
  blocks_names <- complete_data %>% filter(Participant==participant, BlkTrl==1,
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
      TRUE ~ "Error")
    )
  
  p <- complete_data %>% filter(Participant==participant) %>%
    ggplot(aes(x=Trl,y=Speed))+
    geom_line(color='blue')+
    theme_classic() +
    geom_vline(xintercept = blocks_position)+
    geom_point(aes(color=as.factor(Result)))+scale_color_manual(values=c("red",NA))+
    theme(legend.position = "none")+scale_x_continuous(expand = c(0, 0))
  
  p+geom_text(data=positions,aes(x=x,y=y,label=label),nudge_x = 15, nudge_y = -1) +
    ggtitle(paste(substr(participant,1,3),'.',substr(participant,9,10),sep = '')) +
    xlab('Tentativa') + ylab('Velocidade') +
    theme(axis.text = element_text(colour = 'black'))
  
  ggsave(paste('velocidade',participant,'.png',sep = ''),device = 'png',
         path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
         width = 26, height = 7, units = "cm")
}

setwd("C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Resultados/Experimento 2/R")
lapply(dir(pattern="OTM_"),individual_plots)


#### Manutenção Porcentagem de cada categoria e Monte Carlo####
complete_data %>% filter(Block%in%c('maint1','maint2','maint3')) %>%
  group_by(Participant,category) %>% summarize(n=n()) %>%
  pivot_wider(names_from = category, values_from=n) %>%
  mutate (total=sum(Ambas,Formação,Inconsistente,Reversão,na.rm=T)) %>%
  mutate (for_p = sum(Ambas,Formação,na.rm=T)/total*100,
          rev_p = sum(Ambas,Reversão,na.rm=T)/total*100,
          inc_p = sum(Inconsistente,na.rm=T)/total*100) %>% view()



mcf <- replicate(10000,{
  p <- 1/54
  s <- sample(c('F','R','A','I'),size = 108,replace = T, prob = c(12*p,12*p,6*p,24*p))
  mean(s%in%c('F','A')) >= 0.45
})

mean(mcf)

mci <- replicate(10000,{
  p <- 1/54
  s <- sample(c('F','R','A','I'),size = 108,replace = T, prob = c(12*p,12*p,6*p,24*p))
  mean(s%in%c('I')) < 0.33
})

mean(mci)

#### Wrangling matrix de respostas#######################################
library(xlsx)


create_list <- function(x){
  a <- read.csv(x,sep = ';') %>%
    mutate(Speed=1/Chlat*1000,Trl=1:length(Trl))
  a$Block<-factor(a$Block, levels=unique(a$Block))
  a
}

files <- dir(pattern="StFOTM_")
stf_group <- lapply(files,create_list)
stf_group <- bind_rows(stf_group,.id = "Participant")

files <- dir(pattern="StROTM_")
str_group <- lapply(files,create_list)
str_group <- bind_rows(str_group,.id = "Participant")

complete_data <- bind_rows(stf_group, str_group, .id = "Group")

complete_data <- complete_data %>% mutate(hit_stm = case_when(
  .$HitKey==1 ~ .$ChKey1,
  .$HitKey==2 ~ .$ChKey2,
  .$HitKey==3 ~ .$ChKey3,
  .$HitKey==4 ~ .$ChKey4,
  TRUE ~ "Error"
))

complete_data <- complete_data %>% mutate(Group = case_when(
  .$Group==1 ~ 'StF',
  .$Group==2 ~ 'StR',
  TRUE ~ "Error"
))

r_matrix<- function(x,y,wb){
  sheet <- createSheet(wb, paste(y,'P',x,sep = ''))
  mixrev_data <- complete_data %>%
    filter(Group==y,Participant==x)
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

setwd("C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 2")
wb = createWorkbook()
lapply(1:5, r_matrix,y='Controle',wb=wb)


#### Wrangling manutenção#######################################

setwd("C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Resultados/Experimento 2/Follow-up/R")
rm(list=ls())

create_list <- function(x){
  a <- read.csv(x,sep = ';') %>%
    mutate(Speed=1/Chlat*1000,Trl=1:length(Trl))
  a$Block<-factor(a$Block, levels=unique(a$Block))
  a
}

files <- dir(pattern="StF")
stf_group <- lapply(files,create_list)
stf_group <- bind_rows(stf_group,.id = "Participant")

files <- dir(pattern="StR")
str_group <- lapply(files,create_list)
str_group <- bind_rows(str_group,.id = "Participant")

complete_data <- bind_rows(stf_group, str_group, .id = "Group")

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
  .$Group==1 & .$Participant %in% c(1,2,3,4) ~ 30,
  .$Group==1 & .$Participant %in% c(5,6,7,8) ~ 14,
  .$Group==2 & .$Participant %in% c(1,2,3,4,5) ~ 30,
  .$Group==2 & .$Participant %in% c(6,7,8,9) ~ 14,
  TRUE ~ 0
)) %>%
  
  mutate(Participant = case_when(
  .$Group==1 & .$Participant == 8 ~ 9,
  .$Group==1 & .$Participant == 7 ~ 8,
  .$Group==1 & .$Participant == 6 ~ 7,
  .$Group==1 & .$Participant == 5 ~ 6,
  TRUE ~ as.numeric(.$Participant)
)) %>%
  
  mutate(Group=case_when(
    .$Group==1 ~ 'StF',
    .$Group==2 ~ 'StR',
    TRUE ~ 'Error'
)) %>%

mutate(category=as.factor(category))

complete_data$category <- factor(complete_data$category,
                                 levels=c('Formação','Reversão','Inconsistente','Ambas'))

sum_category <- complete_data %>% group_by(Group,Participant,category) %>%
  count(category)

font_size <- 11

test_data <- complete_data %>% filter(Block=='reorg1')

analysis <-aov(Speed ~ Group,data = test_data)
summary(analysis)

#### Boxplot manutenção ####

font_size <- 11

complete_data %>% filter(Block=='reorg1') %>%
  ggplot(aes(Group,Speed))+
  geom_boxplot()+ theme_classic() + ylab('Velocidade (r/s)') +
  theme(axis.text=element_text(size=font_size,colour = 'black'),
        axis.title.y = element_text(size=font_size,colour = 'black'),
        axis.title.x = element_blank())

ggsave('Manut_Boxplot.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
       width = 15, height = 12, units = "cm")

#### Densidade manutenção ####

complete_data %>% filter(Block=='reorg1') %>%
  ggplot(aes(Speed,fill=Group))+
  geom_density(outline.type = 'full',alpha = 0.3)+
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  theme_classic() + xlab('Velocidade (r/s)') + ylab('Densidade') +
  theme(axis.text=element_text(size=font_size,colour = 'black'),
        axis.title = element_text(size=font_size,colour = 'black'),
        legend.text = element_text(size=font_size,colour = 'black'),
        legend.title = element_blank())

ggsave('Manut_Densidade.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
       width = 15, height = 12, units = "cm")

#### Individual por tentativa - manutenção ####
font_size <- 11

individual_manut <- function(participant){

  fup <- complete_data %>% filter(Participant==participant,
                                  Block%in%c("maint1","maint2","maint3")) %>%
    .$Follow_up %>% unique()
  complete_data %>% filter(Participant==participant,
                           Block%in%c("maint1","maint2","maint3")) %>%
    ggplot(aes(Trl,Speed))+
    geom_line()+
    geom_point(aes(color=category)) +
    scale_color_manual(values = c('blue','green','red','black'),drop=FALSE)+
    scale_x_continuous(expand = c(0, 0))+
    theme_classic() + xlab('Tentativa') + ylab('Velocidade') +
    theme(axis.text=element_text(size=font_size,colour = 'black'),
          axis.title.y = element_text(size=font_size),
          legend.title = element_blank(),
          legend.position = 'none') +
    ggtitle(paste(substr(participant,1,3),'.',substr(participant,9,10),' - ',
                  fup,' dias',sep = ''))
  
  ggsave(paste('manut_',participant,'.png',sep = ''),device = 'png',
         path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
         width = 26, height = 6, units = "cm")
}

setwd("C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Resultados/Experimento 2/Follow-up/R")

lapply(dir(pattern="OTM_"),individual_manut)

#### Individual de barras ####

font_size <- 11

# Tive que usar tryCatch para um pular um erro gerado pelo uso de drop=FALSE
# quando não há participante (P5, por exemplo)

manut_ind_bar <- function(x,y) tryCatch({
  
  fup <- complete_data %>% filter(Group==y,Participant==x) %>%
    .$Follow_up %>% pluck(1)
  
  complete_data %>% filter(Group==y,Participant==x) %>%
    
    ggplot(aes(x=category)) + geom_bar(color='black',fill='grey90') +
    scale_y_continuous(expand = c(0,0)) + scale_x_discrete(drop = FALSE) +
    xlab('') + ylab('Frequência') + theme_classic() +
    theme(axis.title = element_text(colour='black',size=font_size),
          axis.text = element_text(colour='black',size=font_size))+
    ggtitle(paste(y,'_P',x,' ',fup,' dias',sep=''))
  
  ggsave(paste('man_bar',y,'P',x,'.png',sep = ''),device = 'png',
         path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação",
         width = 15, height = 10, units = "cm")
  
}, error = function(e) e)

lapply(1:10, manut_ind_bar,y='StR')

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
  mutate(Tentativa=1:9, Relação=rep('AB',9))
AC <- mixrev %>% filter(trial_type=='AC') %>%
  group_by(BlkTrl) %>% summarise(Result=sum(Result)) %>%
  mutate(Tentativa=1:9, Relação=rep('AC',9))
AD <- mixrev %>% filter(trial_type=='AD') %>%
  group_by(BlkTrl) %>% summarise(Result=sum(Result)) %>%
  mutate(Tentativa=1:9, Relação=rep('ADr',9))

bind_rows(AB,AC,AD) %>%
  ggplot(aes(Tentativa,Result,group=Relação,shape=Relação,
             linetype=Relação, fill=Relação)) +
  geom_line()+ scale_linetype_manual(values=c('solid','longdash','dotted'))+
  geom_point(aes(size=Relação),stroke=1,fill='white') +
  scale_shape_manual(values = c(21,24,22)) +
  scale_size_manual(values = c(4,3,3)) +
  coord_cartesian(c(1,9),c(0,19),clip = 'off')+
  scale_y_continuous(breaks=seq(0,19,2),expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(breaks=c(1:9),expand = expansion(mult = c(.03, 0))) +
  ylab('N. de participantes') + theme_classic() +
  theme(legend.title = element_blank(),
        legend.text = element_text(color = 'black', size=font_size),
        axis.title = element_text(color = 'black', size=font_size),
        axis.text = element_text(color = 'black', size=font_size))

ggsave('adrev.png',device = 'png',
       path = "C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 2",
       width = 13, height = 8, units = "cm")

#### Wrangling matrix de respostas#######################################
library(xlsx)

all.files <- dir(pattern="OTM_")

create_list <- function(x){
  a <- read.csv(x,sep = ';') %>%
    mutate(Speed=1/Chlat*1000,Trl=1:length(Trl))
  a$Block<-factor(a$Block, levels=unique(a$Block))
  a
}

files <- dir(pattern="StFOTM_")
adr_group <- lapply(files,create_list)
names(adr_group) <- files
adr_group <- bind_rows(adr_group,.id = "Participant")

files <- dir(pattern="StROTM_")
cont_group <- lapply(files,create_list)
names(cont_group) <- files
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
  .$Group==1 ~ 'StF',
  .$Group==2 ~ 'StR',
  TRUE ~ "Error"
))

r_matrix <- function(x,wb){
  sheet <- createSheet(wb, paste(x,sep = ''))
  mixrev_data <- complete_data %>%
    filter(Participant==x) %>%
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

setwd("C:/Users/Giovan WR/Documents/UFSCar/Doutorado/Qualificação/Experimento 2")
wb = createWorkbook()
lapply(all.files, r_matrix,wb=wb)


#### Correlação ####

data_bl <- complete_data %>% filter(Block=='reorg1')
data_bl <- data_bl %>% filter(!(Group=='StF' & Participant%in%c(5,10)))

data_maint <- complete_data %>% filter(Block=='reorg1')

speed_maint<-data_maint%>%.$Speed

data_all <- data_bl %>% mutate(SpeedMaint=speed_maint)

sum_data_all<-data_all%>%group_by(Group,Participant)%>%summarise(Speed=mean(Speed),SpeedMaint=mean(SpeedMaint))

sum_data_all %>% ggplot(aes(Speed,SpeedMaint)) +
  geom_line()

speed_form<-data_bl$Speed
data_maint<-data_maint%>%mutate(SpeedF=speed_form)
data_maint %>% mutate(inc = category%in%c('Inconsistente')) %>%
  group_by(Group,Participant) %>% summarise(SpeedF=mean(SpeedF),inc=sum(inc)) %>%
  ggplot(aes(x=SpeedF,y=inc))+geom_line()
