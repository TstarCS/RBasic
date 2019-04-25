library(tidyverse)
library(car)

hw1 <- read.csv("//vibo/nfs/CHPublic/�Ȥ�A�ȨƷ~��/�������ΰ�/R�y���ҵ{/HW2/�ҵ{���e���D��/Lottery.csv",fileEncoding = 'UTF-8') %>% 
  select(ID,birthymd="�X�ͤ��",buyymd="�ʶR���",sex="�ʧO",
         pay="��`���B",get="�������B",area="�ʶR�a��",religion="�v�ЫH��")  

hw1_1 <- hw1 %>%  
  mutate(birthymd=as.Date(birthymd),
         age = as.integer(round((Sys.Date()-birthymd)/365,0)),
         get=as.integer(get),
         sex = case_when(
           sex == "��" ~ "�k", 
           sex == "�k" ~ "�k",
           sex == "�k" ~ "�k"
         ))

set.seed(100)
hw1_2 <- hw1_1[sample(1:nrow(hw1_1),3000),]
scatterplotMatrix(~pay+get+age|sex,
                  regLine=F,
                  data=hw1_2,
                  spread=F,
                  lty=2,
                  main="Scatter Plot Matrix",
                  diagonal=list(method="boxplot"),
                  pch=2:3, #pch=2�T���� =3+
                  col= c("green","orange") #�����default�C��
                  )
				  