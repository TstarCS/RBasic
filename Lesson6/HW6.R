# HW6 ----------------------------------------------------------------------
library(tidyverse)
setwd("//vibo/nfs/CHPublic/�Ȥ�A�ȨƷ~��/�������ΰ�/R�y���ҵ{/HW6/�ҵ{���e���D��/")
member <- read.csv("�H���W��.csv",
                   col.names = c("dep","team","name","gender"))
#���=dep ; �էO=team ; �H��=name ; �ʧO=gender
BC10801 <- read.csv("BC10801.csv",
                    col.names = c("name","billcycle","totaldebt","receive"))
BC10802 <- read.csv("BC10802.csv",
                    col.names = c("name","billcycle","totaldebt","receive")) 
#�H���m�W=name ; �b��g��=billcycle ; �e�ת��B=totaldebt ; �^�����B=receive
#typo1-2��b�P�X�{1��b�P
table(BC10802$billcycle)
BC10802$billcycle[BC10802$billcycle=="2019/1/20"] <- "2019/2/20"
#typo2-�^�����B�W�L�e�ת��B
str(BC10802)
BC10802 <- BC10802 %>% 
  mutate(diff=totaldebt-receive)
summary(BC10802) #�o�{���t��
BC10802$receive[BC10802$diff == -102] <- BC10802$totaldebt[BC10802$diff == -102] #��^�������B�վ㦨�e�ת��B
BC108 <- bind_rows(BC10801,BC10802 %>% 
            select(name,billcycle,totaldebt,receive)) #�X�{warning����]�O����ɮת�billcycle�]�l�ӼƤ��@��

df <- right_join(member,BC108,by="name")
summary(df)

ggplot(df, aes(team, receive,fill = gender)) + 
  geom_violin() +   
  facet_wrap(~ gender) +
  labs(x = "�էO", y = "�^�����B", title = "�p���^��")  +  
  theme(axis.title.x = element_text(color='darkblue',face = "bold"),
        axis.title.y = element_text(color='darkblue',face = "bold", angle=0, vjust = 0.5),
        title =  element_text(color="darkgreen",face = "bold")) + 
  scale_fill_discrete(name="�ʧO")
