###�W�ұЧ�###
setwd("C:/Users/ErinKuo/Desktop/r_class")
# Day1 --------------------------------------------------------------------
#install.packages("ggplot2")
library(ggplot2)
#library(tidyverse)
library(gridExtra) #�e������
set.seed(100)
df <- diamonds[sample(1:nrow(diamonds),5000),]
#�{�Ѹ��
View(df)
str(df)
summary(df)
table(df$cut)
#�令����r
df <- df %>% 
  mutate(cut = case_when(
    cut == "Fair" ~ "���q",
    cut == "Good" ~ "�n",
    cut == "Very Good" ~ "�D�`�n",
    cut == "Premium" ~ "�W�S��",
    cut == "Ideal" ~ "����"
  ))
table(df$cut)

#�p���^��
ggplot(df, aes(cut, price)) + 
  geom_violin() 
#�ХL��fill�i�H���C��ϧO���P���O
ggplot(df, aes(cut, price,fill = cut)) + 
  geom_violin() 
#�u�n�Y���ܼƪ���
ggplot(df, aes(cut, price,fill = cut)) + 
  geom_violin()  + 
  scale_x_discrete(limits=c("���q", "�n", "�D�`�n"))

ggplot(df, aes(cut, price,fill = cut)) + 
  geom_violin()  + 
  scale_x_discrete(limits=c("���q", "�n", "�D�`�n")) +
  facet_wrap(~ clarity)

ggplot(df, aes(cut, price,fill = cut)) + 
  geom_violin()  + 
  scale_x_discrete(limits=c("���q", "�n", "�D�`�n")) +
  facet_wrap(~ clarity) +
  theme_dark()

#boxplot
ggplot(df, aes(cut, price)) +
  geom_boxplot()
#�ХL�̾ާ@fill,�H�αЧ���D�ήy�ЦW��
ggplot(df, aes(cut, price,fill=cut)) +  
  geom_boxplot()+
  labs(x = "���u", y = "����", title = "��Ž��")   

#�c�l���~���C��Poutlier�C��M�ϧ�
ggplot(df, aes(cut, price,fill=cut)) +
  geom_boxplot(colour = "#3366FF",
               outlier.colour = "red", 
               outlier.shape = 1)+
  labs(x = "���u", y = "����", title = "��Ž��")  

#��b
ggplot(df, aes(cut, price,fill=cut)) +
  geom_boxplot(colour = "#3366FF",
               outlier.colour = "red", 
               outlier.shape = 1)+
  labs(x = "���u", y = "����", title = "��Ž��")  +
  coord_flip()

#�e������
grid.arrange(ggplot(df, aes(cut, price,fill = cut)) + 
               geom_violin()+
               labs(x = "���u", y = "����", title = "�p���^��")   ,
             
             ggplot(df, aes(cut, price,fill=cut)) +  
               geom_boxplot()+
               labs(x = "���u", y = "����", title = "��Ž��")  ,
             
             nrow = 1, ncol = 2)


# hist �s���ܼơA�����C�Ӷ��j��500
#�̰򥻪��e�k
ggplot(df, aes(x = price)) +
  geom_histogram(binwidth = 500)
#�[�W�C��&���D&�y�ЦW��
ggplot(df, aes(x = price)) +
  geom_histogram(binwidth = 500,  
                 fill = "orange",
                 color = "red",
                 aes(y=..density..)) +
  labs(x = "����", y = "�Ӽ�", title = "�����") +  
  theme(axis.title.x = element_text(color='brown',face = "bold"),
        axis.title.y = element_text(color='brown',face = "bold")) +
  geom_density(stat="density", alpha=I(0.2), fill="blue")

#
ggplot(df, aes(x = price, fill = cut)) +
  geom_histogram(binwidth = 500) +
  labs(x = "����", y = "�Ӽ�", title = "�����") +  
  theme(axis.title.x = element_text(color='brown',face = "bold"),
        axis.title.y = element_text(color='brown',face = "bold")) +
  geom_density(stat="density", alpha=I(0.2), fill="blue") + #�Ͷսu
  facet_wrap( ~ cut) 
