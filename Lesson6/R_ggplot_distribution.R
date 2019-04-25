###上課教材###
setwd("C:/Users/ErinKuo/Desktop/r_class")
# Day1 --------------------------------------------------------------------
#install.packages("ggplot2")
library(ggplot2)
#library(tidyverse)
library(gridExtra) #畫面切割
set.seed(100)
df <- diamonds[sample(1:nrow(diamonds),5000),]
#認識資料
View(df)
str(df)
summary(df)
table(df$cut)
#改成中文字
df <- df %>% 
  mutate(cut = case_when(
    cut == "Fair" ~ "普通",
    cut == "Good" ~ "好",
    cut == "Very Good" ~ "非常好",
    cut == "Premium" ~ "獨特的",
    cut == "Ideal" ~ "完美"
  ))
table(df$cut)

#小提琴圖
ggplot(df, aes(cut, price)) + 
  geom_violin() 
#教他們fill可以用顏色區別不同類別
ggplot(df, aes(cut, price,fill = cut)) + 
  geom_violin() 
#只要某些變數的圖
ggplot(df, aes(cut, price,fill = cut)) + 
  geom_violin()  + 
  scale_x_discrete(limits=c("普通", "好", "非常好"))

ggplot(df, aes(cut, price,fill = cut)) + 
  geom_violin()  + 
  scale_x_discrete(limits=c("普通", "好", "非常好")) +
  facet_wrap(~ clarity)

ggplot(df, aes(cut, price,fill = cut)) + 
  geom_violin()  + 
  scale_x_discrete(limits=c("普通", "好", "非常好")) +
  facet_wrap(~ clarity) +
  theme_dark()

#boxplot
ggplot(df, aes(cut, price)) +
  geom_boxplot()
#請他們操作fill,以及教改標題及座標名稱
ggplot(df, aes(cut, price,fill=cut)) +  
  geom_boxplot()+
  labs(x = "切工", y = "價格", title = "盒鬚圖")   

#箱子的外框顏色與outlier顏色和圖形
ggplot(df, aes(cut, price,fill=cut)) +
  geom_boxplot(colour = "#3366FF",
               outlier.colour = "red", 
               outlier.shape = 1)+
  labs(x = "切工", y = "價格", title = "盒鬚圖")  

#轉軸
ggplot(df, aes(cut, price,fill=cut)) +
  geom_boxplot(colour = "#3366FF",
               outlier.colour = "red", 
               outlier.shape = 1)+
  labs(x = "切工", y = "價格", title = "盒鬚圖")  +
  coord_flip()

#畫面切割
grid.arrange(ggplot(df, aes(cut, price,fill = cut)) + 
               geom_violin()+
               labs(x = "切工", y = "價格", title = "小提琴圖")   ,
             
             ggplot(df, aes(cut, price,fill=cut)) +  
               geom_boxplot()+
               labs(x = "切工", y = "價格", title = "盒鬚圖")  ,
             
             nrow = 1, ncol = 2)


# hist 連續型變數，說明每個間隔為500
#最基本的畫法
ggplot(df, aes(x = price)) +
  geom_histogram(binwidth = 500)
#加上顏色&標題&座標名稱
ggplot(df, aes(x = price)) +
  geom_histogram(binwidth = 500,  
                 fill = "orange",
                 color = "red",
                 aes(y=..density..)) +
  labs(x = "價格", y = "個數", title = "直方圖") +  
  theme(axis.title.x = element_text(color='brown',face = "bold"),
        axis.title.y = element_text(color='brown',face = "bold")) +
  geom_density(stat="density", alpha=I(0.2), fill="blue")

#
ggplot(df, aes(x = price, fill = cut)) +
  geom_histogram(binwidth = 500) +
  labs(x = "價格", y = "個數", title = "直方圖") +  
  theme(axis.title.x = element_text(color='brown',face = "bold"),
        axis.title.y = element_text(color='brown',face = "bold")) +
  geom_density(stat="density", alpha=I(0.2), fill="blue") + #趨勢線
  facet_wrap( ~ cut) 

