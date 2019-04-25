###上課教材###
setwd("C:/Users/ErinKuo/Desktop/r_class")
setwd("D:/ErinKuo/Desktop/Lesson7/")
# Day2 --------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
set.seed(100)
df <- diamonds[sample(1:nrow(diamonds),5000),]
###################
#馬賽克
#install.packages("vcd")
library(vcd)
ftable(Titanic) #描述鐵達尼號故事，介紹資料
mosaic(~ Class + Sex, 
       data = Titanic,
       main = "Titanic")
mosaic(~ Class + Sex + Age, 
       data = Titanic,
       main = "Titanic")
mosaic(Titanic,
       main = "Survival on the Titanic")
mosaic(~ Class + Survived, 
       data = Titanic,
       main = "Survival on the Titanic",
       shade = T,
       legend = T)
mosaic(Titanic,
       shade = T,
       legend = T,
       main = "Survival on the Titanic") #說明相關係數
# Day 3 -------------------------------------------------------------------
library(ggplot2)
library(dplyr)
set.seed(100)
df <- diamonds[sample(1:nrow(diamonds),5000),]
#basic bar
ggplot(df, aes(x=as.factor(clarity))) + 
  geom_bar() + 
  labs(x = "純淨度", y="數量", title = "鑽石")
#bar
ggplot(df, aes(x=as.factor(clarity), fill=clarity)) + 
  geom_bar() + 
  labs(x = "純淨度", y="數量", title = "鑽石")
#詳細可參考之前的顏色檔案
ggplot(df, aes(x=as.factor(clarity), fill=clarity)) + 
  geom_bar() + 
  labs(x = "純淨度", y="數量", title = "鑽石") + 
  scale_fill_brewer(palette = "Set1") 
#flip
ggplot(df, aes(x=as.factor(clarity), fill=clarity)) + 
  geom_bar() + 
  scale_fill_brewer(palette = "Set1") +
  coord_flip()
#order 
df1 <- df %>% 
  group_by(clarity) %>%  
  summarise(val = n()) %>% 
  mutate(clarity = fct_reorder(clarity, val))
ggplot(df1, aes(x=clarity, y = val,fill=clarity)) + 
  geom_bar(stat = "identity") + 
  labs(x = "純淨度", y="數量", title = "鑽石") + 
  scale_fill_brewer(palette = "Set1") +
  coord_flip()

#雷達圖
library(fmsb)
set.seed(99)
data=as.data.frame(matrix(sample(0:20,15,replace=F),ncol=5))
colnames(data)=c("Math","English","Biology","Music","R-coding")
rownames(data)=paste0("學生",letters[1:3])
View(data)
#加雷達圖的最大最小值
data=rbind(rep(20,5) , rep(0,5) , data)
View(data)
#Default radar
radarchart(data)
#Custom features
#調整透明度用rgb，檔案放在公網可參考
colors_border=c(rgb(0.2,0.5,0.5,0.9),
                rgb(0.8,0.2,0.5,0.9) ,
                rgb(0.7,0.5,0.1,0.9))
colors_in=c(rgb(0.2,0.5,0.5,0.4),
            rgb(0.8,0.2,0.5,0.4) ,
            rgb(0.7,0.5,0.1,0.4))
radarchart(data,
           axistype=1 , #軸的型態，0-5 
           pcol=colors_border , 
           pfcol=colors_in , 
           plwd=2 ,  #線的粗度
           plty=1,   #線的型態，1-6
           cglcol="grey", #雷達外框顏色
           cglty=1, #雷達外框線型
           axislabcol="darkgreen", #雷達軸數字
           caxislabels=seq(0,20,5), 
           cglwd=2,  #雷達外框線粗度
           vlcex=1 , #各科字體大小
           title = "研究生能力雷達圖"
)
legend(x=0.8,  #調整圖例位置
       y=0.8, 
       legend = rownames(data[-c(1,2),]), 
       bty = "n", 
       pch=20 , 
       col=colors_in , 
       text.col = "darkblue", 
       cex=1, #圖例字大小
       title = "學生代表")