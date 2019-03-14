###上課教材###
setwd("C:/Users/ErinKuo/Desktop/r_class")
# Day1 --------------------------------------------------------------------
#install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
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

#
# ggplot(df, aes(x = price)) +
#   geom_histogram(binwidth = 500, 
#                  aes(fill=..count..)) +
#   labs(x = "價格", y = "個數", title = "直方圖") +  
#   theme(axis.title.x = element_text(color='brown',face = "bold"),
#         axis.title.y = element_text(color='brown',face = "bold")) +
#   geom_density(stat="density", alpha=I(0.2), fill="blue") +
#   scale_fill_gradient("Count", low="green", high="red")

# 區別fill放的位置
# ggplot(df, aes(price, fill = cut)) +
#   geom_histogram(binwidth = 500)+
#   labs(x = "價格", y = "個數", title = "直方圖")

# HW1 ----------------------------------------------------------------------

member <- read.csv("C:/Users/ErinKuo/Desktop/r_class/HW/人員名單.csv",
                   col.names = c("dep","team","name","gender"))
  #單位=dep ; 組別=team ; 人員=name ; 性別=gender
BC10801 <- read.csv("C:/Users/ErinKuo/Desktop/r_class/HW/BC10801.csv",
                    col.names = c("name","billcycle","totaldebt","receive"))
BC10802 <- read.csv("C:/Users/ErinKuo/Desktop/r_class/HW/BC10802.csv",
              col.names = c("name","billcycle","totaldebt","receive")) 
  #人員姓名=name ; 帳單週期=billcycle ; 委案金額=totaldebt ; 回收金額=receive
#typo1-2月帳周出現1月帳周
table(BC10802$billcycle)
BC10802$billcycle[BC10802$billcycle=="2019/1/20"] <- "2019/2/20"
#typo2-回收金額超過委案金額
str(BC10802)
BC10802 <- BC10802 %>% 
  mutate(diff=totaldebt-receive)
summary(BC10802) #發現有負值
BC10802$receive[BC10802$diff == -102] <- BC10802$totaldebt[BC10802$diff == -102]
BC108 <- bind_rows(BC10801,BC10802 %>% select(name,billcycle,totaldebt,receive))

df <- right_join(member,BC108,by="name")
summary(df)

ggplot(df, aes(team, receive,fill = gender)) + 
  geom_violin() +   
  facet_wrap(~ gender) +
  labs(x = "組別", y = "回收金額", title = "小提琴圖")  +  
  theme(axis.title.x = element_text(color='darkblue',face = "bold"),
        axis.title.y = element_text(color='darkblue',face = "bold", angle=0, vjust = 0.5),
        title =  element_text(color="darkgreen",face = "bold")) + 
  scale_fill_discrete(name="性別")
  

# Day2 --------------------------------------------------------------------
library(ggplot2)
set.seed(100)
df <- diamonds[sample(1:nrow(diamonds),5000),]

#Scatter
ggplot(df, aes(carat, price)) +   
  geom_point(stat = "identity",
             position = "identity") 

#Scatter-加上信心水準default為0.95
ggplot(df, aes(carat, price)) +  
  geom_point(stat = "identity",
             position = "identity") + 
  geom_smooth(method="lm",se = T) #se為信心水準

#調整shape,給他們練習
ggplot(df, aes(carat, price)) +  #這裡是資料集及軸及顏色設定
  geom_point(stat = "identity",
             position = "identity",
             shape=17) 
#Scatter-加上顏色以不同CUT區分
ggplot(df, aes(carat, price, colour = cut)) +  #這裡是資料集及軸及顏色設定
  geom_point(stat = "identity",
             position = "identity")+
  labs(x = "克拉", y = "價格", title = "散布圖") +  
  theme(axis.title.x = element_text(color='darkgreen',face = "bold"),
        axis.title.y = element_text(color='darkgreen',face = "bold"))
#Scatter-
ggplot(df, aes(carat, price, colour = cut)) +  #這裡是資料集及軸及顏色設定
  geom_point(stat = "identity",
             position = "identity")+
  labs(x = "克拉", y = "價格", title = "散布圖") +  
  theme(axis.title.x = element_text(color='darkgreen',face = "bold"),
        axis.title.y = element_text(color='darkgreen',face = "bold")) 


#Correlogram
library(ggcorrplot)
df1 <- df[sapply(df, is.numeric)]
corr <- round(cor(df1),1)
ggcorrplot(df, hc.order = TRUE,  #尚未於此電腦run過
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), #漸層顏色
           title="Correlogram of diamonds", 
           ggtheme=theme_bw)


#馬賽克
#install.packages("vcd")
library(vcd)
mosaic(Titanic,shade=T,legend=T)

#Heatmap
dfff <- df[,c("carat","price")]
corr2 <- round(cor(dfff),1)
ggplot(dfff, aes(carat, price)) + 
  geom_bin2d(stat = "bin2d",
           position = "identity")

# HW2 ----------------------------------------------------------------------



