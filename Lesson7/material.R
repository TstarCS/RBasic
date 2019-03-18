###上課教材###
setwd("C:/Users/ErinKuo/Desktop/r_class")
# Day1 --------------------------------------------------------------------
#install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(gridExtra) #畫面切割
library(ggcorrplot) #筆電不能做
library(car)

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
#只要某些類別的圖
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
  labs(x = "價格", y = "密度", title = "直方圖") +  
  theme(axis.title.x = element_text(color='brown',face = "bold"),
        axis.title.y = element_text(color='brown',face = "bold")) +
  geom_density(stat="density", alpha=I(0.2), fill="blue") #alpha表示透明度

#
ggplot(df, aes(x = price, fill = cut)) +
  geom_histogram(binwidth = 500) +
  labs(x = "價格", y = "個\n數", title = "直方圖") +  
  theme(axis.title.x = element_text(color='brown',face = "bold"),
        axis.title.y = element_text(color='brown',face = "bold", angle=0, vjust = 0.5)) +
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
library(tidyverse)
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
library(tidyverse)
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
  stat_smooth(se = T) #解釋跳出的warning
#線性
ggplot(df, aes(carat, price)) +  
  geom_point(stat = "identity",
             position = "identity") + 
  stat_smooth(se = T,method = "lm")
#回到model=default,根據不同切割方式比較
ggplot(df, aes(carat, price)) +  
  geom_point(stat = "identity",
             position = "identity") + 
  stat_smooth(se = T)+
  facet_wrap(~cut)

#調整shape,給他們練習
ggplot(df, aes(carat, price)) +  #這裡是資料集及軸及顏色設定
  geom_point(stat = "identity",
             position = "identity",
             shape=17)  + 
  stat_smooth(se = T)+
  facet_wrap(~cut)
#給他們圖形,自己畫畫看。Scatter-加上顏色以不同CUT區分-
ggplot(df, aes(carat, price, colour = cut)) +  #這裡是資料集及軸及顏色設定
  geom_point(stat = "identity",
             position = "identity")+
  labs(x = "克拉", y = "價格", title = "散布圖") +  
  theme(axis.title.x = element_text(color='darkgreen',face = "bold"),
        axis.title.y = element_text(color='darkgreen',face = "bold"))
#筆電不能做#當變數很多，不想要一個一個慢慢畫時，可以一次看到多變項間彼此關係
library(car)
scatterplotMatrix(~carat+price+depth+table,
                  data=df,
                  main="Scatter Plot Matrix")


#Correlogram
library(ggcorrplot) #筆電不能做
df1 <- df[sapply(df, is.numeric)]
corr <- round(cor(df1),1)
View(corr)
#把相關係數畫成圖形
library(car)
ggcorrplot(corr, 
           hc.order = F,  
           type = "lower", 
           lab = TRUE,  
           lab_size = 3,  
           method="circle", 
           colors = c("red", "white", "lightgreen"), 
           title="Correlogram of diamonds", 
           ggtheme=theme_bw)
#相同的,用來文字說明
# ggcorrplot(corr, 
#            hc.order = F,  #是否把相關係數大小作排序
#            type = "lower", #full,upper,lower
#            lab = TRUE,  #要不要標示數字
#            lab_size = 3,  #字型大小
#            method="circle", #square
#            colors = c("red", "white", "lightgreen"), #漸層顏色
#            title="Correlogram of diamonds", 
#            ggtheme=theme_bw)

#熱力圖----------------
#用我們的df-類別
heat <- df %>% 
  group_by(cut,color) %>% 
  summarise(number=n()) %>% 
  ungroup()

ggplot(heat, aes(cut, color)) + 
  geom_tile(aes(fill = number)) + 
  theme_bw() + 
  scale_fill_gradient(low="white", high="red") +  #調整顏色
  labs(x = "切工", y = "顏色", title = "熱力圖") 
# ggplot(df, aes(clarity, cut)) + 
#   geom_tile(aes(fill = price)) + 
#   theme_bw() + 
#   scale_fill_gradient(low="white", high="red")

#匯出圖形
getwd()
ggsave(plot, file="plot2.png")
ggsave(plot, file="plot3.png", width=6, height=4, units = "in")

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
mosaic(Titanic,
       shade = T,
       legend = T,
       main = "Survival on the Titanic") #說明相關係數


# HW2 ----------------------------------------------------------------------
library(tidyverse)
library(car)

hw1 <- read.csv("//vibo/nfs/CHPublic/客戶服務事業部/部門公用區/R語言課程/HW2/課程內容及題目/Lottery.csv",fileEncoding = 'UTF-8') %>% 
  select(ID,birthymd="出生日期",buyymd="購買日期",sex="性別",
         pay="投注金額",get="中獎金額",area="購買地區",religion="宗教信仰")  
#hw2 <- read.csv("//vibo/nfs/CHPublic/客戶服務事業部/部門公用區/R語言課程/HW7/課程內容及題目/df2.csv",fileEncoding = 'UTF-8')

hw1_1 <- hw1 %>%  #將
  mutate(birthymd=as.Date(birthymd),
         age = as.integer(round((Sys.Date()-birthymd)/365,0)),
         get=as.integer(get),
         sex = case_when(
           sex == "汝" ~ "女", 
           sex == "女" ~ "女",
           sex == "男" ~ "男"
         ))

set.seed(100)
hw1_2 <- hw1_1[sample(1:nrow(hw1_1),3000),]

scatterplotMatrix(~pay+get+age|sex,
                  regLine=FALSE,
                  data=hw1_2,
                  spread=FALSE,
                  lty=2,
                  main="Scatter Plot Matrix",
                  diagonal=list(method="boxplot"),
                  pch=2:3, #pch=2三角形 =3+
                  col= c("green","orange") #不能用default顏色
                  )

# Day 3 -------------------------------------------------------------------


#善行圖
ggplot(mpg)+geom_bar(width=1, aes(x=factor(1),fill=mpg$class))+
  coord_polar(theta="y") #把y軸方向扭曲,柱子變成彎的


#玫瑰花圖
ggplot(mpg, aes(x = factor(mpg$class),fill=mpg$class)) +
  geom_bar(width = 0.7,aes(color=factor(mpg$class))) + 
  coord_polar(theta="x") #把x軸方向扭曲,柱子變成彎的
ggplot(mpg, aes(x = factor(mpg$class),fill=mpg$class)) +
  geom_bar(width = 0.7,aes(color=factor(mpg$class))) + 
  coord_polar(theta="y") #把y軸方向扭曲,柱子變成彎的