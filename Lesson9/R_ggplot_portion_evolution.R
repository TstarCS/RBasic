###上課教材###
setwd("C:/Users/ErinKuo/Desktop/r_class")
setwd("D:/ErinKuo/Desktop/Lesson7/")
# Day3 --------------------------------------------------------------------
#文字雲
#先安裝cran #檔案放於公網Lchiffon-wordcloud2-8a12a3b.tar
########Mac安裝法########
install.packages('devtools')
# Method 1
devtools::install_github('lchiffon/wordcloud2')
# Method 2
library(devtools)
install_github('lchiffon/wordcloud2')
########Mac安裝法end########

library(tidyverse)
library(wordcloud2) 

###########英文字之文字雲Example###########
head(demoFreq)
#基本雲
wordcloud2(demoFreq, 
           size=0.5,
           color='random-dark',
           fontFamily = "Cambria")
#星形
wordcloud2(demoFreq, 
           size=0.5,
           color='random-dark',
           shape = 'pentagon',
           fontFamily = "Cambria")
#排列成文字
letterCloud(demoFreq, word = '\U2603', 
            color='white' , 
            backgroundColor="black")
			
###########中文字之文字雲Example###########
setwd("D:/ErinKuo/Desktop/Lesson7")
library(jiebaR)
library(plyr)
data_cust <- read.csv("D:/ErinKuo/Desktop/Lesson7/Inbound_CT.csv")   #讀取資料

###########斷詞 part ###########
#挑出要擷取的文字向量
words <- data_cust$calliii[1:10000] 
#將原文字內容，剔除標點符號、數字及英文後，變成一個物件
combindString <- function(s){
  tem <- ''
  for (i in 1:length(s))tem <- paste(tem,s[i],sep = "")
  tem <- gsub("[ ，#+、【】!？。：0-9a-zA-Z/():@-_]","",tem) #除去標點符號、數字、英文字母
  tem
}
title<-combindString(words) #獲取所有的標題描述並連接

setwd("D:/ErinKuo/Desktop/Lesson7") #設置工作目錄
analyzer <- worker(type = "mix",stop_word = "stop_word.txt") #stop_word停止詞
newww <- readLines("add_word.txt", encoding= 'cp950') #讀取新增詞檔案
new_user_word(analyzer,newww) #新增定義詞
results<-(analyzer<=title)    #將文字切割
	#將文字切割的第二種方法
	seg <- segment(title, analyzer, mod='mix')

###########轉成data frame part###########
wordseggg<-data.frame(cust_memo=results,
                      stringsAsFactors = F) #將切完字的向量變成data.frame
wordseggg$frequency <- 1  #每個詞建立出現的頻率
#求和的function
sumBygroup<-function(df)sum(df[,2])
#各詞出現的頻率~
groupstatis<-ddply(wordseggg,.(cust_memo),sumBygroup) %>%  #與table不同在於:仍為data.frame  。 table(wordseggg$cust_memo)
  select(cust_memo,frequency=V1) 
#定義顏色向量
#使用詞雲函數
wordcloud2(groupstatis, color = 'random-dark',
           backgroundColor = "white",
           size = 0.42, shape = 'circle')

letterCloud(groupstatis, 
            word = "TSTAR",
            color='white' , 
            backgroundColor="black",
            size = 1)

groupstatis %>% ## 詞彙頻率圖:看頻率大於500的詞彙detail
  filter(frequency > 500) %>%
  mutate(word = reorder(cust_memo, frequency)) %>%
  ggplot(aes(word, frequency)) +
  geom_col(fill = "darkgreen") +
  xlab(NULL) +
  coord_flip()

# HW3 ---------------------------------------------------------------------
#將學過的圖形結合在工作中
# Day4 --------------------------------------------------------------------
###比例圖
library(ggplot2)
set.seed(100)
df <- diamonds[sample(1:nrow(diamonds),5000),]
#圓餅圖
#ggplot的圓餅圖基底是堆疊的長條圖
ggplot(df) + 
  geom_bar(width=1, aes(x=factor(1),fill=clarity)) + #主要是要呈現fill，x=factor(1)只是一個手段
  scale_fill_brewer(palette = "Set1")

#圓餅圖
ggplot(df) + 
  geom_bar(width=1, aes(x=factor(1),fill=clarity))+
  scale_fill_brewer(palette = "Set1") +
  coord_polar(theta="y")   #把y軸方向扭曲,柱子變成彎的。可試試看x

#如果不想顯示多餘的座標
ggplot(df) + 
  geom_bar(width=1, aes(x=factor(1),fill=clarity))+
  scale_fill_brewer(palette = "Set1") +
  coord_polar(theta="y") +  
  labs(x = '', y = '', title = '') +  #把x軸和y軸拿掉
  theme(axis.text = element_blank(),  #文字
        axis.ticks = element_blank()) #tick

#玫瑰花圖
ggplot(df, aes(x = factor(clarity),fill=clarity)) +
  geom_bar(width = 0.7,aes(color=factor(clarity))) + 
  coord_polar(theta="x") + #把x軸方向扭曲,柱子變成彎的。可自行嘗試y
  scale_fill_brewer(palette = "Set1") +  
  labs(x = '', y = '', title = '') +  #把x軸和y軸拿掉
  theme(axis.ticks = element_blank(), #tick
        axis.text.y= element_blank())  

###Evolution
library(dygraphs)
library(xts)          

#建立一時間序列資料集
data=data.frame(time=seq(from=Sys.Date()-40,to=Sys.Date(),by=1),
                value=sample(200:500,41))
data=xts(x = data$value, order.by = data$time) #根據時間先後順序排序
dygraph(data,
        main = "客服接聽量", xlab = "日期", ylab = "接聽量")

dygraph(data,
        main = "客服接聽量", xlab = "日期", ylab = "接聽量") %>%
  dyOptions(stackedGraph = T, #下方面積
            fillAlpha=0.3)    #調整顏色透明度 0-1
dygraph(data,
        main = "客服接聽量", xlab = "日期", ylab = "接聽量") %>%
  dyOptions(stackedGraph = T, #下方面積
            fillAlpha=0.3,    #調整顏色透明度 0-1
            drawPoints = TRUE,  #點
            pointSize = 4,
            pointShape = "star", #"pentagon", "hexagon", "circle", "star", "plus" or "ex"
            colors = "darkblue")  #color of area

dygraph(data,
        main = "客服接聽量", xlab = "日期", ylab = "接聽量") %>%
  dyOptions(stackedGraph = T, #下方面積
            fillAlpha=0.3,    #調整顏色透明度 0-1
            drawPoints = TRUE,  #點
            pointSize = 4,
            pointShape = "star", #"pentagon", "hexagon", "circle", "star", "plus" or "ex"
            colors = "darkblue",  #color of area
            axisLineWidth = 2,   #軸的粗度
            strokeBorderWidth=1, #點的邊緣寬度
            strokeBorderColor="pink"  #點的顏色
            )
###line plot
library(latticeExtra)
# create data
set.seed(1)
x = 1:100
var1 = cumsum(rnorm(100)) #rnorm(100)是指從標準常態分配中抽出100個數字
var2 = var1^2 #變數2是將變數1平方
data=data.frame(x,var1,var2) #集結成資料集
## 1=== With xyplot, you can easily show both var in the same time :
xyplot(var1 + var2 ~ x, data, type = "l")
## 2=== But it could be nice to have TWO Y axis!
# construct separate plots for each series
obj1 <- xyplot(var1 ~ x, data, type = "l" , lwd=2,
               xlab = "x軸名稱",ylab="第一個y軸")
obj2 <- xyplot(var2 ~ x, data, type = "l", lwd=2,
               ylab = "第二個y軸")
# Make the plot with second y axis:
doubleYScale(obj1, obj2, 
             add.ylab2 = T, #SHOW出第2個Y軸
             text = c("第一份資料", "第二份資料"),
             title = "雙y軸圖形")
			 
			 
			 