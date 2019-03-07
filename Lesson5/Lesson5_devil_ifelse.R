library(tidyverse)
library(magrittr)


# if else -----------------------------------------------------------------

## 產生 高富帥 的向量
person <- c('高富帥', 31, '大學', '男')

if(person[2] >= 34.5){
  ## 年齡不符
  print('Sorry~你年紀不符～')
} else if(!(person[3] %in% c('大學', '碩士'))){
  ## 學歷不符
  print('Sorry~你學歷不符～')
} else{
  ## 年齡、學歷符合條件
  print('恭喜你！！進到下一階段面試啦！')
}


# for loop ----------------------------------------------------------------

## 讀入面試者資料
interviewer <- read_csv('https://bit.ly/2H2GdOi')

## 對面試者編號一號到最後一號執行判斷式
for( i in 1:nrow(interviewer) ){
  
  ## 判斷式
  if(interviewer[i, 2] >= 34.5){
    print('Sorry~你年紀不符～')
  } else if(!(interviewer[i, 3] %in% c('大學', '碩士', '博士'))){
    print('Sorry~你學歷不符～')
  } else{
    print('恭喜你！！進到下一階段面試啦！')
  }
  
}


# while loop --------------------------------------------------------------

## 預設編號
i <- 1

## 設定迴圈執行條件
while(i <= 2){
  
  ## 判斷式
  if(interviewer[i, 2] >= 34.5){
    print('Sorry~你年紀不符～')
  } else if(!(interviewer[i, 3] %in% c('大學', '碩士', '博士'))){
    print('Sorry~你學歷不符～')
  } else{
    print( paste0('恭喜你，', interviewer[i, 1], '！！進到下一階段面試啦！') )
    ## 需自行將標號更新
    i <- i+1
  }
  
}

## 更多迴圈相關的內容，可以參考 https://blog.gtwang.org/r/r-flow-control-and-loops/
## 除了文章中介紹的 for, while, repeat 之外，很常會搭配使用 break, next。
## 問了讓程式更彈性，熟練這些東西是必要的！

# 計算KPI -------------------------------------------------------------------

## dplyr 作法

## 讀取課程KPI檔案
RLessonKPI <- read_csv('https://bit.ly/2ECbDHG')

start.time <- Sys.time()  # 程式開始執行時間

## 用 case_when 產出得分指標
RLessonKPI %<>% mutate(得分 = case_when(
  達成率 > 95 ~ 1,
  95 >= 達成率 & 達成率 > 87 ~ 2,
  87 >= 達成率 & 達成率 > 70 ~ 3,
  70 >= 達成率 & 達成率 > 60 ~ 4,
  60 >= 達成率 ~ 5
))
end.time <- Sys.time()  # 程式執行結束時間

print(paste0('執行時間: ', round(end.time - start.time, 2), '秒'))  # 輸出執行時間

## loop 和 if-else 作法

start.time <- Sys.time()  # 程式開始執行時間

## 用迴圈產出得分指標
for( n in 1:nrow(RLessonKPI) ) {
  if(RLessonKPI$達成率[n] >= 95) {
    RLessonKPI$得分[n] = 1
  } else if( 95 >= RLessonKPI$達成率[n] & RLessonKPI$達成率[n] >= 87){
    RLessonKPI$得分[n] = 2
  } else if( 87 >= RLessonKPI$達成率[n] & RLessonKPI$達成率[n] >= 70){
    RLessonKPI$得分[n] = 3
  } else if( 70 >= RLessonKPI$達成率[n] & RLessonKPI$達成率[n] >= 60){
    RLessonKPI$得分[n] = 4
  } else{
    RLessonKPI$得分[n] = 5
  }
}

end.time <- Sys.time()  # 程式執行結束時間

print(paste0('執行時間: ', round(end.time - start.time, 2), '秒'))  # 輸出執行時間

