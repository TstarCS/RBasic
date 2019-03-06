library(tidyverse)

## 讀取班級成員姓名
class.member <- read_csv('/Users/jude/Documents/GitHub/RBasic/Lesson2/班級成員.csv') %>% 
  select(姓名) %>% 
  filter(姓名 != '恩恩')

## 建立主持人及玩家的兩個 vector
hoster <- c()
player <- c()

for(i in 1:8){
  ## 隨機產出一個數值，區間在0到1。如果數值大於0.5，則該成員為主持人。
  ## 如果主持人已經有4人，則該成員直接分派為玩家
  ## 如果玩家已經有4人，則該成員直接分派為主持人
  if( ((runif(1) > 0.5) & (length(hoster)) <4) | length(player) == 4){
    hoster <- c( hoster, class.member[i,1])
  } else{
    player <- c( player, class.member[i,1])
  }
}

## 顯示主持人及玩家成員
print( paste0('主持人：', paste0(hoster, collapse = ',')))
print( paste0('玩家：', paste0(player, collapse = ',')))
