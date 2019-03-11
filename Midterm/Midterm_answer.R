cat('系統啟動中，請稍候...')
library(tidyverse)
options(warn=-1)

score_part1 <- read.csv('https://bit.ly/2F4Rlbl')
score_part2 <- read.csv('https://bit.ly/2F2w2r2')
score <- left_join(score_part1, score_part2, by = '准考證號')
score$數學[score$數學 == '零分'] = 0
score <- score %>% mutate(數學 = as.integer(as.character(數學)),
                      總成績 = 0.24*(國文+英文+數學)+0.14*(自然+社會),
                      分級 = case_when(
                        總成績 < 35 ~ '底標',
                        總成績 < 50 ~ '後標',
                        總成績 < 75 ~ '均標',
                        總成績 < 88 ~ '前標',
                        88 <= 總成績 ~ '頂標'
                      )) 
repeat{
  findID <- readline('請輸入您的准考證號(離開請按q)： ')
  if(findID == 'q') break
  findID <- as.integer(findID)
  while( is.na(findID) | findID < 1 | findID > 10000){
    findID <- readline('您輸入的資料有誤，請重新輸入： ')
    if(findID == 'q') break
    findID <- as.integer(findID)
  }
  if(findID == 'q') break
  findScore <- score %>% filter(准考證號 == findID)
  cat('您的成績如下：')
  cat(paste0('國文: ', findScore$國文, ', 數學: ', findScore$數學,
             ', 英文: ', findScore$英文, ', 自然: ', findScore$自然,
             ', 社會: ', findScore$社會, ', 總成績: ', findScore$總成績,
             ', 您的成績落在', findScore$分級))
}
cat('謝謝您的使用，再見！')
