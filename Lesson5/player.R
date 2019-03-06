
starting.game <- readline(prompt = '要開始遊戲了嗎(y/n): ')

if(starting.game == 'y'){
  lower <- 0
  upper <- 100
  guess <- sample(lower:upper, 1)
  feedback <- readline(prompt = paste0('我來猜猜...是', guess, '嗎(猜對了：0 / 往上：1 / 往下：-1)： ') )
  while(feedback != 0){
    if(feedback == 1){
      lower <- guess + 1
    } else{
      upper <- guess - 1
    }
    if(upper == lower){
      guess <- upper
      print(paste0('哈哈，我知道了！一定是', guess,'！！'))
      break
    }else{
      guess <- sample(lower:upper,1)
    }
    
    feedback <- readline(prompt = paste0('猜錯了嗎...那是', guess, '嗎(猜對了：0 / 往上：1 / 往下：-1): ') )
  }
  if(feedback == 0){
    print('耶！猜對了')
  }
} else{
  print('好吧！拜拜！')
}
