starting.game <- readline(prompt = '要開始遊戲了嗎？(y/n): ')

if(starting.game == 'y'){
  answer <- sample(0:100,1)
  guess <- readline(prompt = paste0('你要猜多少呢： ') )
  iteration <- 1
  while(guess != answer & iteration < 8){
    if (answer < guess) {
      print(paste0('不是欸！太高囉！你還有', 8-iteration, '次機會'))
      guess <- readline('再猜一個： ')
    } else{
      print(paste0('太低啦！你還有', 8-iteration, '次機會'))
      guess <- readline('再來： ')
    }
    iteration <- iteration + 1
  }
  if(guess != answer){
    print('好可惜QQ，你輸了！')
  } else{
    print('嗚喔！猜對了！')
  }
} else{
  print('好吧！拜拜！')
}