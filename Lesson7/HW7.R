library(tidyverse)
library(car)

hw1 <- read.csv("//vibo/nfs/CHPublic/客戶服務事業部/部門公用區/R語言課程/HW2/課程內容及題目/Lottery.csv",fileEncoding = 'UTF-8') %>% 
  select(ID,birthymd="出生日期",buyymd="購買日期",sex="性別",
         pay="投注金額",get="中獎金額",area="購買地區",religion="宗教信仰")  

hw1_1 <- hw1 %>%  
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
                  regLine=F,
                  data=hw1_2,
                  spread=F,
                  lty=2,
                  main="Scatter Plot Matrix",
                  diagonal=list(method="boxplot"),
                  pch=2:3, #pch=2三角形 =3+
                  col= c("green","orange") #不能用default顏色
                  )
				  