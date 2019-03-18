# HW6 ----------------------------------------------------------------------
library(tidyverse)
setwd("//vibo/nfs/CHPublic/客戶服務事業部/部門公用區/R語言課程/HW6/課程內容及題目/")
member <- read.csv("人員名單.csv",
                   col.names = c("dep","team","name","gender"))
#單位=dep ; 組別=team ; 人員=name ; 性別=gender
BC10801 <- read.csv("BC10801.csv",
                    col.names = c("name","billcycle","totaldebt","receive"))
BC10802 <- read.csv("BC10802.csv",
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
BC10802$receive[BC10802$diff == -102] <- BC10802$totaldebt[BC10802$diff == -102] #把回收的金額調整成委案金額
BC108 <- bind_rows(BC10801,BC10802 %>% 
            select(name,billcycle,totaldebt,receive)) #出現warning的原因是兩個檔案的billcycle因子個數不一樣

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

