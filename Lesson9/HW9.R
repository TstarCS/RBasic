#將圓餅圖標上標籤-----------
library(ggplot2)
set.seed(100)
df <- diamonds[sample(1:nrow(diamonds),5000),]

df1 <- group_by(df,clarity) %>% 
  summarise(val=n()) %>% 
  mutate(prop=paste0(round(val/sum(val)*100,0),"%"),label=paste(val,prop,sep = ";"))
#由於有些餅位置較小，放不進數字，手動調整標籤位置及大小
site_y <- 5000-(df1$val/2 + c(0, cumsum(df1$val)[-length(df1$val)]))
site_x <- c(1.3,1.3,1.2,1.2,1.2,1.3,1.3,1.5)
text_cex <- c(3,4,4,4,4,4,3,3)
ggplot(df1) +
  geom_bar(width=1, aes(x=factor(1),weight=val,fill=clarity))+
  scale_fill_brewer(palette = "Set1") +
  coord_polar(theta="y") +
  labs(x = '', y = '', title = '') +  #把x軸和y軸拿掉
  theme(axis.text = element_blank(),  #文字
        axis.ticks = element_blank()) +  #tick
  geom_text(aes(y = site_y,
                x = site_x, 
                label = label),
            size = text_cex)
