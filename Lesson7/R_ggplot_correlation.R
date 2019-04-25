library(ggplot2)
library(dplyr)

set.seed(100)
df <- diamonds[sample(1:nrow(diamonds),5000),]

#
ggplot(df, aes(carat,price)) + 
  geom_point(stat = "identity",
             position = "identity")

#lm:線性回歸
ggplot(df, aes(carat,price)) + 
  geom_point(stat = "identity",
             position = "identity") +
  geom_smooth(se = T, method = "lm")

#切格
ggplot(df, aes(carat,price)) + 
  geom_point(stat = "identity",
             position = "identity") +
  geom_smooth(se = T) + 
  facet_wrap( ~ cut)

#
ggplot(df, aes(carat,price)) + 
  geom_point(stat = "identity",
             position = "identity",
             shape=17) +
  geom_smooth(se = T) + 
  facet_wrap( ~ cut)

#
ggplot(df, aes(carat,price)) + 
  geom_point(stat = "identity",
             position = "identity",
             shape=17) +
  geom_smooth(se = T) + 
  facet_wrap( ~ cut) + 
  labs(x = "克拉",y = "價格", title="散布圖")

#
ggplot(df, aes(carat,price,colour=cut)) + 
  geom_point(stat = "identity",
             position = "identity",
             shape=17) +
  labs(x = "克拉",y = "價格", title="散布圖")

#multi scatter
#install.packages("car")
library(car)
scatterplotMatrix(~carat + price + depth + table,
                  data = df,
                  main = "Scatter Plot Matrix") #

install.packages("ggcorrplot")
library(ggcorrplot)
df1 <- df[sapply(df, is.numeric)] 
corr <- round(cor(df1),1)
View(corr)
ggcorrplot(corr,
           hc.order = T,
           type = "lower",
           lab = T,
           lab_size = 3,
           method = "circle",
           colors = c("red","white","lightgreen"),
           title="圖的主題",
           ggtheme=theme_bw)


#熱力圖
heat <- df %>% 
  group_by(cut,color) %>% 
  summarise(number=n()) %>% 
  ungroup()

plot <- ggplot(heat, aes(cut,color)) + 
  geom_tile(aes(fill = number)) +
  theme_bw() + 
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "切工", y = "顏色", title = "熱力圖")


setwd("//vibo/nfs/CHPublic/客戶服務事業部/部門公用區/R語言課程/HW3/課程內容及題目/")

getwd() 
ggsave(plot, file="plot4.png")

ggsave(plot, file="plot3.png", width = 6,
       height = 4, units = "cm")



















