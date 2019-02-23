# 資料型態
# integer
str(1L)

# number
str(56.9)

# logic
str(TRUE)

# character
str('hi')
substr('gigd', 2,2)

# date
Sys.Date()
Sys.time()
as.Date(c('2018-05-01','2018-05-05'),'%Y-%m-%d')

## 字串 -> 日期
nowstr <- date()
nowtime <- strptime(nowstr,'%a %b %d %H:%M:%S %Y')
print(nowtime)

## 日期 -> 字串
today <- Sys.Date()
mydate <- format(today,format='%Y-%m-%d')

today <- Sys.Date()
mydate <- strftime(today,format='%Y-%m-%d')

# factor
gender <- c("男", "女", "男", "男", "女")  # 建立一個character vector
gender <- factor(gender)   # 轉換成factor型態
gender

# vector
str(c(1,2,3))
str(c(1,2,'hi'))
str(c(1,2,3)[1])

# list
list(gender="男", age=18, hobby=c("吃飯", "睡覺", "打東東"))

# matrix
a <- matrix(c(1:6), nrow=3, ncol=2) #建立一個3x2的矩陣，依照column分別填入1~6的值
a

# data frame
df <- data.frame('姓名'= c('頭家娘','550', '安潔莉納', '龜雷迪', '佩塔', '維奇', '恩恩', 
                         '餅乾', '星娘'), 
                 '年齡' = c(18, 19, 18, 16, 12, 21, 16, 18, 20),
                 '性別' = c('女','男','女','女','女','女','女','女','女'))
write.csv(df, '/Users/jude/Documents/GitHub/RBasic/班級成員.csv')

## 資料型態
class(df)
df$姓名
str(df$姓名)
str(df$年齡)
summary(df)
str(df)

## 資料選取
df[1,2]
df[,2]
df[1,]
df[df$姓名 == '頭家娘', ]
df$姓名 == '頭家娘'

## 資料運算
df[,2]
sum(df[,2])

# logic
5>6   #  大於
3<7  # 小於
'hi'=='hi'  # 等於
5>=5  # 大於等於
4<=4  # 小於等於
'hey'!='hey'  # 不等於