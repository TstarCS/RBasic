# 歡迎來到 R

system('say -v Mei-Jia 嗨！T-Star的同仁大家好，我是美佳。歡迎來到R的世界。今天我們要熟悉R要怎麼使用，讓你徹底擺脫Excel的糾纏，由耆康帶著我們開始吧！')

# 基礎運算

1 + 50  # 加法
69 - 3  # 減法
168 * 42  # 乘法
77 / 11  # 除法

c(1, 2, 3) * 3  # 向量乘數值
c(4, 5, 6) * c(2, 3, 4)  # 向量乘向量

## Excercise 1

# 基礎統計數值

min(c(1, 2, 3))  # 最小值
max(c(1, 2, 3))  # 最大值
range(c(1, 2, 3))  # 級距
mean(c(1, 2, 3))  # 平均值
sum(c(1, 2, 3))  # 總數

# 賦值

inbound <- c(3424, 3215, 3467, 3562, 3898, 3310)
mean(inbound)
mean(c(3424, 3215, 3467, 3562, 3898, 3310))

# Exercise 2

# 儲存及讀取

write.csv(inbound, './Documents/GitHub/RBasic/進線紀錄.csv')

# Exercise 3
