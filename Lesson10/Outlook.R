# install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
# Load the DCOM library
library(RDCOMClient)

# Date <- Sys.Date()
Date <- as.Date('2019-03-20')

# 啟動 Outlook
Outlook <- COMCreate("Outlook.Application")


# 新建立郵件
# 設定收件人、副本、密件副本、主旨、圖片路徑、內文等資訊

Email <- Outlook$CreateItem(0)
Email[["to"]] <- "example@example.com"  
Email[["cc"]] <- "example@example.com;example@example.com"
Email[["bcc"]] <- "example@example.com"
Email[["subject"]] <- "Hello, This email is sent by R language!"

# 圖片
plot_path <- paste0("~/plot/", Date, ".jpeg")
Email[["attachments"]]$Add(plot_path)
body <- 
  "<h1>分時進線量</h1>
  <p>本日的分時進線量如下圖</p>
  <p>本日的分時進線量如下圖</p>
  <p>若需要進一步檢視原始資料，請至以下路徑確認</p>
    <a href='C:/Users/dummyyu/Desktop/plot'>
      C:/Users/dummyyu/Desktop/plot
    </a>
  </p>"
plot <- 
  paste0( "<img src='cid:",
          basename(plot_path),
          "' width = '600' height = '400'>")

Email[["htmlbody"]] <- paste0(body, plot)
# 夾帶檔案
Email[["attachments"]]$Add("C:/Users/dummyyu/Desktop/plot/example.pptx")

# 讀取回條
Email[["readreceiptrequested"]] = TRUE

# 寄信前確認內容
# Email$Display()
# Send the message
Email$Send()

# Close Outlook, clear the message
rm(Outlook, Email)
