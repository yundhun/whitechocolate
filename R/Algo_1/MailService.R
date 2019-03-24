
install.packages('gmailr')
library(gmailr)
setwd() #set to desired working directory
gmail_auth()
mail <- mime(
  To = "fbgustjs1909@gmail.com",
  From = "fbgustjs1909@gmail.com",
  Subject = "My bot sent this",
  body = "Test successful")
send_message(mail)
