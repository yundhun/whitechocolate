library(gmailr)

gmail_auth()

sendEmail <- function(bid){
  mail <- mime(
    To = "fbgustjs1909@gmail.com",
    From = "fbgustjs1909@gmail.com",
    Subject = "My bot sent this",
    body = bid)
  send_message(mail)
  
  mail <- mime(
    To = "yundhun@naver.com",
    From = "fbgustjs1909@gmail.com",
    Subject = "My bot sent this",
    body = bid)
  send_message(mail)
  
  mail <- mime(
    To = "jihoon.kang82@gmail.com",
    From = "fbgustjs1909@gmail.com",
    Subject = "My bot sent this",
    body = bid)
  send_message(mail)
}
