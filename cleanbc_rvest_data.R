library(rvest)
library(stringr)
library(dplyr)

url <- rvest::read_html("https://newcardealersgoelectric.ca/")
t <- url %>% html_elements("div") %>% 
  html_element("header") 
# t2 <- t %>% html_element("div") %>% html_element("p") %>% html_text() 
# t2 <- unique(t2)
# t2 <- t2[!is.na(t2)]

text <- t %>% html_text("header")
text <- text[!is.na(text)]

text <- str_squish(text)
text <- unique(text)

split <- str_split(text, "\\*")
# Get rebate
rebate_avail <- split[[1]][1]
rebate_avail <- str_split(rebate_avail, "\\$")[[1]][2]
rebate_avail <- as.numeric(gsub(",", "", rebate_avail))
# Get funds reserved
funds_reserved <- split[[1]][2]
funds_reserved <- str_split(funds_reserved, "\\$")[[1]][2]
funds_reserved <- as.numeric(gsub(",", "", funds_reserved))
# Get funds disbursed
funds_disbursed <- split[[1]][3]
funds_disbursed <- str_split(funds_disbursed, "\\$")[[1]][2]
funds_disbursed <- str_split(funds_disbursed, "Go Electric Charging")[[1]][1]
funds_disbursed <- as.numeric(gsub(",", "", funds_disbursed))

timestamp <- Sys.time()

updatedata <- data.frame(timestamp, funds_reserved, funds_disbursed, rebate_avail)

if (file.exists("CleanBC_Rebate_Funds.csv")) {
  datasaved <- read.csv("CleanBC_Rebate_Funds.csv", stringsAsFactors = F)
  datasaved$timestamp <- as.POSIXct(datasaved$timestamp)
  savedata <- bind_rows(datasaved, updatedata)
} else {
  savedata <- updatedata
}

write.csv(x = savedata, file = "CleanBC_Rebate_Funds.csv", quote = F, row.names = F)
write.csv(x = savedata, file = "CleanBC_Rebate_Funds_backup.csv", quote = F, row.names = F)

library(ggplot2)
theme_set(theme_bw())

savedata %>%
  ggplot(aes(x = timestamp)) +
  geom_path(aes(y = funds_reserved)) +
  ylab("Funds Reserved ($)")
savedata %>%
  ggplot(aes(x = timestamp)) +
  geom_path(aes(y = funds_disbursed)) +
  ylab("Funds Disbursed ($)")
savedata %>%
  ggplot(aes(x = timestamp)) +
  geom_path(aes(y = rebate_avail)) +
  ylab("Rebate Avail ($)")
