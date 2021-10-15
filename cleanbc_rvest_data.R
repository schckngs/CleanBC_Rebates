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
attr(timestamp, "tzone") <- "UTC"

updatedata <- data.frame(timestamp, funds_reserved, funds_disbursed, rebate_avail)

if (file.exists("CleanBC_Rebate_Funds.csv")) {
  datasaved <- read.csv("CleanBC_Rebate_Funds.csv", stringsAsFactors = F)
  datasaved$timestamp <- as.POSIXct(datasaved$timestamp, tz = "UTC")
  savedata <- bind_rows(datasaved, updatedata)
} else {
  savedata <- updatedata
}

write.csv(x = savedata, file = "CleanBC_Rebate_Funds.csv", quote = F, row.names = F)
write.csv(x = savedata, file = "CleanBC_Rebate_Funds_backup.csv", quote = F, row.names = F)

library(ggplot2)
library(cowplot)
theme_set(theme_bw())

r <- savedata %>%
  ggplot(aes(x = timestamp)) +
  geom_path(aes(y = funds_reserved), colour = "grey30", size = 1.5) +
  ggtitle("Funds Reserved") + xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_datetime(date_labels = "%m-%d", breaks = "2 months",
                   minor_breaks = "1 month")
d <- savedata %>%
  ggplot(aes(x = timestamp)) +
  geom_path(aes(y = funds_disbursed), colour = "green", size = 1.5) +
  ggtitle("Funds Disbursed") + xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_datetime(date_labels = "%m-%d", breaks = "2 months",
                   minor_breaks = "1 month")
a <- savedata %>%
  ggplot(aes(x = timestamp)) +
  geom_path(aes(y = rebate_avail), colour = "blue", size = 1.5) +
  ggtitle("Rebate Available") + xlab(NULL) + ylab(NULL) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_datetime(date_labels = "%m-%d", breaks = "2 months",
                   minor_breaks = "1 month")

plop <- plot_grid(r, d, a, nrow = 1)

ggsave(filename = "CleanBC_funds_over_time.png", plot = plop, device = "png",
       width = 6.5, height = 2, scale = 1.7)


# n_cars = floor(rebate_avail/3000)
# grid_coords = expand.grid(seq(1,10,1), seq(1,10,1), KEEP.OUT.ATTRS = F, stringsAsFactors = F)
# grid_coords$ID = 1
# ct=2
# while (nrow(grid_coords) < n_cars) {
#   print(ct)
#   gridsub <- grid_coords[grid_coords$ID==1,]
#   gridsub$ID = ct
#   grid_coords = bind_rows(grid_coords, gridsub)
#   ct = ct+1
# }
# grid_coords = grid_coords[1:n_cars,]
# 
# g <- grid_coords %>% ggplot(aes(x = Var1, y = Var2)) +
#   facet_wrap(~ID, ncol = 6) +
#   geom_text(aes(label = emoji("red_car"))) +
#   # geom_emoji("car") +
#   theme_void() +
#   theme(strip.text = element_blank()) +
#   labs(subtitle = paste(n_cars, "EV rebates remaining!"))
# 
# ggsave(plot = g, filename = "Car_num.png", width = 6, height = 4, units = "in", scale = 1.5)


