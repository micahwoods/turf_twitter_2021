# this gets the data organized and calculates the favorites h-index

# load libraries
library(ggplot2)
library(cowplot)
library(lubridate)
library(dplyr)
library(beepr)
library(stringr)
library(rtweet)
library(scales)

# This function returns the H-INDEX when the favorite or retweet count
# is a sorted vector, with the favorites or retweet count in descending order
H_INDEX <- function(x) {
  y <- 1:length(x)
  step1 <- x >= y
  step2 <- max(which(step1 == TRUE))
  return(step2)
}

d1 <- read_twitter_csv("~/Dropbox/R/t21_1_1000.csv")

d2 <- read_twitter_csv("~/Dropbox/R/t21_1001_1193.csv")

d3 <- read_twitter_csv("~/Dropbox/R/t21_1194_1999.csv")

d4 <- read_twitter_csv("~/Dropbox/R/t21_2000_3700.csv")

d5 <- read_twitter_csv("~/Dropbox/R/t21_3702_4499.csv")

d6 <- read_twitter_csv("~/Dropbox/R/t21_4500_4579.csv")

d7 <- read_twitter_csv("~/Dropbox/R/t21_4581_5479.csv")

d8 <- read_twitter_csv("~/Dropbox/R/t21_5481_6089.csv")

d9 <- read_twitter_csv("~/Dropbox/R/t21_6091_6240.csv")

d10 <- read_twitter_csv("~/Dropbox/R/t21_6241_6999.csv")

d11 <- read_twitter_csv("~/Dropbox/R/t21_7000_7131.csv")

d12 <- read_twitter_csv("~/Dropbox/R/t21_7133_7229.csv")

d13 <- read_twitter_csv("~/Dropbox/R/t21_7230_7999.csv")

d14 <- read_twitter_csv("~/Dropbox/R/t21_8000_8104.csv")

d15 <- read_twitter_csv("~/Dropbox/R/t21_8106_8999.csv")

d16 <- read_twitter_csv("~/Dropbox/R/t21_9000_9118.csv")

d17 <- read_twitter_csv("~/Dropbox/R/t21_9120_9240.csv")

d18 <- read_twitter_csv("~/Dropbox/R/t21_9242_9630.csv")

d1_clean <- subset(d1, is_retweet == FALSE)
d2_clean <- subset(d2, is_retweet == FALSE)
d3_clean <- subset(d3, is_retweet == FALSE)
d4_clean <- subset(d4, is_retweet == FALSE)
d5_clean <- subset(d5, is_retweet == FALSE)
d6_clean <- subset(d6, is_retweet == FALSE)
d7_clean <- subset(d7, is_retweet == FALSE)
d8_clean <- subset(d8, is_retweet == FALSE)
d9_clean <- subset(d9, is_retweet == FALSE)
d10_clean <- subset(d10, is_retweet == FALSE)
d11_clean <- subset(d11, is_retweet == FALSE)
d12_clean <- subset(d12, is_retweet == FALSE)
d13_clean <- subset(d13, is_retweet == FALSE)
d14_clean <- subset(d14, is_retweet == FALSE)
d15_clean <- subset(d15, is_retweet == FALSE)
d16_clean <- subset(d16, is_retweet == FALSE)
d17_clean <- subset(d17, is_retweet == FALSE)
d18_clean <- subset(d18, is_retweet == FALSE)

# colnames issue, some have 88, some have 90
colnames88 <- colnames(d1_clean)

d4_clean <- select(d4_clean, colnames88)
d5_clean <- select(d5_clean, colnames88)
d10_clean <- select(d10_clean, colnames88)
d11_clean <- select(d11_clean, colnames88)
d12_clean <- select(d12_clean, colnames88)
d13_clean <- select(d13_clean, colnames88)
d14_clean <- select(d14_clean, colnames88)
d15_clean <- select(d15_clean, colnames88)
d16_clean <- select(d16_clean, colnames88)
d17_clean <- select(d17_clean, colnames88)
d18_clean <- select(d18_clean, colnames88)

d2021 <- rbind.data.frame(d1_clean, d2_clean, d3_clean,
                            d4_clean, d5_clean, d6_clean,
                            d7_clean, d8_clean, d9_clean,
                          d10_clean, d11_clean, d12_clean,
                          d13_clean, d14_clean, d15_clean,
                          d16_clean, d17_clean, d18_clean)

# total tweets include RT 
totalTweets <- length(d1$text) + length(d2$text) + length(d3$text) + 
  length(d4$text) + length(d5$text) + length(d6$text) +
  length(d7$text) + length(d8$text) + length(d9$text) +
  length(d10$text) + length(d11$text) + length(d12$text) +
  length(d13$text) + length(d14$text) + length(d15$text) +
  length(d16$text) + length(d17$text) + length(d18$text) 

# total accounts checked
totalAccts <- length(levels(as.factor(d1$screen_name))) +
  length(levels(as.factor(d2$screen_name))) +
  length(levels(as.factor(d3$screen_name))) +
  length(levels(as.factor(d4$screen_name))) +
  length(levels(as.factor(d5$screen_name))) +
  length(levels(as.factor(d6$screen_name))) +
  length(levels(as.factor(d7$screen_name))) +
  length(levels(as.factor(d8$screen_name))) +
  length(levels(as.factor(d9$screen_name))) +
  length(levels(as.factor(d10$screen_name))) +
  length(levels(as.factor(d11$screen_name))) +
  length(levels(as.factor(d12$screen_name))) +
  length(levels(as.factor(d13$screen_name))) +
  length(levels(as.factor(d14$screen_name))) +
  length(levels(as.factor(d15$screen_name))) +
  length(levels(as.factor(d16$screen_name))) +
  length(levels(as.factor(d17$screen_name))) +
  length(levels(as.factor(d18$screen_name))) 


# unique accounts
# started at 9,630 to scrape
# but naturally some did not tweet in 2021, some retweet only, etc
accs <- length(levels(as.factor(d2021$screen_name)))

# for rapidity
#write_as_csv(d2021, '~/Dropbox/R/all_turf_tweets_2021.csv')

# read it new
d2021 <- read_twitter_csv("~/Dropbox/R/all_turf_tweets_2021.csv")

d2021$date <- ymd_hms(d2021$created_at)

# get only those with 12+ tweets, for once a month
d2021$tweet_count <- ave(d2021$is_retweet, d2021$screen_name, FUN = length)
d21_active <- subset(d2021, tweet_count >= 12)

## Remove the jennifer_360_ fishing account
d21_active <- subset(d21_active, screen_name != "Jennifer_360_")

d21_active_order_fav <- d21_active[with(d21_active, order(screen_name, -favorite_count)), ]

# I will get the same results if I remove all tweets with no faves
# this will save processing time
d21_active_order_fav <- subset(d21_active_order_fav, favorite_count > 0)
# I expect that I can slice this up by screen_name, 
# then output to a new file of screen_name + h-index

# refactor the screen names and number them for easy subsetting
d21_active_order_fav$screen_name <- factor(d21_active_order_fav$screen_name)
d21_active_order_fav$user_integer <- as.numeric(d21_active_order_fav$screen_name)

d21_active_order_fav$screen_name <- as.character(d21_active_order_fav$screen_name)
# create a file to hold the output
fav_index_file <- data.frame()

j <- max(d21_active_order_fav$user_integer)

# create progress bar
pb <- txtProgressBar(min = 0, max = j, style = 3)


# everything below this runs but was giving h-index in the 2000 range which seems improbable
for (i in 1:j) {
  # this gets us a single user to work with
  working_user <- subset(d21_active_order_fav, user_integer == i)
  user <- working_user[1, 4]
  
  # get the vector now of favorites for the working user
  favs <- working_user[, 13]
  
  fav_h_index <- H_INDEX(favs$favorite_count)
  
  # now for that user we should have a fav_h_index
  # I'd like to now write that
  newline <- cbind.data.frame(user, fav_h_index)
  fav_index_file <- rbind.data.frame(fav_index_file, newline)
  
  # show a progress bar
  setTxtProgressBar(pb, i)
}
beep(sound = 3)

fav_index_file$user <- reorder(fav_index_file$screen_name, fav_index_file$fav_h_index)

# order this, then select top 50
order_fav <- fav_index_file[with(fav_index_file, order(-fav_h_index)), ]

# forPlot <- order_fav[1:50, ]
# 
# p <- ggplot(data = forPlot, aes(x = fav_h_index, y = user))  
# p + theme_cowplot(font_family = "Gillius ADF No2 Cond") +
#   background_grid(major = "xy") +
#   geom_point(shape = 1) +
#   labs(x = "favorites h-index",
#        y = "Twitter username",
#        caption = "h-index: number of tweets in 2021 (n) with at least (n) favorites\nan h-index of 50 means that account had 50 tweets\nin 2021 that were favorited 50 or more times",
#        title = "Most likeable turfgrass Twitter accounts in 2019",
#        subtitle = "top 50 of 8,572 accounts")
