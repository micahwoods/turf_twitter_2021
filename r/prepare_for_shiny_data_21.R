# after running all the scripts, get key data into one file
# for showing in Shiny

# order_composite2 has the original ranks
# let this include the forView which is on a per follower/pertweet basis

pre_shiny <- merge(order_composite2, forView, by.x = "screen_name", by.y = "screen_name")

pre_shiny$dark_horse_rank <- rank(-pre_shiny$index, ties.method = "min")

for_shiny <- select(pre_shiny, screen_name, profile_image_url,
                    all_around, all_around_rank, rt_h_index, rt_rank,
                    fav_h_index, fav_rank, tweet_creation_rate, tcr_rank,
                    sum_mentions, sum_mentions_rank, sum_quoted, quoted_rank, 
                    followers_count, follower_rank, 
                    statuses_count, favorites, retweets, fav_tweet, fav_follow, rt_tweet, 
                    rt_follow, index, dark_horse_rank)

# ok, how can I format the profile_image_url?
# how about straight up paste?
for_shiny$profile2 <- paste0("<img src=\"", 
                             for_shiny$profile_image_url, 
                             "\" height = \"40\"></img>")

for_shiny$screen_name0 <- for_shiny$screen_name

for_shiny$screen_name <- paste0('<a href="https://twitter.com/', for_shiny$screen_name, '">',
                                for_shiny$screen_name, '</a>')

d <- for_shiny[, c(1, 26, 3:25, 27)]

# try to make the screen name a hyperlink

d$tweet_creation_rate <- as.numeric(prettyNum(d$tweet_creation_rate, digits = 2))
d$fav_tweet <- as.numeric(prettyNum(d$fav_tweet, digits = 2))
d$fav_follow <- as.numeric(prettyNum(d$fav_follow, digits = 2))
d$rt_tweet <- as.numeric(prettyNum(d$rt_tweet, digits = 2))
d$rt_follow <- as.numeric(prettyNum(d$rt_follow, digits = 2))
d$index <- as.numeric(prettyNum(d$index, digits = 2))

colnames(d) <- c("screen_name", " ", "All_Around",
                 "Rank", "Retweet_H-index", "Rank",
                 "Favorite_H-index", "Rank", "TCR", "TCR_Rank",
                 "Mentions", "Mentions_Rank", "Quoted_count", "Quoted_rank",
                 "Followers", "Followers_Rank",
                 "Tweets", "Favorites", "Retweets", "favs/tweet", "favs/follower",
                 "rt/tweet", "rt/follower", "dark horse", "dark horse rank", "screen_name0")


# read it in again, prettyNum some columns, re-write
#d <- read.csv("~/Documents/Rs/turf_twitter_2017_shiny/for_shiny.csv",
 #             header = TRUE)

# send at this point to Dropbox, work on the shiny app on my other computer
write.csv(d, "~/Dropbox/R/for_shiny21.csv",
                   row.names = FALSE)

