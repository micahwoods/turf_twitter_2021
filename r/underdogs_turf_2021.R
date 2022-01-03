# this script to look at what Jon Wall was asking
# specifically, he wondered about per follower or per tweet basis

# so, first load all the data, or load a lot of tweets
# then, my idea is 

wall <- d21_active %>%
  group_by(screen_name) %>%
  summarise(tweets = length(text),
            favorites = sum(favorite_count),
            retweets = sum(retweet_count))

# I want to intersect these
active_wall <- merge(turf_accounts, wall, 
                    by.x = "screen_name", by.y = "screen_name")

active_wall$fav_tweet <- active_wall$favorites / active_wall$tweets
active_wall$fav_follow <- active_wall$favorites / active_wall$followers_count
active_wall$rt_tweet <- active_wall$retweets / active_wall$tweets
active_wall$rt_follow <- active_wall$retweets / active_wall$followers_count
active_wall$index <- (active_wall$favorites + 2 * active_wall$retweets) /
  active_wall$tweets / active_wall$followers_count


# indexed colnumbers from 2017 script
# col 1 was screen_name, 8 I don't know, 21:29 were these
# forView <- active_wall[, c(1, 8, 21:29)]

# select here the screen_name and recent calcs
forView <- select(active_wall,
                  screen_name,
                  tweets,
                  favorites,
                  retweets,
                  fav_tweet,
                  fav_follow,
                  rt_tweet,
                  rt_follow,
                  index)

