# this takes the previous data from the startup file
# now we look at retweet h-index

d21_active_order_rt <- d21_active[with(d21_active, order(screen_name, -retweet_count)), ]

# I will get the same results if I remove all tweets with no retweets
# this will save processing time
d21_active_order_rt <- subset(d21_active_order_rt, retweet_count > 0)

# I expect that I can slice this up by screen_name, then output to a new file of screen_name + h-index
rt_index_file <- data.frame()

d21_active_order_rt$screen_name_factor <- factor(d21_active_order_rt$screen_name)
d21_active_order_rt$user_integer <- as.numeric(d21_active_order_rt$screen_name_factor)

j <- max(d21_active_order_rt$user_integer)

# create progress bar
pb <- txtProgressBar(min = 0, max = j, style = 3)

# test code here
#working_user <- subset(d21_active_order_rt, screen_name == "asianturfgrass")

for (i in 1:j) {
  # this gets us a single user to work with
  working_user <- subset(d21_active_order_rt, user_integer == i)
  user <- working_user[1, 4]
  
  # now we will calculate the retweet h-index for working_user
  rts <- working_user[, 14]
  
  rt_h_index <- H_INDEX(rts$retweet_count)
  
  # now for that user we should have a rt_h_index
  # I'd like to now write that
  newline <- cbind.data.frame(user, rt_h_index)
  rt_index_file <- rbind.data.frame(rt_index_file, newline)
  
  # show a progress bar
  setTxtProgressBar(pb, i)
}
beep(sound = 3)

rt_index_file$user <- reorder(rt_index_file$screen_name, rt_index_file$rt_h_index)

# order this, then select top 25
order_rt <- rt_index_file[with(rt_index_file, order(-rt_h_index)), ]
# 
# forPlot <- order_rt[1:50, ]
# 
# p <- ggplot(data = forPlot, aes(x = rt_h_index, y = user))  
# p + theme_cowplot(font_family = "Gillius ADF No2 Cond") +
#   scale_x_continuous(breaks = seq(0, 100, 5)) +
#   background_grid(major = "xy") +
#   geom_point(shape = 1) +
#   labs(x = "retweets h-index",
#        y = "Twitter username",
#        caption = "h-index: number of tweets in 2019 (n) each being retweeted at least (n) times\nan h-index of 20 means that account had 20 tweets in 2018 that were retweeted 20 or more times",
#        title = "Some of the most shareable content turf twitter accounts in 2019",
#        subtitle = "Top 50 of 8,572 accounts")

