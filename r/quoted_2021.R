# look at quote h-index or total number of times quoted

clean_quoted <- cbind.data.frame(d21_active$screen_name,
                                   d21_active$quoted_screen_name)

colnames(clean_quoted) <- c("screen_name", "quoted")

# remove the tweets that are not quotes
clean_quotes <- subset(clean_quoted, is.na(quoted) == FALSE)

# now re-express this as a large character string
quotes_data <- as.character(clean_quotes$quoted)

# now use sum of the count in a loop, searching the 'mention_data' 

# from the turf_twitter_2019_followers, I have an "active_now" file

quotes_total <- data.frame()

j <- length(active_now$screen_name)

# create progress bar
pb <- txtProgressBar(min = 0, max = j, style = 3)

for (i in 1:j) {
  user <- as.character(active_now[i, 1])
  
  # having problems with this stringr version as I
  # think it gets fragments such as "GCSAA_NW"
  # when counting for example "GCSAA" but the word boundaries fix this
  
  sum_quoted <- sum(str_count(quotes_data, 
                                paste0("\\b", user, "\\b")))
  
  newline <- cbind.data.frame(user, sum_quoted)
  quotes_total <- rbind.data.frame(quotes_total, newline)
  # show a progress bar
  setTxtProgressBar(pb, i)
}
beep(sound = 3) # beep when done, this takes a long time to run (22 minutes? for these data)

quotes_total$user <- reorder(quotes_total$user, 
                               quotes_total$sum_quoted)

# order this, then select top 50
order_quoted <- quotes_total[with(quotes_total, 
                                      order(-sum_quoted)), ]

# forPlot <- order_quoted[1:50, ]
# 
# p <- ggplot(data = forPlot, aes(x = sum_quoted, y = user))  
# p + theme_cowplot(font_family = "Gillius ADF No2 Cond") +
#   background_grid(major = "xy") +
#   geom_point(shape = 1) +
#   labs(x = "total mentions",
#        y = "Twitter username",
#        title = "Most-mentioned turf Twitter accounts in 2019",
#        subtitle = "top 50 out of 5,773 active accounts")
