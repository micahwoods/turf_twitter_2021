# do charts for the 5 categories plus composite
library(RColorBrewer)
library(ggplot2)
library(cowplot)
library(scales)

d <- read.csv('~/Dropbox/R/for_shiny21.csv',
              header = TRUE, stringsAsFactors = FALSE)

# composite <- subset()
composite <- d

composite$screen_name0 <- reorder(composite$screen_name0, 
                                 composite$All_Around)

# order this, then select top 50
order_composite2 <- composite[with(composite, 
                                   order(All_Around)), ]

forPlot <- order_composite2[1:50, ]

forPlot$level <- as.factor(rep(1:5, each = 10))

rank_label <- data.frame(xlevel = min(forPlot$All_Around),
                         ylevel = 1:50,
                         level = as.factor(rep(1:5, each = 10)))

p <- ggplot(data = forPlot, 
            aes(x = All_Around, 
                y = screen_name0))  
yo <- p + theme_classic(base_family = "Roboto Condensed", 
                        base_size = 15) +
  background_grid(major = "xy") +
  geom_label(data = rank_label,
            aes(x = xlevel,
            y = rev(ylevel),
            label = ylevel,
            colour = level),
            label.size = 0,
            size = 3,
            hjust = 1) +
  geom_point(aes(colour = level)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="none",
        plot.caption = element_text(size = 10)) +
  labs(x = "all around score (lowest score is best)",
       y = "Twitter username",
       caption = "based on sum of ranks in 6 categories:\nfollower count, tweet creation rate, favorites h-index,\nretweets h-index, quoted count, & total mentions",
       title = "2021 influential turf accounts",
       subtitle = "top 50 of 5,409 active accounts") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.05))) +
  scale_y_discrete(limits = rev(unique(sort(forPlot$screen_name0))))

yo

save_plot("~/Dropbox/charts/all_around_2021.png", 
          yo, 
          base_asp = 0.5625, 
          base_height = 9,
          bg = "white")

save_plot("~/Dropbox/charts/all_around_2021.svg", 
          yo, 
          base_asp = 0.5625, 
          base_height = 9,
          bg = "white")

## do one for the mentions
composite <- d

composite$screen_name0 <- reorder(composite$screen_name0, 
                                  composite$Mentions_Rank)

# order this, then select top 50
order_composite2 <- composite[with(composite, 
                                   order(Mentions_Rank)), ]

forPlot <- order_composite2[1:50, ]

forPlot$level <- as.factor(rep(1:5, each = 10))

rank_label <- data.frame(xlevel = min(forPlot$Mentions) * 0.95,
                         ylevel = 1:50,
                         level = as.factor(rep(1:5, each = 10)))

p <- ggplot(data = forPlot, 
            aes(x = Mentions, 
                y = screen_name0))  
yo <- p + theme_classic(base_family = "Roboto Condensed", base_size = 14) +
  background_grid(major = "xy") +
  geom_label(data = rank_label,
             aes(x = xlevel,
                 y = rev(ylevel),
                 label = ylevel,
                 colour = level),
             label.size = 0,
             size = 3,
             hjust = 1) +
  geom_point(aes(colour = level)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="none",
        plot.caption = element_text(size = 10)) +
  labs(x = "Mentions count",
       y = "Twitter username",
       caption = "based on total mentions in 1,016,661 turf industry tweets sent in 2021",
       title = "2021's most-discussed turf accounts",
       subtitle = "top 50 of 5,409 active accounts") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.05))) +
  scale_y_discrete(limits = rev(unique(sort(forPlot$screen_name0))))

yo

save_plot("~/Dropbox/charts/mentions_2021.png", 
          yo, 
          base_asp = 0.5625, 
          base_height = 9,
          bg = "white")

## do one for the favorites
composite <- d

composite$screen_name0 <- reorder(composite$screen_name0, 
                                  composite$Rank.2)

# order this, then select top 50
order_composite2 <- composite[with(composite, 
                                   order(Rank.2)), ]

forPlot <- order_composite2[1:50, ]

forPlot$level <- as.factor(rep(1:5, each = 10))

rank_label <- data.frame(xlevel = min(forPlot$Favorite_H.index) * 0.95,
                         ylevel = 1:50,
                         level = as.factor(rep(1:5, each = 10)))

p <- ggplot(data = forPlot, 
            aes(x = Favorite_H.index, 
                y = screen_name0))  
yo <- p + theme_classic(base_family = "Roboto Condensed", base_size = 14) +
  background_grid(major = "xy") +
  geom_label(data = rank_label,
             aes(x = xlevel,
                 y = rev(ylevel),
                 label = ylevel,
                 colour = level),
             label.size = 0,
             size = 3,
             hjust = 1) +
  geom_point(aes(colour = level)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="none",
        plot.caption = element_text(size = 10)) +
  labs(x = "Favorites h-index",
       y = "Twitter username",
       caption = "Number of tweets in 2021 with at least that many favorites.\nAn h-index of 57 means there were 57 tweets with at least 57 favorites (likes).",
       title = "2021's most-liked turf accounts",
       subtitle = "top 50 of 5,409 active accounts") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.05))) +
  scale_y_discrete(limits = rev(unique(sort(forPlot$screen_name0))))

yo

save_plot("~/Dropbox/charts/likes_2021.png", 
          yo, 
          base_asp = 0.5625, 
          base_height = 9,
          bg = 'white')

## do one for the retweets
composite <- d

composite$screen_name0 <- reorder(composite$screen_name0, 
                                  composite$Rank.1)

# order this, then select top 50
order_composite2 <- composite[with(composite, 
                                   order(Rank.1)), ]

forPlot <- order_composite2[1:50, ]

forPlot$level <- as.factor(rep(1:5, each = 10))

rank_label <- data.frame(xlevel = min(forPlot$Retweet_H.index) * 0.95,
                         ylevel = 1:50,
                         level = as.factor(rep(1:5, each = 10)))

p <- ggplot(data = forPlot, aes(x = Retweet_H.index, y = screen_name0))  
yo <- p + theme_classic(base_family = "Roboto Condensed", base_size = 14) +
  background_grid(major = "xy") +
  geom_label(data = rank_label,
             aes(x = xlevel,
                 y = rev(ylevel),
                 label = ylevel,
                 colour = level),
             label.size = 0,
             size = 3,
             hjust = 1) +
  geom_point(aes(colour = level)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="none",
        plot.caption = element_text(size = 10)) +
  labs(x = "Retweets h-index",
       y = "Twitter username",
       caption = "Number of tweets in 2021 with at least that many retweets.\nAn h-index of 21 means there were 21 tweets with at least 21 retweets.",
       title = "2021's most-retweeted turf accounts",
       subtitle = "top 50 of 5,409 active accounts") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.05))) +
  scale_y_discrete(limits = rev(unique(sort(forPlot$screen_name0))))

yo

save_plot("~/Dropbox/charts/retweets_2021.png", 
          yo, 
          base_asp = 0.5625, 
          base_height = 9,
          bg = 'white')

## do one for the TCR
composite <- d

composite$screen_name0 <- reorder(composite$screen_name0, 
                                  composite$TCR_Rank)

# order this, then select top 50
order_composite2 <- composite[with(composite, 
                                   order(TCR_Rank)), ]

forPlot <- order_composite2[1:50, ]

forPlot$level <- as.factor(rep(1:5, each = 10))

rank_label <- data.frame(xlevel = min(forPlot$TCR) * 0.95,
                         ylevel = 1:50,
                         level = as.factor(rep(1:5, each = 10)))

p <- ggplot(data = forPlot, 
            aes(x = TCR,
                y = screen_name0))  
yo <- p + theme_classic(base_family = "Roboto Condensed", base_size = 14) +
  background_grid(major = "xy") +
  geom_label(data = rank_label,
             aes(x = xlevel,
                 y = rev(ylevel),
                 label = ylevel,
                 colour = level),
             label.size = 0,
             size = 3,
             hjust = 1) +
  geom_point(aes(colour = level)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="none",
        plot.caption = element_text(size = 10)) +
  labs(x = "Tweet creation rate (tweets per hour)",
       y = "Twitter username",
       caption = "Number of unique tweets (not retweets) sent. Adjusted for time.",
       title = "2021's most-active turf accounts",
       subtitle = "top 50 of 5,409 active accounts") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.05))) +
  scale_y_discrete(limits = rev(unique(sort(forPlot$screen_name0))))

yo

save_plot("~/Dropbox/charts/tcr_2021.png", 
          yo, 
          base_asp = 0.5625, 
          base_height = 9,
          bg = 'white')

# do the quoted
composite <- d

composite$screen_name0 <- reorder(composite$screen_name0, 
                                  composite$Quoted_rank)

# order this, then select top 50
order_composite2 <- composite[with(composite, 
                                   order(Quoted_rank)), ]

forPlot <- order_composite2[1:50, ]

forPlot$level <- as.factor(rep(1:5, each = 10))

rank_label <- data.frame(xlevel = min(forPlot$Quoted_count) * 0.95,
                         ylevel = 1:50,
                         level = as.factor(rep(1:5, each = 10)))

p <- ggplot(data = forPlot, 
            aes(x = Quoted_count, 
                y = screen_name0))  
yo <- p + theme_classic(base_family = "Roboto Condensed", base_size = 14) +
  background_grid(major = "xy") +
  geom_label(data = rank_label,
             aes(x = xlevel,
                 y = rev(ylevel),
                 label = ylevel,
                 colour = level),
             label.size = 0,
             size = 3,
             hjust = 1) +
  geom_point(aes(colour = level)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="none",
        plot.caption = element_text(size = 10)) +
  labs(x = "Number of quoted tweets",
       y = "Twitter username",
       caption = "Count of the times a tweet from the acount was quote-tweeted\n(retweeted with a comment added) in #TurfTwitter.",
       title = "2021's most-shared turf accounts",
       subtitle = "top 50 of 5,409 active accounts") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.05))) +
  scale_y_discrete(limits = rev(unique(sort(forPlot$screen_name0))))

yo

save_plot("~/Dropbox/charts/quoted_2021.png", 
          yo, 
          base_asp = 0.5625, 
          base_height = 9,
          bg = 'white')

# do the followers
composite <- d

composite$screen_name0 <- reorder(composite$screen_name0, 
                                  composite$Followers_Rank)

# order this, then select top 50
order_composite2 <- composite[with(composite, 
                                   order(Followers_Rank)), ]

forPlot <- order_composite2[1:50, ]

forPlot$level <- as.factor(rep(1:5, each = 10))

rank_label <- data.frame(xlevel = min(forPlot$Followers) * 0.95,
                         ylevel = 1:50,
                         level = as.factor(rep(1:5, each = 10)))

p <- ggplot(data = forPlot, 
            aes(x = Followers, 
                y = screen_name0))  
yo <- p + theme_classic(base_family = "Roboto Condensed", base_size = 14) +
  background_grid(major = "xy") +
  geom_label(data = rank_label,
             aes(x = xlevel,
                 y = rev(ylevel),
                 label = ylevel,
                 colour = level),
             label.size = 0,
             size = 3,
             hjust = 1) +
  geom_point(aes(colour = level)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="none",
        plot.caption = element_text(size = 10)) +
  labs(x = "Followers",
       y = "Twitter username",
       caption = "Follower counts as of 2021-01-01.",
       title = "2021's most-followed turf accounts",
       subtitle = "top 50 of 5,409 active accounts") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.05)),
                     label = comma) +
  scale_y_discrete(limits = rev(unique(sort(forPlot$screen_name0))))

yo

save_plot("~/Dropbox/charts/followers_2021.png", 
          yo, 
          base_asp = 0.5625, 
          base_height = 9,
          bg = 'white')

# # composite <- subset()
# composite <- d
# 
# composite$screen_name0 <- reorder(composite$screen_name0, 
#                                   - composite$All_Around)
# 
# # order this, then select top 50
# order_composite2 <- composite[with(composite, 
#                                    order(All_Around)), ]
# 
# forPlot <- order_composite2[1:50, ]
# 
# forPlot$level <- as.factor(rep(1:5, each = 10))
# 
# forPlot$screen_name1 <- paste(forPlot$screen_name0, 1:50)
# 
# forPlot$screen_name1 <- reorder(forPlot$screen_name1, 
#                                   - forPlot$All_Around)
# 
# p <- ggplot(data = forPlot, aes(y = All_Around, x = screen_name1))  
# yo <- p + theme_classic(base_family = "Roboto Condensed", base_size = 15) +
#   background_grid(major = "xy") +
#   geom_point(aes(colour = level)) +
#   scale_color_brewer(palette = "Set1") +
#   theme(legend.position="none",
#         plot.caption = element_text(size = 10),
#         axis.text.x = element_text(angle = 40, hjust = 1, colour = "dodgerblue")) +
#   labs(y = "all around score (lowest score is best)",
#        x = "Twitter username",
#        caption = "based on sum of ranks in 6 categories:\nfollower count, tweet creation rate, favorites h-index,\nretweets h-index, quoted count, & total mentions",
#        title = "2021 influential turf accounts",
#        subtitle = "top 50 of 5,409 active accounts")+
#   scale_x_discrete(limits = rev(unique(sort(forPlot$screen_name1))))
# 
# yo
# 
# save_plot("~/Desktop/all_around_2021_169.png", yo, base_asp = 1.78, base_height = 6.5)


# do rookies!
library(rtweet)
library(dplyr)
library(lubridate)

d <- read.csv('~/Dropbox/R/for_shiny21.csv',
              header = TRUE, stringsAsFactors = FALSE)


# if I want to do rookies, get the t20_followers3 and merge it
f3 <- read_twitter_csv("~/Dropbox/R/t21_turf_follower3.csv")

f3_clean <- select(f3, screen_name, account_created_at)

f3_clean$date <- ymd_hms(f3_clean$account_created_at)

f3_clean <- subset(f3_clean, account_created_at >= as.Date("2021-01-01"))

rookies <- merge(d, f3_clean, by.x = "screen_name0", by_y = "screen_name")

composite <- rookies

composite$screen_name0 <- reorder(composite$screen_name0, 
                                  composite$All_Around)

# order this, then select top 50
order_composite2 <- composite[with(composite, 
                                   order(All_Around)), ]

forPlot <- order_composite2[1:20, ]

forPlot$level <- as.factor(rep(1:4, each = 5))

rank_label <- data.frame(xlevel = min(forPlot$All_Around) * 0.99,
                         ylevel = 1:20,
                         level = as.factor(rep(1:4, each = 5)))

p <- ggplot(data = forPlot, aes(x = All_Around, y = screen_name0))  
yo <- p + theme_classic(base_family = "Roboto Condensed", base_size = 15) +
  background_grid(major = "xy") +
  geom_label(data = rank_label,
             aes(x = xlevel,
                 y = rev(ylevel),
                 label = ylevel,
                 colour = level),
             label.size = 0,
             size = 3,
             hjust = 1) +
  geom_point(aes(colour = level)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="none",
        plot.caption = element_text(size = 10)) +
  labs(x = "all around score (lowest score is best)",
       y = "Twitter username",
       caption = "based on sum of ranks in 6 categories:\nfollower count, tweet creation rate, favorites h-index,\nretweets h-index, quoted count, & total mentions",
       title = '2021 influential "rookie" turf industry accounts',
       subtitle = "top 20 of 81 active accounts started in 2021") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.05)),
                     label = comma) +
  scale_y_discrete(limits = rev(unique(sort(forPlot$screen_name0))))

yo

save_plot("~/Dropbox/charts/rookies_2021.png", 
          yo, 
          base_asp = 1.78, 
          base_height = 5,
          bg = 'white')

## look at omitting TCR
# do the followers
composite <- d

# rank omitting TCR
composite$noTCR <- composite$Rank.1 + composite$Rank.2 + composite$Mentions_Rank + composite$Quoted_rank + composite$Followers_Rank

composite$screen_name0 <- reorder(composite$screen_name0, 
                                  composite$noTCR)

# order this, then select top 50
order_composite2 <- composite[with(composite, 
                                   order(noTCR)), ]

forPlot <- order_composite2[1:50, ]

forPlot$level <- as.factor(rep(1:5, each = 10))

rank_label <- data.frame(xlevel = min(forPlot$noTCR) * 0.95,
                         ylevel = 1:50,
                         level = as.factor(rep(1:5, each = 10)))

p <- ggplot(data = forPlot, 
            aes(x = noTCR, 
                y = screen_name0))  
yo <- p + theme_classic(base_family = "Roboto Condensed", base_size = 14) +
  background_grid(major = "xy") +
  geom_label(data = rank_label,
             aes(x = xlevel,
                 y = rev(ylevel),
                 label = ylevel,
                 colour = level),
             label.size = 0,
             size = 3,
             hjust = 1) +
  geom_point(aes(colour = level)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="none",
        plot.caption = element_text(size = 10)) +
  labs(x = "all around score (lowest score is best)",
       y = "Twitter username",
       caption = "based on sum of ranks in 5 categories:\nfollower count, favorites h-index,\nretweets h-index, quoted count, & total mentions",
       title = "2021's most influential turf accounts",
       subtitle = "top 50 of 5,409 active accounts") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.05))) +
  scale_y_discrete(limits = rev(unique(sort(forPlot$screen_name0)))) +
  annotate("label", x = 700, y = 45, hjust = 1,
           family = "Roboto Condensed", 
           label = "This ranking omits the score\nfor tweet creation rate.")

yo

save_plot("~/Dropbox/charts/noTCR_2021.png", 
          yo, 
          base_asp = 0.5625, 
          base_height = 9,
          bg = 'white')

## do rank as followers, follow ratio, retweets, favorites, and quotes
composite <- merge(d, active_now,
                   by.x = 'screen_name0',
                   by.y = 'screen_name')

composite$follow_ratio_rank <- rank(-composite$follow_ratio, ties.method = "min")

# new rank
composite$new5rank <- composite$Rank.1 + composite$Rank.2 + composite$Quoted_rank + composite$Followers_Rank +
  composite$follow_ratio_rank

composite$screen_name0 <- reorder(composite$screen_name0, 
                                  composite$new5rank)

# order this, then select top 50
order_composite2 <- composite[with(composite, 
                                   order(new5rank)), ]

forPlot <- order_composite2[1:50, ]

forPlot$level <- as.factor(rep(1:5, each = 10))

rank_label <- data.frame(xlevel = min(forPlot$new5rank) * 0.95,
                         ylevel = 1:50,
                         level = as.factor(rep(1:5, each = 10)))

p <- ggplot(data = forPlot, 
            aes(x = new5rank, 
                y = screen_name0))  
yo <- p + theme_classic(base_family = "Roboto Condensed", base_size = 14) +
  background_grid(major = "xy") +
  geom_label(data = rank_label,
             aes(x = xlevel,
                 y = rev(ylevel),
                 label = ylevel,
                 colour = level),
             label.size = 0,
             size = 3,
             hjust = 1) +
  geom_point(aes(colour = level)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="none",
        plot.caption = element_text(size = 10)) +
  labs(x = "all around score (lowest score is best)",
       y = "Twitter username",
       caption = "based on sum of ranks in 5 categories:\nfollower count, favorites h-index, retweets h-index,\nquoted count, & followers/following ratio",
       title = "2021's most influential turf accounts",
       subtitle = "top 50 of 5,409 active accounts") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.05))) +
  scale_y_discrete(limits = rev(unique(sort(forPlot$screen_name0)))) +
  annotate("label", x = 1100, 
           y = 45, 
           hjust = 1,
           size = 3,
           family = "Roboto Condensed", 
           label = "This ranking omits the scores\nfor tweet creation rate and mentions,\nand it includes a ranking for the\nfollower/following ratio.")

yo

save_plot("~/Dropbox/charts/new5rank_2021.png", 
          yo, 
          base_asp = 0.5625, 
          base_height = 9,
          bg = 'white')

save_plot("~/Dropbox/charts/new5rank_2021.svg", 
          yo, 
          base_asp = 0.5625, 
          base_height = 9,
          bg = 'white')


# ## do the multiplicative score
# ## meant to be tweets * (rt + quote)/2 per tweet * 0.1 * favs/tweet
# composite <- d
# 
# composite$multi_rank <- composite$Tweets *
#   (((composite$Retweets + composite$Quoted_count * 2) / 2) / composite$Tweets) *
#   ((composite$Favorites / composite$Tweets) * 0.05)
# 
# composite$screen_name0 <- reorder(composite$screen_name0, 
#                                   composite$multi_rank)
# # order this, then select top 50
# order_composite2 <- composite[with(composite, 
#                                    order(-multi_rank)), ]
# 
# forPlot <- order_composite2[1:50, ]
# 
# forPlot$level <- as.factor(rep(1:5, each = 10))
# 
# rank_label <- data.frame(xlevel = min(forPlot$multi_rank) * 0.95,
#                          ylevel = 1:50,
#                          level = as.factor(rep(1:5, each = 10)))
# 
# p <- ggplot(data = forPlot, 
#             aes(x = multi_rank, 
#                 y = screen_name0))  
# yo <- p + theme_classic(base_family = "Roboto Condensed", base_size = 14) +
#   background_grid(major = "xy") +
#   geom_label(data = rank_label,
#              aes(x = xlevel,
#                  y = rev(ylevel),
#                  label = ylevel,
#                  colour = level),
#              label.size = 0,
#              size = 3,
#              hjust = 1) +
#   geom_point(aes(colour = level)) +
#   scale_color_brewer(palette = "Set1") +
#   theme(legend.position="none",
#         plot.caption = element_text(size = 10)) +
#   labs(x = "all around score ",
#        y = "Twitter username",
#        caption = "based on multiplicative",
#        title = "2021's most influential turf accounts",
#        subtitle = "top 50 of 5,409 active accounts") +
#   scale_x_continuous(expand = expansion(mult = c(0.08, 0.05))) +
#   scale_y_discrete(limits = unique(sort(forPlot$screen_name0))) +
#   annotate("label", x = 1100, 
#            y = 45, 
#            hjust = 1,
#            size = 3,
#            family = "Roboto Condensed", 
#            label = "This ranking omits the scores\nfor tweet creation rate and mentions,\nand it includes a ranking for the\nfollower/following ratio.")
# 
# yo
# 
# save_plot("~/Dropbox/charts/multi_rank_2021.png", 
#           yo, 
#           base_asp = 0.5625, 
#           base_height = 9,
#           bg = 'white')
#   
#   
  
  
  
  
  
  













