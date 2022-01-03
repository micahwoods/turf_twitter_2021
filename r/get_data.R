# get data from selected turf twitter accounts in 2021

# this sets up the oauth token
# see https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
appname <- "YOUR APP NAME HERE"

# api key 
key <- "YOUR API KEY HERE"

# api secret 
secret <- "YOUR API SECRET HERE"

# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

# get id of accounts following 
atc <- get_followers("asianturfgrass", n = 20000, retryonratelimit = TRUE)
pace <- get_followers("paceturf", n = 20000, retryonratelimit = TRUE)
unl <- get_followers("pgrbill", n = 20000, retryonratelimit = TRUE)
tomy <- get_followers("striturf_tomy", n = 20000, retryonratelimit = TRUE)
jyri <- get_followers("Amplify_Turf", n = 20000, retryonratelimit = TRUE)
jk <- get_followers("iTweetTurf", n = 20000, retryonratelimit = TRUE)
unruh <- get_followers("jbunruh", n = 20000, retryonratelimit = TRUE)

# get accounts following industry organizations, which I'll use as a confirmation
# that the account is probably interested in turf
gcsaa <- get_followers("gcsaa", n = 25000, retryonratelimit = TRUE)
bigga <- get_followers("BIGGAltd", n = 20000, retryonratelimit = TRUE)
canada <- get_followers("GolfSupers", n = 20000, retryonratelimit = TRUE)
agcsa <- get_followers("TheASTMA", n = 20000, retryonratelimit = TRUE)
iog <- get_followers("thegma_", n = 20000, retryonratelimit = TRUE)
stma <- get_followers("FieldExperts", n = 20000, retryonratelimit = TRUE)

# that's a lot of accounts, and many are duplicates
# to get the accounts to check, first select all those that are following one of the
# turf scientists

followers <- unique(rbind.data.frame(atc, pace, unl, tomy, jyri, jk, unruh))

# make list of unique accounts following the associations
assocations <- unique(rbind.data.frame(gcsaa, bigga, canada, agcsa, iog, stma))

# use the intersect function to select only those accounts that are in both follow lists
followers_assoc <- as.data.frame(base::intersect(followers$user_id, assocations$user_id))

colnames(followers_assoc) <- "user_id"
followers_assoc$user_id <- as.character(followers_assoc$user_id)

# get basic data on these accounts
turf_follower <- lookup_users(followers_assoc$user_id)

# this removes the private accounts, also the least active ones
turf_follower2 <- subset(turf_follower, protected == FALSE & statuses_count >= 50)

# check which have massive following
automated <- subset(turf_follower2, friends_count >= 1e4)

# this attempts to remove those that seem to have automated follow/follower system
# in order to get a large audience, few removed here are turf related
# 29 accounts removed at this stage
turf_follower3 <- subset(turf_follower2, friends_count < 1e4 |
                           screen_name == "WhitbyGC_Greens" |
                           screen_name == "TurfMatters")

# in 2021 it is n = 9630
# in 2020 it is n = 9222
# at this 2019 point it is 8572 accounts
# in 2018 it was 7689 accounts. In 2017 it was 6271 accounts
# I want to loop through these, getting the tweets and making some calculations
# I'm fine with 3000

## write_as_csv(turf_follower3, '~/Dropbox/R/t21_turf_follower3.csv')
## turf_follower3 <- read_twitter_csv("~/Dropbox/R/t21_turf_follower3.csv")

turf_timelines <- data.frame()

turf_follower3[5663, 4]

as.character(turf_follower3[i, 4])

i <- 1

for (i in 1:9630) {
  new_timeline <- get_timeline(user = as.character(turf_follower3[i, 4]), 
                               n = 3000,
                               home = FALSE,
                               parse = TRUE,
                               check = TRUE, 
                               retryonratelimit = TRUE)
  
  
  new_timeline_2021 <- subset(new_timeline,
                              created_at >= "2021-01-01 00:00:00" &
                                created_at <= "2021-12-31 23:59:59")
  turf_timelines <- rbind(turf_timelines, new_timeline_2021)
  print(paste("completed", i, "/9630 accounts"))
  Sys.sleep(6) # needs this, I hgit first rate lamit after 161 accounts
}

write_as_csv(turf_timelines, "~/Dropbox/R/t21_5664_5749.csv")

# now we have data to work with