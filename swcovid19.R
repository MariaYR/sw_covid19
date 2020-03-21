#swcovid19

library(rtweet)
## store api keys (these are fake example values; replace with your own keys)
api_key <- "zmUkEB27bLv5NytcPVsfc0Wa1"
api_secret_key <- "riv4j7lwHIVIs8mhH9pqQe4APp8ncV8YvNevaxh26iiVFwuwbz"
access_token <- "235262823-O6sLg6iMkAH7whP5Oj6IHM8K9WuWzY3eqw7swEjb"
access_token_secret <- "WFlkUA7MlkmgrchIrvuUYj3KDPSNcIqwZbM3gTcFkYS8H"

## authenticate via web browser
my_token <- create_token(
  app = "JumpingJellies",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

swcovid_tweets <- search_tweets(q = "swthrucovid",n = 500000,type = "recent",include_rts = TRUE, parse = TRUE,token = my_token,
                             retryonratelimit = TRUE,verbose = TRUE)
#only 131 tweets

swcovid_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWthruCOVID Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## create lat/lng variables using all available tweet and profile geo-location data
covid_latlong <- lat_lng(swcovid_tweets)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(covid_latlong, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

# view column with screen names - top 6
head(swcovid_tweets$screen_name)
unique(swcovid_tweets$screen_name)

# what users are tweeting with #swthrucovid
swcovid_users <- search_users("#swthrucovid",
                      n = 500)
#0 users

#try swtech

swtech_tweets <- search_tweets(q = "swtech",n = 500000,type = "recent",include_rts = TRUE, parse = TRUE,token = my_token,
                               retryonratelimit = TRUE,verbose = TRUE)

#218 tweets

swtech_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWtech Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## create lat/lng variables using all available tweet and profile geo-location data
swtech_latlong <- lat_lng(swtech_tweets)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(swtech_latlong, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))


# what users are tweeting with #swtech
swtech_users <- search_users("#swtech",
                              n = 500)

# just view the first 2 users - the data frame is large!
head(swtech_users, n = 2)

# how many locations are represented
length(unique(swtech_users$location))
#23

# install descriptive stats libraries
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)

#where are users from 
swtech_users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(15) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "#SWtech Twitter users during #COVID19- unique locations ")

names(swtech_users)

# remove http elements manually
swtech_tweets$stripped_text <- gsub("http.*","",  swtech_tweets$text)
swtech_tweets$stripped_text <- gsub("https.*","", swtech_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swtech_tweets_clean <- swtech_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
swtech_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #swtech tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
cleaned_tweet_words <- swtech_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swtech_tweets_paired_words <- swtech_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swtech_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

library(tidyr)
swtech_tweets_separated_words <- swtech_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swtech_tweets_filtered <- swtech_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swtech_words_counts <- swtech_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swtech_words_counts)

# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swtech_words_counts %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - SWtech",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swtech_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #swtech.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#socialworkmonthtweets
swmonth_tweets <- search_tweets(q = "socialworkmonth",n = 500000,type = "recent",include_rts = FALSE, parse = TRUE,token = my_token,
                               retryonratelimit = TRUE,verbose = TRUE)

#701

#tweets over collection time (+/- 9 days)
swmonth_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWmonth Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# install descriptive stats libraries
# library(ggplot2)
# library(dplyr)
# library(tidytext)
# library(igraph)
# library(ggraph)

# remove http elements manually
swmonth_tweets$stripped_text <- gsub("http.*","",  swmonth_tweets$text)
swmonth_tweets$stripped_text <- gsub("https.*","", swmonth_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swmonth_tweets_clean <- swmonth_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words 
swmonth_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #SWmonth tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
swmonth_cleaned_tweet_words <- swmonth_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
swmonth_cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in #swmonth tweets",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
#library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swmonth_tweets_paired_words <- swmonth_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swmonth_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

#library(tidyr)
swmonth_tweets_separated_words <- swmonth_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swmonth_tweets_filtered <- swmonth_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swmonth_words_counts <- swmonth_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swmonth_words_counts)

# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swmonth_words_counts %>%
  filter(n >= 12) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - SWmonth",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swmonth_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #swmonth",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#swmonth tweets
swmonth2_tweets <- search_tweets(q = "swmonth",n = 500000,type = "recent",include_rts = FALSE, parse = TRUE,token = my_token,
                                retryonratelimit = TRUE,verbose = TRUE)
#228

#tweets over collection time (+/- 9 days)
swmonth2_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWmonth Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# install descriptive stats libraries
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)

# remove http elements manually
swmonth2_tweets$stripped_text <- gsub("http.*","",  swmonth2_tweets$text)
swmonth2_tweets$stripped_text <- gsub("https.*","", swmonth2_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swmonth2_tweets_clean <- swmonth2_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
swmonth2_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #swmonth tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
swmonth2_cleaned_tweet_words <- swmonth2_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
swmonth2_cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in #swmonth tweets",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swmonth2_tweets_paired_words <- swmonth2_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swmonth2_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

library(tidyr)
swmonth2_tweets_separated_words <- swmonth2_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swmonth2_tweets_filtered <- swmonth2_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swmonth2_words_counts <- swmonth2_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swmonth2_words_counts)

# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swmonth2_words_counts %>%
  filter(n >= 12) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - SWmonth",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swmonth2_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #swmonth.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################


#swgenerationsstrong tweets
swgens_tweets <- search_tweets(q = "swgenerationsstrong",n = 500000,type = "recent",include_rts = FALSE, parse = TRUE,token = my_token,
                                retryonratelimit = TRUE,verbose = TRUE)
#152

#tweets over collection time (+/- 9 days)
swgens_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWgenerationsStrong Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# install descriptive stats libraries
# library(ggplot2)
# library(dplyr)
# library(tidytext)
# library(igraph)
# library(ggraph)

# remove http elements manually
swgens_tweets$stripped_text <- gsub("http.*","",  swgens_tweets$text)
swgens_tweets$stripped_text <- gsub("https.*","", swgens_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swgens_tweets_clean <- swgens_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
swgens_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #SWGenerationsStrong tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
swgens_cleaned_tweet_words <- swgens_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
swgens_cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in #SWGenerationsStrong tweets",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
# library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swgens_tweets_paired_words <- swgens_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swgens_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

# library(tidyr)

swgens_tweets_separated_words <- swgens_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swgens_tweets_filtered <- swgens_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swgens_words_counts <- swgens_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swgens_words_counts)

# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swgens_words_counts %>%
  filter(n >= 12) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - SwGenerationsStrong",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swgens_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #SWgenerationsStrong.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#socialworkmonthtweets
swers_tweets <- search_tweets(q = "socialworkers",n = 500000,type = "recent",include_rts = FALSE, parse = TRUE,token = my_token,
                                retryonratelimit = TRUE,verbose = TRUE)

#792

#tweets over collection time (+/- 9 days)
swers_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #socialworkers Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# # install descriptive stats libraries
# library(ggplot2)
# library(dplyr)
# library(tidytext)
# library(igraph)
# library(ggraph)

# remove http elements manually
swers_tweets$stripped_text <- gsub("http.*","",  swers_tweets$text)
swers_tweets$stripped_text <- gsub("https.*","", swers_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swers_tweets_clean <- swers_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
swers_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #socialworkers tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
swers_cleaned_tweet_words <- swers_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
swers_cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in #socialworker tweets",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
# library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swers_tweets_paired_words <- swers_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swers_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

# library(tidyr)
swers_tweets_separated_words <- swers_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swers_tweets_filtered <- swers_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swers_words_counts <- swers_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swers_words_counts)
# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swers_words_counts %>%
  filter(n >= 12) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - socialworkers",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swers_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #socialworkers.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#sswr timeline
sswr <- get_timeline("SSWRorg", n = 3200)
#88 tweets

##############################
##############################################
###################################################################


#cswe timeline
cswe <- get_timeline("CSocialWorkEd", n = 3200)
names(cswe)
#3197 tweets

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
cswe %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by CSWE",
    subtitle = "Twitter status (tweet) counts aggregated by day from march 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


# remove http elements manually
cswe$stripped_text <- gsub("http.*","",  cswe$text)
cswe$stripped_text <- gsub("https.*","", cswe$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
cswe_tweets_clean <- cswe %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)
  

# plot the top 15 words -- notice any issues?
cswe_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @CSWE tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
cswe_cleaned_tweet_words <- cswe_tweets_clean %>%
  anti_join(stop_words) 

names(cswe_cleaned_tweet_words)

# plot the top 15 words
cswe_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @CSWE tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

#network of words

# remove punctuation, convert to lowercase, add id for each tweet!
cswe_tweets_paired_words <- cswe %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

cswe_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

cswe_tweets_separated_words <- cswe_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

names(cswe_tweets_separated_words)

cswe_tweets_filtered <- cswe_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

names(cswe_tweets_filtered)

# new bigram counts:
cswe_words_counts <- cswe_tweets_filtered %>%
  count(word1, word2, sort = TRUE)%>%
  dplyr::select(created_at)

names(cswe_words_counts)
head(cswe_words_counts)

# plot climate change word network
# (plotting graph edges is currently broken)
cswe_words_counts %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets from CSWE",
       #subtitle = "During COVID-19 (After 3/1/2020) ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- cswe_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @CSWE.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#IFSW timeline
ifsw <- get_timeline("ifsw", n = 3200)
#1132 tweets 

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
ifsw %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by IFSW",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
ifsw$stripped_text <- gsub("http.*","",  ifsw$text)
ifsw$stripped_text <- gsub("https.*","", ifsw$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
ifsw_tweets_clean <- ifsw %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)

names(ifsw_tweets_clean)
names(ifsw)

# plot the top 15 words -- notice any issues?
ifsw_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @IFSW tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
isfw_cleaned_tweet_words <- ifsw_tweets_clean %>%
  anti_join(stop_words) 

names(isfw_cleaned_tweet_words)

# plot the top 15 words
isfw_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @IFSW tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")


# join sentiment classification to the tweet words
bing_word_counts <- cswe_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @CSWE.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################


#nasw timeline
nasw <- get_timeline("nasw", n = 3200)
#3191 twets

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
nasw %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


# remove http elements manually
nasw$stripped_text <- gsub("http.*","",  nasw$text)
nasw$stripped_text <- gsub("https.*","", nasw$stripped_text)

head(nasw$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_tweets_clean <- nasw %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_cleaned_tweet_words <- nasw_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_cleaned_tweet_words)

# plot the top 15 words
nasw_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")


# join sentiment classification to the tweet words
bing_word_counts <- nasw_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWNYC timeline
naswnyc <- get_timeline("naswnyc", n = 3200)
#3098 tweets 

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswnyc %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW_NYC",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswnyc$stripped_text <- gsub("http.*","",  naswnyc$text)
naswnyc$stripped_text <- gsub("https.*","", naswnyc$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswnyc_tweets_clean <- naswnyc %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswnyc_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWNYC tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswnyc_cleaned_tweet_words <- naswnyc_tweets_clean %>%
  anti_join(stop_words) 

names(cswe_cleaned_tweet_words)

# plot the top 15 words
naswnyc_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW_NYC tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswnyc_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWNYC.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################

#NASWNJ timeline
naswnj <- get_timeline("NASWNJ", n = 3200)
#3197

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswnj %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW_NJ",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswnj$stripped_text <- gsub("http.*","",  naswnj$text)
naswnj$stripped_text <- gsub("https.*","", naswnj$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswnj_tweets_clean <- naswnj %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswnj_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASWNJ tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswnj_cleaned_tweet_words <- naswnj_tweets_clean %>%
  anti_join(stop_words) 

names(naswnj_cleaned_tweet_words)

# plot the top 15 words
naswnj_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWNJ tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswnj_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWNJ.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################

#NASWNC timeline
naswnc <- get_timeline("NASWNC", n = 3200)
#3182

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswnc %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASWNC",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswnc$stripped_text <- gsub("http.*","",  naswnc$text)
naswnc$stripped_text <- gsub("https.*","", naswnc$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswnc_tweets_clean <- naswnc %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
naswnc_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-NC tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswnc_cleaned_tweet_words <- naswnc_tweets_clean %>%
  anti_join(stop_words) 

names(naswnc_cleaned_tweet_words)

# plot the top 15 words
naswnc_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASWNC tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswnc_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASWNC.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWMI timeline NASWMI
naswmi <- get_timeline("NASWMI", n = 3200)
#3196

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswmi %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW_MI",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswmi$stripped_text <- gsub("http.*","",  naswmi$text)
naswmi$stripped_text <- gsub("https.*","", naswmi$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswmi_tweets_clean <- naswmi %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
naswmi_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-MI tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswmi_cleaned_tweet_words <- naswmi_tweets_clean %>%
  anti_join(stop_words) 

names(naswmi_cleaned_tweet_words)

# plot the top 15 words
naswmi_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-MI tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswmi_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-MI.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWMA timeline
naswma <- get_timeline("NASWMA", n = 3200)
#3196

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswma %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-MA",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswma$stripped_text <- gsub("http.*","",  naswma$text)
naswma$stripped_text <- gsub("https.*","", naswma$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswma_tweets_clean <- naswma %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswma_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-MA tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswma_cleaned_tweet_words <- naswma_tweets_clean %>%
  anti_join(stop_words) 

names(naswma_cleaned_tweet_words)

# plot the top 15 words
naswma_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-MA tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswma_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-MA.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################

#NASWAZ timeline
naswaz <- get_timeline("NASWAZ", n = 3200)
#1015

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswaz %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-AZ",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswaz$stripped_text <- gsub("http.*","",  naswaz$text)
naswaz$stripped_text <- gsub("https.*","", naswaz$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswaz_tweets_clean <- naswaz %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words
naswaz_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-AZ tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswaz_cleaned_tweet_words <- naswaz_tweets_clean %>%
  anti_join(stop_words) 

names(naswaz_cleaned_tweet_words)

# plot the top 15 words
naswaz_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-AZ tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswaz_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-AZ.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASW_Kentucky timeline
naswkentucky <- get_timeline("NASW_Kentucky", n = 3200)
#3198

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswkentucky %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-KY",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswkentucky$stripped_text <- gsub("http.*","",  naswkentucky$text)
naswkentucky$stripped_text <- gsub("https.*","", naswkentucky$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswkentucky_tweets_clean <- naswkentucky %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswkentucky_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-KY tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswkentucky_cleaned_tweet_words <- naswkentucky_tweets_clean %>%
  anti_join(stop_words) 

names(naswkentucky_cleaned_tweet_words)

# plot the top 15 words
naswkentucky_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-KY tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswkentucky_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-KY.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASW_GA timeline
naswga <- get_timeline("NASW_GA", n = 3200)
#399

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswga %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-GA",
    subtitle = "Twitter status (tweet) counts aggregated by day from Jan 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswga$stripped_text <- gsub("http.*","",  naswga$text)
naswga$stripped_text <- gsub("https.*","", naswga$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswga_tweets_clean <- naswga %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswga_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-GA tweets after 1/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswga_cleaned_tweet_words <- naswga_tweets_clean %>%
  anti_join(stop_words) 

names(naswga_cleaned_tweet_words)

# plot the top 15 words
naswga_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-GA tweets after 1/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswga_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-GA.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASW_PA timeline
naswpa <- get_timeline("NASW_PA", n = 3200)
#1951

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswpa %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-PA",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswpa$stripped_text <- gsub("http.*","",  naswpa$text)
naswpa$stripped_text <- gsub("https.*","", naswpa$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswpa_tweets_clean <- naswpa %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswpa_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-PA tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswpa_cleaned_tweet_words <- naswpa_tweets_clean %>%
  anti_join(stop_words) 

names(naswpa_cleaned_tweet_words)

# plot the top 15 words
naswpa_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-PA tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswpa_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-PA.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWAlabama timeline
naswalabama <- get_timeline("NASWAlabama", n = 3200)
#1385

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswalabama %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-AL",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswalabama$stripped_text <- gsub("http.*","",  naswalabama$text)
naswalabama$stripped_text <- gsub("https.*","", naswalabama$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswalabama_tweets_clean <- naswalabama %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswalabama_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-AL tweets after 1/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswalabama_cleaned_tweet_words <- naswalabama_tweets_clean %>%
  anti_join(stop_words) 

names(naswalabama_cleaned_tweet_words)

# plot the top 15 words
naswalabama_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-AL tweets after 1/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswalabama_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-AL",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################


#NASWIl timeline
naswil <- get_timeline("NASWIl", n = 3200)
#3186

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswil %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-IL",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswil$stripped_text <- gsub("http.*","",  naswil$text)
naswil$stripped_text <- gsub("https.*","", naswil$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswil_tweets_clean <- naswil %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswil_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-IL tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswil_cleaned_tweet_words <- naswil_tweets_clean %>%
  anti_join(stop_words) 

names(naswil_cleaned_tweet_words)

# plot the top 15 words
naswil_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-IL tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswil_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-IL.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWTX timeline
naswtx <- get_timeline("NASWTX", n = 3200)
#1851

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswtx %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-TX",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswtx$stripped_text <- gsub("http.*","", naswtx$text)
naswtx$stripped_text <- gsub("https.*","", naswtx$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswtx_tweets_clean <- naswtx %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswtx_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-TX tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswtx_cleaned_tweet_words <- naswtx_tweets_clean %>%
  anti_join(stop_words) 

names(naswtx_cleaned_tweet_words)

# plot the top 15 words
naswtx_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-TX tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswtx_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-TX.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWMN timeline
naswmn <- get_timeline("NASWMN", n = 3200)
#901

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswmn %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-MN",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswmn$stripped_text <- gsub("http.*","",  naswmn$text)
naswmn$stripped_text <- gsub("https.*","", naswmn$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswmn_tweets_clean <- naswmn %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words 
naswmn_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-MN tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswmn_cleaned_tweet_words <- naswmn_tweets_clean %>%
  anti_join(stop_words) 

names(naswmn_cleaned_tweet_words)

# plot the top 15 words
naswmn_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-MN tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswmn_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-MN",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################

#NASWalaska timeline
naswalaska <- get_timeline("NASWalaska", n = 3200)
#346

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswalaska %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-AK",
    subtitle = "Twitter status (tweet) counts aggregated by day from Jan 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswalaska$stripped_text <- gsub("http.*","",  naswalaska$text)
naswalaska$stripped_text <- gsub("https.*","", naswalaska$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswalaska_tweets_clean <- naswalaska %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswalaska_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-AK tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswalaska_cleaned_tweet_words <- naswalaska_tweets_clean %>%
  anti_join(stop_words) 

names(naswalaska_cleaned_tweet_words)

# plot the top 15 words
naswalaska_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-AK tweets after 1/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswalaska_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-AK",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWCA_Advocacy timeline
naswca <- get_timeline("NASWCA_Advocacy", n = 3200)
#1371

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswca %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-CA",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswca$stripped_text <- gsub("http.*","",  naswca$text)
naswca$stripped_text <- gsub("https.*","", naswca$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswca_tweets_clean <- naswca %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswca_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-CA tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswca_cleaned_tweet_words <- naswca_tweets_clean %>%
  anti_join(stop_words) 

names(naswca_cleaned_tweet_words)

# plot the top 15 words
naswca_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-CA tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswca_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-CA",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWWI timeline
naswwi <- get_timeline("NASWWI", n = 3200)
#699

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswwi %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-WI",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswwi$stripped_text <- gsub("http.*","",  naswwi$text)
naswwi$stripped_text <- gsub("https.*","", naswwi$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswwi_tweets_clean <- naswwi %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswwi_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-WI tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswwi_cleaned_tweet_words <- naswwi_tweets_clean %>%
  anti_join(stop_words) 

names(naswwi_cleaned_tweet_words)

# plot the top 15 words
naswwi_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-WI tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswwi_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-WI",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWOregon timeline
naswor <- get_timeline("NASWOregon", n = 3200)
#679

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswor %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-OR",
    subtitle = "Twitter status (tweet) counts aggregated by day from Jan  1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswor$stripped_text <- gsub("http.*","",  naswor$text)
naswor$stripped_text <- gsub("https.*","", naswor$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswor_tweets_clean <- naswor %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswor_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-OR tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswor_cleaned_tweet_words <- naswor_tweets_clean %>%
  anti_join(stop_words) 

names(naswor_cleaned_tweet_words)

# plot the top 15 words
naswor_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-OR tweets after 1/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswor_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-OR",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#KSsocialworkers timeline
naswks <- get_timeline("KSsocialworkers", n = 3200)
#792

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswks %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-KS",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswks$stripped_text <- gsub("http.*","",  naswks$text)
naswks$stripped_text <- gsub("https.*","", naswks$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswks_tweets_clean <- naswks %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswks_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-KS tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswks_cleaned_tweet_words <- naswks_tweets_clean %>%
  anti_join(stop_words) 

names(naswks_cleaned_tweet_words)

# plot the top 15 words
naswks_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-KS tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswks_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-KS",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASW_IN timeline
naswin <- get_timeline("NASW_IN", n = 3200)
#1148

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswin %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-IL",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswin$stripped_text <- gsub("http.*","",  naswin$text)
naswin$stripped_text <- gsub("https.*","", naswin$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswin_tweets_clean <- naswin %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswin_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-IL tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswin_cleaned_tweet_words <- naswin_tweets_clean %>%
  anti_join(stop_words) 

names(naswin_cleaned_tweet_words)

# plot the top 15 words
naswin_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-IL tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswin_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-IL",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASWOhioChapter timeline
naswoh <- get_timeline("NASWOhioChapter", n = 3200)
#799

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswoh %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-OH",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswoh$stripped_text <- gsub("http.*","",  naswoh$text)
naswoh$stripped_text <- gsub("https.*","", naswoh$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswoh_tweets_clean <- naswoh %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswoh_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-OH tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswoh_cleaned_tweet_words <- naswoh_tweets_clean %>%
  anti_join(stop_words) 

names(naswoh_cleaned_tweet_words)

# plot the top 15 words
naswoh_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-OH tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswoh_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-OH",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################

#NASW_NH timeline
naswnh <- get_timeline("NASW_NH", n = 3200)
#332

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswnh %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-NH",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswnh$stripped_text <- gsub("http.*","",  naswnh$text)
naswnh$stripped_text <- gsub("https.*","", naswnh$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswnh_tweets_clean <- naswnh %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswnh_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-NH tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswnh_cleaned_tweet_words <- naswnh_tweets_clean %>%
  anti_join(stop_words) 

names(naswnh_cleaned_tweet_words)

# plot the top 15 words
naswnh_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-NH tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswnh_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-NH",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#NASW_Arkansas timeline
naswar <- get_timeline("NASW_Arkansas", n = 3200)
#347

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswar %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NASW-AR",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswar$stripped_text <- gsub("http.*","",  naswar$text)
naswar$stripped_text <- gsub("https.*","", naswar$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswar_tweets_clean <- naswar %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswar_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NASW-AR tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswar_cleaned_tweet_words <- naswar_tweets_clean %>%
  anti_join(stop_words) 

names(naswar_cleaned_tweet_words)

# plot the top 15 words
naswar_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NASW-AR tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswar_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NASW-AR",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##############################
##############################################
###################################################################


#some people
#director of NASW WV
naswwv <- get_timeline("SamuelHickman", n = 3200)
#3197

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
naswwv %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by Samuel Hickman(NASW-WV)",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
naswwv$stripped_text <- gsub("http.*","",  naswwv$text)
naswwv$stripped_text <- gsub("https.*","", naswwv$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
naswwv_tweets_clean <- naswwv %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
naswwv_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @SamuelHickman(NASW-WV) tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
naswwv_cleaned_tweet_words <- naswwv_tweets_clean %>%
  anti_join(stop_words) 

names(naswwv_cleaned_tweet_words)

# plot the top 15 words
naswwv_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @SamuelHickman(NASW-WV) tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- naswwv_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @SamuelHIckman(NASW-WV)",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


##############################
##############################################
###################################################################

#president elect of NASW
nasw_prez <- get_timeline("Mittjoy", n = 3200)
#3135

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
nasw_prez %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by MIttJoy(NASW-President)",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nasw_prez$stripped_text <- gsub("http.*","",  nasw_prez$text)
nasw_prez$stripped_text <- gsub("https.*","", nasw_prez$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nasw_prez_tweets_clean <- nasw_prez %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nasw_prez_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @MIttJoy(NASW-President) tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nasw_prez_cleaned_tweet_words <- nasw_prez_tweets_clean %>%
  anti_join(stop_words) 

names(nasw_prez_cleaned_tweet_words)

# plot the top 15 words
nasw_prez_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @MittJoy tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nasw_prez_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @MittJoy (NASW-President)",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#other SW orgs 
#NASWFoundation timeline
naswfoundation <- get_timeline("NASWFoundation", n = 3200)
#127

##############################
##############################################
###################################################################

#TheNSWM network for social work management  timeline
nswm <- get_timeline("TheNSWM", n = 3200)
#3198

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
nswm %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NWSM",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
nswm$stripped_text <- gsub("http.*","",  nswm$text)
nswm$stripped_text <- gsub("https.*","", nswm$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
nswm_tweets_clean <- nswm %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
nswm_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NSWM tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
nswm_cleaned_tweet_words <- nswm_tweets_clean %>%
  anti_join(stop_words) 

names(nswm_cleaned_tweet_words)

# plot the top 15 words
nswm_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NSWM tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- nswm_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NSWm",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################


#ACOSA timeline
acosa <- get_timeline("acosaorg", n = 3200)
#3178

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time
acosa %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by ACOSA",
    subtitle = "Twitter status (tweet) counts aggregated by day from Jan 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
acosa$stripped_text <- gsub("http.*","",  acosa$text)
acosa$stripped_text <- gsub("https.*","", acosa$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
acosa_tweets_clean <- acosa %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
acosa_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @ACOSA tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
acosa_cleaned_tweet_words <- acosa_tweets_clean %>%
  anti_join(stop_words) 

names(acosa_cleaned_tweet_words)

# plot the top 15 words
acosa_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @ACOSA tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- acosa_tweets_clean %>%
  dplyr::filter(created_at > "2020-01-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @ACOSA",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


##############################
##############################################
###################################################################

#The New Social Worker timeline
newsocialwrk <- get_timeline("newsocialworker", n = 3200)
#3198

#tweets over collection time (+/- 9 days)
## plot the frequency of tweets for each user over time

newsocialwrk %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  #dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by NewSocialWorker",
    subtitle = "Twitter status (tweet) counts aggregated by day from March 1st, 2020",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# remove http elements manually
newsocialwrk$stripped_text <- gsub("http.*","",  newsocialwrk$text)
newsocialwrk$stripped_text <- gsub("https.*","", newsocialwrk$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
newsocialwrk_tweets_clean <- newsocialwrk %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


# plot the top 15 words -- notice any issues?
newsocialwrk_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01") %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in @NewSocialWorker tweets after 3/1/2020")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
newsocialwrk_cleaned_tweet_words <- newsocialwrk_tweets_clean %>%
  anti_join(stop_words) 

names(newsocialwrk_cleaned_tweet_words)

# plot the top 15 words
newsocialwrk_cleaned_tweet_words %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in @NewSocialWorker tweets after 3/1/2020",
       subtitle = "Stop words removed from the list")

# join sentiment classification to the tweet words
bing_word_counts <- newsocialwrk_tweets_clean %>%
  dplyr::filter(created_at > "2020-03-01")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic from @NewSocialWork",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

swcovid2_tweets <- search_tweets(q = "swcovid19",n = 500000,type = "recent",include_rts = TRUE, parse = TRUE,token = my_token,
                                retryonratelimit = TRUE,verbose = TRUE)

swcovid2_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #SWcovid19 Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## create lat/lng variables using all available tweet and profile geo-location data
covid2_latlong <- lat_lng(swcovid2_tweets)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(covid2_latlong, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

# how many locations are represented
length(unique(swcovid2_tweets$location))
#133

# install descriptive stats libraries
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)

#where are users from 
swcovid2_tweets %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(15) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users during #SWcovid19- unique locations ")

names(swcovid2_tweets)

# remove http elements manually
swcovid2_tweets$stripped_text <- gsub("http.*","",  swcovid2_tweets$text)
swcovid2_tweets$stripped_text <- gsub("https.*","", swcovid2_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
swcovid2_tweets_tweets_clean <- swcovid2_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plot the top 15 words -- notice any issues?
swcovid2_tweets_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #SWcovid19 tweets")

#remove stop words
# load list of stop words - from the tidytext package
data("stop_words")

#@other_stop_words <- c("amp", "swtech")

# remove stop words from your list of words
swcovid2_cleaned_tweet_words <- swcovid2_tweets_tweets_clean %>%
  anti_join(stop_words) 

# plot the top 15 words -- notice any issues?
swcovid2_cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets using #SWcovid19",
       subtitle = "Stop words removed from the list")

#network of words
# library(devtools)
#install_github("dgrtwo/widyr")
library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
swcovid2_tweets_tweets_paired_words <- swcovid2_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

swcovid2_tweets_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

library(tidyr)
swcovid2_tweets_tweets_separated_words <- swcovid2_tweets_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

swcovid2_tweets_filtered <- swcovid2_tweets_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
swcovid2_words_counts <- swcovid2_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(swcovid2_words_counts)

# library(igraph)
# library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
swcovid2_words_counts %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n))
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - SWcovid19",
       subtitle = "During COVID-19 ",
       x = "", y = "")

# join sentiment classification to the tweet words
bing_word_counts <- swcovid2_tweets_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID-19 pandemic using #SWcovid19",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

##############################
##############################################
###################################################################

#merge all nasw sets

nasw_all <- bind_rows(nasw, nasw_prez, naswalabama, naswalaska, naswar, naswaz, naswca, 
          naswga, naswil, naswin, naswkentucky, naswks, naswma, naswmi, naswmn, naswnc, 
          naswnh, naswnj, naswnyc, naswoh, naswor, naswpa, naswtx, naswwi,naswwv)

sw_orgs <- bind_rows(acosa, cswe, ifsw, newsocialwrk, nswm, sswr)

sw_tags <- bind_rows(swgens_tweets, swmonth_tweets, swmonth2_tweets, swtech_tweets, swcovid_tweets, swcovid2_tweets)


#subtset to after Jan 21st, first confimed COVID-19 case in US
#or December 31st, 2019 when news first broke in China? 
#timeline source: https://www.nbcnews.com/health/health-news/coronavirus-timeline-tracking-critical-moments-covid-19-n1154341

#subset 

#conduct stm 

#estimate effect of by created_at

#use stminsight to look for covid19 topics...
