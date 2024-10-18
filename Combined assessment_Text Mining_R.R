#installing and loading the mongolite library to download the Airbnb data
 #need to run this line of code only once and then you can comment out
library(mongolite)

# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://sphtskialadze:Nala1998@ac-p6myeyl-shard-00-01.vzr4z86.mongodb.net'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()

#######################################################
#if you know or want to learn MQL (MongoQueryLanguage), that is a JSON syntax, feel free to use the following:::
######################################################
#1 subsetting your data based on a condition:
mydf <- airbnb_collection$find('{"bedrooms":2, "price":{"$gt":50}}')

#2 writing an analytical query on the data::
mydf_analytical <- airbnb_collection$aggregate('[{"$group":{"_id":"$room_type", "avg_price": {"$avg":"price"}}}]')

library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(purrr)
library(topicmodels)
library(tm)
library(SnowballC)
library(broom)
library(leaflet)
library(shinyjs)


## Overall look at the data, trying to see the whole data and to have a basic understanding of it.
airbnb_alldf <- airbnb_all %>%
  mutate(country = address$country,
         market = address$market,
         text = summary)

Description <- airbnb_alldf %>%
  mutate(review_scores_rating = review_scores$review_scores_rating) %>%
  select(country, text, review_scores_rating)
countries_counts <- airbnb_alldf %>%
  group_by(address$country) %>%
  summarise(count = n())

US <- airbnb_alldf %>%
  filter(country == "United States")
Brazil <- airbnb_alldf %>%
  filter(country == "Brazil")
Canada <- airbnb_alldf %>%
  filter(country == "Canada")
Australia <- airbnb_alldf %>%
  filter(country == "Australia")
Turkey <- airbnb_alldf %>%
  filter(country == "Turkey")
Spain <- airbnb_alldf %>%
  filter(country == "Spain")
Hong_Kong <- airbnb_alldf %>%
  filter(country == "Hong Kong")
Portugal <- airbnb_alldf %>%
  filter(country == "Portugal")

tokenized_data <- airbnb_alldf %>%
  group_by(country) %>% 
  unnest_tokens(word, description) %>%
  count(country, word, sort = TRUE)

data("stop_words")

# Filter out stop words
filtered_tokenized_data <- tokenized_data %>%
  anti_join(stop_words, by = "word")

US_tokens <- filtered_tokenized_data %>%
  filter(country == "United States")

# Calculate TF-IDF do it for all
tf_idf <- US_tokens %>%
  group_by(word) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  mutate(total_words = sum(n)) %>%
  ungroup() %>%
  mutate(tf = n / total_words) %>%
  group_by(word) %>%
  mutate(doc_freq = n()) %>%
  ungroup() %>%
  mutate(idf = log(n_distinct(US_tokens$country) / doc_freq),
         tf_idf = tf * idf) %>%
  arrange(desc(tf_idf))

#sentiment analysis

nrc_words <- get_sentiments("nrc")
afinn_words <- get_sentiments("afinn")
bing_words <- get_sentiments("bing")

# NRC Sentiment Analysis
nrc_sentiment <- tokenized_data %>%
  inner_join(nrc_words, by = "word")

# AFINN Sentiment Analysis
afinn_sentiment <- tokenized_data %>%
  inner_join(afinn_words, by = "word")

# Bing Sentiment Analysis
bing_sentiment <- tokenized_data %>%
  inner_join(bing_words, by = "word")

# Summarize sentiment analysis to get a sense of the overall sentiment in the descriptions 
nrc_summary <- nrc_sentiment %>%
  count(country, sentiment, sort = TRUE)

# Summarize AFINN sentiment (adjusting for the AFINN score)
afinn_summary <- afinn_sentiment %>%
  group_by(country) %>%
  summarise(total_afinn_score = sum(value))

# Summarize Bing sentiment
bing_summary <- bing_sentiment %>%
  count(country, sentiment, sort = TRUE)

#For AFINN bar plot can show positive vs negative scores
ggplot(afinn_summary, aes(x = country, y = total_afinn_score, fill = total_afinn_score > 0)) +
  geom_col(show.legend = TRUE) +
  labs(title = "AFINN Sentiment Scores by Property Type")

#Bing sentiment plot
ggplot(bing_summary, aes(x = country, y = n, fill = sentiment)) +
  geom_col() +
  labs(title = "Bing Sentiment Distribution by Property Type") +
  theme_minimal() +
  facet_wrap(~sentiment, scales = "free_y")

#Analyzing the most used words by country
# Mostly used in US
mostly_used_US <- Description %>%
  filter(country == "United States") %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  count(word, sort = TRUE) %>%
  ungroup()

##############################################################################################
# Mostly used in the choose contries (Will be used later in the analysis of the Turkish market)
##############################################################################################

mostly_used_Turkey <- Description %>%
  filter(country == "Turkey") %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  count(word, sort = TRUE) %>%
  ungroup()

#Mostly used in Hong-Kong

mostly_used_hong_kong <- Description %>%
  filter(country == "Hong Kong") %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  count(word, sort = TRUE) %>%
  ungroup()

#Australia

mostly_used_australia <- Description %>%
  filter(country == "Australia") %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  count(word, sort = TRUE) %>%
  ungroup()
############################################################################################################
## After looking at the broad picture, there is not much business value that we can create.#################
############################################################################################################
## Let's research deeper and focus our attention on a specific markets in a different regions of the world.
############################################################################################################
                         ###Analysis of bigrams in Turkey market. ####

# Words and bigrams analysis
Description <- Description %>%
  mutate(document_id = row_number())

bigrams_turkey <- Description %>%
  filter(country == "Turkey") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  select(document_id, bigram)

total_documents <- bigrams_turkey %>%
  summarise(total = n_distinct(document_id)) %>%
  pull(total)

bigram_counts_turkey <- bigrams_turkey %>%
  count(bigram, sort = TRUE) %>%
  top_n(15) 

bigram_graph_data <- bigram_counts_turkey %>%
  filter(n > 15) %>%  # Filter to use only bigrams that occur more than 10 times
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  graph_from_data_frame()

ggraph(bigram_graph_data, layout = "fr") +  # Using the Fruchterman-Reingold layout
  geom_edge_link(aes(width = sqrt(n)), edge_alpha = 0.8, color = "gray") +  # Edges with widths adjusted by count
  geom_node_point(size = 5, color = "steelblue") +  # Nodes
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  

# Calculate TF-IDF for Turkey on the bigrams
bigram_tf_idf_turkey <- bigrams_turkey %>%
  group_by(bigram) %>%
  summarise(n = n(),  # Document frequency
            tf = sum(n) / total_documents) %>%  # Term frequency
  mutate(idf = log(total_documents / n),  # Inverse document frequency
         tf_idf = tf * idf) %>%  # TF-IDF score
  arrange(desc(tf_idf))

# Sentiment analyzis on Turkey #

sentiment_turkey <- Description %>%
  filter(country == "Turkey") %>%
  unnest_tokens(word, text)
afinn_turkey <- sentiment_turkey %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarise(sentiment = sum(value), .groups = 'drop') %>%
  mutate(method = "AFINN")

bing_turkey <- sentiment_turkey %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  mutate(method = "Bing et al.")

nrc_turkey <- sentiment_turkey %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  mutate(method = "NRC")

bing_and_nrc_turkey <- bind_rows(bing_turkey, nrc_turkey) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

combined_sentiments_turkey <- bind_rows(
  summarise(afinn_turkey, sentiment = sentiment, method = method),
  summarise(bing_and_nrc_turkey, sentiment = sentiment_score, method = method)
)

ggplot(combined_sentiments_turkey, aes(x = method, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Analysis Comparison for Turkey Descriptions",
       x = "Method",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

                           ## Anlysis on United States Market ##

# Words and bigrams analysis

bigrams_US <- Description %>%
  filter(country == "United States") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  select(document_id, bigram)

bigram_counts_us <- bigrams_US %>%
  count(bigram, sort = TRUE) %>%
  top_n(15) 

bigram_graph_data_us <- bigram_counts_us %>%
  filter(n > 15) %>%  # Filter to use only bigrams that occur more than 10 times
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  graph_from_data_frame()

ggraph(bigram_graph_data_us, layout = "fr") +  # Using the Fruchterman-Reingold layout
  geom_edge_link(aes(width = sqrt(n)), edge_alpha = 0.8, color = "gray") +  # Edges with widths adjusted by count
  geom_node_point(size = 5, color = "steelblue") +  # Nodes
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Calculate TF-IDF for US

bigram_tf_idf_us <- bigrams_US %>%
  group_by(bigram) %>%
  summarise(n = n(),  # Document frequency
            tf = sum(n) / total_documents) %>%  # Term frequency
  mutate(idf = log(total_documents / n),  # Inverse document frequency
         tf_idf = tf * idf) %>%  # TF-IDF score
  arrange(desc(tf_idf))

# Analyzing sentiment for United States # 

sentiment_us <- Description %>%
  filter(country == "United States") %>%
  unnest_tokens(word, text)
afinn_us <- sentiment_us %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarise(sentiment = sum(value), .groups = 'drop') %>%
  mutate(method = "AFINN")

bing_us <- sentiment_us %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  mutate(method = "Bing et al.")

nrc_us <- sentiment_us %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  mutate(method = "NRC")

bing_and_nrc_us <- bind_rows(bing_us, nrc_us) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

combined_sentiments_us <- bind_rows(
  summarise(afinn_us, sentiment = sentiment, method = method),
  summarise(bing_and_nrc_us, sentiment = sentiment_score, method = method)
)

ggplot(combined_sentiments_us, aes(x = method, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Analysis Comparison for United States Descriptions",
       x = "Method",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

                                 ## Analyzing Hong-Kong Market ##

# Words and bigrams analysis

bigrams_hong_kong <- Description %>%
  filter(country == "Hong Kong") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  select(document_id, bigram)

bigram_counts_hong <- bigrams_hong_kong %>%
  count(bigram, sort = TRUE) %>%
  top_n(15) 

bigram_graph_data_hong <- bigram_counts_hong %>%
  filter(n > 15) %>%  # Filter to use only bigrams that occur more than 10 times
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  graph_from_data_frame()

ggraph(bigram_graph_data_hong, layout = "fr") +  # Using the Fruchterman-Reingold layout
  geom_edge_link(aes(width = sqrt(n)), edge_alpha = 0.8, color = "gray") +  # Edges with widths adjusted by count
  geom_node_point(size = 5, color = "steelblue") +  # Nodes
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Calculate TF-IDF for Hong Kong
bigram_tf_idf_hong <- bigrams_hong_kong %>%
  filter()%>%
  group_by(bigram) %>%
  summarise(n = n(),  # Document frequency
            tf = sum(n) / total_documents) %>%  # Term frequency
  mutate(idf = log(total_documents / n),  # Inverse document frequency
         tf_idf = tf * idf) %>%  # TF-IDF score
  arrange(desc(tf_idf))

# Analyzing sentiment for Hong Kong # 

sentiment_hong_kong <- Description %>%
  filter(country == "Hong Kong") %>%
  unnest_tokens(word, text)
afinn_hong_kong <- sentiment_hong_kong %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarise(sentiment = sum(value), .groups = 'drop') %>%
  mutate(method = "AFINN")

bing_hong_kong <- sentiment_hong_kong %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  mutate(method = "Bing et al.")

nrc_hong_kong <- sentiment_hong_kong %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  mutate(method = "NRC")

bing_and_nrc_hong_kong <- bind_rows(bing_hong_kong, nrc_hong_kong) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

combined_sentiments_hong_kong <- bind_rows(
  summarise(afinn_hong_kong, sentiment = sentiment, method = method),
  summarise(bing_and_nrc_hong_kong, sentiment = sentiment_score, method = method)
)

ggplot(combined_sentiments_hong_kong, aes(x = method, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Analysis Comparison for Hong Kong Descriptions",
       x = "Method",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

                                  ## Analyzing Brazilian market ## 
# Words and bigrams analysis

bigrams_brazil <- Description %>%
  filter(country == "Brazil") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  select(document_id, bigram)

bigram_counts_brazil <- bigrams_brazil %>%
  count(bigram, sort = TRUE) %>%
  top_n(15) 

bigram_graph_data_brazil <- bigram_counts_brazil %>%
  filter(n > 15) %>%  # Filter to use only bigrams that occur more than 10 times
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  graph_from_data_frame()

ggraph(bigram_graph_data_brazil, layout = "fr") +  # Using the Fruchterman-Reingold layout
  geom_edge_link(aes(width = sqrt(n)), edge_alpha = 0.8, color = "gray") +  # Edges with widths adjusted by count
  geom_node_point(size = 5, color = "steelblue") +  # Nodes
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Calculate TF-IDF for Brazil #
bigram_tf_idf_brazil <- bigrams_brazil %>%
  group_by(bigram) %>%
  summarise(n = n(),  # Document frequency
            tf = sum(n) / total_documents) %>%  # Term frequency
  mutate(idf = log(total_documents / n),  # Inverse document frequency
         tf_idf = tf * idf) %>%  # TF-IDF score
  arrange(desc(tf_idf))

# Sentiment analyzis on Brazil #

sentiment_brazil <- Description %>%
  filter(country == "Brazil") %>%
  unnest_tokens(word, text)
afinn_brazil <- sentiment_brazil %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarise(sentiment = sum(value), .groups = 'drop') %>%
  mutate(method = "AFINN")

bing_brazil <- sentiment_brazil %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  mutate(method = "Bing et al.")

nrc_brazil <- sentiment_brazil %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  mutate(method = "NRC")

bing_and_nrc_brazil <- bind_rows(bing_brazil, nrc_brazil) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

combined_sentiments_brazil <- bind_rows(
  summarise(afinn_brazil, sentiment = sentiment, method = method),
  summarise(bing_and_nrc_brazil, sentiment = sentiment_score, method = method)
)

ggplot(combined_sentiments_brazil, aes(x = method, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Analysis Comparison for Brazil Descriptions",
       x = "Method",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Now that we have a better understanding of the chosen markets, what are the differences in
# sentiments between them and the most frequent words and TF-IDF analysis? Let's see what the 
# differences are between the successful  accounts and the rest.

############################################################################################
# Properties with score above or equal to 99 and more than 5 reviews according to the data
############################################################################################


high_rated <- airbnb_alldf %>%
  filter(review_scores$review_scores_rating > 99 & number_of_reviews > 5)

##############################################################################
##### Sentiment and Bigrams for the high scoring properties in Hong Kong #####
##############################################################################

bigrams_high_rated_hong_kong <- high_rated %>%
  filter(country == "Hong Kong") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  top_n(15)

sentiment_high_score_hong_kong <- high_rated %>%
  filter(country == "Hong Kong") %>%
  unnest_tokens(word, text)

afinn_hongkong_high <- sentiment_high_score_hong_kong %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarise(sentiment = sum(value), .groups = 'drop') %>%
  mutate(method = "AFINN")

bing_high_hong_kong <- sentiment_high_score_hong_kong %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  mutate(method = "Bing et al.")

nrc_high_hong_kong <- sentiment_high_score_hong_kong %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  mutate(method = "NRC")

bing_and_nrc_hongkong_high <- bind_rows(bing_high_hong_kong, nrc_high_hong_kong) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

high_combined_sentiments_hongkong <- bind_rows(
  summarise(afinn_hongkong_high, sentiment = sentiment, method = method),
  summarise(bing_and_nrc_hongkong_high, sentiment = sentiment_score, method = method)
)

# Plot the sentiment analysis results
ggplot(high_combined_sentiments_hongkong, aes(x = method, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Analysis Comparison for High Scorers in Hong Kong",
       x = "Method",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bigrams Hong Kong

bigrams_high_rated_hong_kong <- high_rated %>%
  filter(country == "Hong Kong") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  top_n(15)

bigram_graph_high_hong_kong <- bigrams_high_rated_hong_kong %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  graph_from_data_frame()

ggraph(bigram_graph_high_hong_kong, layout = "fr") +  # Using the Fruchterman-Reingold layout
  geom_edge_link(aes(width = sqrt(n)), edge_alpha = 0.8, color = "gray") +  # Edges with widths adjusted by count
  geom_node_point(size = 5, color = "steelblue") +  # Nodes
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

###########################################################################
########### Sentiment for the high scoring properties in Brazil ###########
###########################################################################

sentiment_high_score_brazil <- high_rated %>%
  filter(country == "Brazil") %>%
  unnest_tokens(word, text)
afinn_brazil_high <- sentiment_high_score_brazil %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarise(sentiment = sum(value), .groups = 'drop') %>%
  mutate(method = "AFINN")

bing_high_brazil <- sentiment_high_score_brazil %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  mutate(method = "Bing et al.")

nrc_high_brazil <- sentiment_high_score_brazil %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  mutate(method = "NRC")

bing_and_nrc_brazil_high <- bind_rows(bing_high_brazil, nrc_high_brazil) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

high_combined_sentiments_brazil <- bind_rows(
  summarise(afinn_brazil_high, sentiment = sentiment, method = method),
  summarise(bing_and_nrc_brazil_high, sentiment = sentiment_score, method = method)
)

ggplot(high_combined_sentiments_brazil, aes(x = method, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Analysis Comparison for High Scorers in Brazil",
       x = "Method",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bigrams Brazil
bigrams_high_rated_brazil <- high_rated %>%
  filter(country == "Brazil") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  top_n(15)

bigram_graph_high_brazil <- bigrams_high_rated_brazil %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  graph_from_data_frame()

ggraph(bigram_graph_high_brazil, layout = "fr") +  # Using the Fruchterman-Reingold layout
  geom_edge_link(aes(width = sqrt(n)), edge_alpha = 0.8, color = "gray") +  # Edges with widths adjusted by count
  geom_node_point(size = 5, color = "steelblue") +  # Nodes
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

###########################################################################
#### Sentiment and Bigrams for the high scoring properties in Turkey ######
###########################################################################

sentiment_high_score_turkey <- high_rated %>%
  filter(country == "Turkey") %>%
  unnest_tokens(word, text)
afinn_turkey_high <- sentiment_high_score_turkey %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarise(sentiment = sum(value), .groups = 'drop') %>%
  mutate(method = "AFINN")

bing_high_turkey <- sentiment_high_score_turkey %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  mutate(method = "Bing et al.")

nrc_high_turkey <- sentiment_high_score_turkey %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  mutate(method = "NRC")

bing_and_nrc_turkey_high <- bind_rows(bing_high_turkey, nrc_high_turkey) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

high_combined_sentiments_turkey <- bind_rows(
  summarise(afinn_turkey_high, sentiment = sentiment, method = method),
  summarise(bing_and_nrc_turkey_high, sentiment = sentiment_score, method = method)
)

ggplot(high_combined_sentiments_turkey, aes(x = method, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Analysis Comparison for High Scorers in Brazil",
       x = "Method",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bigrams Turkey
bigrams_high_rated_turkey <- high_rated %>%
  filter(country == "Turkey") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  top_n(15)

bigram_graph_high_turkey <- bigrams_high_rated_turkey %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  graph_from_data_frame()

ggraph(bigram_graph_high_turkey, layout = "fr") +  
  geom_edge_link(aes(width = sqrt(n)), edge_alpha = 0.8, color = "gray") +  
  geom_node_point(size = 5, color = "steelblue") +  
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

###########################################################################
## Sentiment and Bigrams for the high scoring properties in United States##
###########################################################################

sentiment_high_score_us <- high_rated %>%
  filter(country == "United States") %>%
  unnest_tokens(word, text)
afinn_us_high <- sentiment_high_score_us %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarise(sentiment = sum(value), .groups = 'drop') %>%
  mutate(method = "AFINN")

bing_high_us <- sentiment_high_score_us %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  mutate(method = "Bing et al.")

nrc_high_us <- sentiment_high_score_us %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  mutate(method = "NRC")

bing_and_nrc_us_high <- bind_rows(bing_high_us, nrc_high_us) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

high_combined_sentiments_us <- bind_rows(
  summarise(afinn_us_high, sentiment = sentiment, method = method),
  summarise(bing_and_nrc_us_high, sentiment = sentiment_score, method = method)
)

ggplot(high_combined_sentiments_us, aes(x = method, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Analysis Comparison for High Scorers in United States",
       x = "Method",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bigrams US
bigrams_high_rated_us <- Description %>%
  filter(country == "United States") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  top_n(15)

bigram_graph_high_us <- bigrams_high_rated_us %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  graph_from_data_frame()

ggraph(bigram_graph_high_us, layout = "fr") +  
  geom_edge_link(aes(width = sqrt(n)), edge_alpha = 0.8, color = "gray") +  
  geom_node_point(size = 5, color = "steelblue") +  
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#########################Running LDA MODEL #######################################

processed_words <- Description %>%
  filter(country == "Turkey", !is.na(text)) %>%
  unnest_tokens(word, text)

# Remove stopwords and apply stemming
processed_words <- processed_words %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = wordStem(word))

# For DTM via word counts
word_counts <- processed_words %>%
  group_by(document_id, word) %>%
  summarise(n = n(), .groups = 'drop')

# For creating corpus (text aggregation for each document)
processed_descriptions <- processed_words %>%
  group_by(document_id) %>%
  summarise(text = paste(word, collapse=" "))

# Creating DTM from corpus
corpus <- Corpus(VectorSource(processed_descriptions$text))
dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf, stopwords = FALSE))  # Note: stopwords already removed

dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]


# Running LDA
lda_model <- LDA(dtm, k = 2)
topics <- tidy(lda_model, matrix = "beta")

# Analyze top terms for each topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(x = "Terms", y = "Importance (Beta)", title = "Top Terms in Each Topic")

######################## Ploting a MAP ##############################

filtered_data <- airbnb_alldf %>%
  filter(country == "United States")

# You will need to create a dataframe with longitude and latitude columns
locations <- data.frame(
  longitude = sapply(filtered_data$address$location$coordinates, function(coords) coords[1]),
  latitude = sapply(filtered_data$address$location$coordinates, function(coords) coords[2])
)

# Filter out any NA coordinates
locations <- na.omit(locations)

# Create a leaflet map
leaflet(locations) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude)

save(airbnb_alldf, file = "C:/Desktop/Unstructured Data/airbnb_alldf.RData")
save(Description, file = "C:/Desktop/Unstructured Data/Description.RData")
save(sentiment_us, file = "C:/Desktop/Unstructured Data/sentiment_us.RData")
save(bing_us, file = "C:/Desktop/Unstructured Data/bing_us.RData")
save(nrc_us, file = "C:/Desktop/Unstructured Data/nrc_us.RData")
save(bing_and_nrc_us, file = "C:/Desktop/Unstructured Data/bing_and_nrc_us.RData")
save(combined_sentiments_us, file = "C:/Desktop/Unstructured Data/combined_sentiments_us.RData")
save(bigrams_US, file = "C:/Desktop/Unstructured Data/bigrams_US.RData")
save(bigram_tf_idf_us, file = "C:/Desktop/Unstructured Data/bigram_tf_idf_us.RData")

save(processed_words, file = "C:/Desktop/Unstructured Data/processed_words.RData")
save(word_counts, file = "C:/Desktop/Unstructured Data/word_counts.RData")
save(processed_descriptions, file = "C:/Desktop/Unstructured Data/processed_descriptions.RData")
save(lda_model, file = "C:/Desktop/Unstructured Data/lda_model.RData")

save(top_terms, file = "C:/Desktop/Unstructured Data/top_terms.RData")
save(total_documents, file = "C:/Desktop/Unstructured Data/total_documents.RData")

afinn <- textdata::lexicon_afinn()

# Save the dataset locally
save(afinn, file = "C:/Desktop/Unstructured Data/afinn_lexicon.RData")

library(textdata)
# AFINN Lexicon
afinn_data <- lexicon_afinn()
save(afinn_data, file = "C:/Desktop/Unstructured Data/afinn_data.RData")

# Bing Lexicon
bing_data <- lexicon_bing()
save(bing_data, file = "C:/Desktop/Unstructured Data/bing_data.RData")

# NRC Lexicon
nrc_data <- lexicon_nrc()
save(nrc_data, file = "C:/Desktop/Unstructured Data/nrc_data.RData")
