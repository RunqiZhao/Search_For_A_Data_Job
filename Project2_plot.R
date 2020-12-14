library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(wordcloud2)
library(ggmap)
library(forcats)
library(leaflet)
library(igraph)
library(ggraph)

# data
listings_ST <- readRDS("listings_ST2.rds")
listings_DS <- readRDS("listings_DS2.rds")
register_google("AIzaSyC9ENZrkqlnOVQw0ndDsEe90KVaat5CxTs")

data(stop_words)
mystopwords <- tibble(word = c("data","required","position","including","skills","ability","u.s","NA",stop_words$word))

# ST to code

# count word
text_description <- tibble(text = listings_ST$description)

text_description  %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(mystopwords) %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


# experience/research/analysis

text_description  %>%
  unnest_tokens(word, text) %>%
  anti_join(mystopwords) %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  wordcloud2(shape='diamond')


text_description <- tibble(listings_ST)
# text_description

bigrams_filtered <- listings_ST  %>% 
  filter(!is.na(description)) %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
  # count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  # filter(!(word1 %in% stop_words$word & word2 %in% stop_words$word)) %>%
  # filter(!(word1 %in% mystopwords$word & word2 %in% mystopwords$word)) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% mystopwords$word) %>%
  filter(!word2 %in% mystopwords$word) 
  

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united %>% count(bigram, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(n, bigram)) +
  geom_col() +
  labs(y = NULL)

# igraph
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

set.seed(2020)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# tf_idf

# cd_tf_idf <- listings_ST  %>%
#   unnest_tokens(word,token = "words", input = description) %>%
#   anti_join(stop_words) %>%
#   anti_join(mystopwords) %>%
#   count(company,word, sort = TRUE) %>%
#   bind_tf_idf(word,company,n) %>%
#   arrange(desc(tf_idf))


# cd_tf_idf %>%
#   # group_by(company) %>%
#   # slice_max(tf_idf, n = 15) %>%
#   # ungroup() %>%
#   filter(tf_idf > 0.15) %>%
#   mutate(word = reorder(word, tf_idf)) %>%
#   ggplot(aes(tf_idf, word)) +
#   geom_col() +
#   labs(y = NULL)



# Exp_Year
Exp_year_ST <- str_extract_all(listings_ST$description, "[0-9].+?year") %>% unlist()
Exp_year_ST <- 
  Exp_year_ST[which(nchar(Exp_year_ST) <= 10)] %>%
  str_extract_all("[[:digit:]]{1,2}") %>%
  unlist() %>%
  as.numeric()
Exp_year_ST <- Exp_year_ST[which(Exp_year_ST <= 10)]
ggplot() +
  geom_histogram(data = NULL, aes(x = Exp_year_ST), binwidth =1,fill="steelblue",alpha = 0.5) +
  scale_x_continuous(breaks = 1:10)

# Location
Loc_ST <- listings_ST %>% separate(location,c("city","state"), sep = ", ") 
Loc_ST_State <- na.omit(Loc_ST$state)
Loc_ST_State <- tibble(state = Loc_ST_State)
p_Loc_ST <- Loc_ST_State %>% count(state, sort = TRUE) %>% filter(n > 3) %>% ggplot() +
  geom_col(aes(x = reorder(state,-n), y = n), fill="steelblue",alpha = 0.5) +
  theme_bw() + 
  xlab("State") +   ylab("Count") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), legend.position='none')

# Statistician in MA
Loc_ST_City <- filter(Loc_ST,state == "MA")
Loc_ST_City <- na.omit(Loc_ST_City$city)
Loc_ST_City <- tibble(city = Loc_ST_City)
p_Loc_ST2_MA <- Loc_ST_City %>% count(city, sort = TRUE) %>%  filter(n > 1) %>% ggplot() +
  geom_col(aes(x = reorder(city,-n), y = n), fill="steelblue",alpha = 0.5) +
  theme_bw() + 
  xlab("City") +   ylab("Count") + 
  theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5), legend.position='none')

# Statistician in CA
Loc_ST_City <- filter(Loc_ST,state == "CA")
Loc_ST_City <- na.omit(Loc_ST_City$city)
Loc_ST_City <- tibble(city = Loc_ST_City)
p_Loc_ST2_CA <- Loc_ST_City %>% count(city, sort = TRUE) %>% filter(n > 1) %>% ggplot() +
  geom_col(aes(x = reorder(city,-n), y = n), fill="steelblue",alpha = 0.5) +
  theme_bw() + 
  xlab("City") +   ylab("Count") + 
  theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5), legend.position='none')

# Words under different location


# Company
text_description_ST <- tibble(text = listings_ST$company)
# text_description_ST

count_ST <- text_description_ST  %>%
  # unnest_tokens(word, text) %>%
  # anti_join(mystopwords) %>%
  count(text, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(text = reorder(text, n)) %>%
  ggplot(aes(n, text)) +
  geom_col() +
  labs(y = NULL)

# Mapping

# p_gg <- qmplot(lon,lat,data = listings_ST,
#            color = I("black"), size = I(2.5))
# p_gg

Link <- paste0('<a href = ',listings_ST$link,'> Details from Indeed </a>')
i_popup <- paste0("<strong>Titel: </strong>", listings_ST$title, "<br>", 
                  "<strong>Company: </strong>", listings_ST$company, "<br>", 
                  "<strong>Location: </strong>", listings_ST$address, "<br>",
                  "<strong>Job Link: </strong>", Link)

LL <- listings_ST[8:9]
LL %>%
  leaflet() %>%
  addTiles() %>%
  setView(-71.0588, 42.36, zoom = 10) %>%
  addMarkers(clusterOptions = markerClusterOptions(),popup=i_popup)



