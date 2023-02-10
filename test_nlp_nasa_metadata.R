library(dplyr)
library(data.table)
library(jsonlite)
library(tidyr)
library(tidytext)

metadata = fromJSON("http://data.nasa.gov/data.json")
names(metadata$dataset)

class(metadata$dataset$title) # char
class(metadata$dataset$description) # char
class(metadata$dataset$keyword) # list

# data tidying
nasa_title = data.table(id = metadata$dataset$identifier,
                        title = metadata$dataset$title)

nasa_desc = data.table(id = metadata$dataset$identifier,
                       desc = metadata$dataset$description)

nasa_desc %>% 
  select(desc) %>%
  sample_n(5)

nasa_keyword = data.table(id = metadata$dataset$identifier,
                          keyword = metadata$dataset$keyword) %>% 
  unnest(keyword)

nasa_title = nasa_title %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words, by = "word")

nasa_desc = nasa_desc %>% 
  unnest_tokens(word, desc) %>% 
  anti_join(stop_words, by = "word")

my_stopwordds = data.table(word = c(as.character(1:10),
                                    "v1.0", "v1", "v03", "l2", "l3", "l4", "v5.2.0",
                                    "v003", "v004", "v005", "v006", "v7"))

nasa_title = nasa_title %>% 
  anti_join(my_stopwordds, by = "word")

nasa_desc = nasa_desc %>% 
  anti_join(my_stopwordds, by = "word")

nasa_keyword %>% 
  group_by(keyword) %>% 
  count(sort = T)

nasa_keyword = nasa_keyword %>% 
  mutate(keyword = toupper(keyword))

library(widyr)

title_word_pairs = nasa_title %>% 
  pairwise_count(word, id, sort = T, upper = F)

title_word_pairs

desc_word_pairs = nasa_desc %>% 
  pairwise_count(word, id, sort = T, upper = F)

library(ggplot2)
library(igraph)
library(ggraph)

set.seed(123)
title_word_pairs %>% 
  filter(n >= 250) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(aes(size = 5)) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

keyword_pairs = nasa_keyword %>% 
  pairwise_count(keyword, id, sort = T, upper = F)

keyword_pairs

set.seed(123)
keyword_pairs %>% 
  filter(n >= 700) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n),
                 edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, "lines")) +
  theme_void()


keyword_cors = nasa_keyword %>% 
  group_by(keyword) %>% 
  filter(n() >= 200) %>% 
  pairwise_cor(keyword, id, sort = T, upper = F)

keyword_cors

set.seed(123)
keyword_cors %>% 
  filter(correlation > .6) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation),
                 edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, "lines")) +
  theme_void()
