library(tidyverse)
library(ggraph)
library(igraph)
library(tidyr)
library(ggrepel)
library(tidytext)

load('../Downloads/textcompiled.RData')

head(bigram_words)

`%nin%` = Negate(`%in%`)

saved <- c('df_all', 'bigram_words')
list_name <- ls()[ls() %nin% saved]
rm(list = list_name)

bigram_separated <- bigram_words %>%
  separate(bigram, c('word1', 'word2'), sep = ' ')
  
bigram_filtered <- bigram_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_count <- bigram_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_count_filtered <- bigram_count %>%
  filter(is.na(word1) != TRUE, is.na(word2) != TRUE) %>%
  top_n(500)

colSums(is.na(bigram_count_filtered))

head(bigram_count_filtered)

graph <- graph_from_data_frame(bigram_count_filtered, directed = FALSE)

graph

library(scales)

V(graph)$degree <- degree(graph)
V(graph)$weighted_deg <- graph.strength(graph = graph, weights = edge_attr(graph)$n)
V(graph)$font_size <- rescale(V(graph)$weighted_deg, to = c(1, 20))
E(graph)$alpha <- (E(graph)$n - min(E(graph)$n)) / (max(E(graph)$n) - min(E(graph)$n))

ggraph(graph) + 
  geom_edge_link(aes(width = n, alpha = alpha)) + 
  geom_node_point(aes(size = font_size)) +
  geom_node_text(aes(label = name, size = font_size), repel = TRUE) +
  theme(panel.background = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank())

### visNetwork

library(visNetwork)

visnetwork_kw <- toVisNetworkData(graph)
visnetwork_kw$nodes$size <- visnetwork_kw$nodes$font_size * 5
visnetwork_kw$nodes$font.size <- visnetwork_kw$nodes$font_size * 5
visnetwork_kw$edges$width <- visnetwork_kw$edges$alpha * 20

visNetwork(visnetwork_kw$nodes, visnetwork_kw$edges, width = '100%', height = '400px') %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(dragNodes = TRUE)
