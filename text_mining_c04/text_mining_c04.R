#'---
#' title: n-grams and correlations
#' output: github_document
#'---

#+ message=FALSE
pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        tidytext,
        ggraph,         # network plots
        igraph,
        widyr,          # calculates pairwise correlations
        janeaustenr
)

#' ## Tokenizing by n-gram
austen_books() %>%
        unnest_tokens(output = bigram,
                      input = text,
                      token = "ngrams",
                      n = 2)

(austen_bigrams <- austen_books() %>%
        unnest_tokens(output = bigram,
                      input = text,
                      token = "ngrams",
                      n = 2) %>%
        filter(!is.na(bigram)))

#' ### Count + filter n-grams
austen_bigrams %>%
        count(bigram, sort = TRUE)

# split column into multiple columns
austen_bigrams %>%
        separate(col = bigram,
                 into = c("word1", "word2"),
                 sep = " ")

austen_bigrams %>%
        separate(col = bigram,
                 into = c("word1", "word2"),
                 sep = " ")

# remove stop-words
(bigrams_filtered <- austen_bigrams %>%
        separate(col = bigram,
                 into = c("word1", "word2"),
                 sep = " ") %>%
        filter(if_all(contains("word"),
                      ~ !(. %in% stop_words$word))))

# bigram count
bigrams_filtered %>%
        count(word1, word2, sort = TRUE)

# recombine columns into one
bigrams_filtered %>%
        unite(col = bigram,
              word1, word2,
              sep = " ")

# tri-gram
austen_books() %>%
        unnest_tokens(output = trigram,
                      input = text,
                      token = "ngrams",
                      n = 3) %>%
        filter(!is.na(trigram)) %>%
        separate(col = trigram,
                 into = c("word1", "word2", "word3"),
                 sep = " ") %>%
        filter(if_all(contains("word"),
                      ~ !(. %in% stop_words$word))) %>%
        count(word1, word2, word3, sort = TRUE)

#' ### Analyze bigrams
bigrams_filtered %>%
        filter(word2 == "street") %>%
        count(book, word1, sort = TRUE)

(bigram_tf_idf <- bigrams_filtered %>%
        unite(col = bigram,
              word1, word2,
              sep = " ") %>%
        count(book, bigram) %>%
        bind_tf_idf(term = bigram,
                    document = book,
                    n = n) %>%
        arrange(desc(tf_idf)))

bigram_tf_idf %>%
        group_by(book) %>%
        slice_max(tf_idf, n = 5) %>%
        ungroup() %>%
        ggplot(aes(x = tf_idf,
                   y = fct_reorder(bigram, tf_idf),
                   fill = book)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~book, ncol = 2, scales = "free") +
        labs(y = NULL,
             title = "Bigrams with the highest tf-idf from each Jane Austen novel") +
        theme_bw()

#' ### Using bigrams to provide context in sentiment analysis
austen_bigrams %>%
        separate(col = bigram,
                 into = c("word1", "word2"),
                 sep = " ") %>%
        filter(word1 == "not") %>%
        count(word1, word2, sort = TRUE)

(afinn <- get_sentiments("afinn"))

#' Most frequent words preceded by “not” + a sentiment
austen_bigrams %>%
        separate(col = bigram,
                 into = c("word1", "word2"),
                 sep = " ") %>%
        filter(word1 == "not") %>%
        inner_join(afinn,
                   by = c(word2 = "word"))

(not_words <- austen_bigrams %>%
        separate(col = bigram,
                 into = c("word1", "word2"),
                 sep = " ") %>%
        filter(word1 == "not") %>%
        inner_join(afinn,
                   by = c(word2 = "word")) %>%
        count(word2, value, sort = TRUE))

not_words %>%
        mutate(contribution = n * value) %>%
        arrange(desc(abs(contribution))) %>%
        head(20) %>%
        ggplot(aes(x = contribution,
                   y = fct_reorder(word2, contribution),
                   fill = contribution > 0)) +
        geom_col(show.legend = FALSE) +
        labs(x = "Sentiment value * number of occurrences",
             y = "Words preceded by \"not\"") +
        theme_bw()

#' Common words that negate the subsequent term: "not", "no", "never", "without"
(negated_words <- austen_bigrams %>%
        separate(col = bigram,
                 into = c("word1", "word2"),
                 sep = " ") %>%
        filter(word1 %in% c("not", "no", "never", "without")) %>%
        inner_join(afinn, by = c(word2 = "word")) %>%
        count(word1, word2, value, sort = TRUE))

negated_words %>%
        mutate(contribution = n * value) %>%
        group_by(word1) %>%
        slice_max(abs(contribution), n = 12) %>%
        ungroup() %>%
        ggplot(aes(x = contribution,
                   y = fct_reorder(word2, contribution),
                   fill = contribution > 0)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~word1, ncol = 2, scales = "free") +
        labs(x = "Sentiment value * number of occurrences",
             y = NULL) +
        theme_bw()

#' ### Visualize a network of bigrams with ggraph
(bigram_counts <- bigrams_filtered %>% 
        count(word1, word2, sort = TRUE))

# filter for only relatively common combinations
(bigram_graph <- bigram_counts %>%
        filter(n > 20) %>%
        igraph::graph_from_data_frame())

#' Common bigrams in Jane Austen’s novels, 
#' showing those that occurred more than 20 times 
#' and where neither word was a stop word
bigram_graph %>%
        ggraph(layout = "fr") +
        geom_edge_link() +
        geom_node_point() +
        geom_node_text(aes(label = name),
                       vjust = 1,
                       hjust = 1)

#' Better looking graph
arrow_direction <- grid::arrow(type = "closed",
                               length = unit(.15, "inches"))

bigram_graph %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n),
                       show.legend = FALSE,
                       arrow = arrow_direction,
                       end_cap = circle(.07, 'inches')) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()

#' ## Correlating pairs of words
(austen_section_words <- austen_books() %>%
        filter(book == "Pride & Prejudice") %>%
        mutate(section = row_number() %/% 10) %>%
        filter(section > 0) %>%
        unnest_tokens(output = word,
                      input = text) %>%
        filter(!word %in% stop_words$word))

# count words co-occuring within sections
(word_pairs <- austen_section_words %>%
        pairwise_count(item = word,
                       feature = section,
                       sort = TRUE))

#' => the most common pair of words in a section is “Elizabeth” and “Darcy” 
#' (the two main characters). 
#' Words that most often occur with Darcy?
word_pairs %>%
        filter(item1 == "darcy")

#' ### Pairwise correlation
(word_cors <- austen_section_words %>%
        group_by(word) %>%
        filter(n() >= 20) %>%
        pairwise_cor(item = word,
                     feature = section,
                     sort = TRUE))

word_cors %>%
        filter(item1 == "pounds")

word_cors %>%
        filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
        group_by(item1) %>%
        slice_max(correlation, n = 6) %>%
        ungroup() %>%
        ggplot(aes(x = correlation,
                   y = fct_reorder(item2, correlation),
                   fill = item1)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~item1, ncol = 2, scales = "free") +
        labs(y = NULL) +
        theme_bw()

word_cors %>%
        filter(correlation > 0.15) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), repel = TRUE) +
        theme_void()


# rmarkdown::render()
