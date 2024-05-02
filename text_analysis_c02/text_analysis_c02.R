#'---
#' title: Analyzing Texts
#' output: github_document
#'---

#+ message=FALSE
pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        tidytext,       # tokenize texts and remove stopwords
        readtext,       # read textual data into R
        sotu,           # metadata and text of State of the Union speeches 
        SnowballC,      # stem words
        widyr,          # calculate co-occurrence
        igraph,         # plot co-occurrence graph
        ggraph,         # plot co-occurrence graph
        tm
)

#' ## Data
(sotu_texts <- readtext(sotu_dir()))

(sotu_whole <- sotu_meta %>%
                arrange(president) %>%
                bind_cols(sotu_texts) %>%
                as_tibble())

(tidy_sotu_words <- sotu_whole %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words))

#' ## Frequencies
#' Which words occur most frequently?
tidy_sotu_words %>% count(word, sort = TRUE)

#' Make a graph of the words that occur more that 2000 times
tidy_sotu_words %>%
        count(word, sort = TRUE) %>%
        filter(n > 2000) %>%
        # reorder values by frequency
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(x = n, y = word)) +
        geom_col(fill = "gray")

#' In any given year, how often is the word ‘peace’ used and 
#' how often is the word ‘war’ used?
tidy_sotu_words %>%
        # select only the words 'war' and 'peace'
        filter(word %in% c("peace", "war")) %>%
        # count occurrences of each per year
        count(year, word)

tidy_sotu_words %>%
        filter(word %in% c("peace", "war")) %>%
        count(year, word) %>%
        ggplot(aes(x = year, y = n, fill = word)) +
        # plot n by year, and use position 'fill' to show the proportion
        geom_col(position = "fill") +
        theme(legend.position = "top")

#' How long was the average speech of each president and 
#' who are the most ‘wordy’ presidents?
# summarize the words per president per speech
tidy_sotu_words %>%
        count(president, doc_id)

# calculate the average number of words per speech
tidy_sotu_words %>%
        count(president, doc_id) %>%
        group_by(president) %>%
        summarise(avg_words = mean(n)) %>%
        arrange(desc(avg_words))

#' ## Term frequency
# count term frequency
tidy_sotu_words %>%
        count(doc_id, word, sort = TRUE) %>%
        group_by(doc_id) %>%
        mutate(n_total = sum(n),
               term_freq = n/n_total)

# plot the distribution of the term frequency for the speeches
tidy_sotu_words %>%
        count(doc_id, word) %>%
        group_by(doc_id) %>%
        mutate(n_total = sum(n),
               term_freq = n/n_total) %>%
        ggplot(aes(term_freq)) +
        geom_histogram()

#' Find the term with the highest term frequency for each president
tidy_sotu_words %>%
        count(president, word) %>%
        group_by(president) %>%
        mutate(n_total = sum(n),
               term_freq = n/n_total) %>%
        arrange(desc(term_freq)) %>%
        # take the top for each president
        top_n(1) %>%
        # print all rows
        print(n = Inf)

#' Pick one president. 
#' For each of his speeches, which is the term with highest term frequency? 
tidy_sotu_words %>%
        filter(president == "Barack Obama") %>%
        count(doc_id, word) %>%
        group_by(doc_id) %>%
        mutate(n_total = sum(n),
               term_freq = n/n_total) %>%
        arrange(desc(term_freq)) %>%
        top_n(1)

#' ## tf-idf
tidy_sotu_words %>%
        count(doc_id, word, sort = TRUE) %>%
        bind_tf_idf(word, doc_id, n)

#' Words in the corpus that have the highest tf-idf scores, 
#' which means words that are particularly distinctive for their documents.
tidy_sotu_words %>%
        count(doc_id, word, sort = TRUE)  %>% 
        bind_tf_idf(word, doc_id, n) %>% 
        arrange(desc(tf_idf))

#' Pick the same president you chose above. 
#' For each of his speeches, which is the term with highest tf-idf?
tidy_sotu_words %>%
        filter(president == "Barack Obama") %>%
        count(doc_id, word, sort = TRUE) %>%
        bind_tf_idf(word, doc_id, n) %>%
        arrange(desc(tf_idf)) %>%
        group_by(doc_id) %>% 
        top_n(1)

#' ## N-Grams: sequences of words
sotu_whole %>%
        # create bigrams
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        select(!c(sotu_type, years_active))

# most common bigrams
sotu_whole %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
        count(bigram, sort = TRUE) # count occurrences and sort descending

# remove stopwords
sotu_whole %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(col = bigram,
                 into = c("word1", "word2"),
                 sep = " ") %>%
        select(!c(years_active, sotu_type))

# select only the words in each column that are not in the stopwords
sotu_whole %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(col = bigram,
                 into = c("word1", "word2"),
                 sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word)

# re-unite the two word columns into bigrams
(sotu_bigrams <- sotu_whole %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(col = bigram,
                 into = c("word1", "word2"),
                 sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word) %>%
        unite(col = bigram,
              word1, word2,
              sep = " "))

sotu_bigrams %>% count(bigram, sort = TRUE)

#' For each president, which is the bigram with highest tf-idf?
sotu_bigrams %>%
        count(president, bigram) %>%
        bind_tf_idf(bigram, president, n) %>%
        group_by(president) %>%
        arrange(desc(tf_idf)) %>%
        top_n(1) %>%
        print(n = Inf)

#' Pick the same president you chose above. 
#' For each of his speeches, which is the bigram with highest tf-idf?
sotu_whole %>%
        filter(president == "Barack Obama") %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(col = bigram,
                 into = c("word1", "word2"),
                 sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word) %>%
        unite(col = bigram,
              word1, word2,
              sep = " ") %>%
        count(doc_id, bigram) %>%
        bind_tf_idf(bigram, doc_id, n) %>%
        group_by(doc_id) %>%
        arrange(desc(tf_idf)) %>%
        top_n(1)

#' ## Co-occurrence
#' Which words occur most commonly together at the end of the speeches?
(sotu_word_pairs <- sotu_whole %>%
        # extract last 100 words
        mutate(speech_end = word(text, -100, end = -1)) %>%
        # tokenize
        unnest_tokens(word, speech_end) %>%
        # remove stopwords %>%
        filter(!word %in% stop_words$word) %>%
        # don't include upper triangle of matrix
        pairwise_count(word, doc_id, sort = TRUE, upper = FALSE))

#' Visualize the co-occurrence network of words that occur together 
#' at the end of 10 or more speeches.
sotu_word_pairs %>%
        # only word pairs that occur 10 or more times
        filter(n >= 10) %>%
        #convert to graph
        graph_from_data_frame() %>%
        # place nodes according to the force-directed algorithm of Fruchterman and Reingold
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n,
                           edge_width = n),
                       edge_colour = "tomato") +
        geom_node_point(size = 5) +
        geom_node_text(aes(label = name),
                       repel = TRUE, 
                       point.padding = unit(0.2, "lines")) +
        theme_void()

#' ## Document-Term Matrix
# make a table with document, term, count
tidy_sotu_words %>% 
        count(doc_id, word) 

(sotu_dtm <- tidy_sotu_words %>% 
        count(doc_id, word) %>% 
        cast_dtm(doc_id, word, n))

class(sotu_dtm)

# look at the terms with tm function
Terms(sotu_dtm) %>% tail()

# most frequent terms
findFreqTerms(sotu_dtm, lowfreq = 5000)

# find terms associated with "citizen"
findAssocs(sotu_dtm, "citizen", corlimit = 0.5)

#' ## Sentiment analysis
tidytext::sentiments

(bing_lex <- get_sentiments("bing"))

(sotu_sentiments <- tidy_sotu_words %>% 
        inner_join(bing_lex))  # join to add sentiment column

#' Visualize the proportion of positive sentiment 
#' (out of the total of positive and negative) 
#' in US State of the Union Addresses over time
sotu_sentiments %>%
        count(year, sentiment) %>%
        pivot_wider(names_from = sentiment,
                    values_from = n) %>%
        mutate(positive_ratio = positive/(negative + positive)) %>%
        # plot
        ggplot(aes(x = year, y = positive_ratio)) +
        geom_line(color = "gray") +
        geom_smooth(span = 0.3, se = FALSE) +
        geom_hline(yintercept = 0.5,
                   linetype = "dotted",
                   color = "orange",
                   size = 1) +
        scale_x_continuous(breaks = seq(1790, 2016, by = 10)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


# rmarkdown::render()
