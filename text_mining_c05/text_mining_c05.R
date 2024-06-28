#'---
#' title: Converting to and from non-tidy formats
#' output: github_document
#'---

#+ message=FALSE
pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        tm,
        tidytext,
        topicmodels,
        quanteda
)

#' ## Tidying `DocumentTermMatrix` objects

data("AssociatedPress", package = "topicmodels")

AssociatedPress

#' Access terms in document
terms <- tm::Terms(AssociatedPress)

head(terms)

#' Convert `DocumentTermMatrix` to one-token-per-document-per-row dataframe. 
#' Including only **non-zero** values

(ap_td <- broom::tidy(AssociatedPress))

#' Testing
ap_td %>%
        inner_join(get_sentiments("bing"),
                   by = c(term = "word"))

ap_td %>%
        inner_join(get_sentiments("bing"),
                   by = c(term = "word")) %>%
        count(sentiment, term, wt = count) %>%
        filter(n >= 200) %>%
        mutate(n = if_else(sentiment == "negative", -n, n)) %>%
        ggplot(aes(x = n,
                   y = fct_reorder(term, n),
                   fill = sentiment)) +
        geom_col() +
        labs(y = NULL) +
        theme(legend.position = "top")

#' ## Tidying `dfm` objects

data("data_corpus_inaugural", package = "quanteda")

(inaug_dfm <- data_corpus_inaugural %>%
        quanteda::tokens() %>%
        quanteda::dfm(verbose = FALSE))

#' Convert `dfm` to one-token-per-document-per-row dataframe. 
(inaug_td <- tidy(inaug_dfm))

#' Find the words most specific to each of the inaugural speeches
(inaug_tf_idf <- inaug_td %>%
                bind_tf_idf(term, document, count) %>%
                arrange(desc(tf_idf)))

#' Pick four notable inaugural addresses 
#' (from Presidents Lincoln, Roosevelt, Kennedy, and Obama), 
#' and visualize the words most specific to each speech

inaug_tf_idf %>% 
        filter(document %in% c("1861-Lincoln",
                               "1961-Kennedy",
                               "1933-Roosevelt",
                               "2009-Obama")) %>%
        group_by(document) %>%
        top_n(n = 6) %>%
        ungroup() %>%
        ggplot(aes(x = tf_idf,
                   y = reorder_within(term, tf_idf, document),
                   fill = document)) +
        geom_col(show.legend = FALSE) +
        scale_y_reordered() +
        facet_wrap(~document, ncol = 2, scales = "free") +
        labs(y = NULL) +
        theme_bw()

#' Extract the year from each document’s name, 
#' and compute the total number of words within each year.

(year_term_counts <- inaug_td %>%
        separate(col = document,
                 into = c("year", "president"),
                 sep = "-") %>%
        complete(year, term, fill = list(count = 0)) %>%
        group_by(year) %>%
        mutate(year_total = sum(count),
               year = as.integer(year)))

#' Pick several words + visualize how they changed in frequency over time
year_term_counts1 %>%
        filter(term %in% c("god",
                           "america",
                           "foreign",
                           "union",
                           "constitution",
                           "freedom")) %>%
        ggplot(aes(year,
                   count/year_total)) +
        geom_point() +
        geom_smooth() +
        facet_wrap(~term, ncol = 2, scales = "free_y") +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(y = "% frequency of word in inaugural address")


#' ## Casting tidy text data into a matrix

ap_td %>%
        cast_dtm(document, term, count)


ap_td %>%
        cast_dfm(document, term, count)


(m <- ap_td %>%
        cast_sparse(document, term, count))

class(m)
dim(m)

#' ### Example: Jane Austen's book
(austen <- janeaustenr::austen_books() %>%
        unnest_tokens(word, text) %>%
        count(book, word))

austen %>%
        cast_dtm(book, word, n)

#' ## Tidy `Corpus` object

data("acq")

acq

# 1st document
acq[[1]]

# tidy corpus
(acq_td <- tidy(acq))

(acq_tokens <- acq_td %>%
        select(-places) %>% 
        unnest_tokens(word, text) %>%
        anti_join(stop_words, by = "word"))

# most common words
acq_tokens %>% count(word, sort = TRUE)

# tf-idf
acq_tokens %>%
        count(id, word) %>%
        bind_tf_idf(word, id, n) %>%
        arrange(desc(tf_idf))


# rmarkdown::render()
