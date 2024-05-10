#'---
#' title: tf-idf
#' output: github_document
#'---

#+ message=FALSE
pacman::p_load(
        tidyverse,      # data management and visualization
        tidytext,
        janeaustenr,
        gutenbergr
)

#' ## Term frequency in Jane Austen’s novels
(book_words <- austen_books() %>%
        unnest_tokens(word, text) %>%
        count(book, word, sort = TRUE))

(total_words <- book_words %>%
        group_by(book) %>%
        summarise(total = sum(n)))

book_words %>%
        left_join(total_words)

#' Visualize distribution of `n/total` 
#' (= `term frequency` = number of times a word appears in a novel divided by 
#' the total number of terms in that novel) for each novel.

book_words %>%
        left_join(total_words) %>%
        ggplot(aes(n/total,
                   fill = book)) +
        geom_histogram(show.legend = FALSE) +
        xlim(NA, 0.0009) +
        facet_wrap(~book, ncol = 2, scales = "free_y") +
        labs(title = "Term frequency distribution in Jane Austen’s novels") +
        theme_bw()

#' => similar distributions for all the novels: 
#' many words occur rarely, fewer words occur frequently.  

#---------------------------------

#' ## Zipf's law
(freq_by_rank <- book_words %>%
        left_join(total_words) %>%
        group_by(book) %>%
        mutate(rank = row_number(),
               term_freq = n/total) %>%
        ungroup())

#' Visualize Zipf's law
freq_by_rank %>%
        ggplot(aes(x = rank,
                   y = term_freq,
                   color = book)) +
        geom_line(linewidth = 1.1,
                  alpha = 0.8) +
        scale_x_log10() +
        scale_y_log10() +
        labs(title = "Zipf’s law for Jane Austen’s novels") +
        theme_bw()

#' Fitting model
# slope close to -1
freq_by_rank %>% 
        filter(rank < 500,
               rank > 10) %>%
        lm(log10(.$term_freq) ~ log10(.$rank),
           data = .)

freq_by_rank %>%
        ggplot(aes(x = rank,
                   y = term_freq,
                   color = book)) +
        geom_abline(intercept = -0.6226,
                    slope = -1.1125,
                    color = "gray50",
                    linetype = 2,
                    linewidth = 1.1) +
        geom_line(linewidth = 1.1,
                  alpha = 0.8) +
        scale_x_log10() +
        scale_y_log10() +
        labs(title = "Fitting an exponent for Zipf’s law with Jane Austen’s novels") +
        theme_bw()

#' ## `tidytext::bind_tf_idf()`
book_words %>%
        bind_tf_idf(term = word,
                    document = book,
                    n = n)

#' => zero for extremely common words

book_words %>%
        bind_tf_idf(term = word,
                    document = book,
                    n = n) %>%
        arrange(desc(tf_idf))

#' Visualize high tf-idf words
book_words %>%
        bind_tf_idf(term = word,
                    document = book,
                    n = n) %>%
        group_by(book) %>%
        slice_max(tf_idf, n = 5) %>%
        ungroup() %>%
        ggplot(aes(x = tf_idf,
                   y = fct_reorder(word, tf_idf),
                   fill = book)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~book, ncol = 2, scales = "free") +
        labs(y = NULL,
             title = "Highest tf-idf words in each Jane Austen novel") +
        theme_bw()

#' ## A corpus of physics texts
#' Physics books from Project Gutenberg: 
#' (37729) Discourse on Floating Bodies by Galileo Galilei, 
#' (14725) Treatise on Light by Christiaan Huygens, 
#' (13476) Experiments with Alternate Currents of High Potential and High Frequency by Nikola Tesla, 
#' (30155) Relativity: The Special and General Theory by Albert Einstein 

physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                              meta_fields = "author")

(physics_words <- physics %>%
        unnest_tokens(word, text) %>%
        count(author, word, sort = TRUE))

#' Calculate tf-idf
(physics_total <- physics_words %>%
        group_by(author) %>%
        summarise(total = sum(n)))

physics_words %>%
        bind_tf_idf(term = word,
                    document = author,
                    n = n) %>%
        group_by(author) %>%
        slice_max(tf_idf, n = 10) %>%
        ungroup() %>%
        ggplot(aes(x = tf_idf,
                   y = fct_reorder(word, tf_idf),
                   fill = author)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~author, ncol = 2, scales = "free") +
        labs(y = NULL,
             title = "Highest tf-idf words in each physics texts") +
        theme_bw()
        

#' Remove less meaningful words
my_stopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm",
                               "ab", "_k", "_k_", "_x"))

physics_words %>%
        anti_join(my_stopwords,
                  by = "word") %>%
        bind_tf_idf(term = word,
                    document = author,
                    n = n) %>%
        mutate(word = str_remove_all(word, "_")) %>%
        group_by(author) %>%
        slice_max(tf_idf, n = 10) %>%
        ungroup() %>%
        ggplot(aes(x = tf_idf,
                   y = fct_reorder(word, tf_idf),
                   fill = author)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~author, ncol = 2, scales = "free") +
        labs(y = NULL,
             title = "Highest tf-idf words in classic physics texts") +
        theme_bw()

# rmarkdown::render()
