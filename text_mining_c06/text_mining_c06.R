#'---
#' title: Topic modelling
#' output: github_document
#'---

#+ message=FALSE
pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        topicmodels,
        tidytext,
        janeaustenr,
        scales
)

#' ## Latent Dirichlet allocation
# data
data("AssociatedPress")
AssociatedPress

#' Fit model
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

#' ### Word-topic probabilities
(ap_topics <- tidy(ap_lda, matrix = "beta"))

ap_topics %>%
        group_by(topic) %>%
        slice_max(beta, n = 10) %>%
        ungroup() %>%
        arrange(topic, -beta) %>%
        ggplot(aes(x = beta,
                   y = reorder_within(term, by = beta, within = topic),
                   fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~topic, scales = "free") +
        scale_y_reordered() +
        labs(y = NULL,
             title = "Most common within each topic")

#' Greatest difference in β between topic 1 and topic 2

(beta_wide <- ap_topics %>%
        mutate(topic = paste0("topic", topic)) %>%
        pivot_wider(names_from = "topic",
                    values_from = "beta") %>%
        # filter for relatively common words, β > 1/1000 in at least 1 topic
        filter(topic1 > 0.001 | topic2 > 0.001) %>%
        mutate(log_ratio = log2(topic2 / topic1)))

# visualize
beta_wide %>%
        group_by(direction = log_ratio > 0) %>%
        slice_max(abs(log_ratio), n = 10) %>%
        ungroup() %>%
        ggplot(aes(x = log_ratio,
                   y = fct_reorder(term, log_ratio),
                   fill = direction)) +
        geom_col(show.legend = FALSE) +
        labs(x = "Log2 ratio of beta in topic 2 / topic 1",
             y = NULL)

#' ### Document-topic probabilities
(ap_documents <- tidy(ap_lda, matrix = "gamma"))


#' ## Example: the great library heist

#' ### Pre-processing
# divide books into chapters --> treat every chapter as separate `document`
# separate chapters into words <-- unnest_tokens()
# remove stop_words
#---

# divide into documents, each representing one chapter
(by_chapter <- austen_books() %>%
        group_by(book) %>%
        mutate(chapter = cumsum(str_detect(text,
                                           regex("^chapter ",
                                                 ignore_case = TRUE)))) %>%
        ungroup() %>%
        filter(chapter > 0) %>%
        unite(document, book, chapter))


# split into words
(by_chapter_word <- by_chapter %>%
        unnest_tokens(word, text))


# find document-word counts
(word_counts <- by_chapter_word %>%
        anti_join(stop_words) %>%
        count(document, word, sort = TRUE))

#' ### LDA on chapters
# convert tidy dataframe to DocumentTermMatrix
(chapters_dtm <- word_counts %>%
                cast_dtm(document, word, n))

# create 6 topic model
(chapters_lda <- LDA(chapters_dtm, k = 6, control = list(seed = 1234)))

#' ### Per-topic-per-word probabilities
(chapter_topics <- tidy(chapters_lda, matrix = "beta"))

# find the top 5 terms within each topic
(top_terms <- chapter_topics %>%
        group_by(topic) %>%
        slice_max(beta, n = 5) %>%
        ungroup() %>%
        arrange(topic, -beta))

# visualize
top_terms %>%
        ggplot(aes(x = beta,
                   y = reorder_within(term, beta, topic),
                   fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~topic, scales = "free") +
        scale_y_reordered() +
        labs(y = NULL)

#' ### Per-document classification
(chapters_gamma <- tidy(chapters_lda, matrix = "gamma"))

# re-separate the document name into title and chapter
(chapters_gamma2 <- chapters_gamma %>%
        separate(col = document,
                 into = c("title", "chapter"),
                 sep = "_",
                 convert = TRUE)) 

chapters_gamma2 %>%
        mutate(title = reorder(title, gamma * topic)) %>%
        ggplot(aes(x = factor(topic), y = gamma)) +
        geom_boxplot() +
        facet_wrap(~title) +
        labs(x = "Topic", y = expression(gamma))

#' Are there any cases where the topic most associated with a chapter belonged to another book?
(chapter_classifications <- chapters_gamma2 %>%
        group_by(title, chapter) %>%
        slice_max(gamma) %>%
        ungroup())

#' Compare each to the “consensus” topic for each book 
#' (aka the most common topic among its chapters) 
#' --> which were most often misidentified?
(book_topics <- chapter_classifications %>%
        count(title, topic) %>%
        group_by(title) %>%
        slice_max(n, n = 1) %>% 
        ungroup() %>%
        transmute(consensus = title,
                  topic))

chapter_classifications %>%
        inner_join(book_topics, by = "topic") %>%
        filter(title != consensus)


#' ### By word assignments: `augment`
(assignments <- augment(chapters_lda, data = chapters_dtm))

(assignments2 <- assignments %>%
        separate(col = document,
                 into = c("title", "chapter"), 
                 sep = "_",
                 convert = TRUE) %>%
        inner_join(book_topics,
                   by = c(".topic" = "topic")))

#' `confusion matrix`
assignments2 %>%
        count(title, consensus, wt = count) %>%
        mutate(across(c(title, consensus),
                      ~str_wrap(., 20)))  %>%
        group_by(title) %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(consensus, title, fill = percent)) +
        geom_tile() +
        scale_fill_gradient2(high = "darkred", label = percent_format()) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank()) +
        labs(x = "Book words were assigned to",
             y = "Book words came from",
             fill = "% of assignments")

#' What were the most commonly mistaken words?
(wrong_words <- assignments2 %>%
                filter(title != consensus))

wrong_words %>%
        count(title, consensus, term, wt = count) %>%
        ungroup() %>%
        arrange(desc(n))

#' E.g.: `Bingley` appears only in `Pride & Prejudice`, 
#' even though it’s assigned to `Sense & Sensibility`
word_counts %>%
        filter(word == "bingley") %>% 
        separate(document, c("book", "chapter"), sep = "_") %>% 
        count(book)


# rmarkdown::render()
