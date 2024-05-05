#'---
#' title: Sentiment analysis
#' output: github_document
#'---

#+ message=FALSE
pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,       # data management and visualization
        tidytext,
        textdata,
        wordcloud,
        reshape2,
        janeaustenr
)

#' ## Sentiments dataset
(sentiment_afinn <- get_sentiments("afinn"))

(sentiment_bing <- get_sentiments("bing"))
 
(sentiment_nrc <- get_sentiments("nrc"))

#' ## `inner_join()`
#' What are the most common joy words in Emma by Jane Austen?
(tidy_austen <- austen_books() %>%
        group_by(book) %>%
        mutate(linenumber = row_number(),
               chapter = cumsum(str_detect(text,
                                           regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
        ungroup() %>%
        unnest_tokens(output = word, input = text) %>%
        anti_join(stop_words))

nrc_joy <- sentiment_nrc %>%
        filter(sentiment == "joy")

nrc_joy

tidy_austen %>%
        count(book)

tidy_austen %>%
        filter(book == "Emma") %>%
        inner_join(nrc_joy)

tidy_austen %>%
        filter(book == "Emma") %>%
        inner_join(nrc_joy) %>%
        count(word, sort = TRUE)

#' How sentiment changes throughout each novel?
tidy_austen %>%
        inner_join(sentiment_bing) %>%
        mutate(index = linenumber %/% 80) %>%
        count(book, index, sentiment)

(sentiment_austen <- tidy_austen %>%
        inner_join(sentiment_bing) %>%
        mutate(index = linenumber %/% 80) %>%
        count(book, index, sentiment) %>%
        pivot_wider(names_from = sentiment,
                    values_from = n,
                    values_fill = 0) %>%
        mutate(sentiment = positive - negative))

sentiment_austen %>%
        ggplot(aes(x = index,
                   y = sentiment,
                   fill = book)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~book, ncol = 2, scales = "free_x")

#' ## Compare three sentiment dictionaries
#' Pride and Prejudice by Jane Austen
(pride_prejudice <- tidy_austen %>% 
        filter(book == "Pride & Prejudice"))

(afinn <- pride_prejudice %>% 
        inner_join(sentiment_afinn) %>% 
        group_by(index = linenumber %/% 80) %>% 
        summarise(sentiment = sum(value)) %>% 
        mutate(method = "AFINN"))

(bing <- pride_prejudice %>%
        inner_join(sentiment_bing) %>%
        mutate(method = "Bing et al.",
               index = linenumber %/% 80))
        

(nrc <- pride_prejudice %>%
        inner_join(sentiment_nrc) %>%
        mutate(method = "NRC",
               index = linenumber %/% 80) %>%
        filter(sentiment %in% c("positive", "negative")))

(bing_nrc <- bing %>% 
        full_join(nrc) %>%
        count(method, index, sentiment) %>%
        pivot_wider(names_from = sentiment,
                    values_from = n,
                    values_fill = 0) %>%
        mutate(sentiment = positive - negative))

bing_nrc %>%
        select(index, sentiment, method) %>%
        full_join(afinn) %>%
        ggplot(aes(x = index,
                   y = sentiment,
                   fill = method)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~method, ncol = 1, scales = "free_y")

#' NRC sentiment is high. 
#' AFINN sentiment has more variance. 
#' Bing et al. sentiment appears to find longer stretches of similar text. 
#' but all three agree roughly on the overall trends in the sentiment through a narrative arc.  
###

#' ## Most common positive and negative words
tidy_austen %>%
        inner_join(sentiment_bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        group_by(sentiment) %>%
        slice_max(n, n = 10) %>%
        ungroup() %>%
        ggplot(aes(x = n,
                   y = reorder(word, n),
                   fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(x = "Contribution to sentiment",
             y = NULL)

#' ## Wordclouds
tidy_austen %>%
        count(word) %>%
        with(wordcloud(word, n, max.words = 100))

tidy_austen %>%
        inner_join(sentiment_bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        reshape2::acast(word ~ sentiment,
                        value.var = "n",
                        fill = 0) %>%
        comparison.cloud(colors = c("gray20", "gray80"),
                         max.words = 100)

#' Units beyond just words
(austen_chapters <- austen_books() %>%
        group_by(book) %>%
        unnest_tokens(chapter,
                      text,
                      token = "regex", 
                      pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
        ungroup())

austen_chapters %>% 
        group_by(book) %>% 
        summarise(chapters = n())

#' What are the most negative chapters in each of Jane Austen’s novels?
(bing_negative <- sentiment_bing %>%
        filter(sentiment == "negative"))
        

(word_count <- tidy_austen %>%
        group_by(book, chapter) %>%
        summarise(words = n()))

tidy_austen %>%
        semi_join(bing_negative) %>%
        group_by(book, chapter) %>%
        summarise(negative_words = n()) %>%
        left_join(word_count,
                  by = c("book", "chapter")) %>%
        mutate(ratio = negative_words/words) %>%
        filter(chapter != 0) %>%
        slice_max(ratio, n = 1) %>%
        ungroup()

# rmarkdown::render()
