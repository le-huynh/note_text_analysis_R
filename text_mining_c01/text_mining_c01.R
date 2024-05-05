#'---
#' title: Tidy text format
#' output: github_document
#'---

#+ message=FALSE
pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        tidytext,
        janeaustenr,     # Jane Austen’s 6 completed, published novels
        gutenbergr
)

#' ### Simple data
#' `tidytext::unnest_tokens()`  

(text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality"))

(text_df <- tibble(line = 1:4,
                   text = text))

text_df %>%
        unnest_tokens(output = word,
                      input = text)

#' ### Jane Austen's novels: `janeaustenr` package
# one-row-per-line format
austen_books()

(original_books <- austen_books() %>%
        group_by(book) %>%
        mutate(linenumber = row_number(),
               chapter = cumsum(str_detect(text, 
                                           regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
        ungroup())

# unnest_tokens
(tidy_books <- original_books %>%
        unnest_tokens(output = word,
                      input = text))

# remove stopwords
(tidy_austen <- tidy_books %>%
        anti_join(stop_words))

# most common words in all books
tidy_austen %>%
        count(word, sort = TRUE)

# visualize the most common words
tidy_austen %>%
        count(word, sort = TRUE) %>%
        filter(n > 600) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)

#' ### `gutenbergr` package
#' Download novels by H.G. Wells: 
#' (35) The Time Machine, 
#' (36) The War of the Worlds, 
#' (5230) The Invisible Man, 
#' (159) The Island of Doctor Moreau
#hgwells <- gutenberg_download(c(35, 36, 5230, 159))

hgwells

# tidying the data
(tidy_hgwells <- hgwells %>%
        unnest_tokens(output = word, input = text) %>%
        anti_join(stop_words))

# most common words
tidy_hgwells %>%
        count(word, sort = TRUE)

#' Download novels by the Brontë sisters: 
#' (1260) Jane Eyre, 
#' (768) Wuthering Heights, 
#' (969) The Tenant of Wildfell Hall, 
#' (9182) Villette, 
#' (767) Agnes Grey
#bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

bronte

# tidying the data
(tidy_bronte <- bronte %>%
        unnest_tokens(output = word, input = text) %>%
        anti_join(stop_words))

# most common words
tidy_bronte %>%
        count(word, sort = TRUE)

#' ### Challenge
#' Calculate the frequency for each word for the works of Jane Austen, 
#' the Brontë sisters, and H.G. Wells
(austen_df <- tidy_austen %>% 
        mutate(author = "Jane Austen") %>%
        select(author, word))

(hgwells_df <- tidy_hgwells %>%
        mutate(author = "H.G. Wells") %>%
        select(author, word))

(bronte_df <- tidy_bronte %>%
        mutate(author = "Brontë Sisters") %>%
        select(author, word))

(freq_df <- austen_df %>%
        full_join(hgwells_df) %>%
        full_join(bronte_df) %>%
        mutate(word = str_extract(word, "[a-z']+")) %>%
        count(author, word) %>%
        group_by(author) %>%
        mutate(proprtion = n/sum(n)) %>%
        select(-n) %>%
        pivot_wider(names_from = author,
                    values_from = proprtion))
        

#' Visualize
fig <- freq_df %>%
        pivot_longer(cols = c(`Brontë Sisters`, `H.G. Wells`),
                     names_to = "author",
                     values_to = "proportion") %>%
        ggplot(aes(y = `Jane Austen`,
                   x = proportion,
                   color = abs(`Jane Austen` - proportion))) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_abline(color = "gray40", lty = 2) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        facet_wrap(~ author) +
        scale_x_log10(labels = scales::percent_format()) +
        scale_y_log10(labels = scales::percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        labs(x = NULL) +
        theme_bw() +
        theme(legend.position = "none")

fig

#' How correlated are the word frequencies between 
#' Austen and the Brontë sisters, and between Austen and Wells?
cor.test(freq_df$`Jane Austen`, freq_df$`Brontë Sisters`)

cor.test(freq_df$`Jane Austen`, freq_df$`H.G. Wells`)

#' The word frequencies are more correlated 
#' between the Austen and Brontë novels than between Austen and H.G. Wells.

# rmarkdown::render()
