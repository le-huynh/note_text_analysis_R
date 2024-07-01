#'---
#' title: Case study - comparing Twitter archives
#' output: github_document
#'---

#+ message=FALSE
pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        lubridate,
        tidytext
)

#' ## Data
(tweets_julia <- rio::import(here("text_mining_c07_example/tweets_julia.csv")) %>%
        tibble())

(tweets_dave <- rio::import(here("text_mining_c07_example/tweets_dave.csv")) %>%
        tibble())

(tweets <- bind_rows(tweets_julia %>% mutate(person = "Julia"),
          tweets_dave %>% mutate(person = "David")) %>%
        mutate(timestamp = ymd_hms(timestamp)))

#' ## Distribution of tweets
tweets %>% 
        ggplot(aes(x = timestamp,
                   fill = person)) +
        geom_histogram(position = "identity",
                       bins = 20,
                       show.legend = FALSE) +
        facet_wrap(~person, ncol = 1) +
        labs(x = "Timestamp",
             y = "Count",
             title = "All tweets from Julia's and David's accounts")

#' David and Julia joined Twitter about a year apart from each other.  
#' There were about 5 years: David was not active on Twitter, Julia was.  
#' => In total, Julia has about 4 times as many tweets as David.  

#---

#' ## word frequencies

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

(tidy_tweets <- tweets %>% 
        filter(!str_detect(text, "^RT")) %>%
        mutate(text = str_replace_all(text, replace_reg, "")) %>%
        unnest_tokens(word, 
                      text, 
                      token = "regex",
                      pattern = unnest_reg) %>%
        filter(!word %in% stop_words$word,
               !word %in% str_remove_all(stop_words$word, "'"),
               str_detect(word, "[a-z]")))

#' Calculate word frequencies for each person
(frequency <- tidy_tweets %>%
        count(person, word, sort = TRUE) %>%
        group_by(person) %>%
        mutate(total = sum(n),
               freq = n/total))

#' Visualize

frequency %>%
        select(-n, -total) %>%
        pivot_wider(names_from = person,
                    values_from = freq) %>%
        ggplot(aes(x = Julia,
                   y = David)) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = scales::percent_format()) +
        scale_y_log10(labels = scales::percent_format()) +
        geom_abline(color = "red", size = 1)

#' => Words near red line: used with equal frequencies by David and Julia.  
#' => Words far away from the line: used much more by one person compared to the other.  
#' David: used Twitter for professional purposes.  
#' Julia: used Twitter for personal purposes.  

#---

#' ## Comparing word usage
# filter tweets during 2016
(tidy_tweets_2016 <- tidy_tweets %>%
        filter(timestamp >= as.Date("2016-01-01"),
               timestamp < as.Date("2017-01-01")))

(word_ratios <- tidy_tweets_2016 %>%
        # remove Twitter usernames
        filter(!str_detect(word, "^@")) %>%
        count(word, person) %>% 
        # keep only the words used more than 10 times
        group_by(word) %>%
        filter(sum(n) >= 10) %>%
        ungroup() %>%
        pivot_wider(names_from = person,
                    values_from = n,
                    values_fill = 0) %>%
        mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
        mutate(logratio = log(David / Julia)) %>%
        arrange(desc(logratio)))

#' Words that have been about equally likely to come from David or Julia’s account during 2016?
word_ratios %>% 
        arrange(abs(logratio))

#' Which words are most likely to be from Julia’s account or from David’s account?
word_ratios %>%
        group_by(direction = logratio > 0) %>%
        slice_max(abs(logratio), n = 10) %>%
        ungroup() %>%
        ggplot(aes(x = logratio,
                   y = fct_reorder(word, logratio),
                   fill = logratio > 0)) +
        geom_col() +
        labs(x = "log odds ratio (David/Julia)",
             y = NULL,
             fill = "Account") +
        scale_fill_discrete(labels = c("Julia", "David")) +
        theme(legend.position = "top")


#' ## Changes in word use
#' Which words’ frequencies have changed the fastest in Twitter feeds?  
#' Aka which words were tweeted about at a higher or lower rate as time has passed?

(words_by_time <- tidy_tweets_2016 %>%
        # remove Twitter usernames
        filter(!str_detect(word, "^@")) %>%
        # which unit of time (e.g., month) each tweet was posted in
        mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>%
        # how many times each author used each word in each time bin
        count(time_floor, person, word) %>%
        # total number of words used in each time bin by each person
        group_by(person, time_floor) %>%
        mutate(time_total = sum(n)) %>%
        # total number of times each word was used by each person
        group_by(person, word) %>%
        mutate(word_total = sum(n)) %>%
        ungroup() %>%
        # keep words used at least 30 times
        filter(word_total >= 30) %>%
        rename(count = n))
        
#' **Note:**  
#' - `count`: how many times that person used that word in that time bin  
#' - `time_total`: how many words that person used during that time bin  
#' - `word_total`: how many times that person used that word over the whole year

#---
#' ### Modelling
#' Was a given word mentioned in a given time bin? 
#' Yes or no? How does the count of word mentions depend on time?

(nested_data <- words_by_time %>%
        nest(data = c(-word, -person)))

# test for 1-person-1-word
(.x <- nested_data %>% pluck("data", 1))

glm(data = .x,
    formula = cbind(count, time_total) ~ time_floor,
    family = "binomial")

# generate model for all rows
(nested_models <- words_by_time %>%
        nest(data = c(-word, -person)) %>%
        mutate(models = map(.x = data,
                            ~ glm(data = .x,
                                  formula = cbind(count, time_total) ~ time_floor,
                                  family = "binomial"))))

nested_models %>% pluck("models", 1)

# extract model info
(slopes <- nested_models %>%
        mutate(models = map(models, broom::tidy)) %>%
        unnest(models) %>% 
        # filter slopes
        filter(term == "time_floor") %>%
        # calculate adjusted p-value
        mutate(adjusted.p.value = p.adjust(p.value)))

#' Which words have changed in frequency at a moderately significant level in our tweets?
# find the significant slopes
(top_slopes <- slopes %>% 
        filter(adjusted.p.value < 0.05))

#' Visualize
words_by_time %>%
        inner_join(top_slopes, by = c("word", "person")) %>%
        ggplot(aes(x = time_floor,
                   y = count/time_total,
                   color = word)) +
        geom_line(size = 1.3) +
        facet_wrap(~person) +
        labs(x = NULL,
             y = "Word frequency",
             color = "Word") +
        theme_bw()

# rmarkdown::render()
