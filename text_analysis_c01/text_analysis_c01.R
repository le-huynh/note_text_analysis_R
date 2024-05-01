#'---
#' title: [optional]
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
                          # ranging from George Washington to Barack Obama
        SnowballC,      # stem words
        widyr,
        igraph,
        ggraph,
        tm
)

#' ## Reading text into R
# state of the union metadata
sotu_meta %>% tibble()

# sotu_dir writes the text files to disk in a temporary dir, 
file_paths <- sotu_dir()
head(file_paths)

# read in the files with readtext
sotu_texts <- readtext(file_paths)

# check the texts
sotu_texts %>% tibble()

# combine the text and metadata
(sotu_whole <- sotu_meta %>%
        arrange(president) %>%
        bind_cols(sotu_texts) %>%
        as_tibble())

sotu_whole %>% tibble()

#' ## String operations
#' ### Count occurrences
#' How man times does the words “citizen” or "Citizen" appear in each speech?
sotu_whole %>% 
        mutate(n_citizen = str_count(text, "[C|c]itizen"))

#' Count word, character, sentence, etc.
(sotu_count <- sotu_whole %>% 
        mutate(n_character = str_count(text, boundary("character")),
               n_word = str_count(text, boundary("word")),
               n_linebreak = str_count(text, boundary("line_break")),
               n_sentence = str_count(text, boundary("sentence")),
               avg_word_per_sentence = n_word/n_sentence) %>%
        select(doc_id,
               n_character, n_word, n_linebreak, n_sentence,
               avg_word_per_sentence))

#' Which speech has shortest/longest average sentences length?
sotu_count %>%
        filter(# shortest average sentences length
               avg_word_per_sentence == min(avg_word_per_sentence) |
               # longest average sentences length
               avg_word_per_sentence == max(avg_word_per_sentence))

#' ### Detect patterns
#' What are the names of the documents where “citizen” and “Citizen” do not occur?
sotu_whole %>%
        filter(!str_detect(text, "[C|c]itizen")) %>%
        select(doc_id)

#' ### Extract words
#' Extract the first 5 words of each speech by Woodrow Wilson
sotu_whole %>%
        filter(president == "Woodrow Wilson") %>%
        pull(text) %>%
        word(end = 5)

#' ### Replace and remove characters
sotu_whole %>%
        filter(president == "Woodrow Wilson") %>%
        pull(text) %>%
        # replace \n by " "
        str_replace_all("\\n", " ") %>%
        word(end = 5)

sotu_whole %>% 
        filter(president == "Woodrow Wilson") %>%  
        pull(text) %>%
        str_replace_all("\\n", " ") %>%
        # remove whitespaces
        str_squish() %>%
        word(end = 5) 

#' ## Tokenize
#' `token`: most commonly single words.  
#' Tokenize the text at the level of words
sotu_whole %>%
        tidytext::unnest_tokens(word, text)

#' Word tokenization with punctuation and no lowercasing
sotu_whole %>%
        unnest_tokens(word,
                      text,
                      to_lower = FALSE,
                      strip_punct = FALSE)

#' Tokenize the text at the level of ngrams or sentences
# Sentence tokenization
sotu_whole %>%
        unnest_tokens(sentence,
                      text,
                      token = "sentences",
                      to_lower = FALSE) %>% 
        select(sentence)

# N-gram tokenization as trigrams
sotu_whole %>%
        unnest_tokens(trigram,
                      text,
                      token = "ngrams",
                      n = 3) %>% 
        select(trigram)

#' ## Stopwords
#' `Stopwords`: highly common words -> provide non-relevant information about content of a text.
# from `tidytext` package
stop_words

stop_words %>% count(lexicon)

sotu_whole %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)

#' ## Word stemming
#' Reduce words to word stem or root form. 
#' E.g., reducing `fishing`, `fished`, and `fisher` to the stem `fish`.  
#' R packages for word stemmer: `hunspell`, `SnowballC`
sotu_whole %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        mutate(word_stem = SnowballC::wordStem(word))

#' Lemmatization: discriminate between words which have different meanings depending on part of speech. 
#' R package: `koRpus`, `TreeTagger`

# rmarkdown::render()

