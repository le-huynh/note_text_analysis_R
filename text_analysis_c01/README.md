optional
================

``` r
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
```

## Reading text into R

``` r
# state of the union metadata
sotu_meta %>% tibble()
```

    ## # A tibble: 240 × 6
    ##        X president          year years_active party       sotu_type
    ##    <int> <chr>             <int> <chr>        <chr>       <chr>    
    ##  1     1 George Washington  1790 1789-1793    Nonpartisan speech   
    ##  2     2 George Washington  1790 1789-1793    Nonpartisan speech   
    ##  3     3 George Washington  1791 1789-1793    Nonpartisan speech   
    ##  4     4 George Washington  1792 1789-1793    Nonpartisan speech   
    ##  5     5 George Washington  1793 1793-1797    Nonpartisan speech   
    ##  6     6 George Washington  1794 1793-1797    Nonpartisan speech   
    ##  7     7 George Washington  1795 1793-1797    Nonpartisan speech   
    ##  8     8 George Washington  1796 1793-1797    Nonpartisan speech   
    ##  9     9 John Adams         1797 1797-1801    Federalist  speech   
    ## 10    10 John Adams         1798 1797-1801    Federalist  speech   
    ## # ℹ 230 more rows

``` r
# sotu_dir writes the text files to disk in a temporary dir, 
file_paths <- sotu_dir()
head(file_paths)
```

    ## [1] "C:\\Users\\trucl\\AppData\\Local\\Temp\\RtmpETtWAu\\file3e7072e276b2/george-washington-1790a.txt"
    ## [2] "C:\\Users\\trucl\\AppData\\Local\\Temp\\RtmpETtWAu\\file3e7072e276b2/george-washington-1790b.txt"
    ## [3] "C:\\Users\\trucl\\AppData\\Local\\Temp\\RtmpETtWAu\\file3e7072e276b2/george-washington-1791.txt" 
    ## [4] "C:\\Users\\trucl\\AppData\\Local\\Temp\\RtmpETtWAu\\file3e7072e276b2/george-washington-1792.txt" 
    ## [5] "C:\\Users\\trucl\\AppData\\Local\\Temp\\RtmpETtWAu\\file3e7072e276b2/george-washington-1793.txt" 
    ## [6] "C:\\Users\\trucl\\AppData\\Local\\Temp\\RtmpETtWAu\\file3e7072e276b2/george-washington-1794.txt"

``` r
# read in the files with readtext
sotu_texts <- readtext(file_paths)

# check the texts
sotu_texts %>% tibble()
```

    ## # A tibble: 240 × 2
    ##    doc_id                   text                                                                                                                                     
    ##    <chr>                    <chr>                                                                                                                                    
    ##  1 abraham-lincoln-1861.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nIn the midst of unprecedented political troubles we have cause of …
    ##  2 abraham-lincoln-1862.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nSince your last annual assembling another year of health and bount…
    ##  3 abraham-lincoln-1863.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nAnother year of health and of sufficiently abundant harvests has p…
    ##  4 abraham-lincoln-1864.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nAgain the blessings of health and abundant harvests claim our pror…
    ##  5 andrew-jackson-1829.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \n\nIt affords me pleasure to tender my friendly greetings to y…
    ##  6 andrew-jackson-1830.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \n\nThe pleasure I have in congratulating you upon your return …
    ##  7 andrew-jackson-1831.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \n\nThe representation of the people has been renewed for the 2…
    ##  8 andrew-jackson-1832.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \n\nIt gives me pleasure to congratulate you upon your return t…
    ##  9 andrew-jackson-1833.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \n\nOn your assembling to perform the high trusts which the peo…
    ## 10 andrew-jackson-1834.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \n\nIn performing my duty at the opening of your present sessio…
    ## # ℹ 230 more rows

``` r
# combine the text and metadata
(sotu_whole <- sotu_meta %>%
        arrange(president) %>%
        bind_cols(sotu_texts) %>%
        as_tibble())
```

    ## # A tibble: 240 × 8
    ##        X president        year years_active party      sotu_type doc_id                   text                                                                       
    ##    <int> <chr>           <int> <chr>        <chr>      <chr>     <chr>                    <chr>                                                                      
    ##  1    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nIn t…
    ##  2    74 Abraham Lincoln  1862 1861-1865    Republican written   abraham-lincoln-1862.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nSinc…
    ##  3    75 Abraham Lincoln  1863 1861-1865    Republican written   abraham-lincoln-1863.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nAnot…
    ##  4    76 Abraham Lincoln  1864 1861-1865    Republican written   abraham-lincoln-1864.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nAgai…
    ##  5    41 Andrew Jackson   1829 1829-1833    Democratic written   andrew-jackson-1829.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ##  6    42 Andrew Jackson   1830 1829-1833    Democratic written   andrew-jackson-1830.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ##  7    43 Andrew Jackson   1831 1829-1833    Democratic written   andrew-jackson-1831.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ##  8    44 Andrew Jackson   1832 1829-1833    Democratic written   andrew-jackson-1832.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ##  9    45 Andrew Jackson   1833 1833-1837    Democratic written   andrew-jackson-1833.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ## 10    46 Andrew Jackson   1834 1833-1837    Democratic written   andrew-jackson-1834.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ## # ℹ 230 more rows

``` r
sotu_whole %>% tibble()
```

    ## # A tibble: 240 × 8
    ##        X president        year years_active party      sotu_type doc_id                   text                                                                       
    ##    <int> <chr>           <int> <chr>        <chr>      <chr>     <chr>                    <chr>                                                                      
    ##  1    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nIn t…
    ##  2    74 Abraham Lincoln  1862 1861-1865    Republican written   abraham-lincoln-1862.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nSinc…
    ##  3    75 Abraham Lincoln  1863 1861-1865    Republican written   abraham-lincoln-1863.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nAnot…
    ##  4    76 Abraham Lincoln  1864 1861-1865    Republican written   abraham-lincoln-1864.txt "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nAgai…
    ##  5    41 Andrew Jackson   1829 1829-1833    Democratic written   andrew-jackson-1829.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ##  6    42 Andrew Jackson   1830 1829-1833    Democratic written   andrew-jackson-1830.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ##  7    43 Andrew Jackson   1831 1829-1833    Democratic written   andrew-jackson-1831.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ##  8    44 Andrew Jackson   1832 1829-1833    Democratic written   andrew-jackson-1832.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ##  9    45 Andrew Jackson   1833 1833-1837    Democratic written   andrew-jackson-1833.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ## 10    46 Andrew Jackson   1834 1833-1837    Democratic written   andrew-jackson-1834.txt  "\n\n Fellow Citizens of the Senate and of the House of Representatives: \…
    ## # ℹ 230 more rows

## String operations

### Count occurrences

How man times does the words “citizen” or “Citizen” appear in each
speech?

``` r
sotu_whole %>% 
        mutate(n_citizen = str_count(text, "[C|c]itizen"))
```

    ## # A tibble: 240 × 9
    ##        X president        year years_active party      sotu_type doc_id                   text                                                              n_citizen
    ##    <int> <chr>           <int> <chr>        <chr>      <chr>     <chr>                    <chr>                                                                 <int>
    ##  1    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt "\n\n Fellow-Citizens of the Senate and House of Representatives…        10
    ##  2    74 Abraham Lincoln  1862 1861-1865    Republican written   abraham-lincoln-1862.txt "\n\n Fellow-Citizens of the Senate and House of Representatives…         8
    ##  3    75 Abraham Lincoln  1863 1861-1865    Republican written   abraham-lincoln-1863.txt "\n\n Fellow-Citizens of the Senate and House of Representatives…        16
    ##  4    76 Abraham Lincoln  1864 1861-1865    Republican written   abraham-lincoln-1864.txt "\n\n Fellow-Citizens of the Senate and House of Representatives…         4
    ##  5    41 Andrew Jackson   1829 1829-1833    Democratic written   andrew-jackson-1829.txt  "\n\n Fellow Citizens of the Senate and of the House of Represen…        20
    ##  6    42 Andrew Jackson   1830 1829-1833    Democratic written   andrew-jackson-1830.txt  "\n\n Fellow Citizens of the Senate and of the House of Represen…        15
    ##  7    43 Andrew Jackson   1831 1829-1833    Democratic written   andrew-jackson-1831.txt  "\n\n Fellow Citizens of the Senate and of the House of Represen…        24
    ##  8    44 Andrew Jackson   1832 1829-1833    Democratic written   andrew-jackson-1832.txt  "\n\n Fellow Citizens of the Senate and of the House of Represen…        20
    ##  9    45 Andrew Jackson   1833 1833-1837    Democratic written   andrew-jackson-1833.txt  "\n\n Fellow Citizens of the Senate and of the House of Represen…        15
    ## 10    46 Andrew Jackson   1834 1833-1837    Democratic written   andrew-jackson-1834.txt  "\n\n Fellow Citizens of the Senate and of the House of Represen…        26
    ## # ℹ 230 more rows

Count word, character, sentence, etc.

``` r
(sotu_count <- sotu_whole %>% 
        mutate(n_character = str_count(text, boundary("character")),
               n_word = str_count(text, boundary("word")),
               n_linebreak = str_count(text, boundary("line_break")),
               n_sentence = str_count(text, boundary("sentence")),
               avg_word_per_sentence = n_word/n_sentence) %>%
        select(doc_id,
               n_character, n_word, n_linebreak, n_sentence,
               avg_word_per_sentence))
```

    ## # A tibble: 240 × 6
    ##    doc_id                   n_character n_word n_linebreak n_sentence avg_word_per_sentence
    ##    <chr>                          <int>  <int>       <int>      <int>                 <dbl>
    ##  1 abraham-lincoln-1861.txt       41572   6998        7080        299                  23.4
    ##  2 abraham-lincoln-1862.txt       50110   8410        8475        391                  21.5
    ##  3 abraham-lincoln-1863.txt       37097   6132        6190        260                  23.6
    ##  4 abraham-lincoln-1864.txt       36442   5975        6043        274                  21.8
    ##  5 andrew-jackson-1829.txt        62888  10547       10629        389                  27.1
    ##  6 andrew-jackson-1830.txt        90614  15109       15263        526                  28.7
    ##  7 andrew-jackson-1831.txt        42826   7198        7248        227                  31.7
    ##  8 andrew-jackson-1832.txt        46830   7887        7943        269                  29.3
    ##  9 andrew-jackson-1833.txt        46876   7912        7957        253                  31.3
    ## 10 andrew-jackson-1834.txt        80208  13472       13544        431                  31.3
    ## # ℹ 230 more rows

Which speech has shortest/longest average sentences length?

``` r
sotu_count %>%
        filter(# shortest average sentences length
               avg_word_per_sentence == min(avg_word_per_sentence) |
               # longest average sentences length
               avg_word_per_sentence == max(avg_word_per_sentence))
```

    ## # A tibble: 2 × 6
    ##   doc_id                    n_character n_word n_linebreak n_sentence avg_word_per_sentence
    ##   <chr>                           <int>  <int>       <int>      <int>                 <dbl>
    ## 1 donald-trump-2019.txt           34312   5724        5914        648                  8.83
    ## 2 martin-van-buren-1840.txt       54996   9007        9065        242                 37.2

### Detect patterns

What are the names of the documents where “citizen” and “Citizen” do not
occur?

``` r
sotu_whole %>%
        filter(!str_detect(text, "[C|c]itizen")) %>%
        select(doc_id)
```

    ## # A tibble: 11 × 1
    ##    doc_id                      
    ##    <chr>                       
    ##  1 dwight-d-eisenhower-1958.txt
    ##  2 gerald-r-ford-1975.txt      
    ##  3 richard-m-nixon-1970.txt    
    ##  4 richard-m-nixon-1971.txt    
    ##  5 richard-m-nixon-1972a.txt   
    ##  6 ronald-reagan-1988.txt      
    ##  7 woodrow-wilson-1916.txt     
    ##  8 woodrow-wilson-1917.txt     
    ##  9 woodrow-wilson-1918.txt     
    ## 10 woodrow-wilson-1919.txt     
    ## 11 woodrow-wilson-1920.txt

### Extract words

Extract the first 5 words of each speech by Woodrow Wilson

``` r
sotu_whole %>%
        filter(president == "Woodrow Wilson") %>%
        pull(text) %>%
        word(end = 5)
```

    ## [1] "\n\nGentlemen of the Congress:\n\nIn pursuance" "\n\nGENTLEMEN OF THE CONGRESS: \n\nThe"         "GENTLEMEN OF THE CONGRESS: \n\nSince"          
    ## [4] "\n\nGENTLEMEN OF THE CONGRESS: \n\nIn"          "Gentlemen of the Congress:\n\nEight months"     "\n\nGENTLEMEN OF THE CONGRESS: \n\nThe"        
    ## [7] "\n\nTO THE SENATE AND HOUSE"                    "\n\nGENTLEMEN OF THE CONGRESS:\n\nWhen I"

### Replace and remove characters

``` r
sotu_whole %>%
        filter(president == "Woodrow Wilson") %>%
        pull(text) %>%
        # replace \n by " "
        str_replace_all("\\n", " ") %>%
        word(end = 5)
```

    ## [1] "  Gentlemen of the"          "  GENTLEMEN OF THE"          "GENTLEMEN OF THE CONGRESS: " "  GENTLEMEN OF THE"          "Gentlemen of the Congress: "
    ## [6] "  GENTLEMEN OF THE"          "  TO THE SENATE"             "  GENTLEMEN OF THE"

``` r
sotu_whole %>% 
        filter(president == "Woodrow Wilson") %>%  
        pull(text) %>%
        str_replace_all("\\n", " ") %>%
        # remove whitespaces
        str_squish() %>%
        word(end = 5) 
```

    ## [1] "Gentlemen of the Congress: In"    "GENTLEMEN OF THE CONGRESS: The"   "GENTLEMEN OF THE CONGRESS: Since" "GENTLEMEN OF THE CONGRESS: In"   
    ## [5] "Gentlemen of the Congress: Eight" "GENTLEMEN OF THE CONGRESS: The"   "TO THE SENATE AND HOUSE"          "GENTLEMEN OF THE CONGRESS: When"

## Tokenize

`token`: most commonly single words.  
Tokenize the text at the level of words

``` r
sotu_whole %>%
        tidytext::unnest_tokens(word, text)
```

    ## # A tibble: 1,988,226 × 8
    ##        X president        year years_active party      sotu_type doc_id                   word           
    ##    <int> <chr>           <int> <chr>        <chr>      <chr>     <chr>                    <chr>          
    ##  1    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt fellow         
    ##  2    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt citizens       
    ##  3    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt of             
    ##  4    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt the            
    ##  5    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt senate         
    ##  6    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt and            
    ##  7    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt house          
    ##  8    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt of             
    ##  9    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt representatives
    ## 10    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt in             
    ## # ℹ 1,988,216 more rows

Word tokenization with punctuation and no lowercasing

``` r
sotu_whole %>%
        unnest_tokens(word,
                      text,
                      to_lower = FALSE,
                      strip_punct = FALSE)
```

    ## # A tibble: 2,184,648 × 8
    ##        X president        year years_active party      sotu_type doc_id                   word           
    ##    <int> <chr>           <int> <chr>        <chr>      <chr>     <chr>                    <chr>          
    ##  1    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt Fellow         
    ##  2    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt -              
    ##  3    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt Citizens       
    ##  4    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt of             
    ##  5    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt the            
    ##  6    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt Senate         
    ##  7    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt and            
    ##  8    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt House          
    ##  9    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt of             
    ## 10    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt Representatives
    ## # ℹ 2,184,638 more rows

Tokenize the text at the level of ngrams or sentences

``` r
# Sentence tokenization
sotu_whole %>%
        unnest_tokens(sentence,
                      text,
                      token = "sentences",
                      to_lower = FALSE) %>% 
        select(sentence)
```

    ## # A tibble: 70,761 × 1
    ##    sentence                                                                                                                                                          
    ##    <chr>                                                                                                                                                             
    ##  1 Fellow-Citizens of the Senate and House of Representatives:   In the midst of unprecedented political troubles we have cause of great gratitude to God for unusua…
    ##  2 You will not be surprised to learn that in the peculiar exigencies of the times our intercourse with foreign nations has been attended with profound solicitude, …
    ##  3 A disloyal portion of the American people have during the whole year been engaged in an attempt to divide and destroy the Union.                                  
    ##  4 A nation which endures factious domestic division is exposed to disrespect abroad, and one party, if not both, is sure sooner or later to invoke foreign interven…
    ##  5 Nations thus tempted to interfere are not always able to resist the counsels of seeming expediency and ungenerous ambition, although measures adopted under such …
    ##  6 The disloyal citizens of the United States who have offered the ruin of our country in return for the aid and comfort which they have invoked abroad have receive…
    ##  7 If it were just to suppose, as the insurgents have seemed to assume, that foreign nations in this case, discarding all moral, social, and treaty obligations, wou…
    ##  8 If we could dare to believe that foreign nations are actuated by no higher principle than this, I am quite sure a sound argument could be made to show them that …
    ##  9 The principal lever relied on by the insurgents for exciting foreign nations to hostility against us, as already intimated, is the embarrassment of commerce.     
    ## 10 Those nations, however, not improbably saw from the first that it was the Union which made as well our foreign as our domestic commerce.                          
    ## # ℹ 70,751 more rows

``` r
# N-gram tokenization as trigrams
sotu_whole %>%
        unnest_tokens(trigram,
                      text,
                      token = "ngrams",
                      n = 3) %>% 
        select(trigram)
```

    ## # A tibble: 1,987,746 × 1
    ##    trigram                 
    ##    <chr>                   
    ##  1 fellow citizens of      
    ##  2 citizens of the         
    ##  3 of the senate           
    ##  4 the senate and          
    ##  5 senate and house        
    ##  6 and house of            
    ##  7 house of representatives
    ##  8 of representatives in   
    ##  9 representatives in the  
    ## 10 in the midst            
    ## # ℹ 1,987,736 more rows

## Stopwords

`Stopwords`: highly common words -\> provide non-relevant information
about content of a text.

``` r
# from `tidytext` package
stop_words
```

    ## # A tibble: 1,149 × 2
    ##    word        lexicon
    ##    <chr>       <chr>  
    ##  1 a           SMART  
    ##  2 a's         SMART  
    ##  3 able        SMART  
    ##  4 about       SMART  
    ##  5 above       SMART  
    ##  6 according   SMART  
    ##  7 accordingly SMART  
    ##  8 across      SMART  
    ##  9 actually    SMART  
    ## 10 after       SMART  
    ## # ℹ 1,139 more rows

``` r
stop_words %>% count(lexicon)
```

    ## # A tibble: 3 × 2
    ##   lexicon      n
    ##   <chr>    <int>
    ## 1 SMART      571
    ## 2 onix       404
    ## 3 snowball   174

``` r
sotu_whole %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)
```

    ## Joining with `by = join_by(word)`

    ## # A tibble: 787,861 × 8
    ##        X president        year years_active party      sotu_type doc_id                   word           
    ##    <int> <chr>           <int> <chr>        <chr>      <chr>     <chr>                    <chr>          
    ##  1    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt fellow         
    ##  2    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt citizens       
    ##  3    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt senate         
    ##  4    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt house          
    ##  5    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt representatives
    ##  6    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt midst          
    ##  7    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt unprecedented  
    ##  8    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt political      
    ##  9    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt troubles       
    ## 10    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt gratitude      
    ## # ℹ 787,851 more rows

## Word stemming

Reduce words to word stem or root form. E.g., reducing `fishing`,
`fished`, and `fisher` to the stem `fish`.  
R packages for word stemmer: `hunspell`, `SnowballC`

``` r
sotu_whole %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        mutate(word_stem = SnowballC::wordStem(word))
```

    ## Joining with `by = join_by(word)`

    ## # A tibble: 787,861 × 9
    ##        X president        year years_active party      sotu_type doc_id                   word            word_stem
    ##    <int> <chr>           <int> <chr>        <chr>      <chr>     <chr>                    <chr>           <chr>    
    ##  1    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt fellow          fellow   
    ##  2    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt citizens        citizen  
    ##  3    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt senate          senat    
    ##  4    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt house           hous     
    ##  5    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt representatives repres   
    ##  6    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt midst           midst    
    ##  7    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt unprecedented   unpreced 
    ##  8    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt political       polit    
    ##  9    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt troubles        troubl   
    ## 10    73 Abraham Lincoln  1861 1861-1865    Republican written   abraham-lincoln-1861.txt gratitude       gratitud 
    ## # ℹ 787,851 more rows

Lemmatization: discriminate between words which have different meanings
depending on part of speech.  
R package: `koRpus`, `TreeTagger`

``` r
# rmarkdown::render()
```
