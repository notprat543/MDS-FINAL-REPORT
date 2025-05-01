pacman::p_load(gutenbergr, dplyr, tidytext,topicmodels,tidyverse,tm,
               scales,stringr, writexl, SnowballC, textstem, ggplot2, forcats, ggwordcloud)


#-----Download books------
# Treasure Island
TI<-gutenberg_download(27780)
TI_text<- TI %>% tail(nrow(TI) - 228) %>% head(7458)  # Remove header and footer

#Robinson Crusoe
options(gutenbergr.mirror = "https://mirrors.xmission.com/gutenberg/")
Robinson_Crusoe<-gutenberg_download(70841)
Robinson_Crusoe_text<-Robinson_Crusoe %>% tail(nrow(Robinson_Crusoe) - 313) %>% head(9724)  # Remove header and footer


# The lost world
url <- "https://www.gutenberg.org/cache/epub/139/pg139.txt"  # Main version
lost_world <- readLines(url, encoding = "UTF-8", warn = FALSE)
lost_world_df <- tibble(gutenberg_id=rep(8614,8363),data.frame(text = lost_world, stringsAsFactors = FALSE))
lost_world_text<-lost_world_df %>% tail(nrow(lost_world_df) - 90) %>% head(7911)  # Remove header and footer
lost_world_text


# King Solomon's mines
options(gutenbergr.mirror = "https://www.gutenberg.org")
King_solomons_mines<-gutenberg_download(2166)
King_solomons_text<-King_solomons_mines %>% tail(nrow(King_solomons_mines) - 159)  # Remove header and footer

# Call of the wild download
url1 <- "https://www.gutenberg.org/cache/epub/215/pg215.txt"  # Main version
cotw_text <- readLines(url1, encoding = "UTF-8", warn = FALSE)
cotw_df <- tibble(gutenberg_id=rep(19678,3448),data.frame(text = cotw_text, stringsAsFactors = FALSE))
cotw_text<-cotw_df %>% tail(nrow(cotw_df) - 52)%>%head(3041)  # Remove header and footer

# Moby Dick download
url2 <- "https://www.gutenberg.org/cache/epub/2701/pg2701.txt"  # Main version
MD <- readLines(url2, encoding = "UTF-8", warn = FALSE)
MD_df <- tibble(gutenberg_id=rep(2489,22314),data.frame(text = MD, stringsAsFactors = FALSE))
MD_text<-MD_df %>% tail(nrow(MD_df) - 845)%>%head(21114)  # Remove header and footer



#------BOOKS------
TI_text #27780
Robinson_Crusoe_text #70841
lost_world_text #8614
King_solomons_text #2166
cotw_text #19678
MD_text #2489

#-------books with chapters--------


# Treasure island
TI<-tibble(TI_text)%>% mutate(book = "Treasure Island",linenumber=row_number(),
                              chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]",ignore_case = TRUE))),
                              part = cumsum(str_detect(text, regex("^part [\\divxlc]",ignore_case = TRUE)))) %>% 
                              filter(!(text == ""))

#Robinson Crusoe
RC<-tibble(Robinson_Crusoe_text)%>%mutate(book = "Robinson Crusoe",linenumber=row_number(),
                                          chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]",ignore_case = TRUE))),
                                          part = cumsum(str_detect(text, regex("^part [\\divxlc]",ignore_case = TRUE)))) %>% 
                                         filter(!(text == ""))

# The lost world
LW<-tibble(lost_world_text)%>%mutate(text = str_squish(lost_world_text$text),book = "The Lost World",
                                     linenumber=row_number(),
                                     chapter = cumsum(str_detect(text, regex("^CHAPTER\\s+[IVXLC]+$", ignore_case = TRUE))),
                                     part = cumsum(str_detect(text, regex("^part [\\divxlc]",ignore_case = TRUE)))) %>% 
                                      filter(!(text == ""))

# King solomon's mines
KS<-tibble(King_solomons_text)%>%mutate(book = "King Solomon's Mines", linenumber=row_number(),
                                        chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]",ignore_case = TRUE))),
                                        part = cumsum(str_detect(text, regex("^part [\\divxlc]",ignore_case = TRUE)))) %>% 
                                        filter(!(text == ""))

# The call of the wild
COTW<-tibble(cotw_text)%>%mutate(book = "The Call of the Wild", linenumber=row_number(),
                                 chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]",ignore_case = TRUE))),
                                 part = cumsum(str_detect(text, regex("^part [\\divxlc]",ignore_case = TRUE)))) %>% 
                                filter(!(text == ""))

# Moby Dick
MD<-tibble(MD_text)%>%mutate(book = "Moby-Dick", linenumber=row_number(),
                             chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]",ignore_case = TRUE))),
                             part = cumsum(str_detect(text, regex("^part [\\divxlc]",ignore_case = TRUE)))) %>% 
                             filter(!(text == ""))





#----- combined books-----

all_books <- bind_rows(TI, RC, LW, KS, COTW, MD)
all_books$text<-str_to_lower(all_books$text)

# removed punctuation
all_books_no_punct<- all_books%>%mutate(text = str_remove_all(text, "[[:punct:]]"))
all_books_no_punct

#csv files
write.csv(all_books,"all_books.csv",row.names = FALSE )
write.csv(all_books_no_punct,"all_books_no_punct.csv",row.names = FALSE )



######################### LOAD HERE###########


#----CSV FILE----
all_books_csv<-read.csv("all_books.csv") ### not used in this trimester
all_books_no_punct_csv<-read.csv("all_books_no_punct.csv")

# removing names from stop words

custom_names <- tolower(c("jim","hawkins", "silver","Squire","Tom", "Joyce","Abraham",
                          "Alan","hispaniola","Ben","Gunn","morgan","smollett","im","ive",
                          "long", "john", "captain","redruth" ,"bildad","loo","thy",
                          "xury","friday","incubu", "bougwan", "ventvögel","gladys", 
                          "zambo","waldron","malone","mcardle","challenger","summerlee",
                          "roxton","white","quatermain","umbopa","twala","gagool","scragga",
                          "infadoos","curtis","kafir","foulata","imotu","khivs","sir","henry",
                          "ignosi","kraal","twalas","zulu","macumazahn","solomons",
                          "you","aye","henrys","solleks","miller","manuel","françois",
                          "perrault","thornton","mercedes","thorntons","thornton's",
                          "hans","charles","billee","dawson","pete","hal","buck","curly",
                          "spitz","dave","pike","dub","leks","ha","ishmael","ahab","moby",
                          "stubb","flask","ahabs", "thee","thou","ye",
                          "solomon","queequeg","starbuck","pequod","pip","peleg"))

data(stop_words) 

# tokenise
all_books_clean<- all_books_no_punct%>%unnest_tokens(word, text) %>%anti_join(stop_words)

# reduce words to base form
all_books_clean<-all_books_clean%>%mutate(word = lemmatize_words(word))

# remove custom words
all_books_clean<-all_books_clean%>%filter(!word %in% custom_names)
all_books_clean$word <- gsub("capn", "captain", all_books_clean$word, ignore.case = TRUE)
word_synonyms <- c("death" = "death", "die" = "death", "dead" = "death","whale's"="whale",
                   "life" = "life", "live" = "life", "living" = "life", "kukuana's"="kukuana",
                   "kukuanas"="kukuana","scientific"="science")
all_books_clean<-all_books_clean%>% mutate( word = recode(word, !!!word_synonyms))



#### --------Word frequencies-----
word_counts<-all_books_clean%>%count(book, word, sort=TRUE)
word_counts
all_books_clean

#MD
all_books_clean%>%filter(book=="Moby-Dick") %>%count(word, sort = TRUE)

#TI
all_books_clean%>%filter(book=="Treasure Island") %>%count(word, sort = TRUE)

#RC
all_books_clean%>%filter(book=="Robinson Crusoe") %>%count(word, sort = TRUE)

#LW
all_books_clean%>%filter(book=="The Lost World") %>%count(word, sort = TRUE)

#KS
all_books_clean%>%filter(book=="King Solomon's Mines") %>%count(word, sort = TRUE)

#COTW
all_books_clean%>%filter(book=="The Call of the Wild") %>%count(word, sort = TRUE)



#----word cloud function----

get_wordcloud<- function(book_name,top_n = 100){
  data_to_plot <- word_counts %>%
    filter(book == book_name) %>%
    slice_max(order_by = n, n = top_n)
  
  p <- ggplot(data_to_plot, aes(label = word, size = n)) +
    geom_text_wordcloud(area_corr = TRUE) +
    scale_size_area(max_size = 12) +
    theme_minimal() +
    labs(title = paste("Word Cloud:", book_name))
  
  print(p)
}

get_wordcloud("Treasure Island")
get_wordcloud("Moby-Dick")
get_wordcloud("Robinson Crusoe")
get_wordcloud("The Lost World")
get_wordcloud("King Solomon's Mines")
get_wordcloud("The Call of the Wild")

############----TF-IDF-----

tf_idf <- word_counts %>%
  bind_tf_idf(term = word, document = book, n = n)

top_tf_idf<-tf_idf%>%
  group_by(book)%>%
  slice_max(tf_idf, n=10, with_ties = FALSE)%>%ungroup()
top_tf_idf

top_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, book)) %>%
  ggplot(aes(tf_idf, word, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ book, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top 10 TF-IDF Words per Book", x = "TF-IDF", y = "Word")


###############--------------TOPIC MODELLING----------

# function to create an lda model
run_lda_for_book <- function(book_df, book_title, k = 2) {
  
  stop_words <- tidytext::stop_words
  
  book_df <- book_df %>%
    filter(book == book_title)
  
  tidy_words <- book_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!word %in% custom_names)%>%
    count(linenumber, word, sort = TRUE)%>%
    mutate(word = lemmatize_words(word))
  
  dtm <- tidy_words %>%
    cast_dtm(linenumber, word, n)
  
  lda_model <- LDA(dtm, k = k, control = list(seed = 1234))
  
  top_terms <- tidy(lda_model, matrix = "beta") %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>%
    ungroup() %>%
    mutate(book = book_title)
  
  return(top_terms)
}

############ NOT USED IN THIS TRIMESTER
# chp_topics<-tidy(book_lda, matrix="beta")
# 
# tp_trm<-chp_topics%>%
#   group_by(topic)%>%
#   slice_max(beta, n=15)%>%
#   ungroup()%>%arrange(topic, -beta)
# 
# chapt_gamma<-tidy(book_lda, matrix="gamma")
# 
# 
# tp_trm %>% mutate(term = reorder_within(term, beta, topic))%>%
#   ggplot(aes(x = beta,
#              y = term,
#              fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   scale_y_reordered() +
#   labs(
#     title = "Top Terms per Topic",
#     x = "Beta (Probability of Word in Topic)",
#     y = NULL
#   )
# ##################################



books_list <- split(all_books, all_books$book)
## LDA
lda_results <- map2_df(books_list, names(books_list), ~ run_lda_for_book(.x, .y))
unique_books <- unique(lda_results$book)

# Creates a named list of plots with top lDA terms
book_lda_plots <- lapply(unique_books, function(bk) {
  lda_results %>%
    filter(book == bk) %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    scale_y_reordered() +
    labs(
      title = paste("Top LDA Terms in", bk),
      x = "Beta (Topic Importance)",
      y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      strip.text = element_text(face = "bold")
    )
})

names(book_lda_plots) <- unique_books


book_lda_plots[["Treasure Island"]]
book_lda_plots[["Moby-Dick"]]
book_lda_plots[["Robinson Crusoe"]]
book_lda_plots[["The Lost World"]]
book_lda_plots[["King Solomon's Mines"]]
book_lda_plots[["The Call of the Wild"]]


##########----------- Sentiment Analysis#####

## load afinn
afinn <- get_sentiments("afinn")

afinn <- afinn %>%
  mutate(value =(((value+5)/(5+5))*(1+1)-1))

book_sentiment <- tidy_books %>%
  inner_join(afinn) %>%
  group_by(book)%>%
  summarise(book_sent = sum(n*value)/sum(n))

book_sentiment

## plots the average sentiment scores for all novels
ggplot(book_sentiment, aes(x = reorder(book, -book_sent), y = book_sent, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Overall Sentiment by Book (AFINN)",
       x = "Book",
       y = "Normalized Sentiment Score") +
  theme_minimal() + coord_flip()

# clean dataset
new_book <- all_books %>%
  unnest_tokens(word, text) %>%
  count(book, part, chapter, word, sort = TRUE) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!word %in% custom_names) %>%
  select(book, word, chapter, n)

new_book

## function to plot sentiment scores by chapter
plot_sentiment_by_book <- function(book_name) {
  new_book %>%
    filter(book == book_name) %>%  # Filter for a specific book
    inner_join(afinn, by = "word") %>%
    group_by(chapter) %>%
    summarise(chap_sent = sum(n * value) / sum(n), .groups = 'drop') %>%
    
    ggplot(aes(x = chapter, y = chap_sent)) +
    geom_point(color = "#2caa71") +
    geom_line(color = "#ffaac1") +
    labs(
      title = paste("Sentiment by Chapter -", book_name),
      x = "Chapter",
      y = "Sentiment Score"
    ) +
    theme_minimal()
}



# Generate plots for each book
plot_sentiment_by_book("Treasure Island")
plot_sentiment_by_book("Moby-Dick")
plot_sentiment_by_book("Robinson Crusoe")
plot_sentiment_by_book("The Lost World")
plot_sentiment_by_book("King Solomon's Mines")
plot_sentiment_by_book("The Call of the Wild")
