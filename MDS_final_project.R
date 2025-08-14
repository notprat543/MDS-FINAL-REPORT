pacman::p_load(gutenbergr, dplyr, tidytext,topicmodels,tidyverse,tm,lexicon,zoo,
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

## Check coverage
text<-read.csv("all_books_no_punct.csv")

# AFINN 
afinn <-  get_sentiments("afinn") %>%
  select(word) 

# Bing 
bing <- get_sentiments("bing") %>%
  select(word) 

# Sentiword
data("hash_sentiment_sentiword", package = "lexicon")

swn <- hash_sentiment_sentiword %>%
  select(x) %>%
  rename(word = x) %>%
  distinct()

# Senticnet

scn<-hash_sentiment_senticnet%>%
  select(x)%>%
  rename(word = x)%>%
  distinct()

# NRC
nrc <- get_sentiments("nrc") %>%
  select(word) %>%
  distinct()


calc_coverage <- function(dict, tokens) {
  common_words <- tokens %>%
    inner_join(dict, by = "word")
  return(nrow(common_words) / nrow(tokens))
}




coverage_afinn <- calc_coverage(afinn, tokens)
coverage_bing <- calc_coverage(bing, tokens)
coverage_nrc  <- calc_coverage(nrc, tokens)
coverage_swn  <- calc_coverage(swn, tokens)
coverage_scn  <- calc_coverage(scn, tokens)




print(data.frame(
  Dictionary = c("AFINN", "Bing", "NRC","SWN","SCN"),
  Coverage = c(coverage_afinn, coverage_bing, coverage_nrc,coverage_swn,coverage_scn)
))

## load senticnet

scn<-hash_sentiment_senticnet%>%
  rename(word= x, value = y)%>%
  distinct()

book_sentiment <- tidy_books %>%
  inner_join(scn) %>%
  group_by(book)%>%
  summarise(book_sent = sum(n*value)/sum(n))

book_sentiment

## plots the average sentiment scores for all novels
ggplot(book_sentiment, aes(x = reorder(book, -book_sent), y = book_sent, fill = book)) +
  geom_col(show.legend = FALSE)+  scale_fill_viridis_d(option = "magma") +
  labs(title = "Overall Sentiment by Book (senticnet)",
       x = "Book",
       y = "Normalized Sentiment Score") +
  theme_minimal() + coord_flip()

### USED LAST TRIMESTER
# # clean dataset
# new_book <- all_books %>%
#   unnest_tokens(word, text) %>%
#   count(book, part, chapter, word, sort = TRUE) %>%
#   anti_join(stop_words, by = "word") %>%
#   mutate(word = lemmatize_words(word)) %>%
#   filter(!word %in% custom_names) %>%
#   select(book, word, chapter, n)
# 
# new_book
# 
# ## function to plot sentiment scores by chapter
# plot_sentiment_by_book <- function(book_name) {
#   new_book %>%
#     filter(book == book_name) %>%  # Filter for a specific book
#     inner_join(afinn, by = "word") %>%
#     group_by(chapter) %>%
#     summarise(chap_sent = sum(n * value) / sum(n), .groups = 'drop') %>%
# 
#     ggplot(aes(x = chapter, y = chap_sent)) +
#     geom_point(color = "#2caa71") +
#     geom_line(color = "#ffaac1") +
#     labs(
#       title = paste("Sentiment by Chapter -", book_name),
#       x = "Chapter",
#       y = "Sentiment Score"
#     ) +
#     theme_minimal()
# }
# 
# 
# 
# # Generate plots for each book
# plot_sentiment_by_book("Treasure Island")
# plot_sentiment_by_book("Moby-Dick")
# plot_sentiment_by_book("Robinson Crusoe")
# plot_sentiment_by_book("The Lost World")
# plot_sentiment_by_book("King Solomon's Mines")
# plot_sentiment_by_book("The Call of the Wild")



########------STANDARDISED SENTIMENT---------

plot_sentiment_standardised <- function(window = 3) {
  new_book %>%
    inner_join(scn, by = "word") %>%
    group_by(book, standardised_chapter) %>%
    summarise(sentiment = sum(n * value) / sum(n), .groups = 'drop') %>%
    group_by(book) %>%
    arrange(standardised_chapter) %>%
    mutate(
      sentiment_ma = rollmean(sentiment, k = window, fill = NA, align = "center")
    ) %>% filter(!book== "Moby-Dick")%>%
    ggplot(aes(x = standardised_chapter, y = sentiment_ma, color = book)) +
    geom_line() +
    geom_point(size = 1, aes(y = sentiment)) +
    #geom_smooth()+
    labs(
      title = "Sentiment Progression (Moving Average)",
      x = "Standardised Chapter",
      y = "Smoothed Sentiment Score"
    ) +
    theme_minimal() +
    theme(legend.title = element_blank())
}


plot_sentiment_standardised()

##### NRC EMOTIONS #####


nrc_lexicon <- get_sentiments("nrc")

nrc_emotion_counts <- new_book %>%
  inner_join(nrc_lexicon, by = "word", relationship = "many-to-many") %>%
  group_by(book, sentiment) %>%
  summarise(count = sum(n), .groups = 'drop')

nrc_emotions_proportions <- nrc_emotion_counts%>%
  group_by(book)%>%
  mutate(proportion = count/sum(count))%>%
  ungroup()

ggplot(nrc_emotions_proportions, aes(x = sentiment, y = proportion, fill = book)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set3")+
  coord_flip() +
  labs(
    title = "NRC Emotion Counts by Book",
    x = "Emotion",
    y = "proportion of Word Count"
  ) +
  theme_minimal()

###---------PROTAGONIST MENTIONS--------


new_book1 <- all_books %>%
  unnest_tokens(word, text) %>%
  count(book, part,standardised_chapter, chapter, word, sort = TRUE) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = lemmatize_words(word)) %>%
  select(book, word, chapter, n, standardised_chapter)

new_book1%>%filter(book== "King Solomon's Mines")%>% filter(word %in% c("allan","quatermain"))
new_book1

protagonists <- tibble(
  book = c(
    "Treasure Island",
    "Robinson Crusoe",
    "The Call of the Wild",
    "The Lost World",
    "Moby-Dick",
    "King Solomon's Mines"
  ),
  protagonist = list(
    c("jim hawkins","jim","hawkins"), 
    c("robinson crusoe", "robinson", "crusoe"),
    c("buck"),
    c("professor challenger", "challenger","professor",
      "edward malone", "edward", "malone"),
    c("ishmael"),
    #c("captain ahab", "captain", "ahab"),
    c("allan quatermain", "allan", "quatermain")))%>%
  unnest(protagonist)



name_mentions1 <- new_book1 %>%
  #filter(book %in% c("Moby-Dick","The Call of the Wild","The Lost World")) %>%                      
  inner_join(protagonists, by = "book") %>%
  filter(word == protagonist) %>%
  group_by(book, standardised_chapter) %>%
  summarise(mentions = sum(n), .groups = "drop") %>%
  complete(book, standardised_chapter = 1:10, fill = list(mentions = 0))%>%
  arrange(book, standardised_chapter) %>%
  group_by(book) %>%
  mutate(rolling_mentions = rollmean(mentions, k = 3, fill = NA, align = "center")) %>%
  ungroup()


ggplot(name_mentions1, aes(x = standardised_chapter, y = rolling_mentions, color = book)) +
  #geom_line(size = 0.7) +
  geom_point(size = 1.5) +
  geom_smooth(se = FALSE)+
  labs(
    title = "Protagonist Mentions by standardised Chapter",
    x = "standardised Chapter",
    y = "Number of Mentions"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())



####-- INDIVIDUAL PROTAGONIST MENTIONS GRAPHS #######


name_mentions2<-new_book1%>%
  filter(book %in% c("Robinson Crusoe"))%>% #,"The Call of the Wild","The Lost World")) %>%                      
  inner_join(protagonists, by = "book") %>%
  filter(word == protagonist) %>%
  group_by(book, chapter) %>%
  summarise(mentions = sum(n), .groups = "drop") %>%
  complete(chapter = full_seq(chapter, 1), book, fill = list(mentions = 0))
name_mentions2


ggplot(name_mentions2, aes(x = chapter, y = mentions, color = book)) +
  geom_line(size = 0.7, color = "steelblue") +
  geom_point(size = 1.5, color="steelblue") +
  #geom_smooth(se = FALSE)+
  labs(
    title = "Protagonist mentions by chapter",
    x = "Chapter",
    y = "Number of Mentions"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())



