# Load packages
require(rvest)
require(tidyverse)
require(tidytext)
require(dplyr)
require(lubridate)
require(topicmodels)
require(widyr)
require(textdata)
require(glmnet)
if (!requireNamespace("doParallel")) {install.packages("doParallel")}
require(doParallel)
if (!requireNamespace("wordcloud")) {install.packages("wordcloud")}
require(wordcloud)
require(reshape2)
require(ROCR)
require(foreach)

# Register doParall to get foreach worked
registerDoParallel()


# Scrape fanwork id first; plug into the url

# Turn the scraping steps into functions

get_alldat <- function(id) {
  
  response <- read_html(paste0("https://archiveofourown.org/works/", id, "?view_adult=true&view_full_work=true"))
  
  rating <- html_nodes(x = response,
                       xpath = "//dd[@class='rating tags']//a[@class='tag']") %>%
    html_text()
  
  warning <- html_nodes(x = response,
                        xpath = "//dd[@class='warning tags']//a[@class='tag']") %>%
    html_text() %>%
    paste(collapse = ", ")
  
  categ <- html_nodes(x = response,
                      xpath = "//dd[@class='category tags']//a[@class='tag']") %>%
    html_text() %>%
    paste(collapse = ", ")
  
  fandom <- html_nodes(x = response,
                       xpath = "//dd[@class='fandom tags']//a[@class='tag']") %>%
    html_text() %>%
    paste(collapse = ", ")
  
  relationship <- html_nodes(x = response,
                             xpath = "//dd[@class='relationship tags']//a[@class='tag']") %>%
    html_text() %>%
    paste(collapse = ", ")
  
  charac <- html_nodes(x = response,
                       xpath = "//dd[@class='character tags']//a[@class='tag']") %>%
    html_text() %>%
    paste(collapse = ", ")
  
  addtag <- html_nodes(x = response,
                       xpath = "//dd[@class='freeform tags']//a[@class='tag']") %>%
    html_text() %>%
    paste(collapse = ", ")
  
  pubdate <- html_nodes(x = response,
                        xpath = "//dl[@class='stats']//dd[@class='published']") %>%
    html_text()
  
  update <- html_nodes(x = response,
                       xpath = "//dl[@class='stats']//dd[@class='status']") %>%
    html_text()
  update <- ifelse(length(update)==0, NA, update)
  
  wordcnt <- html_nodes(x = response,
                        xpath = "//dl[@class='stats']//dd[@class='words']") %>%
    html_text()
  
  chapter <- html_nodes(x = response,
                        xpath = "//dl[@class='stats']//dd[@class='chapters']") %>%
    html_text()
  
  comment <- html_nodes(x = response,
                        xpath = "//dl[@class='stats']//dd[@class='comments']") %>%
    html_text()
  comment <- ifelse(length(comment)==0, NA, comment)
  
  kudo <- html_nodes(x = response,
                     xpath = "//dl[@class='stats']//dd[@class='kudos']") %>%
    html_text()
  kudo <- ifelse(length(kudo)==0, NA, kudo)
  
  bookmark <- html_nodes(x = response,
                         xpath = "//dl[@class='stats']//dd[@class='bookmarks']") %>%
    html_text() 
  bookmark <- ifelse(length(bookmark)==0, NA, bookmark)
  
  hit <- html_nodes(x = response,
                    xpath = "//dl[@class='stats']//dd[@class='hits']") %>%
    html_text()
  hit <- ifelse(length(hit)==0, NA, hit)
  
  title <- html_nodes(x = response,
                      xpath = "//div[@class='preface group']//h2[@class='title heading']") %>%
    html_text(trim = T)
  
  summ <- html_nodes(x = response,
                     xpath = "(//div[@class='summary module']//blockquote[@class='userstuff']//p)[position() < 2]") %>%
    html_text(trim = T)
  summ <- ifelse(length(summ)==0, NA, summ)
  
  article <- html_nodes(x = response,
                        xpath = "//div[@id='chapters']") %>%
    html_text(trim = T) %>%
    #paste(collapse = "<end of chapter>")
    str_remove_all("Chapter Text") %>%
    str_remove_all("Work Text:")
  
  ## Extract the number of chapters  
  chaptcnt <- gsub("^(.*?)/.*", "\\1", chapter) 
  ## Completion status
  complet <- !grepl("\\?", chapter)
  
  dat <- data.frame(id, title, chapter, chaptcnt, complet, rating, warning, categ, fandom, relationship, charac, addtag, pubdate, update, wordcnt, comment, kudo, bookmark, hit, summ, article) %>%
  mutate_all(as.character) %>%
  mutate_at(c('wordcnt', 'comment', 'kudo', 'bookmark', 'hit'), as.numeric)
  
  return(dat)
}


# Test the timing of the funcion
#t0 <- Sys.time()
#test <- get_alldat(8018233)
#test2 <- get_alldat(20542313)
#test <- rbind(test, test2)
#t1 <- Sys.time()
#t1-t0




# Write a function to do topic modeling with different K's
add_lda_feat <- function(summ_mod, addtag_mod, dat, summ_dtm = summ_tm, addtag_dtm = addtag_tm) {
  
  # Create the features according to LDA
  topic <- as.data.frame(posterior(summ_mod, summ_dtm)$topics) 
  topic_name <- paste0("summ_t", colnames(topic))
  colnames(topic) <- topic_name
  topic$id <- as.numeric(rownames(topic))
  dat <- left_join(dat, topic) %>%
    mutate_at(topic_name, .funs = function(x) ifelse(is.na(x), 0, x))
  
  
  # Create the features according to LDA
  topic <- as.data.frame(posterior(addtag_mod, addtag_dtm)$topics)
  topic_name <- paste0("addtag_t", colnames(topic))
  colnames(topic) <- topic_name
  topic$id <- as.numeric(rownames(topic))
  dat <- left_join(dat, topic) %>%
    mutate_at(topic_name, .funs = function(x) ifelse(is.na(x), 0, x))
  
  return(dat)

}



