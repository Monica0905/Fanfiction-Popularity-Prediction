# Source the self-defined functions
source("scripts/Functions.R")


# Load data
# We stored all the web scraped fanfiction data in “fandata.RDATA”.
load("data/fandata.RDATA")
fandata <- fandata %>%
  mutate(id=1:n()) %>%
  select(-article,-kudo,-comment,-bookmark,-chapter) %>%
  mutate(chaptcnt = as.numeric(chaptcnt))


#################################################################
              ###Create the Outcome Variable###
#################################################################

# Add daily hit rate, remove rows with missing hit
fandata <- fandata %>% 
  mutate(pubdate=ymd(pubdate), update= ymd(update),
         update=ifelse(is.na(update),pubdate,update),
         update = as_date(update, origin = lubridate::origin),
         daily_hit=(hit/as.numeric(scrape_date-pubdate))) %>%
  filter(!is.na(hit))

# Restrict data to only fictions posted two months before the scraping date or earlier
# (published dates before 2019-12-01)
# because we assume the daily hit rates of very newly published fanfictions can fluctuate a lot and thus are not very reliable
fandata <- fandata %>%
  filter(pubdate < "2019-12-01")

# Add a binary outcome variable indicating high daily hit rate
fandata <- fandata %>% 
  mutate(pub_year=year(pubdate), up_year=year(update))%>%
  group_by(pub_year,up_year)%>%
  mutate(med_hit=median(daily_hit,na.rm = T),
         high_hit=ifelse(daily_hit>med_hit,1,0))



#################################################################
                ###Create new features###
#################################################################

####### Features about the relationship tags #######

fandata_relationship <- fandata %>%
  ungroup() %>%
  select(id, relationship) %>%
  unnest_tokens(relationship, relationship, token = "regex", pattern = ", ") %>%
  mutate(# Whether the relationship is Sherlock Holms/John Waston
    relation_sj = (grepl("sherlock", relationship) | grepl("holmes", relationship)) & (grepl("john", relationship) | grepl("watson", relationship)),
         # Whether the relationship is Sherlock Holms/other people
    relation_so = (grepl("sherlock", relationship) | grepl("holmes", relationship)) & !(grepl("john", relationship) | grepl("watson", relationship)),
    # Whether the relationship is John Waston/other people
    relation_jo = !(grepl("sherlock", relationship) | grepl("holmes", relationship)) & (grepl("john", relationship) | grepl("watson", relationship)),
    # Whether the relationship is other people/other people
    relation_oo = !(grepl("sherlock", relationship) | grepl("holmes", relationship)) & !(grepl("john", relationship) | grepl("watson", relationship))
  ) %>%
  group_by(id) %>%
  summarise(relation_sj = sum(relation_sj),
            relation_so = sum(relation_so),
            relation_jo = sum(relation_jo),
            relation_oo = sum(relation_oo))

fandata <- left_join(fandata, fandata_relationship) %>%
  # If the four variables are missing, then convert them to 0
  mutate(relation_sj = ifelse(is.na(relation_sj), 0, relation_sj),
         relation_so = ifelse(is.na(relation_so), 0, relation_so),
         relation_jo = ifelse(is.na(relation_jo), 0, relation_jo),
         relation_oo = ifelse(is.na(relation_oo), 0, relation_oo))


####### The number of characters #######

fandata_characters <- fandata %>%
  ungroup() %>%
  select(id, charac) %>%
  unnest_tokens(charac, charac, token = "regex", pattern = ", ") %>% 
  group_by(id)%>%
  summarise(num_charac=n())

fandata<-fandata%>%left_join(fandata_characters,by="id") %>%
  mutate(num_charac = ifelse(is.na(num_charac), 0, num_charac))

# Add binary features about the warning tag
fandata <- fandata %>%
  mutate(no_warn = grepl("No Archive Warnings Apply", warning),
         unclear_warn = grepl("Creator Chose Not To Use Archive Warnings", warning),
         violence = grepl("Graphic Depictions Of Violence", warning),
         death = grepl("Major Character Death", warning),
         rape = grepl("Rape/Non-Con", warning),
         underage = grepl("Underage", warning))


####### Features about the category tags #######

fandata <- fandata %>%
  mutate(cat_mm = grepl("M/M", categ),
         cat_fm = grepl("F/M", categ),
         cat_ff = grepl("F/F", categ),
         cat_multi = grepl("Multi", categ),
         cat_gen = grepl("Gen", categ),
         cat_other = grepl("Other", categ))


####### Number of works other than Sherlock in the fandom tags #######

fandata_fandom <- fandata %>%
  ungroup() %>%
  select(id, fandom) %>%
  unnest_tokens(fandom, fandom, token = "regex", pattern = ", ") %>%
  mutate(other_fandom = !(grepl("sherlock", fandom) | grepl("holmes", fandom))) %>%
  group_by(id) %>%
  summarise(other_fandom = sum(other_fandom))

fandata <- left_join(fandata, fandata_fandom)


####### Whether the summary contains a question mark ("?") #######

fandata <- fandata %>%
  mutate(qmark = grepl("\\?", summ))


####### Whether Sherlock and Waston both appear in the summary #######

fandata<-fandata%>%
  ungroup() %>%
  mutate(summ_sj = (grepl("sherlock", summ) | grepl("holmes", summ)) & (grepl("john", summ) | grepl("watson", summ)))





#################################################################
                   ###Split the data###
#################################################################

# Create training, validation, and test set (60-20-20)
split_size1 <- floor(nrow(fandata)*0.6)
split_size2 <- floor(nrow(fandata)*0.8)

set.seed(123)
fandata <- fandata[sample(1:nrow(fandata), nrow(fandata)), ]

train <- fandata[1:split_size1, ]
valid <- fandata[(split_size1+1):split_size2, ]
test <- fandata[(split_size2+1):nrow(fandata), ]

# Save the three sets
save(train, file = "data/train.RDATA")
save(valid, file = "data/valid.RDATA")
save(test, file = "data/test.RDATA")





#################################################################
      ### Create more features (in the training set) ###
#################################################################

# Unnest the words by sentence, and remove stop words

## Words in the summary
data("stop_words")
summ_words <- train %>%
  ungroup() %>%
  mutate(summ = tolower(summ)) %>%
  unnest_tokens(sentence, summ, token = "sentences") %>%
  group_by(id) %>%
  mutate(sentence_id = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, sentence, token = "words") %>%
  anti_join(stop_words) %>%
  # trim the words
  mutate(word = SnowballC::wordStem(word)) %>%
  filter(!is.na(word))

# Words in the title 
title_words <- train %>%
  ungroup() %>%
  unnest_tokens(word, title, token = "words") %>%
  anti_join(stop_words)%>%
  mutate(word = SnowballC::wordStem(word)) %>%
  filter(!is.na(word))

# Words in the addtional tag
addtag_words <- train %>%
  ungroup() %>%
  unnest_tokens(word, addtag, token = "words") %>%
  anti_join(stop_words)%>%
  mutate(word = SnowballC::wordStem(word)) %>%
  filter(!is.na(word))


####### Length of summary #######

summ_length <- summ_words %>%
  group_by(id) %>%
  summarise(summ_length=n()) %>%
  mutate(summ_length2=(summ_length^2))
train <- train %>% left_join(summ_length,by="id")



####### Term frequency #######

## Summary ##

# Compute term frequency in each of the two hit rate categories
term_freq <- summ_words %>%
  count(word, high_hit, sort = T) %>%
  group_by(high_hit) %>%
  mutate(total = sum(n),
         term_freq = n/total,
         rank = row_number()) 

# Compute Tf-idf 
#tf_idf <- term_freq %>%
  #bind_tf_idf(word, high_hit, n)
# The Tf-idf does not make much sense here
# because most words appear in both high-hit fictions and low-hit fictions.

# Compute the difference in term frequencies between high hit and low hit for each word
highhit_words <- term_freq %>%
  ungroup() %>%
  filter(high_hit==1) %>%
  select(word, n_highhit = n, term_freq_highhit = term_freq)

lowhit_words <- term_freq %>%
  ungroup() %>%
  filter(high_hit==0) %>%
  select(word, n_lowhit = n, term_freq_lowhit = term_freq)

freq_diff_summ <- full_join(highhit_words, lowhit_words) %>%
  mutate(# Ratio of term frequencies (higher freq / lower freq)
         high_hit_term = term_freq_highhit > term_freq_lowhit,
         freq_rt = ifelse(high_hit_term, (term_freq_highhit / term_freq_lowhit), term_freq_lowhit / term_freq_highhit)) %>%
  # Keep only words with counts more than 30, and with the ratio greater than 2
  filter((n_highhit >= 30 | n_lowhit >= 30),
         freq_rt >= 2) %>%
  arrange(desc(freq_rt))

# It seems that summaries with main charaters (Sherlock and John) are more popular,
# those with minor characters (Sebastian Moran, Irene) are less popular.
# ABO topics (alpha and omega) are more popular.


## Title ##

# Compute term frequency in title
term_freq<- title_words %>%
  count(word, high_hit, sort = T) %>%
  group_by(high_hit) %>%
  mutate(total = sum(n),
         term_freq = n/total,
         rank = row_number()) 

# Compute the difference in term frequencies between high hit and low hit for each word
highhit_words <- term_freq %>%
  ungroup() %>%
  filter(high_hit==1) %>%
  select(word, n_highhit = n, term_freq_highhit = term_freq)

lowhit_words <- term_freq %>%
  ungroup() %>%
  filter(high_hit==0) %>%
  select(word, n_lowhit = n, term_freq_lowhit = term_freq)

freq_diff_title <- full_join(highhit_words, lowhit_words) %>%
  mutate(# Ratio of term frequencies (higher freq / lower freq)
    high_hit_term = term_freq_highhit > term_freq_lowhit,
    freq_rt = ifelse(high_hit_term, (term_freq_highhit / term_freq_lowhit), term_freq_lowhit / term_freq_highhit)) %>%
  # Keep only words with counts more than 30, and with the ratio greater than 2
  filter((n_highhit >= 30 | n_lowhit >= 30),
         freq_rt >= 2) %>%
  arrange(desc(freq_rt))

# There is no row in the tibble above.


## Additional tag ##

# Compute term frequency in additional tag
term_freq<- addtag_words %>%
  count(word, high_hit, sort = T) %>%
  group_by(high_hit) %>%
  mutate(total = sum(n),
         term_freq = n/total,
         rank = row_number()) 

# Compute the difference in term frequencies between high hit and low hit for each word
highhit_words <- term_freq %>%
  ungroup() %>%
  filter(high_hit==1) %>%
  select(word, n_highhit = n, term_freq_highhit = term_freq)

lowhit_words <- term_freq %>%
  ungroup() %>%
  filter(high_hit==0) %>%
  select(word, n_lowhit = n, term_freq_lowhit = term_freq)

freq_diff_tag <- full_join(highhit_words, lowhit_words) %>%
  mutate(# Ratio of term frequencies (higher freq / lower freq)
    high_hit_term = term_freq_highhit > term_freq_lowhit,
    freq_rt = ifelse(high_hit_term, (term_freq_highhit / term_freq_lowhit), term_freq_lowhit / term_freq_highhit)) %>%
  # Keep only words with counts more than 30, and with the ratio greater than 2
  filter((n_highhit >= 30 | n_lowhit >= 30),
         freq_rt >= 2) %>%
  arrange(desc(freq_rt))


### Plot ###

# Figure 1. Words with great frequency differences in summary 
png("figures/summ_wordcloud.png", width = 1000, height = 1000)
freq_diff_summ %>%
  mutate(n = ifelse(high_hit_term, n_highhit, n_lowhit),
         hit = ifelse(high_hit_term, "High Hit", "Low Hit")) %>%
  arrange(hit, n) %>%
  acast(word ~ hit, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"))
dev.off()

# Figure 2: plot of words with great frequency differences in additional tags
png("figures/addtag_wordcloud.png", width = 1000, height = 1000)
freq_diff_tag %>%
  mutate(n = ifelse(high_hit_term, n_highhit, n_lowhit),
         hit = ifelse(high_hit_term, "High Hit", "Low Hit")) %>%
  arrange(hit, n) %>%
  acast(word ~ hit, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"))
dev.off()


### Add binary features indicating whether these words are in the summary and addtional tags ###

## Summary ##
summ_id <- summ_words %>%
  group_by(id) %>%
  summarise(sum(word == freq_diff_summ$word[1])) %>%
  select(id)

summ_diff_words <- foreach(i = 1:nrow(freq_diff_summ), .combine = "cbind", .packages = "dplyr") %dopar% {
  
  word.temp <- summ_words %>%
    group_by(id) %>%
    summarise(sum(word == freq_diff_summ$word[i])) %>%
    select(-id)
  
  word.temp
}

words <- paste0("summ_", freq_diff_summ$word)
colnames(summ_diff_words) <- words

summ_diff_words <- summ_diff_words %>%
  mutate_all(.funs = function(x) ifelse(x>0, 1, 0)) %>%
  cbind(summ_id)
  
train <- left_join(train, summ_diff_words) %>%
  # If the summary is missing, then convert all of the word features to 0
  mutate_at(words, .funs = function(x) ifelse(is.na(x), 0, x))


## Additional tags ##
tag_id <- addtag_words %>%
  group_by(id) %>%
  summarise(sum(word == freq_diff_summ$word[1])) %>%
  select(id)

tag_diff_words <- foreach(i = 1:nrow(freq_diff_tag), .combine = "cbind", .packages = "dplyr") %dopar% {
  
  word.temp <- addtag_words %>%
    group_by(id) %>%
    summarise(sum(word == freq_diff_tag$word[i])) %>%
    select(-id)
  
  word.temp
}

words <- paste0("tag_", freq_diff_tag$word)
colnames(tag_diff_words) <- words

tag_diff_words <- tag_diff_words %>%
  mutate_all(.funs = function(x) ifelse(x>0, 1, 0)) %>%
  cbind(tag_id)

train <- left_join(train, tag_diff_words) %>%
  # If the summary is missing, then convert all of the word features to 0
  mutate_at(words, .funs = function(x) ifelse(is.na(x), 0, x))


# Save the freq_diff_summ and freq_diff_tag tibbles
save(freq_diff_summ, file = "data/freq_diff_summ.RDATA")
save(freq_diff_tag, file = "data/freq_diff_tag.RDATA")



####### Sentiment Analysis #######

# Get sentiment of each word
# We use bing because it has more words
summ_words <- summ_words %>%
  left_join(get_sentiments("bing"), by = "word") %>%
  #left_join(get_sentiments("afinn"), by = "word") %>%
  # set sentiment value to be -1 if negative, 
  # 1 if positive, 
  # 0 if the word is not in the bing lexicon
  mutate(sent = case_when(is.na(sentiment) ~ 0,
                          sentiment == "positive" ~ 1,
                          sentiment == "negative" ~ -1))

# Mean sentiment of each summary
summ_sent <- summ_words %>%
  group_by(id) %>%
  summarise(sent = mean(sent, na.rm = T))

train <- left_join(train, summ_sent) %>%
  # Convert NA to 0
  mutate(sent = ifelse(is.na(sent), 0, sent))
  

summary(train$sent[train$high_hit==1]) 
summary(train$sent[train$high_hit==0])
# not much difference

# Sentiment of each sentence
sentence_sent <- summ_words %>%
  group_by(id, sentence_id) %>%
  summarise(sentence_sent = mean(sent, na.rm = T)) %>%
  group_by(id) %>%
  summarise(sentence_sent_mean = mean(sentence_sent, na.rm = T),
            sentence_sent_sd = sd(sentence_sent, na.rm = T))

train <- train %>%
  left_join(sentence_sent, by = "id") %>%
  # Convert NA to 0
  mutate(sentence_sent_sd = ifelse(is.na(sentence_sent_sd), 0, sentence_sent_sd),
         sentence_sent_mean = ifelse(is.na(sentence_sent_mean), 0, sentence_sent_mean))

summary(train$sentence_sent_mean[train$high_hit==1]) 
summary(train$sentence_sent_mean[train$high_hit==0])

summary(train$sentence_sent_sd[train$high_hit==1]) 
summary(train$sentence_sent_sd[train$high_hit==0])



####### Topic Modeling #######
# Convert the data of summaries and additional tags into term matrix
summ_tm <- summ_words %>% 
  count(word, id, sort=T) %>%
  cast_dtm(id, word, n)

addtag_tm <- addtag_words %>% 
  count(word, id, sort=T) %>%
  cast_dtm(id, word, n)

# Create a matrix for different combinations of k1 and k2, 
# where k1 is the number of topics for the topic modeling on summaries,
# and k2 is the number of topics for the topic modeling on additional tags
ks <- data.frame(k1 = seq(5, 20, by = 5), k2 = seq(5, 20, by = 5))
ks <-  tidyr::expand(ks, k1, k2)

# Fit LDA models with different combination of K's
summ_lda <- list()
addtag_lda <- list()
for (i in 1:nrow(ks)) {
  summ_lda[[i]] <- LDA(summ_tm, k = ks$k1[i], control = list(seed = 1234))
  addtag_lda[[i]]<-LDA(addtag_tm, k = ks$k2[i], control = list(seed = 1234))
}

# Save the models
save(summ_lda, file = "data/summ_lda.RDATA")
save(addtag_lda, file = "data/addtag_lda.RDATA")


# We create 16 training sets with different feature sets obtained from the models with different k's
# The function used below is defined in Funtions.R
train_list <- list()
for (i in 1:nrow(ks)) {
  train_list[[i]] <- add_lda_feat(summ_mod = summ_lda[[i]], addtag_mod = addtag_lda[[i]], dat = train)
}

# Save the 16 dat sets of topics
save(train_list, file = "data/train_list.RDATA")














# Create features (with validation data)

# (trimmed words)

# Unnest the words by sentence, and remove stop words
summ_words <- valid %>%
  ungroup() %>%
  mutate(summ = tolower(summ)) %>%
  unnest_tokens(sentence, summ, token = "sentences") %>%
  group_by(id) %>%
  mutate(sentence_id = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, sentence, token = "words") %>%
  anti_join(stop_words) %>%
  mutate(word = SnowballC::wordStem(word)) %>%
  filter(!is.na(word))

# title 
title_words <- valid %>%
  ungroup() %>%
  unnest_tokens(word, title, token = "words") %>%
  anti_join(stop_words)%>%
  mutate(word = SnowballC::wordStem(word)) %>%
  filter(!is.na(word))

# addtional tag
addtag_words <- valid %>%
  ungroup() %>%
  unnest_tokens(word, addtag, token = "words") %>%
  anti_join(stop_words)%>%
  mutate(word = SnowballC::wordStem(word)) %>%
  filter(!is.na(word))

##### Length of summary #######
summ_length <- summ_words %>%
  group_by(id) %>%
  summarise(summ_length=n()) %>%
  mutate(summ_length2=(summ_length^2))
valid <- valid %>% left_join(summ_length,by="id")


####### Term frequency #######

# Add binary features indicating whether these words are in the summary and addtional tags
## Summary
summ_id <- summ_words %>%
  group_by(id) %>%
  summarise(sum(word == freq_diff_summ$word[1])) %>%
  select(id)

summ_diff_words <- foreach(i = 1:nrow(freq_diff_summ), .combine = "cbind", .packages = "dplyr") %dopar% {
  
  word.temp <- summ_words %>%
    group_by(id) %>%
    summarise(sum(word == freq_diff_summ$word[i])) %>%
    select(-id)
  
  word.temp
}

words <- paste0("summ_", freq_diff_summ$word)
colnames(summ_diff_words) <- words

summ_diff_words <- summ_diff_words %>%
  mutate_all(.funs = function(x) ifelse(x>0, 1, 0)) %>%
  cbind(summ_id)

valid <- left_join(valid, summ_diff_words) %>%
  # If the summary is missing, then convert all of the word features to 0
  mutate_at(words, .funs = function(x) ifelse(is.na(x), 0, x))


## Additional tags
tag_id <- addtag_words %>%
  group_by(id) %>%
  summarise(sum(word == freq_diff_summ$word[1])) %>%
  select(id)

tag_diff_words <- foreach(i = 1:nrow(freq_diff_tag), .combine = "cbind", .packages = "dplyr") %dopar% {
  
  word.temp <- addtag_words %>%
    group_by(id) %>%
    summarise(sum(word == freq_diff_tag$word[i])) %>%
    select(-id)
  
  word.temp
}

words <- paste0("tag_", freq_diff_tag$word)
colnames(tag_diff_words) <- words

tag_diff_words <- tag_diff_words %>%
  mutate_all(.funs = function(x) ifelse(x>0, 1, 0)) %>%
  cbind(tag_id)

valid <- left_join(valid, tag_diff_words) %>%
  # If the summary is missing, then convert all of the word features to 0
  mutate_at(words, .funs = function(x) ifelse(is.na(x), 0, x))




####### Topic Modeling #######
# Summary 
# Convert the data into term matrix
summ_tm <- summ_words %>% 
  count(word, id, sort=T) %>%
  cast_dtm(id, word, n)

# Create the features according to LDA
valid_topic <- as.data.frame(posterior(summ_lda, summ_tm)$topics) 
topic_name <- paste0("summ_t", colnames(valid_topic))
colnames(valid_topic) <- topic_name
valid_topic$id <- as.numeric(rownames(valid_topic))
valid <- left_join(valid, valid_topic) %>%
  mutate_at(topic_name, .funs = function(x) ifelse(is.na(x), 0, x))


# Additional tag

# Convert the data into term matrix
addtag_tm <- addtag_words %>% 
  count(word, id, sort=T) %>%
  cast_dtm(id, word, n)

# Create the features according to LDA
valid_topic <- as.data.frame(posterior(addtag_lda, addtag_tm)$topics)
topic_name <- paste0("addtag_t", colnames(valid_topic))
colnames(valid_topic) <- topic_name
valid_topic$id <- as.numeric(rownames(valid_topic))
valid <- left_join(valid, valid_topic) %>%
  mutate_at(topic_name, .funs = function(x) ifelse(is.na(x), 0, x))



####### Sentiment Analysis #######

# Get sentiment of each word
# We use bing because it has more words
summ_words <- summ_words %>%
  left_join(get_sentiments("bing"), by = "word") %>%
  #left_join(get_sentiments("afinn"), by = "word") %>%
  # set sentiment value to be -1 if negative, 
  # 1 if positive, 
  # 0 if the word is not in the bing lexicon
  mutate(sent = case_when(is.na(sentiment) ~ 0,
                          sentiment == "positive" ~ 1,
                          sentiment == "negative" ~ -1))

# Mean sentiment of each summary
summ_sent <- summ_words %>%
  group_by(id) %>%
  summarise(sent = mean(sent, na.rm = T))

valid <- left_join(valid, summ_sent) %>%
  # Convert NA to 0
  mutate(sent = ifelse(is.na(sent), 0, sent))


# Sentiment of each sentence
sentence_sent <- summ_words %>%
  group_by(id, sentence_id) %>%
  summarise(sentence_sent = mean(sent, na.rm = T)) %>%
  group_by(id) %>%
  summarise(sentence_sent_mean = mean(sentence_sent, na.rm = T),
            sentence_sent_sd = sd(sentence_sent, na.rm = T))

valid <- valid %>%
  left_join(sentence_sent, by = "id") %>%
  # Convert NA to 0
  mutate(sentence_sent_sd = ifelse(is.na(sentence_sent_sd), 0, sentence_sent_sd),
         sentence_sent_mean = ifelse(is.na(sentence_sent_mean), 0, sentence_sent_mean))






# Create features (with testing data)

# (trimmed words)

# Unnest the words by sentence, and remove stop words
summ_words <- test %>%
  ungroup() %>%
  mutate(summ = tolower(summ)) %>%
  unnest_tokens(sentence, summ, token = "sentences") %>%
  group_by(id) %>%
  mutate(sentence_id = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, sentence, token = "words") %>%
  anti_join(stop_words) %>%
  mutate(word = SnowballC::wordStem(word)) %>%
  filter(!is.na(word))

# title 
title_words <- test %>%
  ungroup() %>%
  unnest_tokens(word, title, token = "words") %>%
  anti_join(stop_words)%>%
  mutate(word = SnowballC::wordStem(word)) %>%
  filter(!is.na(word))

# addtional tag
addtag_words <- test %>%
  ungroup() %>%
  unnest_tokens(word, addtag, token = "words") %>%
  anti_join(stop_words)%>%
  mutate(word = SnowballC::wordStem(word)) %>%
  filter(!is.na(word))

##### Length of summary #######
summ_length <- summ_words %>%
  group_by(id) %>%
  summarise(summ_length=n()) %>%
  mutate(summ_length2=(summ_length^2))
test <- test %>% left_join(summ_length,by="id")


####### Term frequency #######

# Add binary features indicating whether these words are in the summary and addtional tags
## Summary
summ_id <- summ_words %>%
  group_by(id) %>%
  summarise(sum(word == freq_diff_summ$word[1])) %>%
  select(id)

summ_diff_words <- foreach(i = 1:nrow(freq_diff_summ), .combine = "cbind", .packages = "dplyr") %dopar% {
  
  word.temp <- summ_words %>%
    group_by(id) %>%
    summarise(sum(word == freq_diff_summ$word[i])) %>%
    select(-id)
  
  word.temp
}

words <- paste0("summ_", freq_diff_summ$word)
colnames(summ_diff_words) <- words

summ_diff_words <- summ_diff_words %>%
  mutate_all(.funs = function(x) ifelse(x>0, 1, 0)) %>%
  cbind(summ_id)

test <- left_join(test, summ_diff_words) %>%
  # If the summary is missing, then convert all of the word features to 0
  mutate_at(words, .funs = function(x) ifelse(is.na(x), 0, x))


## Additional tags
tag_id <- addtag_words %>%
  group_by(id) %>%
  summarise(sum(word == freq_diff_summ$word[1])) %>%
  select(id)

tag_diff_words <- foreach(i = 1:nrow(freq_diff_tag), .combine = "cbind", .packages = "dplyr") %dopar% {
  
  word.temp <- addtag_words %>%
    group_by(id) %>%
    summarise(sum(word == freq_diff_tag$word[i])) %>%
    select(-id)
  
  word.temp
}

words <- paste0("tag_", freq_diff_tag$word)
colnames(tag_diff_words) <- words

tag_diff_words <- tag_diff_words %>%
  mutate_all(.funs = function(x) ifelse(x>0, 1, 0)) %>%
  cbind(tag_id)

test <- left_join(test, tag_diff_words) %>%
  # If the summary is missing, then convert all of the word features to 0
  mutate_at(words, .funs = function(x) ifelse(is.na(x), 0, x))




####### Topic Modeling #######
# Summary 
# Convert the data into term matrix
summ_tm <- summ_words %>% 
  count(word, id, sort=T) %>%
  cast_dtm(id, word, n)

# Create the features according to LDA
test_topic <- as.data.frame(posterior(summ_lda, summ_tm)$topics) 
topic_name <- paste0("summ_t", colnames(test_topic))
colnames(test_topic) <- topic_name
test_topic$id <- as.numeric(rownames(test_topic))
test <- left_join(test, test_topic) %>%
  mutate_at(topic_name, .funs = function(x) ifelse(is.na(x), 0, x))


# Additional tag

# Convert the data into term matrix
addtag_tm <- addtag_words %>% 
  count(word, id, sort=T) %>%
  cast_dtm(id, word, n)

# Create the features according to LDA
test_topic <- as.data.frame(posterior(addtag_lda, addtag_tm)$topics)
topic_name <- paste0("addtag_t", colnames(test_topic))
colnames(test_topic) <- topic_name
test_topic$id <- as.numeric(rownames(test_topic))
test <- left_join(test, test_topic) %>%
  mutate_at(topic_name, .funs = function(x) ifelse(is.na(x), 0, x))



####### Sentiment Analysis #######

# Get sentiment of each word
# We use bing because it has more words
summ_words <- summ_words %>%
  left_join(get_sentiments("bing"), by = "word") %>%
  #left_join(get_sentiments("afinn"), by = "word") %>%
  # set sentiment value to be -1 if negative, 
  # 1 if positive, 
  # 0 if the word is not in the bing lexicon
  mutate(sent = case_when(is.na(sentiment) ~ 0,
                          sentiment == "positive" ~ 1,
                          sentiment == "negative" ~ -1))

# Mean sentiment of each summary
summ_sent <- summ_words %>%
  group_by(id) %>%
  summarise(sent = mean(sent, na.rm = T))

test <- left_join(test, summ_sent) %>%
  # Convert NA to 0
  mutate(sent = ifelse(is.na(sent), 0, sent))


# Sentiment of each sentence
sentence_sent <- summ_words %>%
  group_by(id, sentence_id) %>%
  summarise(sentence_sent = mean(sent, na.rm = T)) %>%
  group_by(id) %>%
  summarise(sentence_sent_mean = mean(sentence_sent, na.rm = T),
            sentence_sent_sd = sd(sentence_sent, na.rm = T))

test <- test %>%
  left_join(sentence_sent, by = "id") %>%
  # Convert NA to 0
  mutate(sentence_sent_sd = ifelse(is.na(sentence_sent_sd), 0, sentence_sent_sd),
         sentence_sent_mean = ifelse(is.na(sentence_sent_mean), 0, sentence_sent_mean))


# Save the training, validation, and testing set
save(train, file = "data/train.RDATA")
save(valid, file = "data/valid.RDATA")
save(test, file = "data/test.RDATA")
