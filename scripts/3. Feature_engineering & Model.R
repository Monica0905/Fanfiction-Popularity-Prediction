# Source the self-defined functions
source("scripts/Functions.R")


# Load the training, validation and testing sets
load("data/train_list.RDATA")
load("data/valid.RDATA")
load("data/test.RDATA")

# Load the tibbles of differences in term frequencies
load("data/freq_diff_summ.RDATA")
load("data/freq_diff_tag.RDATA")

# Load the LDA models
load("data/summ_lda.RDATA")
load("data/addtag_lda.RDATA")



#################################################################
        ### Add the features to validation set, ### 
        ### except the ones from topic modeling  ###
#################################################################

# Unnest the words by sentence, and remove stop words
# Summary
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

# Addtional tag
addtag_words <- valid %>%
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





#################################################################
    ### Add topic modeling features to validation set ### 
    ###       Feature Engineering and Model           ###
#################################################################

# Convert the summary and additional tag data into term matrix
summ_tm <- summ_words %>% 
  count(word, id, sort=T) %>%
  cast_dtm(id, word, n)

addtag_tm <- addtag_words %>% 
  count(word, id, sort=T) %>%
  cast_dtm(id, word, n)


# Try every combination of Ks (the number of topics)
# and compute the AUC on the validation set

auc <- NA

for (i in 1:length(train_list)) {
  # Traing set with the topic features
  train_topic <- train_list[[i]]
  
  # Add the topic features to the training set
  valid_topic <- add_lda_feat(summ_mod = summ_lda[[i]], addtag_mod = addtag_lda[[i]], dat = valid)
  
  # Remove Useless variables
  unused_var <- c("id", "title", "rating", "warning", "categ", "fandom", "relationship", "charac", "addtag", "pubdate", "update", "hit", "summ", "scrape_date", "daily_hit", "pub_year", "up_year", "med_hit")
  
  train_topic <- train_topic %>% select(-unused_var) %>% filter(complete.cases(train_topic))
  valid_topic <- valid_topic %>% select(-unused_var) %>% filter(complete.cases(valid_topic))
  
  # Fit the logistic model with LASSO on the training set
  train_y <- as.matrix(as.factor(train_topic$high_hit))
  train_x <- model.matrix(high_hit ~ ., data = train_topic)
  
  logit_lasso <- glmnet(x = train_x, y = train_y, lambda = 0.01, alpha = 1, family = "binomial")
  
  # Compute AUC on the validation set
  valid_y <- as.matrix(as.factor(valid_topic$high_hit))
  valid_x <- model.matrix(high_hit ~ ., data = valid_topic)
  
  pred_lasso <- predict(logit_lasso, valid_x, type = 'response')
  pred_lasso2 <- prediction(pred_lasso, valid_y)
  perf_lasso <- performance(pred_lasso2,'auc')
  auc[i] <- perf_lasso@y.values[[1]]
  
}

# The maximum AUC on validation set
auc 
which.max(auc) # the 4th AUC (0.8083312) is the largest
# The 4th feature set has the greatest AUC, where k1=5, k2=20


# Fit the model with the best combination of Ks
i <- which.max(auc) 
train_topic <- train_list[[i]]
train_topic <- train_topic %>% select(-unused_var) %>% filter(complete.cases(train_topic))
train_y <- as.matrix(as.factor(train_topic$high_hit))
train_x <- model.matrix(high_hit ~ ., data = train_topic)
logit_lasso <- glmnet(x = train_x, y = train_y, lambda = 0.01, alpha = 1, family = "binomial")


### Plot ###
# Figure 3: Coefficients based on LASSO

coefs <- as.matrix(dimnames(logit_lasso$beta)[[1]])
lasso_coef <- as.matrix(logit_lasso$beta)
colnames(lasso_coef) <- "beta"
coef_plot <- as.data.frame(lasso_coef)%>%mutate(predictor=str_remove(rownames(lasso_coef),"TRUE"))%>%filter(beta!=0)

p1 <- ggplot(coef_plot, aes(x=predictor, y=beta)) +
  geom_bar(stat = 'identity', position='dodge') +
  labs(title = "Regularized Coefficients") + 
  theme(axis.text.x = element_text(angle = 90, hjust =1 ), legend.position = c(.9,.9))
p1

ggsave(plot=p1, file='figures/regularized_coefficients.png', height = 12, width = 16)





#################################################################
          ### Add the features to testing set ### 
#################################################################

# Unnest the words by sentence, and remove stop words

## Summary
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

## Addtional tag
addtag_words <- test %>%
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



####### Topic Modeling #######

# Convert the summary and additional tag data into term matrix
summ_tm <- summ_words %>% 
  count(word, id, sort=T) %>%
  cast_dtm(id, word, n)

addtag_tm <- addtag_words %>% 
  count(word, id, sort=T) %>%
  cast_dtm(id, word, n)

# Create the features according to LDA
i <- 4
test_topic <- add_lda_feat(summ_mod = summ_lda[[i]], addtag_mod = addtag_lda[[i]], dat = test) %>% 
  select(-unused_var) 
   
test_topic<- test_topic%>%filter(complete.cases(test_topic))





#################################################################
                  ### Model Performance ### 
#################################################################

# Compute AUC on the testing set
test_y <- as.matrix(as.factor(test_topic$high_hit))
test_x <- model.matrix(high_hit ~ ., data = test_topic)

pred_lasso <- predict(logit_lasso, test_x, type = 'response')
pred_lasso2 <- prediction(pred_lasso, test_y)
perf_lasso <- performance(pred_lasso2,'auc')
(auc_lasso <- perf_lasso@y.values[[1]]) # 0.8045021


# Compute accuracy on the testing set (threshold = 0.5)
pre_hit <- ifelse(pred_lasso>0.5,1,0)
(auccracy<-mean(pre_hit==test_topic$high_hit))

# Compute precision on the testing set (threshold = 0.5)
(precision<-sum(pre_hit==1 & test_topic$high_hit==1)/sum(pre_hit==1))

# Compute recall on the testing set (threshold = 0.5)
(recall<-sum(pre_hit==1 & test_topic$high_hit==1)/sum(test_topic$high_hit==1))
