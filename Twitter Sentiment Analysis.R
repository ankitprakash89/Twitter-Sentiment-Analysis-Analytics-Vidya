library(stringr)
library(plyr)
library(caret)

# Importing the text file containing positive and neg. words

negativewords <- read.delim("C:/Users/Administrator/Downloads/Twitter Sentiment Analysis/negative-words.txt")
neg_words <- negativewords[33:4815,]

# Importing train and test cases
positivewords <- read.delim("C:/Users/Administrator/Downloads/Twitter Sentiment Analysis/positive-words.txt")
pos_words <- positivewords[33:2039,]


twitter_train <- read.csv("C:/Users/Administrator/Downloads/Twitter Sentiment Analysis/twitter_train.csv")
tweet <- twitter_train$tweet

# Lets Create a sentiment score function

score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
     # Remove Graphic characters like emoticons
    tweet = gsub('[^[:graph:]]', ' ',tweet) 
    
    # remove punctuation
    tweet = gsub('[[:punct:]]', '', tweet) 
    
    # remove control characters
    tweet = gsub('[[:cntrl:]]', '', tweet) 
    
    # remove numbers
    tweet = gsub('\\d+', '', tweet) 
    
    # Case to lower
    tweet = tolower(tweet) 
    
    # spliting the tweets by words in a list
    word.list = str_split(tweet, '\\s+') 
    
    # turns the list to vector
    words = unlist(word.list) 
    
    # returns matching values for words from list
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA. we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches) 
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches) 
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}

# Getting analysis report as a dataframe from the function
analysis <- score.sentiment(tweet, pos_words, neg_words)

# Getting predicted value from the function to the dataframe
twitter_train$pred = ifelse(as.integer(analysis$score)<0,1,0)

# To calculate True Positive
true_positive <- 0
for (i in 1:nrow(twitter_train)){
  if(twitter_train$label[i]==0 & twitter_train$pred[i]==0){
    true_positive = true_positive+1
  }
}

# To calculate False Negative
false_positive <- 0
for (i in 1:nrow(twitter_train)){
  if(twitter_train$label[i]==1 & twitter_train$pred[i]==0){
    false_positive = false_positive+1
  }
}

# To calculate True Negative
true_negative<- 0
for (i in 1:nrow(twitter_train)){
  if(twitter_train$label[i]==1 & twitter_train$pred[i]==1){
    true_negative = true_negative+1
  }
}

# To calculate False Negative
false_negative <- 0
for (i in 1:nrow(twitter_train)){
  if(twitter_train$label[i]==0 & twitter_train$pred[i]==1){
    false_negative = false_negative+1
  }
}

# Calculating Precision
Precision = true_positive/(true_positive+false_positive) *100
Precision

# Calculating Recall
Recall = true_positive/(true_positive+ false_negative) *100
Recall

cm = confusionMatrix(twitter_train$pred, twitter_train$label, positive='1')
cm

##Calculating accuracy of the model
accuracy_rf = cm$overall[["Accuracy"]]*100
accuracy_rf

# To get the labels for test case
twitter_test <- read.csv("C:/Users/Administrator/Downloads/Twitter Sentiment Analysis/twitter_test.csv")
tweet <- twitter_test$tweet
analysis <- score.sentiment(tweet, pos_words, neg_words)
twitter_test$label = ifelse(as.integer(analysis$score)>0,1,0)
df <- select(twitter_test,c(id,label))

write.csv(df,"C:/Users/Administrator/Downloads/Twitter Sentiment Analysis/solution.csv", row.names = FALSE )
