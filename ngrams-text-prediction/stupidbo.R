############################################################## 
# Stupid back-off model with 4-grams text predictor (code)   # 
##############################################################
# Returns a dataframe with top words, frequency, and score   #
# Example: input = "how are you?"                            #
# predictSBO(input) returns:                                 #
#                                                            #
# word frequency  score                                      #
# 1       doing        16 0.0889                             #
# 2     feeling        10 0.0556                             #
# 3 celebrating         8 0.0444                             #
# 4       today         8 0.0444                             #
# 5       going         6 0.0333                             #
##############################################################

# load required libraries
library(quanteda)
library(readr)
library(dplyr)
library(tidyr)


#### LOAD NGRAMS
if(!exists("unigrams")|!exists("bigrams")|!exists("trigrams")|!exists("fourgrams")){
        unigrams <- readRDS("./data/unigramsv2.rds")
        bigrams <- readRDS("./data/bigramsv2.rds")
        trigrams <- readRDS("./data/trigramsv2.rds")
        fourgrams <- readRDS("./data/fourgramsv2.rds")
}



#### FUNCTIONS ####

# tokenize and clean the input - output as vector of words
cleaninput <- function(x){
        clean <- tokens(
                x,
                what = "word",
                remove_punct = T,
                remove_symbols = T,
                remove_numbers = T,
                remove_url = T,
        )
        clean <- tokens_tolower(clean)
        clean <- unlist(clean, use.names = F)
        if(length(clean) > 3){
                clean <- tail(clean,3)
                
        }
        return(clean)
}


# find the count of the top prediction output
predCount <- function(input=input, four=fourgrams, tri=trigrams, bi=bigrams){
        n <- length(input)
        if(n == 3){
                output <- four[four$word_1==input[1] & four$word_2==input[2] & four$word_3==input[3],4:5]
        } else if(n==2){
                output <- tri[tri$word_1==input[1] & tri$word_2==input[2],3:4]
        } else if (n==1){
                output <- bi[bi$word_1==input,2:3]
        } 
        
        if (nrow(output) == 0){
                output = NULL
        } else {
                names(output) <- c("word", "frequency")
        }
        return(output)
}


# find the count of times the ngram shows up
obsCount <- function(input=input, four=fourgrams, tri=trigrams, bi=bigrams, uni=unigrams){
        n <- length(input)
        if(n == 3){
                output <- tri[tri$word_1==input[1] & tri$word_2==input[2] & tri$word_3==input[3],4]
        } else if(n==2){
                output <- bi[bi$word_1==input[1] & bi$word_2==input[2],3]
        } else if(n==1){
                output <- uni[uni$word_1==input,2]
        } 
        
        if (length(output) == 0 | is.null(output)){
                output = NULL
        }
        return(output)
}

# return top 10 predictions for the current ngram
# x = 3 - (length of the input)  ... length is the current ngram's (n - 1)
topten <- function(input, lambda, x=0){
        output <- head(predCount(input),10) 
        obs_count <- obsCount(input) 
        
        if(!is.null(output) & !is.null(obs_count)){
                output <- mutate(output, score = round((lambda ^ x)*output[,2] / obs_count,4))
        }
        return(output)
}



#### MODEL ####

#stupid back-off model with 4-grams
predictSBO <- function(input, lambda = 0.4){
        input <- cleaninput(input)
        k <- length(input)
        
        # declaring our empty vars to store predicted words for each ngram later
        poss_outcomes <- NULL
        poss_outcomes2 <- NULL
        poss_outcomes3 <- NULL
        poss_outcomes4 <- NULL
        
        # take the top 10 for each group
        while(k >= 0){
                if(k==3){
                        # for 4gram - return top 10 predictions
                        poss_outcomes <- topten(input, lambda)
                        
                } else if(k==2){
                        # for 3gram - return top 10 predictions
                        poss_outcomes2 <- topten(input, lambda, x=1) #x=3-2=1
                        
                } else if(k==1){
                        # for bigram - return top 10 predictions
                        poss_outcomes3 <- topten(input, lambda, x=2) #x=3-1=1
                        
                } else {
                        if(sum(nrow(poss_outcomes), nrow(poss_outcomes2), nrow(poss_outcomes3)) < 5){
                                # for unigrams - returns top 5 by frequency with score of 0 for default
                                poss_outcomes4 <- head(arrange(unigrams, desc(frequency)),5)
                                poss_outcomes4 <- mutate(poss_outcomes4, score = 0) #assigning unigrams / 0 score for default
                                names(poss_outcomes4) <- c("word", "frequency","score")
                        }
                }
                
                # ngram-1 and re-calculate scores for model iterations to obtain lower degree ngram scores
                k <- k-1
                input <- tail(input, k)
                
                # return a combined list with the top 5 scored words
                if (k < 0){
                        predwords <- rbind(poss_outcomes, poss_outcomes2, poss_outcomes3, poss_outcomes4)
                        predwords <- arrange(predwords, desc(score))
                        #return(head(predwords,5))   # used for baseline calculations
                        return(predwords$word[1:5])
                        break
                }
                
        }

}

# model for benchmarking purposes, only returns vector of top 3 predicted words
predictSBO_baseline <- function(input, lambda = 0.4){
        input <- cleaninput(input)
        k <- length(input)
        
        # declaring our empty vars to store predicted words for each ngram later
        poss_outcomes <- NULL
        poss_outcomes2 <- NULL
        poss_outcomes3 <- NULL
        poss_outcomes4 <- NULL
        
        # take the top 10 for each group
        while(k >= 0){
                if(k==3){
                        # for 4gram - return top 10 predictions
                        poss_outcomes <- topten(input, lambda)
                        
                } else if(k==2){
                        # for 3gram - return top 10 predictions
                        poss_outcomes2 <- topten(input, lambda, x=1) #x=3-2=1
                        
                } else if(k==1){
                        # for bigram - return top 10 predictions
                        poss_outcomes3 <- topten(input, lambda, x=2) #x=3-1=1
                        
                } else {
                        if(sum(nrow(poss_outcomes), nrow(poss_outcomes2), nrow(poss_outcomes3)) < 5){
                                # for unigrams - returns top 5 by frequency with score of 0 for default
                                poss_outcomes4 <- head(arrange(unigrams, desc(frequency)),5)
                                poss_outcomes4 <- mutate(poss_outcomes4, score = 0) #assigning unigrams / 0 score for default
                                names(poss_outcomes4) <- c("word", "frequency","score")
                        }
                }
                
                # ngram-1 and re-calculate scores for model iterations to obtain lower degree ngram scores
                k <- k-1
                input <- tail(input, k)
                
                # return a combined list with the top 5 scored words
                if (k < 0){
                        predwords <- rbind(poss_outcomes, poss_outcomes2, poss_outcomes3, poss_outcomes4)
                        predwords <- head(arrange(predwords, desc(score)),3)
                        predwords <- as.vector(predwords$word)
                        return(predwords)
                        break
                }
        }
}
