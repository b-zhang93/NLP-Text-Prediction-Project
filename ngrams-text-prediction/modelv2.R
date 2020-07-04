library(quanteda)
library(readr)
library(dplyr)
library(tidyr)

# load the ngrams
if(!exists("unigrams") | !exists("bigrams") | !exists("trigrams") | !exists("fourgrams")){
        unigrams <- readRDS("./data/unigramsv2.rds")
        bigrams <- readRDS("./data/bigramsv2.rds")
        trigrams <- readRDS("./data/trigramsv2.rds")
        fourgrams <- readRDS("./data/fourgramsv2.rds")
}


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
        return(clean)
}



# simple n gram model
predictbo <- function(input){
        input <- cleaninput(input)
        n <- length(input)
        predict <- vector()
        
        if(n >= 4){
                input <- tail(input,3)
                n <- length(input)
                
        }
        
        while(n<=3){
                if(n == 0){
                        return("No predictions found. Try again.")
                }
                
                if(n == 3){
                        pred <- fourgrams[fourgrams$word_1 == input[1] &
                                                    fourgrams$word_2 == input[2] &
                                                    fourgrams$word_3 == input[3],][1:3,]$word_4
                        
                } else if(n == 2){
                        pred <- trigrams[trigrams$word_1 == input[1] &
                                                   trigrams$word_2 == input[2],][1:3,]$word_3
                } else {
                        pred <- bigrams[bigrams$word_1 == input,][1:3,]$word_2
                }
                if(length(pred)==0| is.na(pred)){
                        n <- n-1
                        input <- tail(input, n)
                } else {
                        pred <- as.vector(pred)
                        suppressWarnings(
                                return(pred)   
                        )
                        
                        break
                }
        }
}
