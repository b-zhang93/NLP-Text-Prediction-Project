library(tm)
library(RWeka)
library(ggplot2)
library(dplyr)
library(tidyr)
options(mc.cores=1)

print("Generating the ngrams...(this may take a while)")

corpus <- readRDS("./data/corpus_train.rds")
corpus <-data.frame(text=unlist(sapply(corpus,`[`, "content")), 
                           stringsAsFactors = FALSE)

# Function to convert corpus to document matrix and then outputs the ngram frequency table
ngramGenerate <- function(x, n=1){
        output <- NGramTokenizer(x, Weka_control(min = n, max = n))
        output <- table(output)
        output <- data.frame(output)
        output <- arrange(output, desc(Freq))
        names(output) <- c("words", "frequency")
        return(output)
}

# create our ngrams up to n=4
bigram <- ngramGenerate(corpus, 2) 
trigram <- ngramGenerate(corpus,3)
fourgram <- ngramGenerate(corpus,4)

# split the words into dataframes
bigram <- separate(bigram, words, c("one","two"), sep=" ")
trigram <- separate(trigram, words, c("one","two", "three"), sep=" ")
fourgram <- separate(fourgram, words, c("one","two", "three", "four"), sep=" ")

# save our ngrams as data files
saveRDS(unigram, file="./data/unigram.rds")
saveRDS(bigram, file="./data/bigram.rds")
saveRDS(trigram, file="./data/trigram.rds")
saveRDS(fourgram, file="./data/fourgram.rds")


