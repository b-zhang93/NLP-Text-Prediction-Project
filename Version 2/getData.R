# version 2 - using faster and more optimized methods with quanteda to create better ngrams

library(quanteda)
library(readr)
library(dplyr)
library(tidyr)

# load the datasets 
con <- file("./data/en_US.twitter.txt", open="rb")
twitter <- read_lines(con, skip_empty_rows = T)
close(con)

con <- file("./data/en_US.blogs.txt", "rb")
blogs <- read_lines(con, skip_empty_rows = T)
close(con)

con <- file("./data/en_US.news.txt", open="rb")
news <- read_lines(con, skip_empty_rows = T)
close(con)

## NOTE: View data statistics code in the V1 folder. 

## list of profanity words to filter out. Link Below:
## gist.githubusercontent.com/ryanlewis/a37739d710ccdb4b406d/raw/3b70dd644cec678ddc43da88d30034add22897ef/google_twunter_lol)
profanity <- read.csv("./data/profanity.txt", header = F, sep="\n")

# let's create a single corpus and use quanteda to create our subsamples 
set.seed(12345)
tot_data <- c(twitter,blogs,news)
corpus <- corpus(tot_data)
corpus_005 <- corpus_sample(corpus, size=(length(corpus)*0.05)) # a random sample of 5% of our total combined data

#removing uncessary variables
rm(twitter,blogs,news,tot_data)

#tokenize our corpus
tokens <- tokens(
                corpus_005,
                what = "word", #tokenize by sentences
                remove_punct = T,
                remove_symbols = T,
                remove_numbers = T,
                remove_url = T,
                )

tokens <- tokens_tolower(tokens)

# I found that removing all stop words actually reduces accuracy of the models especially
# the last word is a stop word. Thus, I decided to remove a few of the top stop words manually
tokens <- tokens_remove(tokens, c(profanity, "the","a"))

ngramGen <- function(x,n=1, min=2){
        output <- dfm(tokens_ngrams(x,n))
        output <- dfm_trim(output, min_termfreq = min)
        output <- as.data.frame(textstat_frequency(output)[,1:2])
        output <- separate(output, feature, paste("word", c(1:n),sep="_"), sep="_")
}

unigrams <- ngramGen(tokens,1)
bigrams <- ngramGen(tokens,2)
trigrams <- ngramGen(tokens,3)
fourgrams <- ngramGen(tokens,4)

saveRDS(unigrams, file="./data/unigramsv2sw.rds")
saveRDS(bigrams, file="./data/bigramsv2sw.rds")
saveRDS(trigrams, file="./data/trigramsv2sw.rds")
saveRDS(fourgrams, file="./data/fourgramsv2sw.rds")

rm(tokens) #deleting unnecessary variables to free memory


