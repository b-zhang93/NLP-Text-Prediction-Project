library(tm)
library(readr)
library(dplyr)
library(stringi)


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

## list of profanity words to filter out. Link Below:
## gist.githubusercontent.com/ryanlewis/a37739d710ccdb4b406d/raw/3b70dd644cec678ddc43da88d30034add22897ef/google_twunter_lol)
profanity <- read.csv("./data/profanity.txt", header = F, sep="\n")

# Look at the data statistics 
len <- sapply(list(twitter,blogs,news), length) #lines for each file
words <- sapply(list(twitter,blogs,news), stri_stats_latex) #total words

# min and max characters per line (note max 140 for twitter was before the update to 280)
t_char <- range(sapply(twitter, nchar))
b_char <- range(sapply(blogs, nchar))
n_char <- range(sapply(news, nchar))
min_char <- c(t_char[1],b_char[1], n_char[1])
max_char <- c(t_char[2],b_char[2], n_char[2])
total_words <- c(words[4,1][[1]],words[4,2][[1]],words[4,3][[1]])

# data frame output:
data_stats <- data.frame(lines = len, total_words, min_char, max_char, row.names = c("twitter","blogs","news"))
print("Here is an overview of each data file: number of lines, min/max characters per line")
print(data_stats)
print("Let's sample the data to reduce the size and clean it up...")

# Let's sample the data and only use 1% of the total data for easier loading in the future
set.seed(12345)
tw_samp <- sample(twitter, len[1]*0.01)
bl_samp <- sample(blogs, len[2]*0.01)
new_samp <- sample(news, len[3]*0.01)
corpus <- c(tw_samp, bl_samp, new_samp)

# Building our corpus with tm (text mining) package
corpus <- VCorpus(VectorSource(corpus))

# cleaning the corpus with tm functions
delete <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) # gsub function to remove characters
corpus <- tm_map(corpus, delete, "http[[:alnum:]]*") # removes http URL
corpus <- tm_map(corpus, delete, "[@+?&/\\-_[]]") #removes extra junk
corpus <- tm_map(corpus, removePunctuation) #removes any remaining punctuation
corpus <- tm_map(corpus, removeNumbers) #removes any remaining numbers
corpus <- tm_map(corpus, content_transformer(tolower)) #lowercase all words
#corpus <- tm_map(corpus, removeWords, stopwords("english")) #remove stop words
corpus <- tm_map(corpus, removeWords, profanity[,1])  #remove profanity 
corpus <- tm_map(corpus, stripWhitespace) #remove white space
corpus <- tm_map(corpus, PlainTextDocument) #convert to plain text document

# save the sample clean corpus as our training set
saveRDS(corpus, file = "./data//corpus_train.rds" )

