# Building a simple model on the first attempt. This will be the skeleton of the final model.
# Purpose of this is to mainly test the logic works, before we optimize and train it.


library(tm)

# load the ngrams
bigram <- readRDS("./data/bigram.rds")
trigram <- readRDS("./data/trigram.rds")
fourgram <- readRDS("./data/fourgram.rds")

# quick test with a single input 
{
        input <- readline(prompt = "Enter a phrase: ")
}

cleaninput <- function(input){
        input <- tolower(input)
        input <- removePunctuation(input)
        input <- removeNumbers(input)
        input <- removeWords()
        input <- stripWhitespace(input)
        input <- as.list(unlist(strsplit(input, " ")))
        return(input)
}

input <- cleaninput(input)

# simple n gram model
predict <- function(input){
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
                        predict <- fourgram[fourgram$one == as.character(input[1]) &
                                                    fourgram$two == as.character(input[2]) &
                                                    fourgram$three == as.character(input[3]),][1,]$four
                        
                } else if(n == 2){
                        predict <- trigram[trigram$one == as.character(input[2]) &
                                                   trigram$two == as.character(input[3]),][1,]$three
                } else {
                        predict <- bigram[bigram$one == as.character(input[3]),][1,]$two
                }
                
                if(length(predict)==0 | is.na(predict)){
                        n <- n-1
                } else {
                        return(predict)
                        break
                }
        }
}

output <- paste("Predicted next word: ", predict(input))
print(output)