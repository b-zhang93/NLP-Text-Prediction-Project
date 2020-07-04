library(quanteda)
library(dplyr)

# load the ngrams
if(!exists("bigrams") | !exists("trigrams") | !exists("fourgrams")){
        unigrams <- readRDS("./data/unigramsv2.rds")
        bigrams <- readRDS("./data/bigramsv2.rds")
        trigrams <- readRDS("./data/trigramsv2.rds")
}


### Set up test conditions here:
d <- 0.5
{
input <- readline(prompt="Enter Phrase: ")
}
choices <- c()


#### Functions #####

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
        
        n <- length(clean)
        if(n > 2){
                clean <- tail(clean,2)
        }
        
        return(clean)
}

# observed matches probability with discount
obs_trig <- function(input, tri=trigrams){
        output <- tri[tri$word_1==input[1] & tri$word_2==input[2],]
        return(output)
}

# observed probability
obs_trig_prob <- function(input=input, m = obs_trig, bi=bigrams, d = discount){
        if(nrow(m) == 0) return(NULL)
        count <- bi[bi$word_1==input[1] & bi$word_2==input[2],3]
        prob <- (m$frequency-d)/count
        output <- cbind(m[,1:3], prob)
        return(output)
}

# unobserved trigram completion possibilities
unobs_trig <- function(m=obs_trig, uni=unigrams){
        last <- list(m$word_3) #make it last word somehow###########
        last <- unlist(last)
        output <- uni[!(uni$word_1 %in% last),]$word_1
        return(output)
}

# calculate the probability mass to be distributed to unobserved bigrams
abigram <- function(input=input, bi=bigrams, uni=unigrams, d=0.5){
        last <- uni[uni$word_1==input[2],2]
        count <- bi[bi$word_1==input[1] & bi$word_2==input[2],3]
        if (count<1) return(1)
        output <- 1- sum((count-d)/last)
        return(output)
}

#bigram back-off probabilities
BO_bigrams <- function(input=input, unobserved=unobs_trig){
        last <- tail(input,1)
        output <- data.frame(word_1=last, word_2=unobserved, row.names = NULL)
        return(output)
}

BO_bigrams_obs <- function(input, BObig=BO_bigrams, bi=bigrams){
        output <- bigrams[bi$word_1 %in% BObig$word_1 & bigrams$word_2 %in% BObig$word_2,]
        return(output)
}

BO_bigrams_unobs <- function(input, BObig=BO_bigrams, obs=BO_bigrams_obs){
        output <- BObig[!(BObig %in% obs$word_1 & BObig %in% obs$word_2),]
        return(output)
}

obsBObig_probs <- function(input, obs=Bo_bigrams_obs, uni=unigrams, d=discount){
        last <- tail(input,1)
        last <- uni[uni$word_1 == last,]
        output <- (obs$frequency-d)/last$frequency
        output <- data.frame(x=obs[1:2], prob=output)
        return(output)
}

unobsBObig_probs <- function(unobs=BO_bigrams_unobs, uni=unigrams, a=abigram){
        #get the unobserved bigram tails
        last <- unlist(list(unobs$word_2))
        last <- uni[uni$word_1 %in% last,]
        denom <- sum(last$frequency)
        output <- data.frame(word_1=unobs$word_1,word_2=unobs$word_2, 
                             prob=(a*last$frequency/denom))
        return(output)
}

atrigram <- function(input=input, tri=trigrams, bi=bigrams, d=0.5){
        count <- tri[tri$word_1==input[1] & tri$word_2==input[2],4]
        bicount <- bi[bi$word_1==input[1] & bi$word_2==input[2],3]
        if (count<1) return(1)
        output <- 1- sum((count-d)/bicount)
        return(output)
}

# unobserved trigram probability
unobsTri_probs <- function(input=input, kboBigrams=kboBigrams, a=atrigrams){
        total <- sum(kboBigrams$prob)
        first <- head(input,1)
        prob <- a*kboBigrams$prob/total
        output <- data.frame(word_1=first, word_2=kboBigrams[,1], word_3=kboBigrams[,2], prob=prob)
        return(output)
}


# next word function
nextWord <- function(otp=obs_trig_prob, utp=unobsTri_probs, choices){
        kboTrigrams <- rbind(otp, utp)
        
        if(!is.null(choices)){
                kboTrigrams <- kboTrigrams[kboTrigrams$word_3 %in% choices,]
                kboTrigrams <- distinct(kboTrigrams[order(-kboTrigrams$prob),]) #removes duplicates from the rbind
                return(kboTrigrams)
        } else {
                kboTrigrams <- distinct(kboTrigrams[order(-kboTrigrams$prob),])
                output_five <- head(kboTrigrams, 3)
                output_five <- as.vector(output_five$word_3)
                return(output_five)
        }
        
}

# summary function to predict
predictkbo <- function(d=d, input=input, choices=choices, uni=unigrams, bi=bigrams, tri=trigrams){
        input <- cleaninput(input)
        
        if (length(n==1)){
                output <- predictbo(input)
                return(output)
                break
        } else {
                obs_trig <- obs_trig(input, tri)
                obs_trig_prob <- obs_trig_prob(input, obs_trig, bi, d)
                unobs_trig <- unobs_trig(obs_trig, uni)
                abigram <- abigram(input, bi,uni, d)
                BO_bigrams <- BO_bigrams(input, unobs_trig)
                BO_bigrams_obs <- BO_bigrams_obs(input, BO_bigrams)
                BO_bigrams_unobs <- BO_bigrams_unobs(input, BO_bigrams, BO_bigrams_obs)
                
                obsBObig_probs <- obsBObig_probs(input, BO_bigrams_obs, uni, d)
                unobsBObig_prob <- unobsBObig_probs(BO_bigrams_unobs, uni, abigram)
                
                kboBigrams <- rbind(unobsBObig_prob, unobsBObig_prob)
                atrigram <- atrigram(input, tri, bi, d)
                unobs_tri_probs <- unobsTri_probs(input, kboBigrams, atrigram)
                
                output <- nextWord(obs_trig_prob, unobs_tri_probs, choices)
                suppressWarnings(
                        return(output)
                )
        }
        
}

#### benchmark time and output
start_time <- Sys.time()
word <- predict(d,input,choices, unigrams,bigrams,trigrams)
end_time <- Sys.time()
diff_time <- end_time - start_time

show(word)
print(diff_time)
