# Natural-Language-Processing-Project
This is the capstone project for the Data Science Specialization - creating a program that can predict and complete the next phrase / word.
This project involves building a predictive text app that uses predictive analytics and natural language processing to suggest the next word a user inputs. The data for this project was provided by SwiftKey, a leading text suggestion application for mobile phones. [Download Here](http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)

Capstone Deliverables:
- Develop a prediction algorithm that can take text and outputs a single word, or top words
- Create an web application with Shiny App in R
- Application must allow users to input phrases and obtain predictions for the next word

## Files
**Version 2** - Folder containing the most recent scripts. This is the updated code using the Quanteda package. These files are successors of all the scripts from Version 1

**Version 1** - Folder containing the original scripts using the TM package. This is deprecated as everything was rebuilt using Quanteda - a much faster and better package

**Data** - This contains the cleaned and processed data used in the models. Most of these are created with the getData.R scripts in the version folders

**ngrams-text-predict** - This folder contains all the files used to set up the Shiny App. This includes the UI, server, and CSS portion of the web app hosted through Shiny.



## Current Version
**Shiny App:** (http://bzhang93.shinyapps.io/Ngrams-Text-Predictor)

### The Model:
The model used was a simple N-gram back-off model. The ngrams were created using a 5% random sample of the HC Copora dataset from SwiftKey.

The data was cleaned and tokenized into 4-grams (unigrams, bigrams, trigrams, fourgrams). Each of these datasets were transformed into data frames with each column divided into single words and the frequency of the combination of those grams.

The input could then be taken and searched by each word in each of the ngrams. Using back-off, we would first limit the input to 3 words by taking the tail of the phrase, and then based on length we would search the n+1 gram data for any matches. If there were no matches, we would then search again by taking the tail minus 1 and searching the n+1 gram for the shortened input. Added a Stupid backoff model. This allows us to add a scoring method when doing the backoff. In the benchmark, this model performed better than the simple backoff model, but for most cases, the results will be similar. 


## Future Plans
1. Allow users to select different models before submitting their phrases for prediction. *Currently adding a Katz Back-off and Stupid Back-off models (COMPLETED. KATZ BACKOFF MODEL WILL NOT BE ADDED AS IT PERFORMS WORSE THAN THE FIRST TWO)
2. Give users the ability to benchmark the different models
3. Improve look and UI of the application for user experience
4. Suggest words reactively upon input 





