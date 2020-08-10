#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(stringr)
library(markdown)
library(dplyr)
library(tm)
library(stylo)
library(wordcloud)

quadgramlist <- readRDS(file="data_final/quadgram.RData")
trigramlist <- readRDS(file="data_final/trigram.RData")
bigramlist <- readRDS(file="data_final/bigram.RData")
initialPrediction <- readRDS("data_final/start-word-prediction.RData")

cleanInput <- function(input) {
    if (input == "" | is.na(input)) {
        return("")
    }
    input <- tolower(input)
    
    input <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", input, ignore.case = FALSE, perl = TRUE)
    input <- gsub("\\S+[@]\\S+", "", input, ignore.case = FALSE, perl = TRUE)
    input <- gsub("@[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
    input <- gsub("#[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
    
    input <- gsub("[0-9](?:st|nd|rd|th)", "", input, ignore.case = FALSE, perl = TRUE)
    
    input <- gsub("[^\\p{L}'\\s]+", "", input, ignore.case = FALSE, perl = TRUE)

    input <- gsub("[.\\-!]", " ", input, ignore.case = FALSE, perl = TRUE)

    input <- gsub("^\\s+|\\s+$", "", input)
    input <- stripWhitespace(input)

    if (input == "" | is.na(input)) {
        return("")
    }
    
    input <- unlist(strsplit(input, " "))
    
    return(input)
    
}


nextWordPrediction <- function(wordCount,textInput){
    
    if (wordCount>=3) {
        textInput <- textInput[(wordCount-2):wordCount] 
    }else if(wordCount==2) {
        textInput <- c(NA,textInput)   
    }else {
        textInput <- c(NA,NA,textInput)
    }
    wordPrediction <- as.character(quadgramlist[quadgramlist$unigram==textInput[1] & 
                                                    quadgramlist$bigram==textInput[2] & 
                                                    quadgramlist$trigram==textInput[3],][1,]$quadgram)
    if(is.na(wordPrediction)) {
        wordPrediction1 <- as.character(trigramlist[trigramlist$unigram==textInput[2] & 
                                                        trigramlist$bigram==textInput[3],][1,]$trigram)
        
        if(is.na(wordPrediction)) {
            wordPrediction <- as.character(bigramlist[bigramlist$unigram==textInput[3],][1,]$bigram)
            
            if(is.na(wordPrediction)){
                wordPrediction <- as.character(initialPrediction[1])
            }
        }
    }
    print(wordPrediction)
    
    
}

shinyServer(function(input, output) {
    
    wordPrediction <- reactive({
        text <- input$userInput
        textInput <- cleanInput(text)
        wordCount <- length(textInput)
        wordPrediction <- nextWordPrediction(wordCount,textInput)})
    output$userSentence <- renderText({input$userInput})
    output$prediction <- renderPrint(wordPrediction())
})
