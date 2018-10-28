# ref: https://gist.github.com/AdamSpannbauer/135793c68b90b46f44dbe50364c0edf5
# wordcloud2_clickeven_with_reset -----------

#function to return a DF with words and freqs for building wordcloud2
getWordFreq <- function(text_vec = NULL, ngram=2, words_2_exclude = NULL, num_words = 50, is_twitter = FALSE){
  library(quanteda)
  library(dplyr)
  dfm = getDfm(text_vec, ngram, words_2_exclude)
  w = textstat_frequency(dfm)
  num_toks = ifelse(num_words < nrow(w), num_words, nrow(w))
  data.frame(word = w$feature[1:num_toks],
             freq = w$frequency[1:num_toks])
}

# function to get quanteda dfm
getDfm <- function (text_vec = NULL, ngram =2, words_2_exclude = NULL){
  library(quanteda)
  library(dplyr)
  if (is.null(text_vec) | length(text_vec ) == 0) { stop ("In getDfm, input text vector is empty")}
  
  corpus(text_vec) %>%
    tokens(remove_numbers = TRUE, remove_punct = TRUE,
           remove_symbols = TRUE, remove_separators = TRUE,
           remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE) %>%
    tokens_remove(c(stopwords::stopwords(source = "smart"), words_2_exclude)) %>%
    dfm(ngrams = ngram)
}

library(shiny)
library(wordcloud2)
shinyApp(
  ui=shinyUI(fluidPage(
    
    # script to change input$selected_word based on user click
    tagList(
      tags$script(HTML(
        "$(document).on('click', '#canvas', function() {",
        'word = document.getElementById("wcSpan").innerHTML;', 
        "Shiny.onInputChange('selected_word', word);",
        "});"
      ))
    ),
    wordcloud2Output("my_wc"),
    actionLink('remove_selected_word', 'Clear Selected Word'),
    h3(textOutput("s_word"))
    
  )),
  
  server=shinyServer(function(input,output,session){
    reactive_values <- reactiveValues(s_word = NULL)
    
    demoFreq = getWordFreq(data_char_ukimmig2010)
    output$my_wc = renderWordcloud2({
      set.seed(123)
      wordcloud2(demoFreq, 
                 size =0.5, 
                 ellipticity = 1, 
                 minRotation = -pi/8, 
                 maxRotation = pi/8,
                 shape='circle')
    })
    
    observeEvent(input$selected_word, {
      str = paste0("selected word: ", unlist(strsplit(input$selected_word, ":"))[1])
      # cat(str)
      reactive_values$s_word = input$selected_word
      output$print  = renderPrint(str)
    })
    
    output$s_word <- renderText({
      if(is.null(reactive_values$s_word )){return("No selection")}
      paste0("You selected: ", unlist(strsplit(reactive_values$s_word, ":"))[1])
    })
    
    observeEvent(input$remove_selected_word, {
      reactive_values$s_word = NULL
    })
  })
)