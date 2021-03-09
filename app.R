
# ==================== Twitter Talks, Shiny Sparks! ================================= #
# Contact - Murali                                                                    #

# ============ Install and load the packages =======================
library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
#library(twitteR)
#library(rtweet)
library(NLP)
library(tm) # text mining
library(stringr)
library(SnowballC) # text stemming
library(RColorBrewer) # Color Palettes
#library(wordcloud)
library(wordcloud2)
#library(topicmodels)
library(tidytext)
library(slam)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)

library(text2vec)

library(LDAvis)

library(tokenizers)

library(readxl)

# setting working directory
# setting working directory
setwd('C:\\Users\\U058507\\UCB\\My Objectives 2021\\Bone China\\R Shiny\\Test')

Sys.setlocale(,"CHS")  


# docs is a csv file with a "text" column, e.g.   
#docs <- read.csv("./docs.csv",sep=",",header=TRUE)
docs <- read_excel('./Bone_China_cleaned.xlsx')

ui <- navbarPage(
  title = "NLP app",
  
  tabPanel("Topic Model",icon = icon("group"), 
           fluidPage(
             
             headerPanel(""),
             titlePanel(p(h2("Topic Modelling example",style = "color:#4d3a7d"))),
             
             #sidebarPanel(
             wellPanel(tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'), style = "background: lightgrey",
                       id = "leftPanel",
                       sliderInput("nTopics", "Number of topics to display", min = 5, max = 50, value = 10, step=5),
                       sliderInput("nTerms", "#top terms per topic", min = 10, max = 50, value = 20, step=5),
                       tags$hr(),
                       actionButton(inputId = "GoButton", label = "Go",  icon("refresh"))
             ),
             mainPanel( 
               tabPanel("Topic Visualisation", hr(),helpText(h2("Please select a topic!")),  visOutput('visChart')))
           )
  )
)

# server
server <- function(input, output, session) {
  
  Topic_Subset <- reactive({
    
    docs <- docs$text  
    nTopics <- input$nTopics
    
    # topic model using text2vec package
    tokens = docs %>%
      tolower %>%
      word_tokenizer
    
    
    it = itoken(tokens, progressbar = FALSE)
    v = create_vocabulary(it,stopwords=tm::stopwords("en")) 
    vectorizer = vocab_vectorizer(v)
    dtm = create_dtm(it, vectorizer, type = "dgTMatrix")
    
    lda_model = text2vec::LDA$new(n_topics = nTopics, doc_topic_prior = 0.1, topic_word_prior = 0.01)
    lda_model$fit_transform(x = dtm, n_iter = 1000, 
                            convergence_tol = 0.001, n_check_convergence = 25, 
                            progressbar = FALSE)
    
    return(lda_model) # 
  })
  
  output$visChart <- renderVis({
    
    input$GoButton
    
    isolate({
      nterms    <- input$nTerms
      lda_model <- Topic_Subset()
    })
    
    lda_model$plot(out.dir = "./results", R = nterms, open.browser = FALSE)
    
    readLines("./results/lda.json")
    
  })
}


shinyApp(ui = ui, server = server)