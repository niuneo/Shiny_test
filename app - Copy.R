library("shiny")


ui <- shinyUI(
  pageWithSidebar(
    headerPanel("Enter Text"),
    sidebarPanel(
      textInput("txt", "Text", "default text")
    ),
    mainPanel(textOutput("upperCaseText"))
  )
)


# server
server <- shinyServer(function(input, output){
  ds <- reactive(
    {
      dataset <- toupper(input$txt)
    })
  
  output$upperCaseText <- renderText(ds())
})


shinyApp(ui = ui, server = server)