
library(shiny)
library(shinyWidgets)

shinyUI(fluidPage(
    tags$head(
        tags$script("
          Shiny.addCustomMessageHandler('reset-input', function(message) {
            Shiny.setInputValue('btnLabel', null);
          });
          Shiny.addCustomMessageHandler('scroll-text-area', function(message) {
            let objDiv = document.getElementById('text_history');
            objDiv.scrollTop = objDiv.scrollHeight - objDiv.clientHeight;
          });
        ")
    ),
    titlePanel('Coursera - Word prediction'),
    sidebarLayout(
        sidebarPanel(
            h3('Word predictions'),
            textOutput('words')
        ),
        mainPanel(
            textAreaInput('text_history', 'Text Area')
        )
    )
))
