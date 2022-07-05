
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Text Prediction App"),
    br(),
    helpText("Predicts the next most likely word as you type"),
    br(),

    # Sidebar with text box
    sidebarLayout(
        sidebarPanel(
            textInput("prompt", "Enter sentence prompt"),
        ),

        # Output of the prediction model
        mainPanel(
            h3("Predictions"),
            br(),
            verbatimTextOutput("pred")
                        
        )
    ),
    h4("Author: James R. Milks"),
    h5("Updated: 05 July 2022"),
    "Originally submitted in fulfillment of the requirements of the Capstone Course in the Data Science Specialization from John Hopkins University",
    br(),
    "Code and algorithm available at",
    a(href = "https://github.com/jrmilks74/predictive_text", "https://github.com/jrmilks74/predictive_text")
    
)
)
