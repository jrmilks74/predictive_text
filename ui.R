
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
            verbatimTextOutput("pred"),
            br(),
            br(),
            h4("Author: James R. Milks"),
            h5("Updated: July 14, 2021"),
            br(),
            helpText("Submitted in fulfillment of the requirements of the Capstone Course",
                     "in the Data Science Specialization from John Hopkins University")
        )
    )
))
