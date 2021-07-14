library(shiny)
library(sbo)

load("model.rda")

model_pred <- sbo_predictor(model)

shinyServer(function(input, output) {
    
    output$pred <- renderText({
        predict(model_pred, input$prompt)
        })
    })
