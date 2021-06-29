library(shiny)
library(ggplot2)
library(tidyverse)


ui <- fluidPage(titlePanel(h1("Actividad 9")), p('Realizado fuera de Fecha'),
    sliderInput(inputId = "cant",label = "tamaño muestral:",min = 1,max = 500,value = 30),
    selectInput('distri','Distribución', c("Gamma","Normal")),
    plotOutput("plot2")
)


server <- function(input,output){
    output$plot2<-renderPlot({
        
        if (input$distri == 'Normal'){
            dataf<-data.frame(x=rnorm(input$cant)) %>%
                ggplot(aes(x)) + geom_histogram(binwidth = 0.5)
            print(dataf)
        }else{
            dataf<-data.frame(x=rgamma(
                n=input$cant, shape=1)) %>%
                ggplot(aes(x))+geom_histogram(binwidth = 0.5)
            print(dataf)
            
            }})}
    
 
shinyApp(ui, server)


