library(shiny)
library(tidyverse)
library(ggplot2)

ui<- fluidPage(
    titlePanel("Actividad 10"),
    
    sidebarLayout(position = "right",
    sidebarPanel("Panel Izquierdo", 
                 sliderInput(inputId = "cant", label = "Tamaño Muestral", min=1,max=100,value=15),
                 selectInput('distri', 'Distribucion',c("Gamma","Normal","Ambas")),
                 textInput("bins","Ingrese el numero de bins", value=10),
                 img(src="imagen.jpg", height=140,width=140)),

    mainPanel(
        tabsetPanel(
            tabPanel("Histograma", plotOutput("hist")),
            tabPanel("Plot", fluidRow(
                column(width=5,
                       plotOutput("p1")
                ),
                column(width=5,
                       plotOutput("p2")
                ),
                column(width=5,
                       plotOutput("p3")
                ),
                column(width=5,
                       plotOutput("p4")
                )
            )),
            tabPanel("Resumen"),
            tabPanel("Tabla")
        ))),)


server <- function(input,output){

    datos <- reactive(
        if (input$distri=="Normal") {
            data.frame(valores=rnorm(input$cant),tipo="normal")
        }else if(input$distri=="Gamma"){
            data.frame(valores=rgamma(n=input$cant,shape = 1),tipo="gamma")
        }else{
            rbind(data.frame(valores=rnorm(input$cant),tipo="normal"),
                  data.frame(valores=rgamma(n=input$cant,shape = 1),tipo="gamma"))
        }
    )
   
     
   grafico<- reactive(
        if (input$distri=="Ambas") {
          datos()%>% ggplot(aes(valores))+geom_histogram(bins=input$bins)+facet_wrap(~tipo)

            }else{
        datos()%>%ggplot(aes(valores))+geom_histogram(bins=input$bins)
        
          }
   )
    
   output$hist<-renderPlot(print(grafico()))
   output$p1<-renderPlot(print(grafico()))
   output$p2<-renderPlot(print(grafico()))
   output$p3<-renderPlot(print(grafico()))
   output$p4<-renderPlot(print(grafico()))
       
   }

shinyApp(ui, server)