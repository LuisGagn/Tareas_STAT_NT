library(shiny)
library(tidyverse)
library(DT)
# Es un borrador aclarado
propinas <- read_csv("propina.csv" )


ui <- fluidPage(
    titlePanel("Datos de Propina"),
    sidebarLayout(
    sidebarPanel(
            actionButton("Prende", "Generar Graficos",width = 225),
            actionButton("Apaga", "Borrar Graficos", width = 225),
            selectInput('varcolor', 'Variable en color',
                        c("sexo", "fuma", "dia", "momento") ), 
            selectInput("digitos","Seleccione redondeo", c(0,1,2))),
        
        
        mainPanel(
        tabsetPanel(
                tabPanel("Bivariado",plotOutput("scat" ), dataTableOutput("tablita")),
                tabPanel("Univariado", plotOutput("graf"))
            ))
            ))
    


server <- function(input, output){
    
   # BOTONES 
    v <- reactiveValues(data = NULL)
    
    observeEvent(input$Prende, {
        f= propinas
        v$d1 <- f
    })
    
    observeEvent(input$Apaga, {
        v$d1 <- NULL
    })  
    
  # TABLA    
    tabla<- reactive(
        if(is.null(v$d1)){return()}else{
        v$d1 %>% group_by(.data[[input$varcolor]])%>%
                summarise("mean_prop"=mean(propina), "sd_prop"=sd(propina), 
                          "mean_total"=mean(total), "sd_total"=sd(total))%>%
                mutate(across(where(is.numeric), round, as.numeric(input$digitos)))})

  # GRAFICOS
    
  # GRAFICO 1   
  G1<-reactive(if(is.null(v$d1)){return()}else{    
                          ggplot(data = v$d1,
                          aes(x = total, y = propina,
                          colour = .data[[input$varcolor]] ))+
                          geom_point() + theme(aspect.ratio = 1) +
                          scale_x_continuous(name ="Total de la cuenta") +
                          scale_y_continuous(name = "Propina")}) 
    
  # GRAFICO 2
  G2<-reactive(if(is.null(v$d1)){return()}else{
              v$d1 %>% group_by(.data[[input$varcolor]])%>% 
              summarise(n=n())%>%
              ggplot(aes(x=eval(as.name(input$varcolor)), y=n))+
              geom_col()+
              xlab(as.name(input$varcolor))+
              ylab("Cantidad")})
  
  
    
  # CALLS 
        output$scat <- renderPlot(G1())
    
        output$graf <- renderPlot(G2())
    
        output$tablita <- DT::renderDataTable(tabla())
}






shinyApp(ui, server)