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

  
# TABLA    
        
    
    tabla <- reactive(
        
        if (input$varcolor=="sexo") {propinas %>% group_by(sexo)
        }else if (input$varcolor=="fuma") {propinas %>% group_by(fuma) 
        }else if (input$varcolor=="dia") {propinas %>% group_by(dia)
        }else{propinas %>% group_by(momento)
        })
    
    output$tablita <- DT::renderDataTable(tabla()%>%summarise("mean_prop"=mean(propina), "sd_prop"=sd(propina), 
                                                              "mean_total"=mean(total), "sd_total"=sd(total))%>%
                                          mutate(across(where(is.numeric), round, as.numeric(input$digitos))))
    
################################################################################
    
   
# GRAFICO  
    output$scat <- renderPlot({
        ggplot(data = propinas,
               aes(x = total, y = propina,
                   colour = .data[[input$varcolor]] ))+
            geom_point() + theme(aspect.ratio = 1) +
            scale_x_continuous(name ="Total de la cuenta") +
            scale_y_continuous(name = "Propina")
    })
    output$graf <- renderPlot(tabla()%>% summarise(n=n())%>%
                                  ggplot(aes(x=eval(as.name(input$varcolor)), y=n))+
                                  geom_col()+
                                  xlab(as.name(input$varcolor))+
                                  ylab("Cantidad"))
        
    
    }

# GRAFICO 2

# Datos
 

 






shinyApp(ui, server)