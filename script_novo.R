library(shiny)
library(tidyverse)
library(recipes)
library(caret)


# Base de dados ------------------------------------------------------------

data("diamonds", package = "ggplot2")

diamonds <- diamonds %>% 
  sample_frac(0.25)

variaveis <- diamonds %>% 
  select(-price) %>% 
  names()

variaveis_numericas <- diamonds %>% 
  select_if(is.numeric) %>% 
  select(-price) %>% 
  names()

source("utils.R")

# UI -----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Visualizando modelos aditivos generalizados"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput(
        inputId = "modelo_distribuicao",
        label = "Usar distribuição",
        choices = c("Normal", "Gama")
      ),
      checkboxInput(
        inputId = "modelo_transformacao",
        label = "Usar log do preço?"
      ),
      selectInput(
        inputId = "modelo_vars_excluir",
        label = "Excluir variaveis",
        choices = variaveis,
        multiple = TRUE
      ),
      br(),
      actionButton(
        inputId = "modelo_rodar",
        label = "Ajustar modelo"
      ),
      br(),
      br(),
      selectInput(
        inputId = "grafico_variavel",
        label = "Visualizar gráfico para:",
        choices = variaveis_numericas
      )
    ),
    mainPanel = mainPanel(
      column(
        width = 6,
        tableOutput(outputId = "resultado_tabela")
      ),
      column(
        width = 2,
        tableOutput(outputId = "resultado_qualidade")
      ),
      plotOutput(outputId = "grafico_gam"),
      uiOutput("id")
    )
  )
)

# Server -------------------------------------------------------------------

server <- function(input, output, session) {
  
  output$id <- renderUI({
    
    checkboxInput(
      
    )
    
  })
  
  modelo <- eventReactive(input$modelo_rodar, {
    
    receita <- recipe(price ~ ., data = diamonds)
    
    if(input$modelo_distribuicao == "Normal") {
      if(input$modelo_transformacao) {
        receita <- receita %>% 
          step_log(price)
      }
      familia = gaussian()
    } else if(input$modelo_distribuicao == "Gama") {
      if(input$modelo_transformacao) {
        familia = Gama(link = "log")
      } else {
        familia = Gama(link = "inverse")
      }
    }
    
    
    variaveis_excluidas <- input$modelo_vars_excluir
    
    if(!is.null(input$modelo_vars_excluir)) {
      receita <- receita %>% 
        step_rm(one_of(variaveis_excluidas))
    }
    
    
    
    train(
      receita,
      diamonds,
      method = "gam",
      family = familia,
      trControl = trainControl("cv", 2)
    )
    
  })
  
  output$resultado_tabela <- renderTable({
    
    modelo()$finalModel %>% 
      broom::tidy(parametric = !input$selecao)
    
  })
  
  output$resultado_qualidade <- renderTable({
    
    modelo()$results %>% 
      select(select:MAE)
    
  })
  
  output$grafico_gam <- renderPlot({
    
    gam_plot(modelo()$finalModel, input$grafico_variavel)
    
  })
  
}

shinyApp(ui, server)
