
# Pacotes ------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(recipes)
library(caret)


# Base ---------------------------------------------------------------------

data("diamonds", package = "ggplot2")

source("utils.R")

diamonds <- diamonds %>% 
  sample_frac(0.25)

variaveis <- diamonds %>% 
  select(-price) %>% 
  names()

variaveis_numericas <- diamonds %>% 
  select_if(is.numeric) %>% 
  select(-price) %>% 
  names()

# UI -----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Visualizando modelos aditivos generalizados"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "modelo_familia",
        label = "Usar distribuição",
        choices = c("Normal", "Gama")
      ),
      checkboxInput(
        inputId = "transformacao_log",
        label = "Usar log do preço?",
        value = FALSE
      ),
      selectInput(
        inputId = "variaveis_excluidas",
        label = "Excluir variáveis",
        choices = variaveis,
        multiple = TRUE
      ),
      br(),
      actionButton(
        inputId = "rodar_modelo",
        label = "Ajustar modelo"
      ),
      br(),
      br(),
      selectInput(
        inputId = "variavel_grafico_gam",
        label = "Visualizar gráfico para:",
        choices = variaveis_numericas
      ),
      checkboxInput(
        inputId = "ver_termos_suavizados",
        label = "Ver termos suavizados?",
        value = TRUE
      )
    ),
    mainPanel(
      column(
        width = 6,
        tableOutput(outputId = "modelo_tabela")
      ),
      column(
        width = 2,
        tableOutput(outputId = "modelo_qualidade")
      ),
      plotOutput(outputId = "modelo_grafico")
    )
  )
)


# Server -------------------------------------------------------------------

server <- function(input, output, session) {
  
  modelo <- eventReactive(input$rodar_modelo, {
    
    receita <- recipe(price ~ ., diamonds)
    
    vars <- input$variaveis_excluidas
    
    if(isTruthy(input$variaveis_excluidas)) {
      receita <- receita %>% 
        step_rm(one_of(vars))
    }
    
    if(input$modelo_familia == "Normal") {
      if(input$transformacao_log) {
        receita <- receita %>% 
          step_log(all_outcomes())
      }
      familia = gaussian()
    } else if(input$modelo_familia == "Gama") {
      if(input$transformacao_log) {
        familia = Gamma(link = "log")
      } else {
        familia = Gamma()
      }
    }
    
    train(
      receita,
      diamonds,
      method = "gam",
      family = familia,
      trControl = trainControl(method = "cv", number = 2)
    )
    
  })
  
  output$modelo_tabela <- renderTable({

    req(modelo())
    
    modelo()$finalModel %>% 
      broom::tidy(parametric = !input$ver_termos_suavizados)
    
  })
  
  output$modelo_qualidade <- renderTable({
    
    req(modelo())
    
    modelo()$results %>%
      select(select:MAE)
    
  })
  
  output$modelo_grafico <- renderPlot({
    
    req(modelo())
    
    gam_plot(
      modelo()$finalModel, 
      var = input$variavel_grafico_gam
    )
    
  })
  

}

shinyApp(ui, server)

