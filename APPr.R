library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Comparación Estadística: Prueba t vs ANOVA"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sube un archivo CSV o XLSX", accept = c(".csv", ".xlsx")),
      uiOutput("var_select"),
      selectInput("graph_type", "Selecciona el tipo de gráfico:",
                  choices = c("Barras con error estándar", 
                              "Dispersión con líneas de error", 
                              "Campana de Gauss")),
      actionButton("run_analysis", "Ejecutar análisis")
    ),
    
    mainPanel(
      textOutput("method_info"),
      verbatimTextOutput("result"),
      plotOutput("summary_plot")
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read.csv(input$file$datapath, sep = ";")
    } else if (ext == "xlsx") {
      read_excel(input$file$datapath)
    } else {
      showNotification("Formato no soportado", type = "error")
      return(NULL)
    }
  })
  
  output$var_select <- renderUI({
    req(data())
    tagList(
      selectInput("group_var", "Variable de grupo (categórica):",
                  choices = names(data())),
      selectInput("numeric_var", "Variable numérica:",
                  choices = names(data()))
    )
  })
  
  observeEvent(input$run_analysis, {
    req(data())
    df <- data()
    group_var <- df[[input$group_var]]
    numeric_var <- df[[input$numeric_var]]
    
    if (!is.numeric(numeric_var)) {
      showNotification("La variable numérica debe ser de tipo numérico", type = "error")
      return()
    }
    
    group_var <- as.factor(group_var)
    df$group_var <- group_var
    
    ngroups <- length(unique(group_var))
    
    result_text <- ""
    method_used <- ""
    
    if (ngroups == 2) {
      test <- t.test(numeric_var ~ group_var)
      result_text <- capture.output(print(test))
      method_used <- "Se ha aplicado la prueba t de Student (2 grupos)."
      
    } else if (ngroups == 3) {
      model <- aov(numeric_var ~ group_var)
      result_text <- capture.output(summary(model))
      method_used <- "Se ha aplicado ANOVA (3 grupos)."
      
    } else {
      result_text <- "La variable de grupo debe tener exactamente 2 o 3 niveles."
      method_used <- "No se pudo aplicar ninguna prueba."
    }
    
    output$method_info <- renderText({ method_used })
    output$result <- renderPrint({ cat(paste(result_text, collapse = "\n")) })
    
    if (ngroups %in% c(2, 3)) {
    
      summary_df <- df %>%
        group_by(group_var) %>%
        summarise(
          media = mean(numeric_var, na.rm = TRUE),
          se = sd(numeric_var, na.rm = TRUE) / sqrt(n()),
          .groups = 'drop'
        )
      
  
      output$summary_plot <- renderPlot({
        if (input$graph_type == "Barras con error estándar") {
          ggplot(summary_df, aes(x = group_var, y = media, fill = group_var)) +
            geom_col(width = 0.5, color = "black", show.legend = FALSE) +
            geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.2) +
            labs(x = input$group_var, y = paste("Media de", input$numeric_var)) +
            theme_minimal()
          
        } else if (input$graph_type == "Dispersión con líneas de error") {
          ggplot(df, aes(x = group_var, y = numeric_var, color = group_var)) +
            geom_jitter(width = 0.2, alpha = 0.6, show.legend = FALSE) +
            stat_summary(fun = mean, geom = "point", shape = 18, size = 4) +
            stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
            labs(x = input$group_var, y = input$numeric_var) +
            theme_minimal()
          
        } else if (input$graph_type == "Campana de Gauss") {
          ggplot(df, aes(x = numeric_var, fill = group_var, color = group_var)) +
            geom_density(alpha = 0.3) +
            labs(x = input$numeric_var, y = "Densidad", title = "Distribución por grupo") +
            theme_minimal()
        }
      })
    } else {
      output$summary_plot <- renderPlot({})
    }
  })
}

shinyApp(ui = ui, server = server)
