library(report)
library(shiny)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)


interface <- fluidPage(
  titlePanel("Statistical Test Generator (T-test/ANOVA)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Upload data file (.csv or .xlsx)",
                accept = c(".csv", ".xlsx")),
      actionButton("process", "Run Analysis", class = "btn-primary"),
      helpText("Each column is treated as a separate group for analysis."),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Statistical Output", verbatimTextOutput("stats_output")),
        tabPanel("Normal Distribution", plotOutput("norm_dist")),
        tabPanel("Reporte Detallado", verbatimTextOutput("full_report"))
      )
    )
  )
)

backend <- function(input, output, session) {
  
  imported_data <- reactive({
    req(input$dataset)
    file_extension <- tools::file_ext(input$dataset$name)
    
    raw_data <- switch(file_extension,
                       csv = read.csv(input$dataset$datapath, sep=";", dec=".", stringsAsFactors = FALSE),
                       xlsx = read_excel(input$dataset$datapath),
                       stop("Unsupported file format"))
    
    raw_data <- raw_data[, colSums(is.na(raw_data)) < nrow(raw_data)]
    transformed_data <- pivot_longer(raw_data, 
                                     cols = everything(), 
                                     names_to = "category", 
                                     values_to = "measurement") %>%
      mutate(measurement = as.numeric(measurement)) %>%
      filter(!is.na(measurement)) %>%
      mutate(category = as.factor(category))
    
    return(transformed_data)
  })
  
  stats_results <- eventReactive(input$process, {
    dataset <- imported_data()
    group_count <- n_distinct(dataset$category)
    
    if(group_count == 2){
      t_test_result <- t.test(measurement ~ category, data = dataset)
      list(
        test_type = "Two-Sample T-test", 
        results = t_test_result, 
        t_value = as.numeric(t_test_result$statistic), 
        dataset = dataset
      )
      
    } else if(group_count >= 3){
      anova_result <- aov(measurement ~ category, data = dataset)
      list(
        test_type = "One-way ANOVA", 
        results = summary(anova_result), 
        model_fit = anova_result, 
        t_value = NA, 
        dataset = dataset
      )
      
    } else {
      list(
        test_type = "Error", 
        results = "Analysis requires at least 2 groups to compare.", 
        t_value = NA
      )
    }
  })
  
  output$stats_output <- renderPrint({
    req(stats_results())
    cat("Analysis Method:", stats_results()$test_type, "\n\n")
    print(stats_results()$results)
  })
  
  output$norm_dist <- renderPlot({
    analysis <- stats_results()
    if(analysis$test_type != "Two-Sample T-test"){
      plot.new()
      title("Normal distribution curve only available for T-test")
      return()
    }
    
    t_statistic <- analysis$t_value
    x_coords <- seq(-4, 4, length.out = 400)
    y_coords <- dnorm(x_coords)
    alpha_level <- 0.05
    lower_bound <- qnorm(alpha_level/2)
    upper_bound <- qnorm(1 - alpha_level/2)
    
    ggplot(data.frame(x_coords, y_coords), aes(x_coords, y_coords)) +
      geom_line(color = "skyblue", size = 1.2) +
      geom_area(data = subset(data.frame(x_coords, y_coords), x_coords <= lower_bound),
                aes(x_coords, y_coords), fill = "tomato", alpha = 0.3) +
      geom_area(data = subset(data.frame(x_coords, y_coords), x_coords >= upper_bound),
                aes(x_coords, y_coords), fill = "tomato", alpha = 0.3) +
      geom_vline(xintercept = t_statistic, color = "forestgreen", linetype = "dashed", size = 1) +
      geom_vline(xintercept = lower_bound, color = "purple", linetype = "dashed") +
      geom_vline(xintercept = upper_bound, color = "yellow", linetype = "dashed") +
      labs(title = "Normal Distribution", 
           subtitle = paste("t-statistic =", round(t_statistic, 2)),
           x = "t-value", 
           y = "Density") +
      theme_minimal()
  })
  
  output$full_report <- renderPrint({
    analysis <- stats_results()
    if(analysis$test_type != "One-way ANOVA") 
      return("Este reporte solo está disponible para análisis ANOVA (3+ grupos).")
    
    enhanced_data <- analysis$dataset
    enhanced_data$factor_b <- rep(c("Low", "High"), length.out = nrow(enhanced_data))
    interaction_model <- aov(measurement ~ category * factor_b, data = enhanced_data)
    
    cat("Modelo de Interacción: measurement ~ category * factor_b\n\n")
    
    modelo_anova <- interaction_model
    
    cat("Análisis de Varianza (ANOVA)\n")
    cat("---------------------------\n\n")
    cat("Hemos realizado un análisis de varianza (ANOVA) examinando el efecto de 'category' y 'factor_b' sobre 'measurement'.\n\n")
    
    sum_result <- summary(modelo_anova)
    
    cat("Resultados Principales:\n")
    
    p_values <- unlist(lapply(summary(modelo_anova), function(x) x$"Pr(>F)"[1:2]))
    
    if (!is.na(p_values[1]) && p_values[1] < 0.05) {
      cat("* El factor 'category' tiene un efecto estadísticamente significativo (p < 0.05)\n")
    } else {
      cat("* El factor 'category' no muestra un efecto estadísticamente significativo (p > 0.05)\n")
    }
    
    if (!is.na(p_values[2]) && p_values[2] < 0.05) {
      cat("* El factor 'factor_b' tiene un efecto estadísticamente significativo (p < 0.05)\n")
    } else {
      cat("* El factor 'factor_b' no muestra un efecto estadísticamente significativo (p > 0.05)\n")
    }
    
    interaccion_p <- summary(modelo_anova)[[1]]$"Pr(>F)"[3]
    if (!is.na(interaccion_p) && interaccion_p < 0.05) {
      cat("* La interacción entre 'category' y 'factor_b' es estadísticamente significativa (p < 0.05)\n")
    } else {
      cat("* No se encontró interacción significativa entre 'category' y 'factor_b' (p > 0.05)\n")
    }
    
    cat("\nTabla de Resultados:\n")
    print(sum_result)
    
    cat("\nInterpretación:\n")
    cat("Si p < 0.05 para un factor, rechazamos la hipótesis nula de que no hay diferencia entre los grupos.\n")
    cat("Esto sugiere que ese factor tiene un impacto significativo en la variable de respuesta 'measurement'.\n")
  })
}

shinyApp(interface, backend)
