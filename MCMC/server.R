# ============================================================================
# MONTE CARLO ENGINE - Server Logic (SECURED VERSION)
# ============================================================================

server <- function(input, output, session) {
  
  # ==========================================================================
  # 🔐 SÉCURITÉ — VALIDATION CENTRALISÉE DES FORMULES
  # ==========================================================================
  
  safe_math_formula <- reactive({
    
    req(input$math_formula)
    
    formula <- input$math_formula
    
    # Limite longueur (DoS)
    if (nchar(formula) > 100) {
      stop("La formule est trop longue (max 100 caractères).")
    }
    
    # Caractères autorisés uniquement
    if (!grepl("^[0-9x+*/^()\\. a-zA-Z\\-]+$", formula)) {
      stop("Seules les opérations mathématiques de base sont autorisées.")
    }
    
    # Fonctions R interdites
    forbidden <- c(
      "system", "eval", "parse", "library", "require",
      "file", "unlink", "setwd", "Sys", "::", ":::"
    )
    
    if (any(grepl(paste(forbidden, collapse = "|"), formula))) {
      stop("Fonctions R interdites détectées.")
    }
    
    # Commandes LaTeX dangereuses
    if (grepl("\\\\(href|url|html)", formula)) {
      stop("Commandes LaTeX interdites.")
    }
    
    formula
  })
  
  
  safe_eval <- function(formula, x) {
    env <- new.env(parent = baseenv())
    env$x <- x
    env$pi <- pi
    env$e <- exp(1)
    eval(parse(text = formula), envir = env)
  }
  
  # ==========================================================================
  # RÉACTIVITÉ : STOCKAGE DES SIMULATIONS
  # ==========================================================================
  
  sim_data <- reactiveValues(
    current = NULL,
    history = NULL,
    metadata = NULL,
    simulation_done = 0
  )
  
  # ==========================================================================
  # INDICATEURS D'ÉTAT
  # ==========================================================================
  
  output$has_simulation <- reactive({
    !is.null(sim_data$current) && length(sim_data$current) > 0
  })
  outputOptions(output, "has_simulation", suspendWhenHidden = FALSE)
  
  output$has_comparison <- reactive({
    !is.null(sim_data$history) && !is.null(sim_data$current)
  })
  outputOptions(output, "has_comparison", suspendWhenHidden = FALSE)
  
  # ==========================================================================
  # SIMULATION
  # ==========================================================================
  
  observeEvent(input$simulate, {
    
    disable("simulate")
    on.exit(enable("simulate"))
    
    validate(
      need(input$n_sim >= 1000, "Minimum 1000 simulations requis"),
      need(input$n_sim <= 100000, "Maximum 100000 simulations")
    )
    
    if (!is.null(sim_data$current)) {
      sim_data$history <- sim_data$current
    }
    
    res <- withProgress(message = "Simulation en cours...", value = 0, {
      
      n <- input$n_sim
      dist <- input$dist_type
      
      params <- if (dist != "custom") {
        switch(dist,
               "norm" = list(mean = input$mean, sd = input$sd),
               "unif" = list(min = input$min, max = input$max),
               "exp"  = list(rate = input$rate))
      } else NULL
      
      custom_method <- if (dist == "custom") input$custom_method else NULL
      
      custom_formula <- if (dist == "custom") {
        safe_math_formula()
      } else NULL
      
      x_min <- if (dist == "custom" && input$custom_method == "density") input$x_min else NULL
      x_max <- if (dist == "custom" && input$custom_method == "density") input$x_max else NULL
      
      incProgress(0.2)
      
      tryCatch({
        monte_carlo(
          n = n,
          dist = dist,
          params = params,
          custom_method = custom_method,
          custom_formula = custom_formula,
          x_min = x_min,
          x_max = x_max
        )
      }, error = function(e) {
        showNotification(
          HTML(htmltools::htmlEscape(e$message)),
          type = "warning",
          duration = 6
        )
        NULL
      })
    })
    
    if (is.null(res) || length(res) == 0) {
      showNotification("La simulation a échoué.", type = "error", duration = 6)
      return()
    }
    
    sim_data$current <- res
    sim_data$simulation_done <- sim_data$simulation_done + 1
    
    showNotification(
      tagList(
        icon("circle-check"),
        tags$strong(" Simulation terminée"),
        tags$br(),
        sprintf("%s échantillons générés", format(length(res), big.mark = " "))
      ),
      type = "message",
      duration = 5,
      id = "sim_complete"
    )
    
    updateNavbarPage(session, "main_nav", selected = "results")
  })
  
  # ==========================================================================
  # RESET
  # ==========================================================================
  
  observeEvent(input$reset, {
    sim_data$current <- NULL
    sim_data$history <- NULL
    sim_data$metadata <- NULL
    sim_data$simulation_done <- 0
    showNotification("Historique réinitialisé", type = "warning", duration = 2)
  })
  
  # ==========================================================================
  # APERÇUS
  # ==========================================================================
  
  output$latex_preview <- renderUI({
    formula <- safe_math_formula()
    latex_expr <- r_to_latex(formula)
    latex_safe <- htmltools::htmlEscape(latex_expr)
    
    withMathJax(
      tags$div(HTML(paste0("$$f(x) = ", latex_safe, "$$")))
    )
  })
  
  output$density_preview <- renderPlot({
    req(input$x_min, input$x_max)
    
    tryCatch({
      formula <- safe_math_formula()
      x_vals <- seq(input$x_min, input$x_max, length.out = 500)
      y_vals <- sapply(x_vals, safe_eval, formula = formula)
      y_vals[!is.finite(y_vals)] <- 0
      
      ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
        geom_line(linewidth = 1.2) +
        geom_area(alpha = 0.15) +
        theme_minimal()
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = e$message) +
        theme_void()
    })
  })
  
  # ==========================================================================
  # ONGLET RÉSULTATS : OUTPUTS
  # ==========================================================================
  
  # --- Cartes de statistiques clés ---
  
  output$stat_mean <- renderText({
    req(sim_data$current)
    sprintf("%.4f", mean(sim_data$current, na.rm = TRUE))
  })
  
  output$stat_median <- renderText({
    req(sim_data$current)
    sprintf("%.4f", median(sim_data$current, na.rm = TRUE))
  })
  
  output$stat_sd <- renderText({
    req(sim_data$current)
    sprintf("%.4f", sd(sim_data$current, na.rm = TRUE))
  })
  
  output$stat_n <- renderText({
    req(sim_data$current)
    format(length(sim_data$current), big.mark = " ")
  })
  
  
  # --- Histogramme interactif ---
  
  output$hist_plot <- plotly::renderPlotly({
    req(sim_data$current)
    
    data <- sim_data$current
    data <- data[is.finite(data)]
    
    p <- ggplot(data.frame(x = data), aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 50, 
                     fill = "#34495e", 
                     color = "white", 
                     alpha = 0.7) +
      geom_density(color = "#2c3e50", linewidth = 1.2) +
      labs(
        x = "Valeur",
        y = "Densité"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold", color = "#2c3e50")
      )
    
    ggplotly(p) %>%
      layout(hovermode = "x unified")
  })
  
  
  # --- QQ-Plot ---
  
  output$qq_plot <- renderPlot({
    req(sim_data$current)
    
    data <- sim_data$current
    data <- data[is.finite(data)]
    
    qqnorm(data, 
           main = "",
           pch = 16, 
           col = rgb(52, 73, 94, 100, maxColorValue = 255),
           cex = 0.8)
    qqline(data, col = "#2c3e50", lwd = 2)
    grid(col = "#dee2e6")
  })
  
  
  # --- Tableau de statistiques ---
  
  output$stats_table <- renderTable({
    req(sim_data$current)
    stat_desc(sim_data$current)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  
  # --- Test de normalité ---
  
  output$normality_test <- renderUI({
    req(sim_data$current)
    
    test <- test_normalite(sim_data$current)
    
    if (is.na(test$p_value)) {
      return(tags$div(
        class = "alert alert-secondary",
        test$conclusion
      ))
    }
    
    alert_class <- if (test$p_value > 0.05) "alert-success" else "alert-danger"
    
    tags$div(
      class = paste("alert", alert_class),
      tags$p(tags$strong("Statistique W :"), sprintf("%.4f", test$statistic)),
      tags$p(tags$strong("p-value :"), sprintf("%.4e", test$p_value)),
      tags$hr(),
      tags$p(test$conclusion)
    )
  })
  
  
  # ==========================================================================
  # ONGLET COMPARAISON
  # ==========================================================================
  
  # --- Statut de la comparaison ---
  
  output$comparison_status <- renderUI({
    
    if (is.null(sim_data$history)) {
      return(
        card(
          card_header("Statut"),
          card_body(
            tags$div(
              class = "alert alert-secondary",
              "Aucune simulation précédente disponible. ",
              "Lancez une première simulation, modifiez les paramètres, puis relancez pour activer la comparaison."
            )
          )
        )
      )
    }
    
    card(
      card_header("Statut"),
      card_body(
        tags$div(
          class = "alert alert-success",
          "Comparaison disponible entre la simulation actuelle et la précédente."
        ),
        tags$p(
          tags$strong("Simulation actuelle :"),
          format(length(sim_data$current), big.mark = " "), " valeurs"
        ),
        tags$p(
          tags$strong("Simulation précédente :"),
          format(length(sim_data$history), big.mark = " "), " valeurs"
        )
      )
    )
  })
  
  
  # --- Histogrammes superposés ---
  
  output$comparison_hist <- plotly::renderPlotly({
    req(sim_data$current, sim_data$history)
    
    data_current <- sim_data$current[is.finite(sim_data$current)]
    data_previous <- sim_data$history[is.finite(sim_data$history)]
    
    df <- rbind(
      data.frame(valeur = data_current, simulation = "Actuelle"),
      data.frame(valeur = data_previous, simulation = "Précédente")
    )
    
    p <- ggplot(df, aes(x = valeur, fill = simulation)) +
      geom_density(alpha = 0.5, linewidth = 1) +
      scale_fill_manual(values = c("Actuelle" = "#34495e", "Précédente" = "#7f8c8d")) +
      labs(
        x = "Valeur",
        y = "Densité",
        fill = "Simulation"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p)
  })
  
  
  # --- Boxplots comparatifs ---
  
  output$comparison_boxplot <- renderPlot({
    req(sim_data$current, sim_data$history)
    
    data_current <- sim_data$current[is.finite(sim_data$current)]
    data_previous <- sim_data$history[is.finite(sim_data$history)]
    
    df <- rbind(
      data.frame(valeur = data_current, simulation = "Actuelle"),
      data.frame(valeur = data_previous, simulation = "Précédente")
    )
    
    ggplot(df, aes(x = simulation, y = valeur, fill = simulation)) +
      geom_boxplot(alpha = 0.7, outlier.color = "#e74c3c") +
      scale_fill_manual(values = c("Actuelle" = "#34495e", "Précédente" = "#7f8c8d")) +
      labs(
        x = "",
        y = "Valeur"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  
  # --- Tests statistiques de comparaison ---
  
  output$comparison_tests <- renderUI({
    req(sim_data$current, sim_data$history)
    
    tests <- test_comparaison(sim_data$current, sim_data$history)
    
    tagList(
      tags$h6("Test t de Student", style = "font-weight: 600; margin-top: 0;"),
      tags$div(
        class = if (tests$t_test$p_value > 0.05) "alert alert-success" else "alert alert-danger",
        tags$p(tags$strong("Statistique t :"), sprintf("%.4f", tests$t_test$statistic)),
        tags$p(tags$strong("p-value :"), sprintf("%.4e", tests$t_test$p_value)),
        tags$p(tests$t_test$conclusion, style = "margin-bottom: 0;")
      ),
      
      tags$h6("Test de Wilcoxon (non-paramétrique)", style = "font-weight: 600; margin-top: 20px;"),
      tags$div(
        class = if (tests$wilcox_test$p_value > 0.05) "alert alert-success" else "alert alert-danger",
        tags$p(tags$strong("Statistique W :"), sprintf("%.4f", tests$wilcox_test$statistic)),
        tags$p(tags$strong("p-value :"), sprintf("%.4e", tests$wilcox_test$p_value)),
        tags$p(tests$wilcox_test$conclusion, style = "margin-bottom: 0;")
      ),
      
      tags$h6("Test F de variance", style = "font-weight: 600; margin-top: 20px;"),
      tags$div(
        class = if (tests$var_test$p_value > 0.05) "alert alert-success" else "alert alert-danger",
        tags$p(tags$strong("Statistique F :"), sprintf("%.4f", tests$var_test$statistic)),
        tags$p(tags$strong("p-value :"), sprintf("%.4e", tests$var_test$p_value)),
        tags$p(tests$var_test$conclusion, style = "margin-bottom: 0;")
      )
    )
  })
  
  
  # --- Tableau de statistiques comparées ---
  
  output$comparison_stats_table <- renderTable({
    req(sim_data$current, sim_data$history)
    
    stats_current <- stat_desc(sim_data$current)
    stats_previous <- stat_desc(sim_data$history)
    
    data.frame(
      Statistique = stats_current$Statistique,
      Actuelle = sprintf("%.4f", stats_current$Valeur),
      Précédente = sprintf("%.4f", stats_previous$Valeur),
      Différence = sprintf("%.4f", stats_current$Valeur - stats_previous$Valeur)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
}
