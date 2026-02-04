#ui.R

ui <- page_navbar(
  title = "Monte Carlo Engine",
  id = "main_nav",
  
  # définition du thème
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#2c3e50",
    primary = "#34495e",
    secondary = "#7f8c8d",
    base_font = font_google("IBM Plex Sans"),
    heading_font = font_google("IBM Plex Sans"),
    code_font = font_google("IBM Plex Mono"),
    font_scale = 0.95
  ),
  
  # CSS 
  header = tagList(
    tags$head(
      tags$style(HTML("
        /* Design */
        body {
          background: #f8f9fa;
        }
        
        .card {
          border: 1px solid #dee2e6;
          box-shadow: none;
          border-radius: 2px;
        }
        
        .card-header {
          background: #ffffff;
          border-bottom: 2px solid #34495e;
          font-weight: 600;
          font-size: 0.9rem;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          color: #2c3e50;
        }
        
        /* Éditeur de loi - Zone centrale */
        .law-editor {
          background: #ffffff;
          border: 2px solid #34495e;
          padding: 30px;
          border-radius: 2px;
          margin: 20px 0;
        }
        
        .law-editor-title {
          font-size: 1.4rem;
          font-weight: 600;
          color: #2c3e50;
          margin-bottom: 20px;
          border-bottom: 2px solid #34495e;
          padding-bottom: 10px;
        }
        
        /* Preview mathématique */
        .math-preview {
          background: #f8f9fa;
          border-left: 3px solid #34495e;
          padding: 20px;
          margin: 15px 0;
          font-family: 'IBM Plex Mono', monospace;
        }
        
        /* Graphique preview */
        .preview-plot {
          border: 1px solid #dee2e6;
          background: #ffffff;
          padding: 10px;
        }
        
        /* Boutons sobres */
        .btn-primary {
          background: #34495e;
          border: none;
          border-radius: 2px;
          font-weight: 600;
          letter-spacing: 0.3px;
          padding: 12px 30px;
        }
        
        .btn-primary:hover {
          background: #2c3e50;
        }
        
        .btn-outline-secondary {
          border: 2px solid #7f8c8d;
          border-radius: 2px;
          font-weight: 600;
        }
        
        /* Statistiques - Design minimal */
        .stat-box {
          text-align: center;
          padding: 20px;
          background: #ffffff;
          border: 1px solid #dee2e6;
        }
        
        .stat-value {
          font-size: 2rem;
          font-weight: 600;
          color: #2c3e50;
          font-family: 'IBM Plex Mono', monospace;
        }
        
        .stat-label {
          font-size: 0.75rem;
          color: #7f8c8d;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          margin-top: 5px;
        }
        
        /* Alertes scientifiques */
        .alert {
          border-radius: 2px;
          border-left: 3px solid;
        }
        
        .alert-info {
          background: #f8f9fa;
          border-left-color: #34495e;
          color: #2c3e50;
        }
        
        /* Zone désactivée avant simulation */
        .results-disabled {
          opacity: 0.3;
          pointer-events: none;
          position: relative;
        }
        
        .results-disabled::before {
          content: 'Lancez une simulation pour voir les résultats';
          position: absolute;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          background: rgba(52, 73, 94, 0.95);
          color: white;
          padding: 15px 30px;
          border-radius: 2px;
          font-weight: 600;
          z-index: 1000;
          white-space: nowrap;
        }
      "))
    ),
    withMathJax(),
    shinyjs::useShinyjs()
  ),
  
  # ========================================================================
  # ONGLET 1 : DÉFINIR LA LOI (PAGE PRINCIPALE)
  # ========================================================================
  
  nav_panel(
    title = "Définir la loi",
    value = "define",
    
    layout_columns(
      col_widths = c(12),
      
      # l'éditeur de loi
      card(
        class = "law-editor",
        
        tags$div(
          class = "law-editor-title",
          "Définition de la loi de probabilité"
        ),
        
        # choix du type
        layout_columns(
          col_widths = c(6, 6),
          
          selectInput(
            "dist_type", 
            tags$div(
              tags$strong("Type de distribution"),
              style = "font-size: 0.9rem; color: #2c3e50;"
            ),
            choices = c(
              "Loi Normale" = "norm", 
              "Loi Uniforme" = "unif", 
              "Loi Exponentielle" = "exp",
              "Loi personnalisée" = "custom"
            ),
            selected = "norm"
          ),
          
          numericInput(
            "n_sim", 
            tags$div(
              tags$strong("Nombre de simulations"),
              style = "font-size: 0.9rem; color: #2c3e50;"
            ),
            value = 10000, 
            min = 1000, 
            max = 100000,
            step = 1000
          )
        ),
        
        tags$hr(style = "border-color: #dee2e6; margin: 25px 0;"),
        
        # les paramètres selon la loi chosie
        
        # Normale
        conditionalPanel(
          condition = "input.dist_type == 'norm'",
          
          tags$h5("Paramètres de la loi Normale", style = "color: #34495e; margin-bottom: 20px;"),
          
          layout_columns(
            col_widths = c(6, 6),
            
            numericInput("mean", "Moyenne (μ)", value = 0, min = NA, max = NA),
            numericInput("sd", "Écart-type (σ)", value = 1, min = 0.01, step = 0.1)
          ),
          
          tags$div(
            class = "math-preview",
            withMathJax("$$X \\sim \\mathcal{N}(\\mu, \\sigma^2)$$"),
            withMathJax("$$f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$$")
          )
        ),
        
        # Uniforme
        conditionalPanel(
          condition = "input.dist_type == 'unif'",
          
          tags$h5("Paramètres de la loi Uniforme", style = "color: #34495e; margin-bottom: 20px;"),
          
          layout_columns(
            col_widths = c(6, 6),
            
            numericInput("min", "Minimum (a)", value = 0, min = NA, max = NA),
            numericInput("max", "Maximum (b)", value = 1, min = NA, max = NA)
          ),
          
          tags$div(
            class = "math-preview",
            withMathJax("$$X \\sim \\mathcal{U}(a, b)$$"),
            withMathJax("$$f(x) = \\frac{1}{b-a} \\text{ pour } x \\in [a,b]$$")
          )
        ),
        
        # Exponentielle
        conditionalPanel(
          condition = "input.dist_type == 'exp'",
          
          tags$h5("Paramètres de la loi Exponentielle", style = "color: #34495e; margin-bottom: 20px;"),
          
          numericInput("rate", "Taux (λ)", value = 1, min = 0.1, step = 0.1),
          
          tags$div(
            class = "math-preview",
            withMathJax("$$X \\sim \\text{Exp}(\\lambda)$$"),
            withMathJax("$$f(x) = \\lambda e^{-\\lambda x} \\text{ pour } x \\geq 0$$")
          )
        ),
        
        # Loi personnalisée
        conditionalPanel(
          condition = "input.dist_type == 'custom'",
          
          tags$h5("Définition d'une loi personnalisée", style = "color: #34495e; margin-bottom: 20px;"),
          
          radioButtons(
            "custom_method", 
            "Méthode de génération",
            choices = c(
              "Par fonction de densité f(x)" = "density",
              "Par fonction quantile F⁻¹(u)" = "quantile"
            ),
            selected = "density",
            inline = TRUE
          ),
          
          # --- méthode si on choisit la densité ---
          conditionalPanel(
            condition = "input.custom_method == 'density'",
            
            tags$div(
              style = "margin: 20px 0;",
              
              textInput(
                "math_formula", 
                tags$div(
                  tags$strong("Fonction de densité f(x)"),
                  tags$span(" — Utilisez: x, pi, e, sqrt(), exp(), log(), sin(), cos()", 
                            style = "font-size: 0.8rem; color: #7f8c8d; font-weight: normal;")
                ),
                value = "exp(-x^2/2) / sqrt(2*pi)",
                placeholder = "Ex: (1/pi) * 1/(1 + x^2)"
              ),
              
              layout_columns(
                col_widths = c(6, 6),
                numericInput("x_min", "Support : minimum", value = -5, min = NA, max = NA),
                numericInput("x_max", "Support : maximum", value = 5, min = NA, max = NA)
              )
            ),
            
            # la visualisation latex
            tags$div(
              class = "math-preview",
              tags$strong("Aperçu mathématique :"),
              uiOutput("latex_preview")
            ),
            
            # visu de la densité de la loi
            tags$div(
              class = "preview-plot",
              tags$strong("Aperçu de la densité", style = "display: block; margin-bottom: 10px; color: #2c3e50;"),
              plotOutput("density_preview", height = "250px")
            ),
            
            # exemples de formules
            tags$details(
              tags$summary("Voir des exemples de formules", style = "cursor: pointer; color: #34495e; font-weight: 600; margin-top: 15px;"),
              tags$div(
                style = "margin-top: 10px; padding: 15px; background: #f8f9fa; border-left: 3px solid #7f8c8d;",
                tags$p(tags$code("exp(-x^2/2) / sqrt(2*pi)"), " → Normale standard"),
                tags$p(tags$code("(1/pi) * 1/(1 + x^2)"), " → Distribution de Cauchy"),
                tags$p(tags$code("2*x"), " sur [0,1] → Distribution triangulaire"),
                tags$p(tags$code("exp(-abs(x))"), " → Distribution de Laplace"),
                tags$p(tags$code("3*x^2"), " sur [0,1] → Distribution Beta-like"),
                
              )
            )
          ),
          
          # --- méthode si on choisit la fonction quantile ---
          conditionalPanel(
            condition = "input.custom_method == 'quantile'",
            
            textInput(
              "quantile_formula", 
              tags$div(
                tags$strong("Fonction quantile F⁻¹(u)"),
                tags$span(" — u ∈ [0,1] uniforme", 
                          style = "font-size: 0.8rem; color: #7f8c8d; font-weight: normal;")
              ),
              value = "qnorm(u, mean = 0, sd = 1)",
              placeholder = "Ex: -log(1-u) pour exponentielle"
            ),
            
            tags$div(
              class = "math-preview",
              tags$strong("Aperçu :"),
              tags$p("Cette méthode génère X = F⁻¹(U) où U ~ Uniforme(0,1)", style = "margin-top: 10px;")
            ),
            
            tags$details(
              tags$summary("Voir des exemples", style = "cursor: pointer; color: #34495e; font-weight: 600; margin-top: 15px;"),
              tags$div(
                style = "margin-top: 10px; padding: 15px; background: #f8f9fa; border-left: 3px solid #7f8c8d;",
                tags$p(tags$code("qnorm(u, 0, 1)"), " → Normale standard"),
                tags$p(tags$code("-log(1-u)"), " → Exponentielle(1)"),
                tags$p(tags$code("tan(pi*(u - 0.5))"), " → Cauchy"),
                tags$p(tags$code("u^(1/3)"), " → Distribution Beta-like")
              )
            )
          )
        ),
        
        tags$hr(style = "border-color: #dee2e6; margin: 30px 0;"),
        
        # boutons pour lancer la simulation ou réinitialiser
        layout_columns(
          col_widths = c(8, 4),
          
          actionButton(
            "simulate", 
            "Lancer la simulation Monte Carlo",
            class = "btn-primary w-100",
            style = "height: 50px; font-size: 1rem;"
          ),
          
          actionButton(
            "reset",
            "Réinitialiser",
            class = "btn-outline-secondary w-100",
            style = "height: 50px;"
          )
        )
      )
    )
  ),
  
  # ========================================================================
  # ONGLET 2 : RÉSULTATS (désactivé tant qu'on n'a pas simulé)
  # ========================================================================
  
  nav_panel(
    title = "Résultats",
    value = "results",
    
    # Message si pas encore de simulation
    conditionalPanel(
      condition = "output.has_simulation == false",
      
      tags$div(
        style = "text-align: center; margin-top: 100px;",
        tags$h3("Aucune simulation disponible", style = "color: #7f8c8d;"),
        tags$p("Retournez à l'onglet 'Définir la loi' pour lancer une simulation.", 
               style = "color: #95a5a6; font-size: 1.1rem;")
      )
    ),
    
    # Résultats disponibles
    conditionalPanel(
      condition = "output.has_simulation == true",
      
      # Statistiques clés
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        
        tags$div(
          class = "stat-box",
          tags$div(class = "stat-value", textOutput("stat_mean")),
          tags$div(class = "stat-label", "Moyenne")
        ),
        
        tags$div(
          class = "stat-box",
          tags$div(class = "stat-value", textOutput("stat_median")),
          tags$div(class = "stat-label", "Médiane")
        ),
        
        tags$div(
          class = "stat-box",
          tags$div(class = "stat-value", textOutput("stat_sd")),
          tags$div(class = "stat-label", "Écart-type")
        ),
        
        tags$div(
          class = "stat-box",
          tags$div(class = "stat-value", textOutput("stat_n")),
          tags$div(class = "stat-label", "Échantillon")
        )
      ),
      
      # Graphiques principaux
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Distribution empirique"),
          card_body(
            plotly::plotlyOutput("hist_plot", height = "400px")
          )
        ),
        
        card(
          card_header("QQ-Plot (test de normalité)"),
          card_body(
            plotOutput("qq_plot", height = "400px")
          )
        )
      ),
      
      # Analyses statistiques
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Statistiques descriptives"),
          card_body(
            tableOutput("stats_table")
          )
        ),
        
        card(
          card_header("Test de normalité (Shapiro-Wilk)"),
          card_body(
            uiOutput("normality_test")
          )
        )
      )
    )
  ),
  
  # ========================================================================
  # ONGLET 3 : COMPARAISON
  # ========================================================================
  
  nav_panel(
    title = "Comparaison",
    value = "comparison",
    
    # Statut
    uiOutput("comparison_status"),
    
    # Comparaisons visuelles et tests
    conditionalPanel(
      condition = "output.has_comparison == true",
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Distributions comparées"),
          card_body(
            plotly::plotlyOutput("comparison_hist", height = "400px")
          )
        ),
        
        card(
          card_header("Boxplots comparatifs"),
          card_body(
            plotOutput("comparison_boxplot", height = "400px")
          )
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Tests statistiques"),
          card_body(
            uiOutput("comparison_tests")
          )
        ),
        
        card(
          card_header("Tableau comparatif"),
          card_body(
            tableOutput("comparison_stats_table")
          )
        )
      )
    )
  ),
  
  # ========================================================================
  # ONGLET 4 : DOCUMENTATION
  # ========================================================================
  
  nav_panel(
    title = "Documentation",
    value = "about",
    
    card(
      card_header("Monte Carlo Engine"),
      card_body(
        tags$h4("Simulation Monte Carlo pour distributions personnalisées"),
        
        tags$p(
          "Cette application permet de définir des lois de probabilité (prédéfinies ou personnalisées) ",
          "et de générer des échantillons aléatoires selon ces lois, puis d'analyser leurs propriétés statistiques."
        ),
        
        tags$h5("Workflow"),
        tags$ol(
          tags$li(tags$strong("Définir"), " : Choisir ou créer votre loi de probabilité"),
          tags$li(tags$strong("Simuler"), " : Générer N échantillons aléatoires"),
          tags$li(tags$strong("Analyser"), " : Visualiser et tester les propriétés statistiques"),
          tags$li(tags$strong("Comparer"), " : Comparer deux simulations différentes")
        ),
        
        tags$h5("Méthodes de génération"),
        
        tags$p(tags$strong("Par densité f(x)"), " : Utilise la méthode du rejet (rejection sampling)"),
        tags$ul(
          tags$li("Définissez f(x) et le support [a, b]"),
          tags$li("L'algorithme génère des points (x, y) et accepte ceux sous la courbe")
        ),
        
        tags$p(tags$strong("Par fonction quantile F⁻¹(u)"), " : Méthode de l'inversion"),
        tags$ul(
          tags$li("Génère u ~ Uniforme(0,1)"),
          tags$li("Calcule x = F⁻¹(u)")
        ),
        
        tags$h5("Tests statistiques disponibles"),
        tags$ul(
          tags$li(tags$strong("Shapiro-Wilk"), " : Test de normalité"),
          tags$li(tags$strong("Student t-test"), " : Comparaison de moyennes"),
          tags$li(tags$strong("Wilcoxon"), " : Test non-paramétrique"),
          tags$li(tags$strong("Test F"), " : Comparaison de variances")
        )
      )
    )
  )
)