# app.R
library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Titulo principal */
      .app-title { color: #0B2D5B; font-size: 2.2rem; font-weight: 700; margin-top: 0; }

      /* Skip link */
      .skip-link {
        position: absolute; left: -999px; top: 0;
        background: #000; color: #fff; padding: 10px 14px;
        z-index: 10000; border-radius: 6px;
      }
      .skip-link:focus { left: 10px; top: 10px; outline: 3px solid #ffd400; }

      /* Focus outlines visibles */
      :focus { outline: 3px solid #0b5fff !important; outline-offset: 2px; }
      button:focus, a:focus, input:focus, select:focus, textarea:focus { box-shadow: none !important; }

      /* Screen-reader only */
      .sr-only {
        position: absolute !important;
        width: 1px; height: 1px; padding: 0; margin: -1px;
        overflow: hidden; clip: rect(0,0,0,0); white-space: nowrap; border: 0;
      }

      /* Modo accesible: alto contraste + letra grande */
      body.a11y { font-size: 18px; line-height: 1.6; background: #FFFFFF; color: #000000; }
      body.a11y .btn, body.a11y input, body.a11y select, body.a11y textarea { font-size: 18px; }
      body.a11y .btn { border-width: 2px; font-weight: bold; }
      body.a11y a { color: #0000EE; text-decoration: underline; font-weight: bold; }
      body.a11y .well { background: #FFFFFF; border: 3px solid #000000; }
      body.a11y .tabbable > .nav > li > a { font-size: 18px; color: #000000; font-weight: bold; }
      body.a11y table { font-size: 16px; color: #000000; }
      body.a11y .shiny-output-error { color: #D32F2F; font-weight: 700; }

      /* Footer fijo */
      #footer-fixed {
        position: fixed; bottom: 0; left: 0; width: 100%;
        background: #f9f9f9; padding: 8px; text-align: center;
        font-size: 0.8em; color: #555; border-top: 1px solid #ccc; z-index: 9999;
      }
      body { padding-bottom: 60px; }

      /* Reducir movimiento */
      @media (prefers-reduced-motion: reduce) {
        * { animation-duration: 0.01ms !important; transition-duration: 0.01ms !important; }
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleA11y', function(on) {
        document.body.classList.toggle('a11y', !!on);
      });
    "))
  ),

  # Skip link y cabecera semantica
  tags$a(class = "skip-link", href = "#main-content", "Saltar al contenido principal"),
  tags$header(
    tags$h1("ECONOMETRIA: Modelos de Regresion", class = "app-title"),
    tags$p(id = "app-desc", class = "sr-only",
           "Aplicacion para cargar datos, ajustar modelos de regresion y analizar supuestos.")
  ),

  # Modo accesible + footer
  checkboxInput("a11y_mode", "Modo accesible (alto contraste y letra grande)", value = FALSE, width = "100%"),
  tags$div(id = "footer-fixed", HTML("&copy; 2026 Oscar Trull &middot; Aplicacion Shiny")),
  tags$hr(),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sube un CSV", accept = c(".csv")),
      checkboxInput("header", "El CSV tiene cabecera", TRUE),
      radioButtons("sep", "Separador",
                   choices = c(Coma = ",", PuntoyComa = ";", Tab = "\t"),
                   selected = ","),
      tags$hr(),
      checkboxInput("use_example", "Usar dataset de ejemplo (mtcars)", value = FALSE),
      tags$hr(),
      
      uiOutput("var_ui"),
      
      selectInput("model_type", "Tipo de modelo",
                  choices = c(
                    "Regresión lineal (lm)" = "lm",
                    "Cochrane–Orcutt AR(1) (lm)" = "co",
                    "Ridge (cv.glmnet)" = "ridge",
                    "Lasso (cv.glmnet)" = "lasso",
                    "Elastic Net (cv.glmnet)" = "enet",
                    "Regresión logística (glm binomial)" = "logit"
                  ),
                  selected = "lm"),
      
      uiOutput("reg_options_ui"),
      
      checkboxInput("scale_num", "Estandarizar variables numéricas (z-score)", value = FALSE),
      checkboxInput("rm_na", "Eliminar filas con NA", value = TRUE),
      
      actionButton("fit", "Ajustar modelo", class = "btn-primary"),
      tags$hr(),
      
      h4("Predicción"),
      sliderInput("train_pct", "Porcentaje entrenamiento", min = 50, max = 90, value = 80, step = 5),
      actionButton("split_fit", "Entrenar / Test", class = "btn-success"),
      tags$hr(),
      
      downloadButton("download_model_summary", "Descargar resumen (txt)"),
      downloadButton("download_predictions", "Descargar predicciones (csv)")
    ),
    
    mainPanel(id = "main-content",
      tabsetPanel(
        tabPanel("Datos",
                 h4("Vista previa"),
                 tableOutput("head_tbl"),
                 tags$hr(),
                 h4("Grafico de dispersion (variables seleccionadas)"),
                 helpText("Matriz de dispersion entre Y y los predictores X ..."),
                 fluidRow(
                   column(4,
                          radioButtons("scatter_color_by", "Colorear puntos por",
                                       choices = c("Ninguno"="none","Variable categorica"="cat"),
                                       selected = "none", inline = TRUE)
                   ),
                   column(4,
                          conditionalPanel(condition = "input.scatter_color_by == 'cat'",
                                           uiOutput("scatter_color_var_ui"))
                   ),
                   column(4,
                          checkboxInput("scatter_smooth",
                                        "Anadir linea de tendencia (loess)", value = TRUE)
                   )
                 ),
                 tags$div(role="img", `aria-label`="Matriz de dispersion entre variables seleccionadas", plotOutput("scatter_matrix_plot", height = "600px")),
                 tags$hr(),
                 h4("Resumen estadistico"),
                 verbatimTextOutput("summary_txt")
        ),
        
        tabPanel("Modelo",
                 h4("Fórmula"),
                 verbatimTextOutput("formula_txt"),
                 h4("Resumen del modelo"),
                 verbatimTextOutput("model_txt")
        ),
        
        tabPanel("Multicolinealidad",
                 wellPanel(
                   h4("Criterio de corte para VIF"),
                   fluidRow(
                     column(6, radioButtons("vif_criterion", "Umbral de referencia",
                                            choices = c("VIF > 10" = "10", "VIF > 5" = "5",
                                                        "VIF > 2.5" = "2.5", "GVIF^(1/2Df) > 1.96" = "1.96",
                                                        "Personalizado" = "custom"), selected = "10")),
                     column(6, conditionalPanel(
                       condition = "input.vif_criterion == 'custom'",
                       numericInput("vif_custom", "Valor personalizado",
                                    value = 10, min = 1, step = 0.1)))
                   )),
                 
                 h4("VIF / GVIF"),
                 verbatimTextOutput("vif_note"),
                 tableOutput("vif_tbl"),
                 tags$hr(),
                 
                 h4("Matriz de correlación entre predictores numéricos"),
                 verbatimTextOutput("corr_note"),
                 tableOutput("corr_matrix_tbl"),   
                   
                 tags$div(role="img", `aria-label`="Mapa de calor de correlaciones entre predictores", plotOutput("corr_plot")),
                 tags$hr(),
                 h4("Matriz inversa de correlación (= VIF en diagonal)"),
                 verbatimTextOutput("inv_corr_note"),
                 tableOutput("inv_corr_tbl"),
                 tags$hr(),
                 
                 h4("Índice de condición (matriz de diseño)"),
                 tableOutput("cond_tbl"),
                 tags$hr(),
                 
                 h4("Aliasing / singularidades"),
                 verbatimTextOutput("alias_txt"),
                 tags$hr(),
                 
                 h4("Sugerencias automáticas"),
                 verbatimTextOutput("mc_suggest_txt")
        ),
        
        tabPanel("Normalidad",
                 h4("Tests de normalidad de los residuos (solo lm / Cochrane-Orcutt)"),
                 verbatimTextOutput("norm_note"),
                 tags$hr(),
                 
                 h4("Shapiro-Wilk"),
                 helpText("H0: residuos normales. p < 0.05 sugiere no normalidad. Solo para n <= 5000."),
                 tableOutput("norm_sw_tbl"),
                 tags$hr(),
                 
                 h4("Kolmogorov-Smirnov (Lilliefors)"),
                 helpText("Requiere paquete 'nortest'. Valido para muestras grandes."),
                 tableOutput("norm_ks_tbl"),
                 tags$hr(),
                 
                 h4("Jarque-Bera"),
                 helpText("Requiere paquete 'tseries'. Basado en asimetria y curtosis; habitual en econometria."),
                 tableOutput("norm_jb_tbl"),
                 tags$hr(),
                 
                 h4("Grafico de probabilidad normal (Q-Q plot)"),
                 helpText("Los puntos deben seguir la linea diagonal si los residuos son normales."),
                 tags$div(role="img", `aria-label`="Grafico Q-Q de normalidad de residuos", plotOutput("norm_qq_plot", height = "420px")),
                 tags$hr(),
                 
                 h4("Histograma de residuos con curva normal"),
                 tags$div(role="img", `aria-label`="Histograma de residuos con curva normal superpuesta", plotOutput("norm_hist_plot", height = "360px"))
        ),
        
        tabPanel("Gráficos",
                 h4("Diagnóstico / Ajuste"),
                 tags$div(role="img", `aria-label`="Grafico de diagnostico del modelo 1", plotOutput("plot1")),
                 tags$div(role="img", `aria-label`="Grafico de diagnostico del modelo 2", plotOutput("plot2")),
                 tags$hr(),
                 h4("Residuos por variable explicativa"),
                 uiOutput("resid_var_ui"),
                 tags$div(role="img", `aria-label`="Grafico de residuos frente a variable explicativa", plotOutput("resid_var_plot"))
        ),
                 
        tabPanel("Heterocedasticidad",
                 h4("Pruebas (solo lm / Cochrane–Orcutt)"),
                 verbatimTextOutput("hetero_note"),
                 tableOutput("bp_tbl"),
                 tableOutput("white_tbl"),
                 tableOutput("ncv_tbl"),
                 tags$hr(),
                 h4("Gráficos"),
                 tags$div(role="img", `aria-label`="Grafico Scale-Location para detectar heterocedasticidad", plotOutput("scale_location_plot")),
                 tags$div(role="img", `aria-label`="Grafico de residuos frente a valores ajustados", plotOutput("resid_fitted_plot")),
                 
                 h4("Regresión de residuos² sobre predictores"),
                 helpText("Ajusta lm(e² ~ X_seleccionadas) y muestra la tabla ANOVA. Un predictor significativo indica que la varianza del error depende de él (heterocedasticidad)."),
                 uiOutput("resid2_vars_ui"),
                 actionButton("fit_resid2", "Ajustar modelo de residuos²", class = "btn-warning"),
                 tags$br(), tags$br(),
                 h5("Fórmula ajustada"),
                 verbatimTextOutput("resid2_formula_txt"),
                 h5("Tabla ANOVA"),
                 tableOutput("resid2_anova_tbl"),
                 h5("Resumen del modelo (R², F-global)"),
                 verbatimTextOutput("resid2_summary_txt"),
                 tags$hr()
        ),
        
        tabPanel("Autocorrelación",
                 h4("Pruebas (solo lm / Cochrane–Orcutt)"),
                 verbatimTextOutput("auto_note"),
                 uiOutput("auto_options_ui"),
                 tags$div(role="img", `aria-label`="Grafico de residuos frente a tiempo o indice", plotOutput("resid_time_plot")),
                 
                 tableOutput("dw_tbl"),
                 tableOutput("bg_tbl"),
                 tableOutput("lb_tbl"),
                 tags$hr(),
                 h4("ACF / PACF de residuos"),
                 tags$div(role="img", `aria-label`="Grafico de autocorrelacion ACF de residuos", plotOutput("acf_plot")),
                 tags$div(role="img", `aria-label`="Grafico de autocorrelacion parcial PACF de residuos", plotOutput("pacf_plot"))
        ),
        tabPanel("Métricas / Test",
                 h4("Métricas (train/test)"),
                 tableOutput("metrics_tbl"),
                 h4("Matriz de confusión (si logística)"),
                 tableOutput("cm_tbl")
        ),
        
        
         tabPanel("Restricciones",
                 h4("Test de hipotesis sobre restricciones lineales"),
                 helpText(
                   tags$b("Sintaxis:"), " usa ", tags$code("b[nombre]"),
                   " para referirte a cada coeficiente. Escribe una restriccion por linea.",
                   tags$br(),
                   tags$b("Ejemplos:"),
                   tags$br(),
                   tags$code("b[cyl] = 0"),        " — coeficiente de cyl igual a cero",
                   tags$br(),
                   tags$code("b[cyl] = b[hp]"),    " — igualdad entre coeficientes",
                   tags$br(),
                   tags$code("b[cyl] - 2*b[hp] = 0"), " — combinacion lineal",
                   tags$br(),
                   tags$code("b[wt] = 2"),          " — coeficiente igual a constante",
                   tags$br(),
                   tags$code("b[(Intercept)] = 0"), " — para el intercepto"
                 ),
                 
                 tags$hr(),
                 
                 fluidRow(
                   column(8,
                     textAreaInput("hyp_text",
                                   label    = "Restricciones (una por linea)",
                                   value    = "",
                                   rows     = 5,
                                   width    = "100%",
                                   placeholder = "Ej:\nb[cyl] = 0\nb[cyl] - 2*b[hp] = 0\nb[wt] = 2")
                   ),
                   column(4,
                     br(),
                     radioButtons("hyp_test_type", "Tipo de test",
                                  choices  = c("F (lm)" = "F", "Chi2 (Wald)" = "Chisq"),
                                  selected = "F"),
                     br(),
                     actionButton("run_hyp", "Ejecutar test",
                                  class = "btn-primary", width = "100%")
                   )
                 ),
                 
                 tags$hr(),
                 
                 h4("Coeficientes del modelo (referencia)"),
                 verbatimTextOutput("hyp_coef_txt"),
                 tags$hr(),
                 
                 h4("Resultado del test"),
                 verbatimTextOutput("hyp_result_txt"),
                 tableOutput("hyp_table_tbl"),
                 tags$hr(),
                 
                 h4("Interpretacion"),
                 verbatimTextOutput("hyp_interp_txt")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ---- Accesibilidad: alternar modo alto contraste / letra grande ----
  observeEvent(input$a11y_mode, {
    session$sendCustomMessage("toggleA11y", isTRUE(input$a11y_mode))
  }, ignoreInit = FALSE)
  
  # ---- cargar datos ----
  raw_data <- reactive({
    if (isTRUE(input$use_example)) return(mtcars)
    req(input$file)
    read.csv(input$file$datapath, header = input$header, sep = input$sep, stringsAsFactors = FALSE)
  })
  
  data_clean <- reactive({
    df <- raw_data()
    if (isTRUE(input$rm_na)) df <- na.omit(df)
    
    for (nm in names(df)) {
      if (is.character(df[[nm]])) df[[nm]] <- as.factor(df[[nm]])
    }
    
    if (isTRUE(input$scale_num)) {
      num_cols <- sapply(df, is.numeric)
      df[num_cols] <- scale(df[num_cols])
    }
    df
  })
  
  # ---- UI dinámica: elegir variables ----
  output$var_ui <- renderUI({
    df <- data_clean()
    cols <- names(df)
    tagList(
      selectInput("y", "Variable respuesta (Y)", choices = cols),
      selectizeInput("x", "Predictores (X)", choices = cols, multiple = TRUE,
                     options = list(placeholder = "Elige uno o varios predictores")),
      checkboxInput(
        "include_intercept",
        "Incluir constante (intercepto)",
        value = TRUE
      )
    )
  })
  
  # ---- opciones dinámicas según modelo ----
  output$reg_options_ui <- renderUI({
    df <- data_clean()
    cols <- names(df)
    
    if (input$model_type %in% c("ridge", "lasso", "enet")) {
      tagList(
        helpText("Modelos penalizados convierten factores a dummies automáticamente (model.matrix)."),
        sliderInput("enet_alpha", "Elastic Net alpha (0=ridge, 1=lasso)",
                    min = 0, max = 1, value = 0.5, step = 0.05),
        checkboxInput("use_lambda_1se", "Usar lambda.1se (más simple/robusto)", value = TRUE)
      )
    } else if (input$model_type == "co") {
      tagList(
        helpText("Cochrane–Orcutt asume errores AR(1). Ordena por una variable temporal/índice."),
        selectInput("order_var", "Variable de orden (tiempo/índice)", choices = cols),
        numericInput("co_max_iter", "Máx. iteraciones", value = 50, min = 5),
        numericInput("co_tol", "Tolerancia", value = 1e-6, min = 1e-10)
      )
    } else {
      NULL
    }
  })
  
  # ---- vista previa datos ----
  output$head_tbl <- renderTable(head(data_clean(), 10))
  output$summary_txt <- renderPrint(summary(data_clean()))
  
  # ---- Scatter matrix ----
  output$scatter_color_var_ui <- renderUI({
    df   <- data_clean()
    vars <- names(df)
    cats <- vars[sapply(df[vars], function(x) is.factor(x) || is.character(x) || length(unique(na.omit(x))) <= 10)]
    if (length(cats) == 0) return(helpText("No hay variables categoricas disponibles."))
    selectInput("scatter_color_var", "Variable de color", choices = cats)
  })
  
  output$scatter_matrix_plot <- renderPlot({
    req(input$y)
    df   <- data_clean()
    xvars <- input$x
    validate(need(length(xvars) > 0, "Selecciona al menos un predictor (X) en el panel lateral."))
    
    all_vars <- unique(c(input$y, xvars))
    # Mantener solo numericas + variable de color si aplica
    num_vars <- all_vars[sapply(df[all_vars], function(v) is.numeric(v))]
    validate(need(length(num_vars) >= 2, "Necesitas al menos 2 variables numericas para el grafico."))
    
    # Variable de color
    col_vec <- rep("#2196F3", nrow(df))
    pch_vec <- rep(16, nrow(df))
    legend_needed <- FALSE
    if (!is.null(input$scatter_color_by) && input$scatter_color_by == "cat" &&
        !is.null(input$scatter_color_var) && input$scatter_color_var %in% names(df)) {
      cv      <- as.factor(df[[input$scatter_color_var]])
      paleta  <- c("#E53935","#2196F3","#43A047","#FB8C00","#8E24AA",
                   "#00ACC1","#F4511E","#6D4C41","#546E7A","#039BE5")
      col_vec <- paleta[(as.integer(cv) - 1) %% length(paleta) + 1]
      pch_vec <- (as.integer(cv) - 1) %% 5 + 16
      legend_needed <- TRUE
      levels_cv <- levels(cv)
      cols_cv   <- paleta[(seq_along(levels_cv) - 1) %% length(paleta) + 1]
    }
    
    n   <- length(num_vars)
    add_smooth <- isTRUE(input$scatter_smooth)
    
    op <- par(mfrow = c(n, n),
              mar   = c(3, 3, 2, 1),
              oma   = c(0, 0, 3, 0))
    on.exit(par(op), add = TRUE)
    
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        xi <- df[[num_vars[j]]]
        yi <- df[[num_vars[i]]]
        
        if (i == j) {
          # Diagonal: nombre de variable + histograma
          hist(xi, col = "#BBDEFB", border = "white",
               main = "", xlab = "", ylab = "", axes = FALSE)
          box()
          mtext(num_vars[i], side = 3, line = 0.2, cex = 0.85, font = 2)
        } else {
          plot(xi, yi,
               pch  = pch_vec,
               col  = adjustcolor(col_vec, alpha.f = 0.7),
               cex  = 0.75,
               xlab = "", ylab = "",
               main = "")
          # Etiquetas de ejes solo en bordes
          if (i == n) mtext(num_vars[j], side = 1, line = 2, cex = 0.7)
          if (j == 1) mtext(num_vars[i], side = 2, line = 2, cex = 0.7)
          
          # Linea de tendencia loess
          if (add_smooth) {
            ok <- complete.cases(xi, yi)
            if (sum(ok) >= 5) {
              tryCatch({
                lo <- loess(yi[ok] ~ xi[ok], span = 0.75)
                xs <- seq(min(xi[ok]), max(xi[ok]), length.out = 100)
                lines(xs, predict(lo, xs), col = "#E53935", lwd = 1.5)
              }, error = function(e) NULL)
            }
          }
          
          # Correlacion en esquina superior
          r_val <- tryCatch(cor(xi, yi, use = "complete.obs"), error = function(e) NA)
          if (!is.na(r_val)) {
            col_r <- if (abs(r_val) > 0.7) "#B71C1C" else if (abs(r_val) > 0.4) "#E65100" else "#555"
            legend("topright", legend = paste0("r=", round(r_val, 2)),
                   bty = "n", cex = 0.75, text.col = col_r)
          }
        }
      }
    }
    
    mtext("Matriz de dispersion", outer = TRUE, cex = 1.1, font = 2, line = 1)
    
    if (legend_needed) {
      par(fig = c(0, 1, 0, 1), oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
      legend("bottom", legend = levels_cv, col = cols_cv,
             pch = 16, horiz = TRUE, bty = "n", cex = 0.8,
             title = input$scatter_color_var)
    }
  })
  
  # ---- Scatter matrix ----
  output$scatter_color_var_ui <- renderUI({
    df   <- data_clean()
    vars <- names(df)
    cats <- vars[sapply(df[vars], function(x) is.factor(x) || is.character(x) || length(unique(na.omit(x))) <= 10)]
    if (length(cats) == 0) return(helpText("No hay variables categoricas disponibles."))
    selectInput("scatter_color_var", "Variable de color", choices = cats)
  })
  
  output$scatter_matrix_plot <- renderPlot({
    req(input$y)
    df   <- data_clean()
    xvars <- input$x
    validate(need(length(xvars) > 0, "Selecciona al menos un predictor (X) en el panel lateral."))
    
    all_vars <- unique(c(input$y, xvars))
    # Mantener solo numericas + variable de color si aplica
    num_vars <- all_vars[sapply(df[all_vars], function(v) is.numeric(v))]
    validate(need(length(num_vars) >= 2, "Necesitas al menos 2 variables numericas para el grafico."))
    
    # Variable de color
    col_vec <- rep("#2196F3", nrow(df))
    pch_vec <- rep(16, nrow(df))
    legend_needed <- FALSE
    if (!is.null(input$scatter_color_by) && input$scatter_color_by == "cat" &&
        !is.null(input$scatter_color_var) && input$scatter_color_var %in% names(df)) {
      cv      <- as.factor(df[[input$scatter_color_var]])
      paleta  <- c("#E53935","#2196F3","#43A047","#FB8C00","#8E24AA",
                   "#00ACC1","#F4511E","#6D4C41","#546E7A","#039BE5")
      col_vec <- paleta[(as.integer(cv) - 1) %% length(paleta) + 1]
      pch_vec <- (as.integer(cv) - 1) %% 5 + 16
      legend_needed <- TRUE
      levels_cv <- levels(cv)
      cols_cv   <- paleta[(seq_along(levels_cv) - 1) %% length(paleta) + 1]
    }
    
    n   <- length(num_vars)
    add_smooth <- isTRUE(input$scatter_smooth)
    
    op <- par(mfrow = c(n, n),
              mar   = c(3, 3, 2, 1),
              oma   = c(0, 0, 3, 0))
    on.exit(par(op), add = TRUE)
    
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        xi <- df[[num_vars[j]]]
        yi <- df[[num_vars[i]]]
        
        if (i == j) {
          # Diagonal: nombre de variable + histograma
          hist(xi, col = "#BBDEFB", border = "white",
               main = "", xlab = "", ylab = "", axes = FALSE)
          box()
          mtext(num_vars[i], side = 3, line = 0.2, cex = 0.85, font = 2)
        } else {
          plot(xi, yi,
               pch  = pch_vec,
               col  = adjustcolor(col_vec, alpha.f = 0.7),
               cex  = 0.75,
               xlab = "", ylab = "",
               main = "")
          # Etiquetas de ejes solo en bordes
          if (i == n) mtext(num_vars[j], side = 1, line = 2, cex = 0.7)
          if (j == 1) mtext(num_vars[i], side = 2, line = 2, cex = 0.7)
          
          # Linea de tendencia loess
          if (add_smooth) {
            ok <- complete.cases(xi, yi)
            if (sum(ok) >= 5) {
              tryCatch({
                lo <- loess(yi[ok] ~ xi[ok], span = 0.75)
                xs <- seq(min(xi[ok]), max(xi[ok]), length.out = 100)
                lines(xs, predict(lo, xs), col = "#E53935", lwd = 1.5)
              }, error = function(e) NULL)
            }
          }
          
          # Correlacion en esquina superior
          r_val <- tryCatch(cor(xi, yi, use = "complete.obs"), error = function(e) NA)
          if (!is.na(r_val)) {
            col_r <- if (abs(r_val) > 0.7) "#B71C1C" else if (abs(r_val) > 0.4) "#E65100" else "#555"
            legend("topright", legend = paste0("r=", round(r_val, 2)),
                   bty = "n", cex = 0.75, text.col = col_r)
          }
        }
      }
    }
    
    mtext("Matriz de dispersion", outer = TRUE, cex = 1.1, font = 2, line = 1)
    
    if (legend_needed) {
      par(fig = c(0, 1, 0, 1), oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
      legend("bottom", legend = levels_cv, col = cols_cv,
             pch = 16, horiz = TRUE, bty = "n", cex = 0.8,
             title = input$scatter_color_var)
    }
  })
  
  # ---- construir fórmula ----
  formula_obj <- reactive({
    
    req(input$y, input$x)
    
    validate(
      need(length(input$x) > 0,
           "Selecciona al menos un predictor (X).")
    )
    
    rhs <- paste(input$x, collapse = " + ")
    
    if (isTRUE(input$include_intercept)) {
      
      as.formula(
        paste(input$y, "~", rhs)
      )
      
    } else {
      
      as.formula(
        paste(input$y, "~ 0 +", rhs)
      )
      
    }
    
  })
  output$formula_txt <- renderPrint(formula_obj())
  
  # ---- helpers multicolinealidad ----
  vif_manual <- function(X) {
    keep <- apply(X, 2, function(z) stats::sd(z) > 0)
    X <- X[, keep, drop = FALSE]
    if (ncol(X) < 2) return(data.frame(term = colnames(X), VIF = rep(NA_real_, ncol(X))))
    
    R <- suppressWarnings(stats::cor(X))
    eig <- eigen(R, symmetric = TRUE)
    tol <- 1e-10
    inv <- eig$vectors %*% diag(ifelse(eig$values > tol, 1 / eig$values, 0), nrow = length(eig$values)) %*% t(eig$vectors)
    data.frame(term = colnames(X), VIF = diag(inv), row.names = NULL)
  }
  
  design_matrix <- reactive({
    req(model_fit())
    m <- model_fit()
    # Solo tiene sentido para lm "real"
    validate(need(inherits(m, "lm"), "La matriz de diseño se calcula para modelos lm."))
    X <- model.matrix(m)
    if ("(Intercept)" %in% colnames(X)) X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
    X
  })
  
  # ---- ajuste del modelo ----
  model_fit <- eventReactive(input$fit, {
    df <- data_clean()
    f <- formula_obj()
    
    if (input$model_type == "lm") {
      return(lm(f, data = df, x = TRUE, y = TRUE, model = TRUE))
    }
    
    if (input$model_type == "logit") {
      return(glm(f, data = df, family = binomial()))
    }
    
    if (input$model_type %in% c("ridge", "lasso", "enet")) {
      if (!requireNamespace("glmnet", quietly = TRUE)) {
        stop("Falta el paquete 'glmnet'. Instálalo con install.packages('glmnet').")
      }
      
      X <- model.matrix(f, data = df)
      if ("(Intercept)" %in% colnames(X)) X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
      y <- df[[input$y]]
      if (is.factor(y)) stop("Para Ridge/Lasso/ENet (continuo), Y debe ser numérica.")
      
      alpha <- if (input$model_type == "ridge") 0 else if (input$model_type == "lasso") 1 else input$enet_alpha
      
      cv <- glmnet::cv.glmnet(x = X, y = y, alpha = alpha, standardize = TRUE)
      lambda_used <- if (isTRUE(input$use_lambda_1se)) cv$lambda.1se else cv$lambda.min
      fit <- glmnet::glmnet(x = X, y = y, alpha = alpha, standardize = TRUE, lambda = lambda_used)
      
      return(list(kind = "glmnet", alpha = alpha, cv = cv, fit = fit, formula = f))
    }
    
    if (input$model_type == "co") {
      ord <- order(df[[input$order_var]])
      dfo <- df[ord, , drop = FALSE]
      
      # 1) paquete orcutt
      if (requireNamespace("orcutt", quietly = TRUE)) {
        m0 <- lm(f, data = dfo, x = TRUE, y = TRUE, model = TRUE)
        co <- orcutt::cochrane.orcutt(m0)
        return(list(kind = "co", base_lm = m0, co = co, order_var = input$order_var))
      }
      
      # 2) fallback iterativo simple
      m <- lm(f, data = dfo, x = TRUE, y = TRUE, model = TRUE)
      rho_old <- 0
      
      for (it in seq_len(input$co_max_iter)) {
        e <- resid(m)
        rho <- sum(e[-1] * e[-length(e)]) / sum(e[-length(e)]^2)
        
        y <- model.response(model.frame(m))
        X <- model.matrix(m)
        
        y_t <- y[-1] - rho * y[-length(y)]
        X_t <- X[-1, , drop = FALSE] - rho * X[-nrow(X), , drop = FALSE]
        
        aux <- as.data.frame(X_t)
        aux$y_t <- y_t
        colnames(aux) <- make.names(colnames(aux))
        frm <- as.formula(paste("y_t ~", paste(colnames(aux)[colnames(aux) != "y_t"], collapse = " + "), "- 1"))
        
        
        
        m <- lm(frm, data = aux)
        
        if (abs(rho - rho_old) < input$co_tol) break
        rho_old <- rho
      }
      
      return(list(kind = "co_fallback", model = m, rho = rho_old, order_var = input$order_var))
    }
    
    stop("Tipo de modelo no soportado.")
  })
  
  # ---- imprimir resumen según tipo ----
  output$model_txt <- renderPrint({
    req(model_fit())
    m <- model_fit()
    
    if (is.list(m) && !is.null(m$kind) && m$kind == "glmnet") {
      cat("Modelo: glmnet\n")
      cat("alpha =", m$alpha, "\n")
      cat("lambda usado =", m$fit$lambda, "\n\n")
      cat("Coeficientes (no-cero):\n")
      cf <- as.matrix(glmnet::coef.glmnet(m$fit))
      nz <- which(abs(cf) > 0)
      print(cf[nz, , drop = FALSE])
      cat("\nCV: lambda.min =", m$cv$lambda.min, " | lambda.1se =", m$cv$lambda.1se, "\n")
      return(invisible(NULL))
    }
    
    if (is.list(m) && !is.null(m$kind) && m$kind == "co") {
      cat("Modelo: Cochrane–Orcutt (paquete orcutt)\n\n")
      print(m$co)
      cat("\nResumen del lm original:\n")
      print(summary(m$base_lm))
      return(invisible(NULL))
    }
    
    if (is.list(m) && !is.null(m$kind) && m$kind == "co_fallback") {
      cat("Modelo: Cochrane–Orcutt (fallback)\n")
      cat("rho estimado =", m$rho, "\n\n")
      print(summary(m$model))
      return(invisible(NULL))
    }
    
    print(summary(m))
  })
  
  # ---- Multicolinealidad ----
  
  # ---- helper: umbral VIF activo ----
  vif_threshold <- reactive({
    cr <- input$vif_criterion
    if (is.null(cr)) return(10)
    if (cr == "custom") {
      v <- input$vif_custom
      if (is.null(v) || !is.finite(v) || v <= 0) return(10)
      return(v)
    }
    as.numeric(cr)
  })
  
  output$vif_note <- renderText({
    req(model_fit())
    if (input$model_type != "lm") return("La multicolinealidad se analiza aquí solo para regresión lineal (lm).")
    thr <- vif_threshold()
    paste0("Criterio activo: VIF (o GVIF^(1/2Df)) > ", thr,
           " se considera problemático y se marca con (**) en la tabla.")
  })
  
  output$vif_tbl <- renderTable({
    req(model_fit())
    validate(need(input$model_type == "lm", "Solo disponible para regresión lineal (lm)."))
    m <- model_fit()
    thr <- vif_threshold()
    
    if (requireNamespace("car", quietly = TRUE)) {
      v <- car::vif(m)
      if (is.matrix(v)) {
        out <- data.frame(
          term = rownames(v),
          GVIF = v[, "GVIF"],
          Df = v[, "Df"],
          `GVIF^(1/(2*Df))` = v[, "GVIF^(1/(2*Df))"],
          row.names = NULL
        )
        rut <- out[order(out$`GVIF^(1/(2*Df))`, decreasing = TRUE), ]
        out$Alerta <- ifelse(out$`GVIF^(1/(2*Df))` > thr, "** ALTO **", "")
        return(out)
        
      } else {
        out <- data.frame(term = names(v), VIF = as.numeric(v), row.names = NULL)
        out <- out[order(out$VIF, decreasing = TRUE), ]
        out$Alerta <- ifelse(out$VIF > thr, "** ALTO **", "")
        return(out)
      }
    }
    
    X <- design_matrix()
    out <- vif_manual(X)
    out[order(out$VIF, decreasing = TRUE), ]
  }, digits = 4)
  
  output$corr_note <- renderText({
    req(model_fit(), input$x)
    if (input$model_type != "lm") return("Correlación mostrada solo para lm (opcional).")
    "Se muestra la correlación solo entre predictores numéricos seleccionados."
  })
  
  # ---- Reactive: matriz de correlación numérica ----
  corr_matrix_react <- reactive({
    req(model_fit(), input$x)
    validate(need(input$model_type == "lm", "Solo disponible para regresión lineal (lm)."))
    df <- data_clean()
    xvars <- input$x
    num_vars <- xvars[sapply(df[xvars], is.numeric)]
    validate(need(length(num_vars) >= 2, "Necesitas al menos 2 predictores numéricos."))
    cor(df[, num_vars, drop = FALSE], use = "pairwise.complete.obs")
  })
  
  output$corr_matrix_tbl <- renderTable({
    R <- corr_matrix_react()
    as.data.frame(round(R, 4))
  }, rownames = TRUE, digits = 4)
  
  output$corr_plot <- renderPlot({
    req(model_fit(), input$x)
    validate(need(input$model_type == "lm", "Solo disponible para regresión lineal (lm)."))
    
    R <- corr_matrix_react()
    op <- par(mar = c(6, 6, 2, 2))
    on.exit(par(op), add = TRUE)
    image(1:ncol(R), 1:nrow(R), t(R)[, nrow(R):1], axes = FALSE,
          col  = colorRampPalette(c("#2166AC", "white", "#B2182B"))(100),
          zlim = c(-1, 1),
          xlab = "", ylab = "", main = "Heatmap de correlación")
    axis(1, at = 1:ncol(R), labels = colnames(R), las = 2, cex.axis = 0.8)
    axis(2, at = 1:nrow(R), labels = rev(rownames(R)), las = 2, cex.axis = 0.8)
    # Añadir valores numéricos
    for (i in seq_len(ncol(R))) {
      for (j in seq_len(nrow(R))) {
        text(i, nrow(R) + 1 - j, labels = round(R[j, i], 2), cex = 0.75)
      }
    }
    box()
  })
  
  # ---- Matriz inversa de correlación ----
  output$inv_corr_note <- renderText({
    req(model_fit(), input$x)
    if (input$model_type != "lm") return("Solo disponible para regresión lineal (lm).")
    thr <- vif_threshold()
    paste0("La diagonal de la matriz inversa equivale al VIF de cada predictor (criterio > ",
           thr, " marcado con **).")
  })
  output$inv_corr_tbl <- renderTable({
    req(model_fit(), input$x)
    validate(need(input$model_type == "lm", "Solo disponible para regresión lineal (lm)."))
    R <- corr_matrix_react()
    
    # Inversión robusta vía eigendescomposición
    eig <- eigen(R, symmetric = TRUE)
    tol <- 1e-10
    inv <- eig$vectors %*%
      diag(ifelse(eig$values > tol, 1 / eig$values, 0), nrow = length(eig$values)) %*%
      t(eig$vectors)
    colnames(inv) <- colnames(R)
    rownames(inv) <- rownames(R)
    
    # Marcar diagonal (VIF) que supere el umbral
    thr <- vif_threshold()
    out <- as.data.frame(round(inv, 4))
    diag_vals <- diag(inv)
    diag_labels <- ifelse(diag_vals > thr,
                          paste0(round(diag_vals, 4), " **"),
                          as.character(round(diag_vals, 4)))
    for (i in seq_len(nrow(out))) out[i, i] <- diag_labels[i]
    out
  }, rownames = TRUE)
  
  
  output$cond_tbl <- renderTable({
    req(model_fit())
    validate(need(input$model_type == "lm", "Solo disponible para regresión lineal (lm)."))
    X <- design_matrix()
    validate(need(ncol(X) >= 2, "No hay suficientes columnas para calcular el índice de condición."))
    
    Xs <- scale(X)
    s <- svd(Xs)$d
    kappa_val <- max(s) / min(s[s > 1e-12])
    cond_index <- max(s) / s
    
    data.frame(
      metric = c("condition_number (kappa)", "max_condition_index"),
      value  = c(kappa_val, max(cond_index, na.rm = TRUE))
    )
  }, digits = 4)
  
  output$alias_txt <- renderPrint({
    req(model_fit())
    validate(need(input$model_type %in% c("lm", "co"), "Aliasing disponible para modelos basados en lm."))
    m <- model_fit()
    base <- if (is.list(m) && !is.null(m$kind) && m$kind == "co") m$base_lm else m
    if (!inherits(base, "lm")) {
      cat("No disponible (modelo no es lm).")
    } else {
      print(alias(base))
    }
  })
  
  output$mc_suggest_txt <- renderText({
    req(model_fit(), input$x)
    if (!(input$model_type %in% c("lm", "co"))) return("Sugerencias disponibles para modelos lineales (lm/CO).")
    
    df <- data_clean()
    f <- formula_obj()
    m0 <- lm(f, data = df)
    
    vifs <- NULL
    if (requireNamespace("car", quietly = TRUE)) {
      v <- car::vif(m0)
      if (is.matrix(v)) {
        vifs <- v[, "GVIF^(1/(2*Df))"]
        names(vifs) <- rownames(v)
      } else {
        vifs <- v
      }
    } else {
      X <- model.matrix(m0)
      if ("(Intercept)" %in% colnames(X)) X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
      R <- suppressWarnings(cor(X))
      if (all(is.finite(R))) {
        invR <- try(solve(R), silent = TRUE)
        if (!inherits(invR, "try-error")) {
          vifs <- diag(invR)
          names(vifs) <- colnames(X)
        }
      }
    }
    
    num_vars <- input$x[sapply(df[input$x], is.numeric)]
    high_pairs <- character(0)
    if (length(num_vars) >= 2) {
      Rn <- cor(df[, num_vars, drop = FALSE], use = "pairwise.complete.obs")
      idx <- which(abs(Rn) > 0.9 & upper.tri(Rn), arr.ind = TRUE)
      if (nrow(idx) > 0) {
        high_pairs <- apply(idx, 1, function(k) {
          paste0(colnames(Rn)[k[2]], " ~ ", rownames(Rn)[k[1]],
                 " (|r|=", round(Rn[k[1], k[2]], 2), ")")
        })
      }
    }
    
    lines <- c()
    if (!is.null(vifs)) {
      thr <- vif_threshold()
      top <- sort(vifs, decreasing = TRUE)
      top <- top[is.finite(top)]
      high <- top[top > thr]
      low  <- top[top <= thr]
      
      if (length(high) > 0) {
        lines <- c(lines, paste0("• Variables con VIF/GVIF > ", thr, " (revisar, combinar o penalizar):"))
        for (nm in names(head(high, 10))) lines <- c(lines, paste0("  ** ", nm, ": ", round(high[[nm]], 2)))
        lines <- c(lines, "")
      }
      if (length(low) > 0) {
        lines <- c(lines, paste0("• Variables con VIF/GVIF <= ", thr, " (sin problema aparente):"))
        for (nm in names(head(low, 10))) lines <- c(lines, paste0("  - ", nm, ": ", round(low[[nm]], 2)))
        lines <- c(lines, "")
      }
    } else {
      lines <- c(lines, "• No se pudo calcular VIF (matriz singular o faltan paquetes).", "")
    }
    
    if (length(high_pairs) > 0) {
      lines <- c(lines, "• Pares con correlación numérica muy alta (|r|>0.9):")
      lines <- c(lines, paste0("  - ", high_pairs))
      lines <- c(lines, "")
    }
    
    lines <- c(lines,
               "Recomendaciones:",
               "• Si hay singularidad/aliasing: elimina una de las variables redundantes (o una dummy de un factor).",
               "• Si hay VIF alto: prueba Ridge/Lasso/Elastic Net o reduce/combina predictores (p.ej., PCA).",
               "• Si hay autocorrelación en residuos (datos temporales): prueba Cochrane–Orcutt.")
    
    paste(lines, collapse = "\n")
  })
  
  
  
  # ---- helpers: obtener un modelo lm base y residuos ----
  lm_base <- reactive({
    req(model_fit())
    m <- model_fit()
    if (inherits(m, "lm")) return(m)
    if (is.list(m) && !is.null(m$kind) && m$kind == "co") return(m$base_lm)
    if (is.list(m) && !is.null(m$kind) && m$kind == "co_fallback") {
      # En fallback tenemos base_lm en la lista si viene de split_results; aquí usamos el modelo transformado si existe
      if (!is.null(m$base_lm) && inherits(m$base_lm, "lm")) return(m$base_lm)
      return(NULL)
    }
    NULL
  })
  
  residuals_base <- reactive({
    mb <- lm_base()
    if (is.null(mb)) return(NULL)
    resid(mb)
  })
  
  fitted_base <- reactive({
    mb <- lm_base()
    if (is.null(mb)) return(NULL)
    fitted(mb)
  })
  
  # ---- Normalidad ----
  output$norm_note <- renderText({
    if (is.null(lm_base())) {
      return("Disponible para modelos lineales (lm) y para el lm base de Cochrane-Orcutt.")
    }
    "Todos los tests contrastan H0: los residuos del modelo siguen una distribucion normal."
  })
  
  output$norm_sw_tbl <- renderTable({
    mb <- lm_base()
    validate(need(!is.null(mb), "Ajusta primero un modelo lm o Cochrane-Orcutt."))
    r <- resid(mb)
    validate(need(length(r) >= 3, "Se necesitan al menos 3 observaciones."))
    if (length(r) > 5000) {
      return(data.frame(Test = "Shapiro-Wilk", Resultado = "No aplicable: n > 5000"))
    }
    sw <- shapiro.test(r)
    data.frame(
      Test      = "Shapiro-Wilk",
      Statistic = unname(sw$statistic),
      p_value   = sw$p.value,
      Conclusion = ifelse(sw$p.value < 0.05, "Rechaza normalidad (p<0.05)", "No rechaza normalidad")
    )
  }, digits = 6)
  
  output$norm_ks_tbl <- renderTable({
    mb <- lm_base()
    validate(need(!is.null(mb), "Ajusta primero un modelo lm o Cochrane-Orcutt."))
    r <- resid(mb)
    validate(need(length(r) >= 3, "Se necesitan al menos 3 observaciones."))
    if (!requireNamespace("nortest", quietly = TRUE)) {
      return(data.frame(Test = "Kolmogorov-Smirnov (Lilliefors)",
                        Resultado = "Instala el paquete 'nortest': install.packages('nortest')"))
    }
    ks <- nortest::lillie.test(r)
    data.frame(
      Test      = "Kolmogorov-Smirnov (Lilliefors)",
      Statistic = unname(ks$statistic),
      p_value   = ks$p.value,
      Conclusion = ifelse(ks$p.value < 0.05, "Rechaza normalidad (p<0.05)", "No rechaza normalidad")
    )
  }, digits = 6)
  
  output$norm_jb_tbl <- renderTable({
    mb <- lm_base()
    validate(need(!is.null(mb), "Ajusta primero un modelo lm o Cochrane-Orcutt."))
    r <- resid(mb)
    validate(need(length(r) >= 3, "Se necesitan al menos 3 observaciones."))
    if (!requireNamespace("tseries", quietly = TRUE)) {
      return(data.frame(Test = "Jarque-Bera",
                        Resultado = "Instala el paquete 'tseries': install.packages('tseries')"))
    }
    jb <- tseries::jarque.bera.test(r)
    data.frame(
      Test      = "Jarque-Bera",
      Statistic = unname(jb$statistic),
      df        = unname(jb$parameter),
      p_value   = jb$p.value,
      Conclusion = ifelse(jb$p.value < 0.05, "Rechaza normalidad (p<0.05)", "No rechaza normalidad")
    )
  }, digits = 6)
  
  output$norm_qq_plot <- renderPlot({
    mb <- lm_base()
    validate(need(!is.null(mb), "Ajusta primero un modelo lm o Cochrane-Orcutt."))
    r <- resid(mb)
    
    # Q-Q plot con banda de confianza (simulacion de envolvente)
    op <- par(mar = c(5, 5, 4, 2))
    on.exit(par(op), add = TRUE)
    
    qqnorm(r,
           main = "Q-Q plot de residuos",
           xlab = "Cuantiles teoricos (normal)",
           ylab = "Cuantiles muestrales (residuos)",
           pch  = 16, col = "#2196F3", cex = 0.8)
    qqline(r, col = "#E53935", lwd = 2, lty = 1)
    
    # Banda de confianza bootstrap al 95%
    n   <- length(r)
    set.seed(42)
    sim <- replicate(500, sort(rnorm(n, mean(r), sd(r))))
    q_lo <- apply(sim, 1, quantile, 0.025)
    q_hi <- apply(sim, 1, quantile, 0.975)
    probs <- ppoints(n)
    q_teo <- qnorm(probs)
    polygon(c(q_teo, rev(q_teo)), c(q_lo, rev(q_hi)),
            col = adjustcolor("#E53935", alpha.f = 0.12), border = NA)
    
    legend("topleft",
           legend = c("Residuos", "Linea normal", "Banda 95%"),
           col    = c("#2196F3", "#E53935", adjustcolor("#E53935", 0.3)),
           pch    = c(16, NA, 15),
           lty    = c(NA, 1, NA),
           lwd    = c(NA, 2, NA),
           bty    = "n", cex = 0.85)
  })
  
  output$norm_hist_plot <- renderPlot({
    mb <- lm_base()
    validate(need(!is.null(mb), "Ajusta primero un modelo lm o Cochrane-Orcutt."))
    r <- resid(mb)
    
    op <- par(mar = c(5, 5, 4, 2))
    on.exit(par(op), add = TRUE)
    
    h <- hist(r, plot = FALSE)
    ylim_max <- max(h$density, dnorm(0, 0, sd(r))) * 1.15
    
    hist(r, freq = FALSE,
         col  = "#BBDEFB", border = "white",
         main = "Histograma de residuos",
         xlab = "Residuos", ylab = "Densidad",
         ylim = c(0, ylim_max))
    
    # Curva normal teorica
    xs <- seq(min(r) - sd(r), max(r) + sd(r), length.out = 300)
    lines(xs, dnorm(xs, mean(r), sd(r)), col = "#E53935", lwd = 2)
    
    # Curva de densidad empirica
    lines(density(r), col = "#2196F3", lwd = 2, lty = 2)
    
    legend("topright",
           legend = c("Normal teorica", "Densidad empirica"),
           col    = c("#E53935", "#2196F3"),
           lty    = c(1, 2), lwd = 2,
           bty    = "n", cex = 0.85)
  })
  
  # ---- Heterocedasticidad ----
  output$hetero_note <- renderText({
    if (is.null(lm_base())) {
      return("Disponible para modelos lineales (lm) y para el lm base de Cochrane–Orcutt. Instala 'lmtest' y (opcional) 'car' para más pruebas.")
    }
    "Reglas habituales: p-valor pequeño en BP/White o NCV sugiere heterocedasticidad."
  })
  
  output$bp_tbl <- renderTable({
    mb <- lm_base()
    validate(need(!is.null(mb), "No hay modelo lm disponible (ajusta lm o Cochrane–Orcutt)."))
    
    if (!requireNamespace("lmtest", quietly = TRUE)) {
      return(data.frame(Test = "Breusch-Pagan", Result = "Falta paquete lmtest"))
    }
    bp <- lmtest::bptest(mb)
    data.frame(
      Test = "Breusch-Pagan",
      Statistic = unname(bp$statistic),
      df = unname(bp$parameter),
      p_value = bp$p.value
    )
  }, digits = 6)
  
  output$white_tbl <- renderTable({
    mb <- lm_base()
    validate(need(!is.null(mb), "No hay modelo lm disponible (ajusta lm o Cochrane–Orcutt)."))
    
    if (!requireNamespace("lmtest", quietly = TRUE)) {
      return(data.frame(Test = "White (aprox)", Result = "Falta paquete lmtest"))
    }
    # Aproximacion tipo White: bptest con fitted + fitted^2
    # Puede fallar si hay colinealidad perfecta o pocos grados de libertad
    f  <- fitted(mb)
    wt <- tryCatch(
      lmtest::bptest(mb, ~ f + I(f^2)),
      error = function(e) tryCatch(
        lmtest::bptest(mb, ~ f),
        error = function(e2) NULL
      )
    )
    if (is.null(wt)) {
      return(data.frame(
        Test      = "White (aprox)",
        Statistic = NA_real_, df = NA_real_, p_value = NA_real_,
        Nota      = "No calculable (gl insuficientes o fitted constantes)"
      ))
    }
    data.frame(
      Test      = "White (aprox: fitted + fitted^2)",
      Statistic = unname(wt$statistic),
      df        = unname(wt$parameter),
      p_value   = wt$p.value
    )
  }, digits = 6)
  
  output$ncv_tbl <- renderTable({
    mb <- lm_base()
    validate(need(!is.null(mb), "No hay modelo lm disponible (ajusta lm o Cochrane–Orcutt)."))
    
    if (!requireNamespace("car", quietly = TRUE)) {
      return(data.frame(Test = "NCV (car::ncvTest)", Result = "Paquete 'car' no instalado (opcional)"))
    }
    
    # car::ncvTest usa model.frame internamente; para evitar problemas de evaluación del 'data' original,
    # re-ajustamos el lm sobre el model frame ya almacenado en el objeto.
    mf <- mb$model
    if (is.null(mf) || !is.data.frame(mf)) {
      # último recurso: intentar construir model.frame
      mf <- tryCatch(model.frame(mb), error = function(e) NULL)
    }
    validate(need(!is.null(mf) && is.data.frame(mf), "No se pudo recuperar el data.frame del modelo para ncvTest."))
    
    m2 <- lm(formula(mb), data = mf, x = TRUE, y = TRUE, model = TRUE)
    ncv <- tryCatch(car::ncvTest(m2), error = function(e) e)
    
    if (inherits(ncv, "error")) {
      return(data.frame(Test = "NCV (car::ncvTest)", Result = paste("Error:", ncv$message)))
    }
    
    data.frame(
      Test = "NCV (score test)",
      Statistic = unname(ncv$ChiSquare),
      df = unname(ncv$Df),
      p_value = ncv$p
    )
  }, digits = 6)
  
  # ---- Regresión de residuos² ----
  output$resid2_vars_ui <- renderUI({
    req(lm_base(), input$x)
    validate(need(!is.null(lm_base()), "Ajusta primero un modelo lm."))
    df <- data_clean()
    # Ofrecer todos los predictores del modelo + ajustados como opción extra
    choices_r2 <- c(input$x, "fitted(modelo)" )
    selectizeInput("resid2_vars", "Variables para explicar e²",
                   choices  = choices_r2,
                   selected = input$x,
                   multiple = TRUE,
                   options  = list(placeholder = "Selecciona una o varias variables"))
  })
  
  resid2_model <- eventReactive(input$fit_resid2, {
    mb <- lm_base()
    validate(need(!is.null(mb), "Ajusta primero un modelo lm o Cochrane–Orcutt."))
    validate(need(length(input$resid2_vars) > 0, "Selecciona al menos una variable."))
    
    # Usar model.frame para garantizar alineacion exacta de filas (gestiona NAs automaticamente)
    mf <- tryCatch(model.frame(mb), error = function(e) NULL)
    validate(need(!is.null(mf), "No se pudo recuperar el data.frame del modelo."))
    
    e2  <- resid(mb)^2
    aux <- data.frame(e2 = e2)
    
    for (v in input$resid2_vars) {
      if (v == "fitted(modelo)") {
        aux[["fitted_modelo"]] <- fitted(mb)
      } else {
        validate(need(v %in% names(mf) || v %in% names(data_clean()),
                      paste("Variable no encontrada en el modelo:", v)))
        if (v %in% names(mf)) {
          # La variable ya esta en el model.frame: perfectamente alineada
          aux[[v]] <- mf[[v]]
        } else {
          # Esta en data_clean pero no en model.frame: alinear por rownames
          df  <- data_clean()
          idx <- suppressWarnings(as.integer(rownames(mf)))
          col_vals <- if (all(!is.na(idx))) {
            df[[v]][idx]
          } else {
            df[[v]][seq_len(nrow(mf))]
          }
          validate(need(length(col_vals) == nrow(aux),
                        paste("La longitud de", v, "no coincide con los residuos.")))
          aux[[v]] <- col_vals
        }
      }
    }
    
    validate(need(nrow(aux) > 0 && sum(complete.cases(aux)) > 0,
                  "No hay observaciones completas para ajustar el modelo de residuos2."))
    
    rhs <- paste(gsub("fitted\\(modelo\\)", "fitted_modelo", input$resid2_vars), collapse = " + ")
    frm <- as.formula(paste("e2 ~", rhs))
    lm(frm, data = aux, na.action = na.omit)
  })
  
  output$resid2_formula_txt <- renderPrint({
    req(resid2_model())
    formula(resid2_model())
  })
  
  output$resid2_anova_tbl <- renderTable({
    req(resid2_model())
    m2 <- resid2_model()
    av <- as.data.frame(anova(m2))
    av$Term <- rownames(av)
    av <- av[, c("Term", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")]
    # Marcar significatividad
    av$Sig <- ifelse(is.na(av$`Pr(>F)`), "",
                     ifelse(av$`Pr(>F)` < 0.001, "***",
                            ifelse(av$`Pr(>F)` < 0.01,  "**",
                                   ifelse(av$`Pr(>F)` < 0.05,  "*",
                                          ifelse(av$`Pr(>F)` < 0.1,   ".", "")))))
    av
  }, digits = 4, na = "")
  
  output$resid2_summary_txt <- renderPrint({
    req(resid2_model())
    s <- summary(resid2_model())
    cat(sprintf("R²        = %.4f\n", s$r.squared))
    cat(sprintf("R² ajust. = %.4f\n", s$adj.r.squared))
    fstat <- s$fstatistic
    if (!is.null(fstat)) {
      pval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
      cat(sprintf("F-global  = %.4f  (df1=%g, df2=%g)  p = %.4e\n",
                  fstat[1], fstat[2], fstat[3], pval))
    }
    cat("\nInterpretación: si algún predictor es significativo (p < 0.05),\n")
    cat("la varianza del error NO es constante respecto a esa variable\n")
    cat("→ evidencia de heterocedasticidad.\n")
  })
  
  # ---- Gráficos heterocedasticidad ----
  
  output$resid_fitted_plot <- renderPlot({
    mb <- lm_base()
    validate(need(!is.null(mb), "No hay modelo lm disponible."))
    plot(fitted(mb), resid(mb),
         xlab = "Ajustados", ylab = "Residuos",
         main = "Residuos vs Ajustados")
    abline(h = 0, lty = 2)
  })
  
  output$scale_location_plot <- renderPlot({
    mb <- lm_base()
    validate(need(!is.null(mb), "No hay modelo lm disponible."))
    r <- resid(mb)
    f <- fitted(mb)
    y <- sqrt(abs(r))
    plot(f, y,
         xlab = "Ajustados", ylab = "sqrt(|residuos|)",
         main = "Scale-Location (heterocedasticidad)")
  })
  
  
  # ---- Restricciones lineales ----
  output$hyp_coef_txt <- renderPrint({
    req(model_fit())
    m <- model_fit()
    
    # Extraer coeficientes del modelo base (lm, logit, co)
    cf <- if (inherits(m, "lm") || inherits(m, "glm")) {
      coef(m)
    } else if (is.list(m) && !is.null(m$kind)) {
      if (m$kind == "co")         coef(m$base_lm)
      else if (m$kind == "co_fallback") coef(if (!is.null(m$base_lm)) m$base_lm else m$model)
      else NULL
    } else NULL
    
    if (is.null(cf)) {
      cat("Coeficientes no disponibles para este tipo de modelo.")
    } else {
      cat("Terminos disponibles (usa esta notacion en las restricciones):\n\n")
      cat(paste0("  b[", names(cf), "]  =  ", round(cf, 5)), sep = "\n")
      cat("\n")
    }
  })
  
  hyp_result <- eventReactive(input$run_hyp, {
    req(model_fit())
    validate(need(nchar(trimws(input$hyp_text)) > 0,
                  "Escribe al menos una restriccion."))
    
    if (!requireNamespace("car", quietly = TRUE)) {
      return(list(error = "Instala el paquete 'car': install.packages('car')"))
    }
    
    m <- model_fit()
    # Obtener modelo lm base
    base <- if (inherits(m, "lm") || inherits(m, "glm")) {
      m
    } else if (is.list(m) && !is.null(m$kind) && m$kind %in% c("co", "co_fallback")) {
      m$base_lm
    } else {
      NULL
    }
    validate(need(!is.null(base),
                  "Las restricciones lineales solo estan disponibles para modelos lm, glm y Cochrane-Orcutt."))
    
    # Parsear restricciones: separar por salto de linea y/o coma
    # ---------- Parser de notacion b[var] ----------
    # Convierte "b[cyl] - 2*b[hp] = 0" -> "cyl - 2*hp = 0"
    # que es el formato que acepta car::linearHypothesis
    parse_restrictions <- function(raw_text, coef_names) {
      lines <- trimws(unlist(strsplit(raw_text, "\n")))
      lines <- lines[nchar(lines) > 0]
      parsed <- character(0)
      errors <- character(0)
      for (ln in lines) {
        # Extraer nombres dentro de b[...] usando perl=TRUE para soporte de grupos
        m <- gregexpr("b\\[([^\\]]+)\\]", ln, perl = TRUE)
        ref_names <- regmatches(ln, m)[[1]]
        # ref_names contiene "b[cyl]", "b[hp]", etc. -- extraer el interior
        ref_names <- gsub("^b\\[|\\]$", "", ref_names)
        bad <- ref_names[!ref_names %in% coef_names]
        if (length(bad) > 0) {
          errors <- c(errors, paste0(
            "Coeficiente no encontrado: b[", bad, "]\n",
            "  Disponibles: ", paste(paste0("b[", coef_names, "]"), collapse = ", ")
          ))
        } else {
          # Traducir b[nombre] -> nombre para car::linearHypothesis
          translated <- gsub("b\\[([^\\]]+)\\]", "\\1", ln, perl = TRUE)
          parsed <- c(parsed, translated)
        }
      }
      list(hyps = parsed, errors = errors)
    }    
    raw      <- trimws(input$hyp_text)
    cf_names <- names(coef(base))
    parsed   <- parse_restrictions(raw, cf_names)
    
    if (length(parsed$errors) > 0) {
      return(list(error = paste(
        "Error en las restricciones:
",
        paste(parsed$errors, collapse = "
"),
        "
 
Coeficientes disponibles:
",
        paste(paste0("  b[", cf_names, "]"), collapse = "
")
      )))
    }
    
    validate(need(length(parsed$hyps) > 0, "No se encontraron restricciones validas."))
    hyps <- parsed$hyps
    # ------------------------------------------------
    
    test_type <- input$hyp_test_type
    
    res <- tryCatch(
      car::linearHypothesis(base, hyps, test = test_type),
      error = function(e) list(error = conditionMessage(e))
    )
    
    list(result = res, hyps = hyps, raw_hyps = trimws(unlist(strsplit(raw, "\n"))),
         test_type = test_type, model = base)
  })
  
  output$hyp_result_txt <- renderPrint({
    req(hyp_result())
    hr <- hyp_result()
    if (!is.null(hr$error)) {
      cat("Error al ejecutar el test:\n", hr$error, "\n")
      return(invisible(NULL))
    }
    # Mostrar en notacion b[var] si disponible, si no la traducida
    display_hyps <- if (!is.null(hr$raw_hyps)) hr$raw_hyps else hr$hyps
    display_hyps <- display_hyps[nchar(trimws(display_hyps)) > 0]
    cat("Restricciones evaluadas:\n")
    for (h in display_hyps) cat("  ", h, "\n")
    cat("\nTipo de test:", hr$test_type, "\n")
  })
  
  output$hyp_table_tbl <- renderTable({
    req(hyp_result())
    hr <- hyp_result()
    if (!is.null(hr$error)) return(NULL)
    
    res <- hr$result
    df_out <- as.data.frame(res)
    df_out <- df_out[, colSums(is.na(df_out)) < nrow(df_out), drop = FALSE]
    
    # linearHypothesis devuelve 1 o 2 filas segun el numero de restricciones
    etiquetas <- c("Sin restriccion", "Con restriccion")
    df_out$Modelo <- etiquetas[seq_len(nrow(df_out))]
    
    # Mover Modelo al frente
    df_out <- df_out[, c("Modelo", setdiff(names(df_out), "Modelo")), drop = FALSE]
    df_out
  }, digits = 4, na = "")
  
  output$hyp_interp_txt <- renderText({
    req(hyp_result())
    hr <- hyp_result()
    if (!is.null(hr$error)) return("")
    
    res  <- hr$result
    # Extraer p-valor de la ultima fila (la que tiene el estadistico)
    pval <- tryCatch({
      pcol <- grep("Pr \\(", names(res), value = TRUE)[1]
      res[[pcol]][nrow(res)]
    }, error = function(e) NA_real_)
    
    if (is.na(pval)) return("No se pudo extraer el p-valor.")
    
    alpha <- 0.05
    sig   <- pval < alpha
    
    n_hyps <- length(hr$hyps)
    tipo   <- if (hr$test_type == "F") "F" else "Chi-cuadrado"
    
    paste0(
      "Con un nivel de significacion del 5%:\n\n",
      if (sig) {
        paste0("RECHAZO de H0: los datos NO son compatibles con las restricciones impuestas ",
               "(p = ", formatC(pval, digits = 4, format = "f"), " < 0.05).\n",
               "Conclusion: al menos una de las ", n_hyps, " restriccion(es) es incompatible con los datos.")
      } else {
        paste0("NO se rechaza H0: los datos son compatibles con las restricciones impuestas ",
               "(p = ", formatC(pval, digits = 4, format = "f"), " >= 0.05).\n",
               "Conclusion: las ", n_hyps, " restriccion(es) no son rechazadas al 5%.")
      },
      "\n\nTest utilizado: ", tipo,
      if (hr$test_type == "F") " (recomendado para muestras pequeñas y modelos lm)"
      else " (asintoticamente valido, mayor potencia en muestras grandes)"
    )
  })
  
  # ---- Autocorrelación ----
  output$auto_note <- renderText({
    if (is.null(lm_base())) {
      return("Disponible para modelos lineales (lm) y para el lm base de Cochrane–Orcutt. Instala 'lmtest' para Durbin–Watson y Breusch–Godfrey.")
      
      
      output$auto_options_ui <- renderUI({
        # Opciones para autocorrelación: orden BG y lag Ljung-Box
        tagList(
          sliderInput("bg_order", "Orden Breusch–Godfrey", min = 1, max = 10, value = 1, step = 1),
          sliderInput("lb_lag", "Lag Ljung–Box", min = 1, max = 30, value = 10, step = 1),
          helpText("Para Cochrane–Orcutt: usa la variable de orden en el panel lateral (order_var). Para lm: si tus datos son temporales, crea/elige una columna de tiempo y ordénalos antes de ajustar.")
        )
      })
      
    }
    "Reglas habituales: p-valor pequeño en DW/BG/Ljung-Box sugiere autocorrelación."
  })
  
  output$dw_tbl <- renderTable({
    mb <- lm_base()
    validate(need(!is.null(mb), "No hay modelo lm disponible (ajusta lm o Cochrane–Orcutt)."))
    
    if (!requireNamespace("lmtest", quietly = TRUE)) {
      return(data.frame(Test = "Durbin-Watson", Result = "Falta paquete lmtest"))
    }
    dw <- lmtest::dwtest(mb)
    data.frame(
      Test = "Durbin-Watson",
      Statistic = unname(dw$statistic),
      p_value = dw$p.value,
      Alternative = as.character(dw$alternative)
    )
  }, digits = 6)
  
  output$bg_tbl <- renderTable({
    mb <- lm_base()
    req(input$bg_order)
    validate(need(!is.null(mb), "No hay modelo lm disponible (ajusta lm o Cochrane–Orcutt)."))
    
    if (!requireNamespace("lmtest", quietly = TRUE)) {
      return(data.frame(Test = "Breusch-Godfrey", Result = "Falta paquete lmtest"))
    }
    bg <- lmtest::bgtest(mb, order = input$bg_order)
    data.frame(
      Test = "Breusch-Godfrey (orden 1)",
      Statistic = unname(bg$statistic),
      df = unname(bg$parameter),
      p_value = bg$p.value
    )
  }, digits = 6)
  
  
  
  
  
  output$lb_tbl <- renderTable({
    r <- residuals_base()
    validate(need(!is.null(r), "No hay residuos disponibles (ajusta lm o Cochrane–Orcutt)."))
    
    # Ljung-Box base R
    # order = min(10, n/5) como regla rápida
    req(input$lb_lag)
    lag <- min(input$lb_lag, max(1, length(r) - 1))
    lb <- Box.test(r, lag = lag, type = "Ljung-Box")
    data.frame(
      Test = paste0("Ljung-Box (lag=", lag, ")"),
      Statistic = unname(lb$statistic),
      df = unname(lb$parameter),
      p_value = lb$p.value
    )
  }, digits = 6)
  
  output$acf_plot <- renderPlot({
    r <- residuals_base()
    validate(need(!is.null(r), "No hay residuos disponibles."))
    acf(r, main = "ACF de residuos")
  })
  
  output$pacf_plot <- renderPlot({
    r <- residuals_base()
    validate(need(!is.null(r), "No hay residuos disponibles."))
    pacf(r, main = "PACF de residuos")
  })
  
  
  output$resid_time_plot <- renderPlot({
    # Residuos vs tiempo/índice: útil si hay orden temporal
    mb <- lm_base()
    validate(need(!is.null(mb), "No hay modelo lm disponible."))
    df <- data_clean()
    
    # Preferir order_var si existe y está seleccionado (panel lateral para CO)
    tvar <- NULL
    if (!is.null(input$order_var) && input$order_var %in% names(df)) {
      tvar <- input$order_var
    }
    
    # Si no hay order_var, usar el índice natural
    r <- resid(mb)
    n <- length(r)
    
    if (!is.null(tvar)) {
      # Intentar alinear residuos con filas del modelo (lm conserva rownames)
      mf <- model.frame(mb)
      # rownames de mf suelen corresponder a df tras na.omit; recuperamos la variable de tiempo
      tt <- df[rownames(mf), tvar, drop = TRUE]
      plot(tt, r, xlab = tvar, ylab = "Residuos", main = "Residuos vs tiempo/índice")
      abline(h = 0, lty = 2)
    } else {
      plot(seq_len(n), r, xlab = "Índice", ylab = "Residuos", main = "Residuos vs índice")
      abline(h = 0, lty = 2)
    }
  })
  
  
  # ---- train/test + métricas (lm, logística, glmnet, Cochrane–Orcutt) ----
  split_results <- eventReactive(input$split_fit, {
    df <- data_clean()
    f <- formula_obj()
    
    # Partición
    set.seed(123)
    n <- nrow(df)
    validate(need(n >= 5, "Necesitas al menos 5 filas para train/test."))
    idx <- sample(seq_len(n), size = floor(n * input$train_pct / 100))
    train <- df[idx, , drop = FALSE]
    test  <- df[-idx, , drop = FALSE]
    validate(need(nrow(test) >= 2, "Con este porcentaje, el conjunto test queda demasiado pequeño."))
    
    # Helpers métricas
    rmse <- function(y, p) sqrt(mean((y - p)^2))
    r2   <- function(y, p) 1 - sum((y - p)^2) / sum((y - mean(y))^2)
    acc  <- function(y, p) mean(y == p)
    
    # ---- lm ----
    if (input$model_type == "lm") {
      m <- lm(f, data = train)
      pred_train <- predict(m, newdata = train)
      pred_test  <- predict(m, newdata = test)
      
      ytr <- train[[input$y]]
      yte <- test[[input$y]]
      
      metrics <- data.frame(
        set = c("train", "test"),
        RMSE = c(rmse(ytr, pred_train), rmse(yte, pred_test)),
        R2   = c(r2(ytr, pred_train),   r2(yte, pred_test))
      )
      
      return(list(
        kind = "lm",
        model = m,
        train = train, test = test,
        pred_train = pred_train, pred_test = pred_test,
        metrics = metrics, cm = NULL
      ))
    }
    
    # ---- logística ----
    if (input$model_type == "logit") {
      m <- glm(f, data = train, family = binomial())
      prob_train <- predict(m, newdata = train, type = "response")
      prob_test  <- predict(m, newdata = test,  type = "response")
      
      cls <- function(p) ifelse(p >= 0.5, 1, 0)
      
      ytr <- train[[input$y]]
      yte <- test[[input$y]]
      
      if (is.factor(ytr)) {
        ytr01 <- as.integer(ytr == levels(ytr)[2])
        yte01 <- as.integer(yte == levels(yte)[2])
      } else {
        ytr01 <- as.integer(ytr)
        yte01 <- as.integer(yte)
      }
      
      pred_tr <- cls(prob_train)
      pred_te <- cls(prob_test)
      
      metrics <- data.frame(
        set = c("train", "test"),
        Accuracy = c(acc(ytr01, pred_tr), acc(yte01, pred_te))
      )
      
      cm <- table(Real = yte01, Pred = pred_te)
      
      return(list(
        kind = "logit",
        model = m,
        train = train, test = test,
        pred_train = prob_train, pred_test = prob_test,
        pred_class_train = pred_tr, pred_class_test = pred_te,
        metrics = metrics, cm = as.data.frame.matrix(cm)
      ))
    }
    
    # ---- glmnet: ridge/lasso/enet ----
    if (input$model_type %in% c("ridge", "lasso", "enet")) {
      validate(need(requireNamespace("glmnet", quietly = TRUE),
                    "Falta el paquete 'glmnet'. Instálalo con install.packages('glmnet')."))
      
      alpha <- if (input$model_type == "ridge") 0 else if (input$model_type == "lasso") 1 else input$enet_alpha
      
      Xtr <- model.matrix(f, data = train)
      Xte <- model.matrix(f, data = test)
      if ("(Intercept)" %in% colnames(Xtr)) Xtr <- Xtr[, colnames(Xtr) != "(Intercept)", drop = FALSE]
      if ("(Intercept)" %in% colnames(Xte)) Xte <- Xte[, colnames(Xte) != "(Intercept)", drop = FALSE]
      
      # Alinear columnas (por seguridad si algún nivel de factor no aparece en test/train)
      all_cols <- union(colnames(Xtr), colnames(Xte))
      add_missing <- function(X, all_cols) {
        miss <- setdiff(all_cols, colnames(X))
        if (length(miss) > 0) {
          X <- cbind(X, matrix(0, nrow = nrow(X), ncol = length(miss),
                               dimnames = list(NULL, miss)))
        }
        X[, all_cols, drop = FALSE]
      }
      Xtr <- add_missing(Xtr, all_cols)
      Xte <- add_missing(Xte, all_cols)
      
      ytr <- train[[input$y]]
      yte <- test[[input$y]]
      validate(need(is.numeric(ytr) && is.numeric(yte), "Para Ridge/Lasso/ENet, Y debe ser numérica (continua)."))
      
      cv <- glmnet::cv.glmnet(x = Xtr, y = ytr, alpha = alpha, standardize = TRUE)
      lambda_used <- if (isTRUE(input$use_lambda_1se)) cv$lambda.1se else cv$lambda.min
      fit <- glmnet::glmnet(x = Xtr, y = ytr, alpha = alpha, standardize = TRUE, lambda = lambda_used)
      
      pred_train <- as.numeric(predict(fit, newx = Xtr, s = lambda_used))
      pred_test  <- as.numeric(predict(fit, newx = Xte, s = lambda_used))
      
      metrics <- data.frame(
        set = c("train", "test"),
        RMSE = c(rmse(ytr, pred_train), rmse(yte, pred_test)),
        R2   = c(r2(ytr, pred_train),   r2(yte, pred_test))
      )
      
      return(list(
        kind = "glmnet",
        model = list(alpha = alpha, cv = cv, fit = fit, lambda = lambda_used),
        train = train, test = test,
        pred_train = pred_train, pred_test = pred_test,
        metrics = metrics, cm = NULL
      ))
    }
    
    # ---- Cochrane–Orcutt (AR1) ----
    if (input$model_type == "co") {
      validate(need(!is.null(input$order_var) && input$order_var %in% names(df),
                    "Selecciona una variable de orden (tiempo/índice)."))
      
      # Ordenar train y test por la variable temporal (manteniendo filas)
      ord_tr <- order(train[[input$order_var]])
      ord_te <- order(test[[input$order_var]])
      train_o <- train[ord_tr, , drop = FALSE]
      test_o  <- test[ord_te,  , drop = FALSE]
      
      base <- lm(f, data = train_o, x = TRUE, y = TRUE, model = TRUE)
      
      # Función de predicción a partir de coeficientes y fórmula
      predict_from_coef <- function(coefs, newdata, f) {
        mm <- model.matrix(f, data = newdata)
        # asegurar mismos nombres (por si faltan columnas)
        miss <- setdiff(names(coefs), colnames(mm))
        if (length(miss) > 0) {
          # columnas faltantes -> 0
          for (mname in miss) mm <- cbind(mm, setNames(data.frame(rep(0, nrow(mm))), mname))
        }
        mm <- mm[, names(coefs), drop = FALSE]
        as.numeric(mm %*% coefs)
      }
      
      # Intentar orcutt si existe
      if (requireNamespace("orcutt", quietly = TRUE)) {
        co_obj <- orcutt::cochrane.orcutt(base)
        
        # Extraer coeficientes de forma robusta
        coefs <- NULL
        if (!is.null(co_obj$Cochrane.Orcutt) && !is.null(co_obj$Cochrane.Orcutt$coefficients)) {
          coefs <- co_obj$Cochrane.Orcutt$coefficients
        } else if (!is.null(co_obj$coefficients)) {
          coefs <- co_obj$coefficients
        }
        
        # Si no podemos extraerlos, caemos al lm base (no debería pasar en instalaciones típicas)
        if (is.null(coefs)) coefs <- coef(base)
        
        pred_train <- predict_from_coef(coefs, train_o, f)
        pred_test  <- predict_from_coef(coefs, test_o,  f)
        
        ytr <- train_o[[input$y]]
        yte <- test_o[[input$y]]
        
        metrics <- data.frame(
          set = c("train", "test"),
          RMSE = c(rmse(ytr, pred_train), rmse(yte, pred_test)),
          R2   = c(r2(ytr, pred_train),   r2(yte, pred_test))
        )
        
        return(list(
          kind = "co",
          model = list(base_lm = base, co = co_obj, coefficients = coefs),
          train = train_o, test = test_o,
          pred_train = pred_train, pred_test = pred_test,
          metrics = metrics, cm = NULL
        ))
      }
      
      # Fallback: usar el mismo ajuste "simple" que en model_fit (transformación iterativa)
      m <- base
      rho_old <- 0
      
      for (it in seq_len(input$co_max_iter)) {
        e <- resid(m)
        rho <- sum(e[-1] * e[-length(e)]) / sum(e[-length(e)]^2)
        
        y <- model.response(model.frame(m))
        X <- model.matrix(m)
        
        y_t <- y[-1] - rho * y[-length(y)]
        X_t <- X[-1, , drop = FALSE] - rho * X[-nrow(X), , drop = FALSE]
        
        aux <- as.data.frame(X_t)
        aux$y_t <- y_t
        colnames(aux) <- make.names(colnames(aux))
        frm <- as.formula(paste("y_t ~", paste(colnames(aux)[colnames(aux) != "y_t"], collapse = " + "), "- 1"))
        m <- lm(frm, data = aux)
        
        if (abs(rho - rho_old) < input$co_tol) break
        rho_old <- rho
      }
      
      # En fallback, no tenemos coeficientes directamente sobre variables originales con nombres estándar.
      # Para no engañar, usamos el lm base para predicción y reportamos una nota en métricas.
      pred_train <- predict(base, newdata = train_o)
      pred_test  <- predict(base, newdata = test_o)
      ytr <- train_o[[input$y]]
      yte <- test_o[[input$y]]
      
      metrics <- data.frame(
        set = c("train", "test"),
        RMSE = c(rmse(ytr, pred_train), rmse(yte, pred_test)),
        R2   = c(r2(ytr, pred_train),   r2(yte, pred_test))
      )
      
      return(list(
        kind = "co_fallback",
        model = list(base_lm = base, rho = rho_old, transformed_model = m),
        train = train_o, test = test_o,
        pred_train = pred_train, pred_test = pred_test,
        metrics = metrics, cm = NULL
      ))
    }
    
    stop("Tipo de modelo no soportado en train/test.")
  })
  
  output$metrics_tbl <- renderTable({
    req(split_results())
    split_results()$metrics
  })
  
  output$cm_tbl <- renderTable({
    req(split_results())
    split_results()$cm
  }, rownames = TRUE)
  
  # ---- gráficos (solo lm/logit; el resto muestra aviso) ----
  output$plot1 <- renderPlot({
    req(model_fit())
    if (input$model_type == "lm") {
      m <- model_fit()
      plot(fitted(m), resid(m),
           xlab = "Ajustados", ylab = "Residuos",
           main = "Residuos vs Ajustados")
      abline(h = 0, lty = 2)
      return(invisible(NULL))
    }
    
    if (input$model_type == "logit") {
      m <- model_fit()
      df <- data_clean()
      prob <- predict(m, type = "response")
      y <- df[[input$y]]
      if (is.factor(y)) y <- as.integer(y == levels(y)[2])
      plot(prob, y, xlab = "Probabilidad predicha", ylab = "Y (0/1)",
           main = "Logística: prob. predicha vs Y")
      return(invisible(NULL))
    }
    
    plot.new()
    text(0.5, 0.5, "Gráficos de diagnóstico: disponibles para lm y logística.", cex = 1)
  })
  
  output$plot2 <- renderPlot({
    req(model_fit())
    if (input$model_type == "lm") {
      m <- model_fit()
      qqnorm(resid(m), main = "QQ-plot de residuos")
      qqline(resid(m))
      return(invisible(NULL))
    }
    
    if (input$model_type == "logit") {
      m <- model_fit()
      prob <- predict(m, type = "response")
      hist(prob, main = "Histograma de probabilidades", xlab = "Probabilidad")
      return(invisible(NULL))
    }
    
    plot.new()
    text(0.5, 0.5, "Gráficos: disponibles para lm y logística.", cex = 1)
  })
  
  # ---- residuos vs variable explicativa (solo lm) ----
  output$resid_var_ui <- renderUI({
    req(model_fit(), input$x)
    if (input$model_type != "lm") return(NULL)
    selectInput("resid_x", "Selecciona variable explicativa", choices = input$x)
  })
  
  output$resid_var_plot <- renderPlot({
    req(model_fit(), input$resid_x)
    validate(need(input$model_type == "lm", "Solo disponible para regresión lineal"))
    m <- model_fit()
    df <- data_clean()
    x <- df[[input$resid_x]]
    r <- resid(m)
    plot(x, r,
         xlab = input$resid_x, ylab = "Residuos",
         main = paste("Residuos vs", input$resid_x))
    abline(h = 0, lty = 2)
  })
  
  # ---- descargas ----
  model_summary_to_text <- function(m) {
    con <- textConnection("out", "w", local = TRUE)
    sink(con); on.exit({sink(); close(con)}, add = TRUE)
    
    if (is.list(m) && !is.null(m$kind) && m$kind == "glmnet") {
      cat("Modelo: glmnet\n")
      cat("alpha =", m$alpha, "\n")
      cat("lambda usado =", m$fit$lambda, "\n\n")
      cat("Coeficientes (no-cero):\n")
      cf <- as.matrix(glmnet::coef.glmnet(m$fit))
      nz <- which(abs(cf) > 0)
      print(cf[nz, , drop = FALSE])
      cat("\nCV: lambda.min =", m$cv$lambda.min, " | lambda.1se =", m$cv$lambda.1se, "\n")
    } else if (is.list(m) && !is.null(m$kind) && m$kind == "co") {
      cat("Modelo: Cochrane–Orcutt (paquete orcutt)\n\n")
      print(m$co)
      cat("\nResumen del lm original:\n")
      print(summary(m$base_lm))
    } else if (is.list(m) && !is.null(m$kind) && m$kind == "co_fallback") {
      cat("Modelo: Cochrane–Orcutt (fallback)\n")
      cat("rho estimado =", m$rho, "\n\n")
      print(summary(m$model))
    } else {
      print(summary(m))
    }
    
    paste(out, collapse = "\n")
  }
  
  output$download_model_summary <- downloadHandler(
    filename = function() paste0("model_summary_", Sys.Date(), ".txt"),
    content = function(file) {
      req(model_fit())
      m <- model_fit()
      txt <- model_summary_to_text(m)
      writeLines(c("Fórmula:", deparse(formula_obj()), "", txt), file)
    }
  )
  
  output$download_predictions <- downloadHandler(
    filename = function() paste0("predictions_", Sys.Date(), ".csv"),
    content = function(file) {
      req(split_results())
      res <- split_results()
      out <- res$test
      
      if (res$kind %in% c("lm", "glmnet", "co", "co_fallback")) {
        out$pred <- res$pred_test
      } else if (res$kind == "logit") {
        out$prob <- res$pred_test
        out$pred_class <- res$pred_class_test
      } else {
        stop("No hay predicciones implementadas para este tipo.")
      }
      
      write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

