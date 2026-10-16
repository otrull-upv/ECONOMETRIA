# app.R
# Aplicación Shiny didáctica: Machine Learning aplicado a regresión
# Árboles de decisión, Random Forest y XGBoost (con alternativa automática)

library(shiny)

# -----------------------------
# Paquetes opcionales
# -----------------------------
has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# rpart forma parte de la distribución habitual de R; si no está, la app lo indica.
if (!has_pkg("rpart")) stop("Esta aplicación necesita el paquete 'rpart'. Instálalo con install.packages('rpart').")

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .app-title { color:#0B2D5B; font-size:2.2rem; font-weight:700; margin-top:0; }
      .section-title { color:#0B2D5B; font-weight:700; }
      .skip-link { position:absolute; left:-999px; top:0; background:#000; color:#fff; padding:10px 14px; z-index:10000; border-radius:6px; }
      .skip-link:focus { left:10px; top:10px; outline:3px solid #ffd400; }
      :focus { outline:3px solid #0b5fff !important; outline-offset:2px; }
      button:focus, a:focus, input:focus, select:focus, textarea:focus { box-shadow:none !important; }
      .sr-only { position:absolute!important; width:1px; height:1px; padding:0; margin:-1px; overflow:hidden; clip:rect(0,0,0,0); white-space:nowrap; border:0; }
      body.a11y { font-size:18px; line-height:1.6; background:#fff; color:#000; }
      body.a11y .btn, body.a11y input, body.a11y select, body.a11y textarea { font-size:18px; }
      body.a11y .well { background:#fff; border:2px solid #000; }
      #footer-fixed { position:fixed; bottom:0; left:0; width:100%; background:#f9f9f9; padding:8px; text-align:center; font-size:.8em; color:#555; border-top:1px solid #ccc; z-index:9999; }
      body { padding-bottom:60px; }
      .info-box { background:#f5f8fc; border-left:5px solid #0B5B8A; padding:12px 15px; margin:10px 0 15px 0; }
      .warning-box { background:#fff8e1; border-left:5px solid #f0ad4e; padding:12px 15px; margin:10px 0 15px 0; }
      .metric-box { background:#f7f7f7; border:1px solid #ddd; border-radius:5px; padding:12px; margin-bottom:10px; }
      @media (prefers-reduced-motion: reduce) { * { animation-duration:.01ms!important; transition-duration:.01ms!important; } }
    ")), 
    tags$script(HTML("Shiny.addCustomMessageHandler('toggleA11y', function(on){document.body.classList.toggle('a11y',!!on);});"))
  ),
  
  tags$a(class="skip-link", href="#main-content", "Saltar al contenido principal"),
  tags$header(
    tags$h1("MACHINE LEARNING: Regresión basada en árboles", class="app-title"),
    tags$p(class="sr-only", "Aplicación para comprender árboles de decisión, Random Forest y XGBoost aplicados a regresión.")
  ),
  checkboxInput("a11y_mode", "Modo accesible (alto contraste y letra grande)", FALSE, width="100%"),
  tags$div(id="footer-fixed", HTML("&copy; 2026 Oscar Trull &middot; Aplicación Shiny")),
  tags$hr(),
  
  sidebarLayout(
    sidebarPanel(width=3,
                 fileInput("file", "Sube un CSV", accept=c(".csv")),
                 checkboxInput("header", "El CSV tiene cabecera", TRUE),
                 radioButtons("sep", "Separador", choices=c(Coma=",", PuntoyComa=";", Tab="\t"), selected=","),
                 checkboxInput("use_example", "Usar dataset de ejemplo (mtcars)", TRUE),
                 tags$hr(),
                 uiOutput("var_ui"),
                 selectInput("model_type", "Modelo", choices=NULL),
                 uiOutput("model_options_ui"),
                 checkboxInput("rm_na", "Eliminar filas con NA", TRUE),
                 actionButton("fit", "Ajustar modelo", class="btn-primary", width="100%"),
                 tags$hr(),
                 h4("Validación"),
                 sliderInput("train_pct", "Porcentaje de entrenamiento", min=50, max=90, value=80, step=5),
                 actionButton("split_fit", "Entrenar / Test", class="btn-success", width="100%"),
                 tags$hr(),
                 downloadButton("download_summary", "Descargar resumen (txt)"),
                 downloadButton("download_predictions", "Descargar predicciones (csv)")
    ),
    
    mainPanel(id="main-content",
              tabsetPanel(
                tabPanel("Datos",
                         h4("Vista previa"), tableOutput("head_tbl"), tags$hr(),
                         h4("Resumen estadístico"), verbatimTextOutput("summary_txt"), tags$hr(),
                         h4("Relación Y–X"), plotOutput("scatter_plot", height="450px")
                ),
                
                tabPanel("Modelo",
                         h4("Modelo seleccionado"), verbatimTextOutput("model_info"),
                         h4("Predictores utilizados"), tableOutput("predictors_tbl"),
                         tags$hr(),
                         h4("Importancia de variables"),
                         helpText("La importancia permite comparar qué variables contribuyen más a la predicción. No debe interpretarse automáticamente como causalidad."),
                         plotOutput("importance_plot", height="450px"),
                         tableOutput("importance_tbl")
                ),
                
                tabPanel("Árbol / nodos",
                         div(class="info-box", HTML("<b>Objetivo didáctico:</b> observar cómo un árbol divide progresivamente los datos. Cada nodo representa una regla de decisión y cada hoja una predicción final.")),
                         conditionalPanel("input.model_type == 'cart'",
                                          h4("Árbol de decisión"), plotOutput("cart_tree_plot", height="650px"),
                                          h4("Reglas del árbol"), verbatimTextOutput("cart_rules"),
                                          h4("Tabla de nodos"), tableOutput("cart_nodes_tbl")
                         ),
                         conditionalPanel("input.model_type == 'rf'",
                                          h4("Árbol individual del Random Forest"),
                                          fluidRow(column(5, uiOutput("rf_tree_ui")), column(7, helpText("Selecciona un árbol del bosque para observar sus reglas."))),
                                          plotOutput("rf_tree_plot", height="700px"),
                                          h4("Nodos del árbol seleccionado"), tableOutput("rf_nodes_tbl")
                         ),
                         conditionalPanel("input.model_type == 'xgb'",
                                          h4("Estructura de los árboles XGBoost"),
                                          fluidRow(column(5, uiOutput("xgb_tree_ui")), column(7, helpText("XGBoost construye muchos árboles pequeños de forma secuencial. La tabla muestra las reglas de cada nodo."))),
                                          plotOutput("xgb_tree_plot", height="650px"),
                                          h4("Nodos del árbol seleccionado"), tableOutput("xgb_nodes_tbl")
                         ),
                         conditionalPanel("input.model_type == 'fallback'",
                                          div(class="warning-box", HTML("<b>Alternativa automática:</b> XGBoost no está disponible en este entorno. La aplicación utiliza Random Forest si está instalado y, en caso contrario, un árbol CART.")),
                                          uiOutput("fallback_tree_ui"),
                                          h4("Árbol alternativo"), plotOutput("fallback_tree_plot", height="700px"),
                                          h4("Nodos"), tableOutput("fallback_nodes_tbl")
                         )
                ),
                
                tabPanel("Importancia",
                         h4("Importancia de variables"),
                         helpText("Permite analizar qué predictores son más relevantes para el modelo. En Random Forest se basa en medidas del bosque; en XGBoost se puede mostrar Gain, Cover y Weight."),
                         plotOutput("importance_plot2", height="500px"),
                         tableOutput("importance_tbl2")
                ),
                
                tabPanel("Train / Test",
                         h4("Métricas de predicción"), tableOutput("metrics_tbl"),
                         tags$hr(),
                         h4("Predicciones frente a valores observados"), plotOutput("pred_plot", height="500px"),
                         tags$hr(),
                         h4("Predicciones del conjunto de test"), tableOutput("pred_tbl")
                ),
                
                tabPanel("Interpretación",
                         h4("Lectura didáctica del modelo"),
                         verbatimTextOutput("interpretation_txt"),
                         tags$hr(),
                         h4("Comparación con regresión lineal"),
                         helpText("La comparación es conceptual: el árbol no impone una relación lineal entre X e Y y puede representar interacciones y umbrales."),
                         plotOutput("comparison_plot", height="450px")
                )
              )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  observeEvent(input$a11y_mode, session$sendCustomMessage("toggleA11y", isTRUE(input$a11y_mode)), ignoreInit=FALSE)
  
  raw_data <- reactive({
    if (isTRUE(input$use_example)) return(mtcars)
    req(input$file)
    read.csv(input$file$datapath, header=input$header, sep=input$sep, stringsAsFactors=FALSE, check.names=FALSE)
  })
  
  data_clean <- reactive({
    df <- raw_data()
    if (isTRUE(input$rm_na)) df <- na.omit(df)
    for (nm in names(df)) if (is.character(df[[nm]])) df[[nm]] <- as.factor(df[[nm]])
    df
  })
  
  output$var_ui <- renderUI({
    df <- data_clean(); cols <- names(df)
    tagList(
      selectInput("y", "Variable respuesta (Y)", choices=cols, selected=cols[1]),
      selectizeInput("x", "Predictores (X)", choices=cols, multiple=TRUE,
                     selected=setdiff(cols, cols[1]), options=list(placeholder="Elige uno o varios predictores"))
    )
  })
  
  # Modelos disponibles. XGBoost es opcional; siempre hay una alternativa.
  model_choices <- reactive({
    out <- c("Árbol de decisión CART"="cart")
    if (has_pkg("randomForest")) out <- c(out, "Random Forest"="rf")
    if (has_pkg("xgboost")) out <- c(out, "XGBoost"="xgb")
    if (!has_pkg("xgboost")) out <- c(out, "Alternativa a XGBoost"="fallback")
    out
  })
  
  observe({
    ch <- model_choices()
    updateSelectInput(session, "model_type", choices=ch, selected=if ("rf" %in% unname(ch)) "rf" else "cart")
  })
  
  output$model_options_ui <- renderUI({
    req(input$model_type)
    if (input$model_type == "cart" || input$model_type == "fallback") {
      tagList(
        numericInput("cp", "Complejidad mínima (cp)", value=0.01, min=0.0001, max=0.5, step=0.005),
        numericInput("minsplit", "Mínimo de observaciones para dividir", value=10, min=2, step=1),
        numericInput("maxdepth", "Profundidad máxima", value=5, min=1, max=30, step=1)
      )
    } else if (input$model_type == "rf") {
      tagList(
        numericInput("ntree", "Número de árboles", value=200, min=10, max=2000, step=10),
        numericInput("mtry", "Variables candidatas por división (mtry)", value=NULL, min=1, step=1),
        numericInput("nodesize", "Tamaño mínimo de hoja", value=5, min=1, step=1)
      )
    } else if (input$model_type == "xgb") {
      tagList(
        numericInput("nrounds", "Número de árboles (nrounds)", value=100, min=10, max=1000, step=10),
        numericInput("max_depth", "Profundidad máxima", value=3, min=1, max=12, step=1),
        numericInput("eta", "Tasa de aprendizaje (eta)", value=0.08, min=0.001, max=1, step=0.01),
        numericInput("subsample", "Submuestra de observaciones", value=0.8, min=0.3, max=1, step=0.05),
        numericInput("colsample", "Submuestra de variables", value=0.8, min=0.3, max=1, step=0.05)
      )
    }
  })
  
  output$head_tbl <- renderTable(head(data_clean(), 10), rownames=TRUE)
  output$summary_txt <- renderPrint(summary(data_clean()))
  
  output$scatter_plot <- renderPlot({
    df <- data_clean(); req(input$y, input$x); validate(need(length(input$x)>=1,"Selecciona al menos un predictor."))
    x <- input$x[1]
    plot(df[[x]], df[[input$y]], pch=19, col="#2166AC", xlab=x, ylab=input$y,
         main=paste("Relación entre", x, "y", input$y))
    if (is.numeric(df[[x]]) && is.numeric(df[[input$y]])) {
      lines(lowess(df[[x]], df[[input$y]]), col="#B2182B", lwd=2)
    }
  })
  
  formula_obj <- reactive({
    req(input$y, input$x)
    validate(need(length(input$x)>0,"Selecciona al menos un predictor."))
    as.formula(paste("`", input$y, "` ~ ", paste(sprintf("`%s`", input$x), collapse=" + "), sep=""))
  })
  
  # Preparación para modelos que necesitan matriz numérica.
  matrix_data <- function(train, test, y, x) {
    f <- as.formula(paste("~", paste(sprintf("`%s`", x), collapse=" + ")))
    Xtr <- model.matrix(f, train)
    Xte <- model.matrix(f, test)
    if ("(Intercept)" %in% colnames(Xtr)) Xtr <- Xtr[, colnames(Xtr)!="(Intercept)",drop=FALSE]
    if ("(Intercept)" %in% colnames(Xte)) Xte <- Xte[, colnames(Xte)!="(Intercept)",drop=FALSE]
    allc <- union(colnames(Xtr), colnames(Xte))
    add_cols <- function(X, allc) {
      miss <- setdiff(allc, colnames(X))
      if (length(miss)) for (z in miss) X <- cbind(X, setNames(data.frame(rep(0,nrow(X))),z))
      X[,allc,drop=FALSE]
    }
    list(Xtr=add_cols(Xtr,allc), Xte=add_cols(Xte,allc))
  }
  
  fit_model <- eventReactive(input$fit, {
    df <- data_clean(); req(input$y,input$x)
    validate(need(is.numeric(df[[input$y]]), "Y debe ser numérica para regresión."))
    f <- formula_obj()
    
    if (input$model_type == "cart") {
      return(list(kind="cart", fit=rpart::rpart(f, data=df, method="anova", control=rpart::rpart.control(cp=input$cp, minsplit=input$minsplit, maxdepth=input$maxdepth)), data=df))
    }
    
    if (input$model_type == "rf") {
      validate(need(has_pkg("randomForest"), "No está instalado randomForest. La aplicación ofrece una alternativa."))
      x <- df[input$x]
      mtry_val <- input$mtry
      if (is.null(mtry_val) || !is.finite(mtry_val)) mtry_val <- max(1, floor(sqrt(length(input$x))))
      fit <- randomForest::randomForest(x=x, y=df[[input$y]], ntree=input$ntree, mtry=min(mtry_val,ncol(x)), nodesize=input$nodesize, importance=TRUE)
      return(list(kind="rf", fit=fit, data=df))
    }
    
    if (input$model_type == "xgb") {
      validate(need(has_pkg("xgboost"), "XGBoost no está disponible. Selecciona la alternativa automática."))
      mx <- matrix_data(df, df, input$y, input$x)
      fit <- xgboost::xgb.train(
        params=list(objective="reg:squarederror", max_depth=input$max_depth, eta=input$eta,
                    subsample=input$subsample, colsample_bytree=input$colsample, eval_metric="rmse"),
        data=xgboost::xgb.DMatrix(mx$Xtr, label=df[[input$y]]), nrounds=input$nrounds, verbose=0
      )
      return(list(kind="xgb", fit=fit, data=df, X=mx$Xtr))
    }
    
    # Fallback: si no hay XGBoost, usar RF; si tampoco existe, CART.
    if (has_pkg("randomForest")) {
      x <- df[input$x]
      mtry_val <- max(1, floor(sqrt(length(input$x))))
      fit <- randomForest::randomForest(x=x, y=df[[input$y]], ntree=200, mtry=min(mtry_val,ncol(x)), nodesize=5, importance=TRUE)
      return(list(kind="fallback_rf", fit=fit, data=df))
    }
    fit <- rpart::rpart(f, data=df, method="anova", control=rpart::rpart.control(cp=input$cp, minsplit=input$minsplit, maxdepth=input$maxdepth))
    list(kind="fallback_cart", fit=fit, data=df)
  })
  
  model_fit <- reactive({
    req(fit_model()); fit_model()
  })
  
  output$model_info <- renderPrint({
    m <- model_fit()
    cat("Modelo:", switch(m$kind, cart="Árbol CART", rf="Random Forest", xgb="XGBoost", fallback_rf="Random Forest (fallback)", fallback_cart="Árbol CART (fallback)"), "\n\n")
    if (inherits(m$fit,"rpart")) print(m$fit)
    if (inherits(m$fit,"randomForest")) print(m$fit)
    if (m$kind=="xgb") print(m$fit)
  })
  
  output$predictors_tbl <- renderTable({
    req(input$x)
    data.frame(Predictor=input$x, Tipo=sapply(data_clean()[input$x], function(z) if(is.factor(z)) "Categórica" else class(z)[1]), row.names=NULL)
  })
  
  # Importancia común
  importance_data <- reactive({
    m <- model_fit()
    if (m$kind %in% c("cart","fallback_cart")) {
      z <- m$fit$variable.importance
      validate(need(!is.null(z),"El árbol no presenta importancia calculable."))
      return(data.frame(Variable=names(z), Importancia=as.numeric(z), stringsAsFactors=FALSE))
    }
    if (m$kind %in% c("rf","fallback_rf")) {
      z <- randomForest::importance(m$fit, type=1)
      z <- as.data.frame(z); z$Variable <- rownames(z); rownames(z)<-NULL
      val <- z[[1]]; return(data.frame(Variable=z$Variable, Importancia=as.numeric(val)))
    }
    if (m$kind=="xgb") {
      z <- xgboost::xgb.importance(model=m$fit)
      return(data.frame(Variable=z$Feature, Importancia=z$Gain, stringsAsFactors=FALSE))
    }
  })
  
  render_importance <- function() {
    z <- importance_data(); z <- z[order(z$Importancia,decreasing=TRUE),,drop=FALSE]
    z <- head(z,20)
    op <- par(mar=c(5,8,3,2)); on.exit(par(op),add=TRUE)
    barplot(rev(z$Importancia), horiz=TRUE, names.arg=rev(z$Variable), las=1,
            col="#2166AC", xlab="Importancia", main="Importancia de variables")
  }
  output$importance_plot <- renderPlot({ render_importance() })
  output$importance_plot2 <- renderPlot({ render_importance() })
  output$importance_tbl <- renderTable({ z<-importance_data(); z[order(z$Importancia,decreasing=TRUE),] }, digits=5)
  output$importance_tbl2 <- renderTable({ z<-importance_data(); z[order(z$Importancia,decreasing=TRUE),] }, digits=5)
  
  output$cart_tree_plot <- renderPlot({
    m<-model_fit(); validate(need(inherits(m$fit,"rpart"),"El modelo actual no es un árbol CART."))
    plot(m$fit, uniform=TRUE, margin=0.1); text(m$fit, use.n=TRUE, all=TRUE, cex=.8)
  })
  output$cart_rules <- renderPrint({
    m<-model_fit(); validate(need(inherits(m$fit,"rpart"),"El modelo actual no es un árbol CART.")); print(m$fit$frame); cat("\n\nReglas:\n"); rpart::path.rpart(m$fit, nodes=rownames(m$fit$frame)[m$fit$frame$var=="<leaf>"])
  })
  output$cart_nodes_tbl <- renderTable({
    m<-model_fit(); validate(need(inherits(m$fit,"rpart"),"El modelo actual no es un árbol CART."))
    fr<-m$fit$frame; data.frame(Nodo=rownames(fr), Variable=fr$var, N=fr$n, Error=round(fr$dev,4), Prediccion=round(fr$yval,4), row.names=NULL)
  })
  
  output$rf_tree_ui <- renderUI({
    m<-model_fit(); validate(need(inherits(m$fit,"randomForest"),"El modelo actual no es Random Forest."))
    sliderInput("rf_tree", "Árbol del bosque", min=1, max=m$fit$ntree, value=1, step=1)
  })
  rf_tree_data <- reactive({
    m<-model_fit(); req(input$rf_tree); validate(need(inherits(m$fit,"randomForest"),"El modelo actual no es Random Forest."))
    randomForest::getTree(m$fit, k=input$rf_tree, labelVar=TRUE)
  })
  output$rf_nodes_tbl <- renderTable({ rf_tree_data() })
  output$rf_tree_plot <- renderPlot({
    
    tr <- rf_tree_data()
    
    validate(
      need(!is.null(tr), "No se pudo recuperar el árbol."),
      need(nrow(tr) > 0, "El árbol no contiene nodos.")
    )
    
    # --------------------------------------------------
    # Normalizar nombres de columnas
    # --------------------------------------------------
    left  <- as.numeric(tr[["left daughter"]])
    right <- as.numeric(tr[["right daughter"]])
    status <- as.numeric(tr[["status"]])
    
    n <- nrow(tr)
    
    # --------------------------------------------------
    # Calcular profundidad de cada nodo
    # --------------------------------------------------
    depth <- rep(NA_real_, n)
    depth[1] <- 0
    
    # Recorrido desde la raíz
    queue <- 1
    
    while (length(queue) > 0) {
      
      parent <- queue[1]
      queue <- queue[-1]
      
      children <- c(left[parent], right[parent])
      children <- children[is.finite(children) & children > 0]
      
      for (child in children) {
        
        child <- as.integer(child)
        
        if (child >= 1 && child <= n && is.na(depth[child])) {
          depth[child] <- depth[parent] + 1
          queue <- c(queue, child)
        }
      }
    }
    
    # Por seguridad, cualquier nodo no alcanzado desde la raíz
    # se coloca en el nivel 0
    depth[is.na(depth)] <- 0
    
    maxd <- max(depth)
    
    # --------------------------------------------------
    # Posición horizontal de los nodos
    # --------------------------------------------------
    xpos <- rep(NA_real_, n)
    
    levels <- sort(unique(depth))
    
    for (d in levels) {
      
      ids <- which(depth == d)
      
      if (length(ids) == 1) {
        xpos[ids] <- 0.5
      } else {
        xpos[ids] <- seq(
          0.05,
          0.95,
          length.out = length(ids)
        )
      }
    }
    
    ypos <- 1 - (depth / max(1, maxd)) * 0.85
    
    # --------------------------------------------------
    # Preparar espacio gráfico
    # --------------------------------------------------
    plot(
      NA,
      xlim = c(0, 1),
      ylim = c(0, 1),
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      bty = "n",
      main = paste(
        "Árbol", input$rf_tree,
        "del Random Forest"
      )
    )
    
    # --------------------------------------------------
    # Dibujar conexiones
    # --------------------------------------------------
    for (i in seq_len(n)) {
      
      children <- c(left[i], right[i])
      
      for (child in children) {
        
        if (
          is.finite(child) &&
          child > 0 &&
          child <= n
        ) {
          
          segments(
            xpos[i],
            ypos[i],
            xpos[child],
            ypos[child],
            lwd = 1
          )
        }
      }
    }
    
    # --------------------------------------------------
    # Dibujar nodos
    # --------------------------------------------------
    for (i in seq_len(n)) {
      
      if (status[i] == -1) {
        
        # Hoja
        pred <- tr[["prediction"]][i]
        
        label <- paste0(
          "Nodo ", i,
          "\nHOJA",
          "\nŷ = ",
          round(pred, 2)
        )
        
      } else {
        
        # Nodo de decisión
        split_var <- as.character(tr[["split var"]][i])
        split_point <- tr[["split point"]][i]
        
        if (
          is.na(split_point) ||
          !is.finite(as.numeric(split_point))
        ) {
          
          label <- paste0(
            "Nodo ", i,
            "\n",
            split_var
          )
          
        } else {
          
          label <- paste0(
            "Nodo ", i,
            "\n",
            split_var,
            "\n≤ ",
            round(as.numeric(split_point), 3)
          )
        }
      }
      
      text(
        xpos[i],
        ypos[i],
        labels = label,
        cex = 0.75
      )
    }
    
  })
  
  output$xgb_tree_ui <- renderUI({
    m<-model_fit(); validate(need(m$kind=="xgb","El modelo actual no es XGBoost."))
    n<-length(m$fit$evaluation_log$iter)
    sliderInput("xgb_tree", "Árbol", min=0, max=max(0,n-1), value=0, step=1)
  })
  xgb_tree_data <- reactive({
    m<-model_fit(); req(input$xgb_tree); validate(need(m$kind=="xgb","El modelo actual no es XGBoost."))
    xgboost::xgb.model.dt.tree(model=m$fit, trees=input$xgb_tree)
  })
  output$xgb_nodes_tbl <- renderTable({ xgb_tree_data() })
  output$xgb_tree_plot <- renderPlot({
    m<-model_fit(); validate(need(m$kind=="xgb","El modelo actual no es XGBoost."))
    tryCatch(xgboost::xgb.plot.tree(model=m$fit, trees_to_plot=input$xgb_tree, show_node_id=TRUE), error=function(e){
      plot.new(); text(.5,.6,"No se puede dibujar gráficamente este árbol en este entorno.\nUtiliza la tabla de nodos inferior.",cex=1.1); text(.5,.4,e$message,cex=.8)
    })
  })
  
  output$fallback_tree_ui <- renderUI({
    m<-model_fit()
    if (inherits(m$fit,"randomForest")) return(sliderInput("fallback_tree", "Árbol del Random Forest", min=1, max=m$fit$ntree, value=1, step=1))
    NULL
  })
  output$fallback_tree_plot <- renderPlot({
    m<-model_fit(); validate(need(m$kind %in% c("fallback_cart","fallback_rf"),"No hay modelo fallback."))
    if (inherits(m$fit,"rpart")) {
      plot(m$fit,uniform=TRUE,margin=.1); text(m$fit,use.n=TRUE,all=TRUE,cex=.8)
    } else {
      req(input$fallback_tree)
      tr<-randomForest::getTree(m$fit,k=input$fallback_tree,labelVar=TRUE)
      plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1)); title(paste("Árbol",input$fallback_tree,"del Random Forest"))
      n<-nrow(tr); depth<-numeric(n); depth[1]<-0
      for(i in seq_len(n)) if(i>1) depth[i]<-depth[tr$`left daughter`[i]]+1
      maxd<-max(depth); xpos<-numeric(n); bydep<-split(seq_len(n),depth)
      for(d in names(bydep)){ids<-bydep[[d]];xpos[ids]<-seq(.1,.9,length.out=length(ids))}
      ypos<-1-(depth+1)/(maxd+2)
      for(i in seq_len(n)) for(ch in c(tr$`left daughter`[i],tr$`right daughter`[i])) if(ch>0) segments(xpos[i],ypos[i],xpos[ch],ypos[ch])
      for(i in seq_len(n)) { lab<-if(tr$status[i]==-1) paste0("Nodo ",i,"\nhoja\nŷ=",round(tr$prediction[i],2)) else paste0("Nodo ",i,"\n",tr$`split var`[i],"\n",round(tr$`split point`[i],2)); text(xpos[i],ypos[i],lab,cex=.65) }
    }
  })
  output$fallback_nodes_tbl <- renderTable({
    m<-model_fit();
    if (inherits(m$fit,"rpart")) { fr<-m$fit$frame; return(data.frame(Nodo=rownames(fr),Variable=fr$var,N=fr$n,Error=round(fr$dev,4),Prediccion=round(fr$yval,4),row.names=NULL)) }
    req(input$fallback_tree); randomForest::getTree(m$fit,k=input$fallback_tree,labelVar=TRUE)
  })
  
  # Train/test
  split_results <- eventReactive(input$split_fit, {
    df<-data_clean(); req(input$y,input$x); n<-nrow(df); validate(need(n>=10,"Necesitas al menos 10 observaciones."))
    set.seed(123); idx<-sample(seq_len(n),floor(n*input$train_pct/100)); train<-df[idx,,drop=FALSE]; test<-df[-idx,,drop=FALSE]
    ytr<-train[[input$y]]; yte<-test[[input$y]]
    pred_train<-pred_test<-NULL
    mtype<-input$model_type
    
    if (mtype %in% c("cart","fallback")) {
      f<-formula_obj(); fit<-rpart::rpart(f,data=train,method="anova",control=rpart::rpart.control(cp=input$cp,minsplit=input$minsplit,maxdepth=input$maxdepth)); pred_train<-predict(fit,train); pred_test<-predict(fit,test)
    } else if (mtype=="rf") {
      xtr<-train[input$x]; xte<-test[input$x]; mv<-input$mtry; if(is.null(mv)||!is.finite(mv)) mv<-max(1,floor(sqrt(length(input$x))))
      fit<-randomForest::randomForest(x=xtr,y=ytr,ntree=input$ntree,mtry=min(mv,ncol(xtr)),nodesize=input$nodesize,importance=TRUE); pred_train<-predict(fit,xtr); pred_test<-predict(fit,xte)
    } else if (mtype=="xgb") {
      mx<-matrix_data(train,test,input$y,input$x); fit<-xgboost::xgb.train(params=list(objective="reg:squarederror",max_depth=input$max_depth,eta=input$eta,subsample=input$subsample,colsample_bytree=input$colsample,eval_metric="rmse"),data=xgboost::xgb.DMatrix(mx$Xtr,label=ytr),nrounds=input$nrounds,verbose=0); pred_train<-as.numeric(predict(fit,mx$Xtr)); pred_test<-as.numeric(predict(fit,mx$Xte))
    } else if (mtype=="fallback") {
      if (has_pkg("randomForest")) { fit<-randomForest::randomForest(x=train[input$x],y=ytr,ntree=200,mtry=max(1,floor(sqrt(length(input$x)))),nodesize=5,importance=TRUE); pred_train<-predict(fit,train[input$x]); pred_test<-predict(fit,test[input$x]) } else { f<-formula_obj(); fit<-rpart::rpart(f,data=train,method="anova",control=rpart::rpart.control(cp=input$cp,minsplit=input$minsplit,maxdepth=input$maxdepth)); pred_train<-predict(fit,train); pred_test<-predict(fit,test) }
    }
    rmse<-function(y,p)sqrt(mean((y-p)^2)); r2<-function(y,p)1-sum((y-p)^2)/sum((y-mean(y))^2)
    metrics<-data.frame(Conjunto=c("Entrenamiento","Test"),RMSE=c(rmse(ytr,pred_train),rmse(yte,pred_test)),R2=c(r2(ytr,pred_train),r2(yte,pred_test)))
    pred_tbl<-data.frame(Real=yte,Prediccion=as.numeric(pred_test),Error=yte-as.numeric(pred_test))
    list(metrics=metrics,pred_tbl=pred_tbl)
  })
  
  output$metrics_tbl<-renderTable({req(split_results());split_results()$metrics},digits=4)
  output$pred_tbl<-renderTable({req(split_results());head(split_results()$pred_tbl,100)},digits=4)
  output$pred_plot<-renderPlot({
    r<-split_results(); z<-r$pred_tbl; plot(z$Real,z$Prediccion,pch=19,col="#2166AC",xlab="Valor observado",ylab="Predicción",main="Predicciones frente a observaciones"); abline(0,1,col="#B2182B",lwd=2,lty=2)
  })
  
  output$interpretation_txt<-renderPrint({
    m<-model_fit(); imp<-importance_data(); imp<-imp[order(imp$Importancia,decreasing=TRUE),]
    cat("Interpretación del modelo\n\n")
    cat("1. El modelo no presupone una relación lineal entre las variables explicativas y Y.\n")
    cat("2. Las divisiones de los árboles generan reglas del tipo 'si X <= umbral, entonces...'.\n")
    cat("3. En un árbol individual, las hojas contienen predicciones finales para los casos que llegan a ellas.\n")
    cat("4. En Random Forest, muchas predicciones de árboles se combinan para obtener una predicción más estable.\n")
    cat("5. En XGBoost, los árboles se construyen secuencialmente, intentando reducir el error residual de los anteriores.\n")
    cat("6. La importancia de variables indica contribución al modelo predictivo, no causalidad.\n\n")
    cat("Variable con mayor importancia en este ajuste:", imp$Variable[1], "\n")
  })
  
  output$comparison_plot<-renderPlot({
    df<-data_clean(); req(input$y,input$x); x<-input$x[1]
    if(!is.numeric(df[[x]]) || !is.numeric(df[[input$y]])){ plot.new();text(.5,.5,"La comparación gráfica requiere X e Y numéricas.");return() }
    f<-as.formula(paste("`",input$y,"` ~ `",x,"`",sep="")); lmfit<-lm(f,data=df); tree<-rpart::rpart(f,data=df,method="anova")
    xx<-seq(min(df[[x]]),max(df[[x]]),length.out=200); plot(df[[x]],df[[input$y]],pch=19,col="#777777",xlab=x,ylab=input$y,main="Regresión lineal frente a árbol")
    lines(xx,predict(lmfit,newdata=data.frame(setNames(list(xx),x))),col="#B2182B",lwd=2)
    lines(xx,predict(tree,newdata=data.frame(setNames(list(xx),x))),col="#2166AC",lwd=3,type="s")
    legend("topleft",c("Regresión lineal","Árbol"),col=c("#B2182B","#2166AC"),lwd=c(2,3),bty="n")
  })
  
  output$download_summary<-downloadHandler(filename=function(){paste0("resumen_arboles_",Sys.Date(),".txt")},content=function(file){m<-model_fit(); sink(file); cat("MACHINE LEARNING - REGRESIÓN\n\n"); print(m$fit); cat("\n\nImportancia de variables\n"); print(importance_data()); sink()})
  output$download_predictions<-downloadHandler(filename=function(){paste0("predicciones_",Sys.Date(),".csv")},content=function(file){req(split_results());write.csv(split_results()$pred_tbl,file,row.names=FALSE)})
}

shinyApp(ui, server)
