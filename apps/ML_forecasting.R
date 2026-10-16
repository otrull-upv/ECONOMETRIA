library(shiny)
library(nnet)

# ============================================================
# MACHINE LEARNING: SERIES TEMPORALES CON MLP Y NARX
# ============================================================

rmse <- function(y,p) sqrt(mean((y-p)^2,na.rm=TRUE))
mae  <- function(y,p) mean(abs(y-p),na.rm=TRUE)
r2 <- function(y,p) {
  den <- sum((y-mean(y))^2)
  if(den==0) return(NA_real_)
  1-sum((y-p)^2)/den
}

make_lags <- function(y,x=NULL,ly=5,lx=2) {
  n <- length(y)
  ml <- max(ly,if(is.null(x)) 0 else lx)
  if(n<=ml+5) return(NULL)
  z <- data.frame(y=y[(ml+1):n])
  for(k in 1:ly)
    z[[paste0("Y_t-",k)]] <- y[(ml+1-k):(n-k)]
  if(!is.null(x) && length(x)>0)
    for(j in seq_along(x))
      for(k in 0:lx)
        z[[paste0(names(x)[j],"_t-",k)]] <-
    x[[j]][(ml+1-k):(n-k)]
  z
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .app-title{color:#0B2D5B;font-size:2.2rem;font-weight:700}
    .info-box{background:#f5f8fc;border-left:5px solid #0B5B8A;padding:12px 15px;margin:10px 0 15px}
    .warning-box{background:#fff8e1;border-left:5px solid #f0ad4e;padding:12px 15px;margin:10px 0 15px}
    body{padding-bottom:50px}
    #footer-fixed{position:fixed;bottom:0;left:0;width:100%;background:#f9f9f9;padding:8px;text-align:center;border-top:1px solid #ccc;z-index:9999}
  ")),
            tags$script(HTML("Shiny.addCustomMessageHandler('a11y',function(x){document.body.classList.toggle('a11y',!!x);});"))),
  
  h1("MACHINE LEARNING: Series temporales con MLP y NARX",class="app-title"),
  checkboxInput("a11y","Modo accesible",FALSE),
  div(id="footer-fixed",HTML("&copy; 2026 Oscar Trull · Aplicación Shiny")),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Sube un CSV",accept=".csv"),
      checkboxInput("header","El CSV tiene cabecera",TRUE),
      radioButtons("sep","Separador",
                   choices=c(Coma=",",PuntoyComa=";",Tab="\t"),selected=","),
      checkboxInput("example","Usar dataset de ejemplo",TRUE),
      tags$hr(),
      
      uiOutput("vars"),
      selectInput("model","Modelo",
                  c("MLP autoregresivo"="mlp","NARX"="narx")),
      
      numericInput("ly","Retardos de Y",5,1,30,1),
      conditionalPanel(
        "input.model == 'narx'",
        numericInput("lx","Retardos de X",2,0,20,1)
      ),
      numericInput("hidden","Neuronas capa oculta",8,1,50,1),
      numericInput("decay","Regularización (decay)",0.001,0,1,0.001),
      numericInput("maxit","Iteraciones máximas",500,50,5000,50),
      checkboxInput("scale","Estandarizar variables",TRUE),
      
      actionButton("fit","Entrenar red neuronal",
                   class="btn-primary",width="100%"),
      tags$hr(),
      
      sliderInput("train_pct","Porcentaje entrenamiento",
                  50,90,80,5),
      actionButton("split","Entrenar / Test",
                   class="btn-success",width="100%"),
      tags$hr(),
      
      downloadButton("download_summary","Descargar resumen"),
      downloadButton("download_predictions","Descargar predicciones")
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Datos",
                 div(class="info-box",
                     HTML("<b>Idea clave:</b> el orden temporal importa. La validación debe respetar el tiempo y no mezclar aleatoriamente pasado y futuro.")),
                 h4("Vista previa"),tableOutput("head"),
                 h4("Resumen"),verbatimTextOutput("summary"),
                 h4("Serie temporal"),plotOutput("series",height="450px"),
                 h4("Relación con retardos"),plotOutput("lags",height="450px")
        ),
        
        tabPanel("Modelo",
                 h4("Configuración"),verbatimTextOutput("model_info"),
                 h4("Entradas"),tableOutput("inputs"),
                 h4("Arquitectura MLP"),plotOutput("network",height="550px")
        ),
        
        tabPanel("Retardos / NARX",
                 div(class="info-box",
                     HTML("<b>MLP:</b> aprende Y(t) a partir de retardos de Y. <br><br>
                    <b>NARX:</b> incorpora además variables exógenas X y sus retardos.")),
                 h4("Matriz de retardos"),tableOutput("lag_table"),
                 h4("Importancia por permutación"),
                 plotOutput("importance",height="450px"),
                 tableOutput("importance_table")
        ),
        
        tabPanel("Train / Test",
                 h4("Métricas"),tableOutput("metrics"),
                 h4("Predicción frente a observado"),plotOutput("pred",height="450px"),
                 h4("Predicción temporal"),plotOutput("temporal",height="500px"),
                 h4("Predicciones de test"),tableOutput("pred_table")
        ),
        
        tabPanel("Interpretación",
                 h4("Lectura didáctica"),verbatimTextOutput("interpretation"),
                 h4("MLP frente a NARX"),tableOutput("comparison"),
                 div(class="warning-box",
                     HTML("<b>Importante:</b> un buen ajuste sobre entrenamiento no garantiza una buena predicción futura. La validación temporal es fundamental."))
        )
      )
    )
  )
)

server <- function(input,output,session){
  
  observeEvent(input$a11y,
               session$sendCustomMessage("a11y",isTRUE(input$a11y)),ignoreInit=FALSE)
  
  raw <- reactive({
    if(input$example){
      set.seed(123); n <- 144; t <- 1:n
      temp <- 18+9*sin(2*pi*t/12)+rnorm(n,0,1.5)
      y <- 100+.20*t+12*cos(2*pi*t/12)+.35*temp+
        as.numeric(arima.sim(list(ar=.65),n=n,sd=4))
      data.frame(Periodo=seq.Date(as.Date("2015-01-01"),
                                  by="month",length.out=n),
                 Demanda=y,Temperatura=temp)
    } else {
      req(input$file)
      read.csv(input$file$datapath,header=input$header,
               sep=input$sep,stringsAsFactors=FALSE,check.names=FALSE)
    }
  })
  
  dat <- reactive({
    d <- raw()
    for(nm in names(d))
      if(is.character(d[[nm]])){
        z <- suppressWarnings(as.numeric(d[[nm]]))
        if(sum(!is.na(z)) >= .8*nrow(d)) d[[nm]] <- z
      }
    d
  })
  
  output$vars <- renderUI({
    d <- dat()
    nums <- names(d)[vapply(d,is.numeric,logical(1))]
    validate(need(length(nums)>0,"Necesitas al menos una variable numérica."))
    tagList(
      selectInput("y","Variable objetivo Y",nums,nums[1]),
      conditionalPanel(
        "input.model == 'narx'",
        selectizeInput("x","Variables exógenas X",
                       setdiff(nums,nums[1]),multiple=TRUE)
      )
    )
  })
  
  output$head <- renderTable(head(dat(),10),rownames=TRUE)
  output$summary <- renderPrint(summary(dat()))
  
  output$series <- renderPlot({
    d <- dat(); req(input$y)
    plot(d[[input$y]],type="l",lwd=2,xlab="Tiempo",
         ylab=input$y,main=paste("Serie",input$y))
    if(input$model=="narx" && length(input$x)){
      par(new=TRUE)
      plot(d[[input$x[1]]],type="l",lty=2,axes=FALSE,
           xlab="",ylab="")
    }
  })
  
  output$lags <- renderPlot({
    d <- dat(); req(input$y)
    y <- d[[input$y]]; k <- min(input$ly,5)
    old <- par(mfrow=c(ceiling(k/2),min(k,2)))
    on.exit(par(old))
    for(i in 1:k)
      plot(y[(i+1):length(y)],y[1:(length(y)-i)],
           pch=19,xlab=paste0("Y(t-",i,")"),ylab="Y(t)",
           main=paste("Retardo",i))
  })
  
  lagdata <- reactive({
    d <- dat(); req(input$y)
    xx <- NULL
    if(input$model=="narx"){
      validate(need(length(input$x)>0,"Para NARX selecciona al menos una X."))
      xx <- d[input$x]
    }
    z <- make_lags(d[[input$y]],xx,input$ly,
                   if(input$model=="narx") input$lx else 0)
    validate(need(!is.null(z) && nrow(z)>20,
                  "No hay suficientes observaciones después de crear retardos."))
    z
  })
  
  output$lag_table <- renderTable(head(lagdata(),20),digits=4)
  
  fit <- eventReactive(input$fit,{
    z <- lagdata(); n <- nrow(z)
    ntr <- floor(n*input$train_pct/100)
    validate(need(ntr>=15 && n-ntr>=5,"Aumenta el número de observaciones o cambia el porcentaje."))
    
    tr <- z[1:ntr,,drop=FALSE]; te <- z[(ntr+1):n,,drop=FALSE]
    xn <- names(z)[-1]
    
    scx <- list(); sy <- c(mean=0,sd=1)
    trx <- tr[,xn,drop=FALSE]; tex <- te[,xn,drop=FALSE]
    
    if(input$scale){
      for(nm in xn){
        mu <- mean(trx[[nm]],na.rm=TRUE)
        sdv <- sd(trx[[nm]],na.rm=TRUE); if(!is.finite(sdv)||sdv==0) sdv <- 1
        scx[[nm]] <- c(mean=mu,sd=sdv)
        trx[[nm]] <- (trx[[nm]]-mu)/sdv
        tex[[nm]] <- (tex[[nm]]-mu)/sdv
      }
      sy <- c(mean=mean(tr$y),sd=sd(tr$y))
      if(!is.finite(sy["sd"])||sy["sd"]==0) sy["sd"] <- 1
      try <- (tr$y-sy["mean"])/sy["sd"]
    } else try <- tr$y
    
    net <- nnet::nnet(as.matrix(trx),try,size=input$hidden,
                      linout=TRUE,decay=input$decay,
                      maxit=input$maxit,trace=FALSE)
    
    unscale <- function(p) p*sy["sd"]+sy["mean"]
    ptr <- unscale(as.numeric(predict(net,as.matrix(trx))))
    pte <- unscale(as.numeric(predict(net,as.matrix(tex))))
    
    metrics <- data.frame(
      Conjunto=c("Entrenamiento","Test"),
      RMSE=c(rmse(tr$y,ptr),rmse(te$y,pte)),
      MAE=c(mae(tr$y,ptr),mae(te$y,pte)),
      R2=c(r2(tr$y,ptr),r2(te$y,pte))
    )
    
    list(net=net,train=tr,test=te,ptr=ptr,pte=pte,
         metrics=metrics,xn=xn,scx=scx,sy=sy)
  })
  
  output$model_info <- renderPrint({
    r <- fit()
    cat("Modelo:",if(input$model=="mlp")
      "MLP autoregresivo" else "NARX","\n")
    cat("Entradas:",length(r$xn),"\n")
    cat("Neuronas ocultas:",input$hidden,"\n")
    cat("Retardos Y:",input$ly,"\n")
    if(input$model=="narx") cat("Retardos X:",input$lx,"\n")
    cat("Regularización:",input$decay,"\n")
    cat("Iteraciones máximas:",input$maxit,"\n\n")
    print(r$net)
  })
  
  output$inputs <- renderTable({
    data.frame(Entrada=fit()$xn)
  })
  
  output$network <- renderPlot({
    r <- fit(); p <- length(r$xn); h <- input$hidden
    plot(NA,xlim=c(0,1),ylim=c(0,1),axes=FALSE,
         xlab="",ylab="",bty="n",main="Arquitectura de la red")
    a <- seq(.1,.9,length.out=max(1,p))
    b <- seq(.1,.9,length.out=max(1,h))
    for(i in a) for(j in b) segments(.15,i,.5,j,col="grey80")
    for(j in b) segments(.5,j,.85,.5,col="grey60")
    points(rep(.15,p),a,pch=21,bg="white",cex=1.5)
    points(rep(.5,h),b,pch=21,bg="white",cex=1.5)
    points(.85,.5,pch=21,bg="white",cex=1.8)
    text(.15,.97,"Entradas",font=2)
    text(.5,.97,"Capa oculta",font=2)
    text(.85,.97,"Salida",font=2)
    if(p<=12) text(rep(.15,p),a,r$xn,pos=4,cex=.65)
    text(.85,.5,"Y(t)",pos=4)
  })
  
  importance <- reactive({
    r <- fit(); base <- rmse(r$test$y,r$pte)
    z <- numeric(length(r$xn)); set.seed(123)
    for(j in seq_along(r$xn)){
      xx <- r$test[,r$xn,drop=FALSE]
      xx[[r$xn[j]]] <- sample(xx[[r$xn[j]]])
      if(input$scale)
        for(nm in r$xn)
          xx[[nm]] <- (xx[[nm]]-r$scx[[nm]]["mean"])/r$scx[[nm]]["sd"]
      pp <- as.numeric(predict(r$net,as.matrix(xx)))
      pp <- pp*r$sy["sd"]+r$sy["mean"]
      z[j] <- rmse(r$test$y,pp)-base
    }
    data.frame(Entrada=r$xn,Importancia=z)
  })
  
  output$importance_table <- renderTable({
    z <- importance()
    z[order(z$Importancia,decreasing=TRUE),]
  },digits=5)
  
  output$importance <- renderPlot({
    z <- importance()
    z <- z[order(z$Importancia,decreasing=TRUE),]
    z <- head(z,20)
    par(mar=c(5,10,3,2))
    barplot(rev(z$Importancia),horiz=TRUE,las=1,
            names.arg=rev(z$Entrada),
            xlab="Incremento del RMSE al permutar",
            main="Importancia predictiva")
  })
  
  output$metrics <- renderTable(fit()$metrics,digits=4)
  
  output$pred_table <- renderTable({
    r <- fit()
    data.frame(Real=r$test$y,Prediccion=r$pte,
               Error=r$test$y-r$pte)
  },digits=4)
  
  output$pred <- renderPlot({
    r <- fit()
    plot(r$test$y,r$pte,pch=19,
         xlab="Observado",ylab="Predicción",
         main="Predicción frente a observación")
    abline(0,1,lty=2,lwd=2)
  })
  
  output$temporal <- renderPlot({
    r <- fit()
    real <- c(r$train$y,r$test$y)
    pred <- c(r$ptr,r$pte)
    ntr <- length(r$train$y)
    plot(real,type="l",lwd=2,xlab="Tiempo",ylab=input$y,
         main="Predicción temporal")
    lines(pred,lty=2,lwd=2)
    abline(v=ntr+.5,lty=3,lwd=2)
    legend("topleft",c("Observado","Predicción","Inicio test"),
           lty=c(1,2,3),lwd=2,bty="n")
  })
  
  output$interpretation <- renderPrint({
    r <- fit(); z <- importance()
    z <- z[order(z$Importancia,decreasing=TRUE),]
    cat("INTERPRETACIÓN\n\n")
    if(input$model=="mlp"){
      cat("• El MLP utiliza retardos de Y para predecir Y(t).\n")
      cat("• Aprende una relación no lineal entre el pasado y el futuro de la serie.\n")
    } else {
      cat("• NARX significa Nonlinear AutoRegressive with eXogenous inputs.\n")
      cat("• Utiliza retardos de Y y variables exógenas X.\n")
      cat("• Permite estudiar relaciones no lineales entre la dinámica de Y y factores externos.\n")
    }
    cat("\n• La validación mantiene el orden temporal.\n")
    cat("• RMSE, MAE y R2 permiten comparar entrenamiento y test.\n")
    if(nrow(z)) cat("\nEntrada con mayor importancia predictiva:",z$Entrada[1],"\n")
  })
  
  output$comparison <- renderTable(data.frame(
    Caracteristica=c("Entradas","Memoria temporal",
                     "Variables exógenas","Relación"),
    MLP=c("Retardos de Y","Y(t-1), Y(t-2), ...","No","No lineal"),
    NARX=c("Retardos de Y + X","Y y X retardadas","Sí","No lineal")
  ))
  
  output$download_summary <- downloadHandler(
    filename=function() paste0("MLP_NARX_",Sys.Date(),".txt"),
    content=function(file){
      r <- fit(); sink(file)
      cat("MACHINE LEARNING - SERIES TEMPORALES\n\n")
      cat("Modelo:",input$model,"\n")
      cat("Retardos Y:",input$ly,"\n")
      if(input$model=="narx") cat("Retardos X:",input$lx,"\n")
      cat("Neuronas ocultas:",input$hidden,"\n\n")
      print(r$metrics); cat("\nImportancia\n"); print(importance())
      sink()
    })
  
  output$download_predictions <- downloadHandler(
    filename=function() paste0("predicciones_MLP_NARX_",Sys.Date(),".csv"),
    content=function(file) write.csv(
      data.frame(Real=fit()$test$y,Prediccion=fit()$pte),
      file,row.names=FALSE)
  )
}

shinyApp(ui,server)
