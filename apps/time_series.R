# app.R  –  ECONOMETRÍA: Series Temporales
# Pestañas: Datos | Serie temporal | Descomposición | Suavizado | ARIMA | ARIMAX | Validación

library(shiny)
library(bslib)
library(ggplot2)
library(forecast)
library(tseries)
library(DT)
library(lubridate)
library(tsibble)
library(FinTS)
library(zoo)
library(seasonal)   # X-13ARIMA-SEATS / TRAMO-SEATS

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── Helpers ────────────────────────────────────────────────────────────────────
safe_req_numeric <- function(df, cols) {
  if (length(cols) == 0) return(character(0))
  # vapply garantiza un vector logico aunque no haya columnas (sapply devolvia list())
  cols[vapply(cols, function(cn) {
    v <- df[[cn]]
    # exclude list-columns (yearmonth, yearquarter, etc.) and non-numeric
    !is.list(v) && (is.numeric(v) || is.integer(v))
  }, logical(1))]
}

parse_datetime_safely <- function(x) {
  if (inherits(x, c("POSIXct", "POSIXt"))) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x))
  x_chr <- as.character(x)
  if (all(grepl("^\\d{4}-\\d{2}$", x_chr[!is.na(x_chr)]))) {
    out <- suppressWarnings(as.POSIXct(paste0(x_chr, "-01"), format = "%Y-%m-%d", tz = "UTC"))
    if (!all(is.na(out))) return(out)
  }
  if (all(grepl("^\\d{2}[/-]\\d{4}$", x_chr[!is.na(x_chr)]))) {
    cleaned <- gsub("/", "-", x_chr)
    out <- suppressWarnings(as.POSIXct(paste0(cleaned, "-01"), format = "%m-%Y-%d", tz = "UTC"))
    if (!all(is.na(out))) return(out)
  }
  out <- suppressWarnings(ymd_hms(x_chr, tz = "UTC"))
  if (all(is.na(out))) out <- suppressWarnings(ymd_hm(x_chr, tz = "UTC"))
  if (all(is.na(out))) out <- suppressWarnings(ymd(x_chr, tz = "UTC"))
  if (all(is.na(out))) out <- suppressWarnings(dmy_hms(x_chr, tz = "UTC"))
  if (all(is.na(out))) out <- suppressWarnings(dmy_hm(x_chr, tz = "UTC"))
  if (all(is.na(out))) out <- suppressWarnings(dmy(x_chr, tz = "UTC"))
  if (all(is.na(out))) out <- suppressWarnings(mdy_hms(x_chr, tz = "UTC"))
  if (all(is.na(out))) out <- suppressWarnings(mdy_hm(x_chr, tz = "UTC"))
  if (all(is.na(out))) out <- suppressWarnings(mdy(x_chr, tz = "UTC"))
  if (all(is.na(out))) out <- suppressWarnings(as.POSIXct(x_chr, format = "%Y-%m-%d", tz = "UTC"))
  out
}

# NUEVO: construye un indice temporal SIEMPRE de una clase soportada por tsibble
# (yearmonth, yearquarter, Date, POSIXct o numerico). Evita el error
# "Can't obtain the interval due to the mismatched index class".
build_time_index <- function(x) {
  x_chr <- trimws(as.character(x))
  nn    <- x_chr[!is.na(x_chr) & x_chr != ""]
  if (length(nn) == 0) return(NULL)
  
  if (all(grepl("^\\d{4}-\\d{2}$",   nn))) return(tsibble::yearmonth(x_chr))
  if (all(grepl("^\\d{4} ?Q\\d$",    nn))) return(tsibble::yearquarter(x_chr))
  if (all(grepl("^\\d{4}$",          nn))) return(as.integer(x_chr))
  # Columna temporal numerica generica (1, 2, 3... o 2020.083...)
  if (is.numeric(x)) return(as.numeric(x))
  
  dt <- parse_datetime_safely(x)
  if (!inherits(dt, "POSIXct") || all(is.na(dt))) return(NULL)
  # Limpiar clase/atributos: POSIXct "puro" en UTC
  dt <- as.POSIXct(as.numeric(dt), origin = "1970-01-01", tz = "UTC")
  # Sin componente horaria -> Date (intervalo diario/mensual mas fiable)
  ok <- !is.na(dt)
  if (all(format(dt[ok], "%H:%M:%S") == "00:00:00")) dt <- as.Date(dt)
  dt
}

to_ts <- function(tsbl, value_col = "value", freq = 12) {
  y <- as.numeric(tsbl[[value_col]])
  if (any(is.na(y))) {
    ok <- which(!is.na(y))
    if (length(ok) == 0) stop("La columna de valores esta completamente vacia (todos NA).")
    y <- y[ok[1]:ok[length(ok)]]
    if (any(is.na(y))) y <- as.numeric(zoo::na.approx(y, na.rm = FALSE))
  }
  freq <- as.integer(freq)
  
  # Intentar extraer el start real del indice del tsibble
  start_val <- tryCatch({
    idx <- tsibble::index_var(tsbl)
    first_idx <- tsbl[[idx]][[1]]
    if (inherits(first_idx, "yearmonth")) {
      dt <- as.Date(first_idx)
      c(as.integer(format(dt, "%Y")), as.integer(format(dt, "%m")))
    } else if (inherits(first_idx, "yearquarter")) {
      dt <- as.Date(first_idx)
      yr <- as.integer(format(dt, "%Y"))
      qtr <- ceiling(as.integer(format(dt, "%m")) / 3)
      c(yr, qtr)
    } else if (inherits(first_idx, c("POSIXct", "POSIXt", "Date"))) {
      dt <- as.Date(first_idx)
      if (freq == 12) c(as.integer(format(dt, "%Y")), as.integer(format(dt, "%m")))
      else if (freq == 4) c(as.integer(format(dt, "%Y")), ceiling(as.integer(format(dt, "%m"))/3))
      else c(as.integer(format(dt, "%Y")), 1L)
    } else if (is.integer(first_idx) || is.numeric(first_idx)) {
      c(as.integer(first_idx), 1L)
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  if (!is.null(start_val))
    ts(y, start = start_val, frequency = freq)
  else
    ts(y, frequency = freq)
}

apply_diffs <- function(x, d = 0, D = 0) {
  y <- x
  if (d > 0) y <- diff(y, differences = d)
  if (D > 0) y <- diff(y, lag = frequency(x), differences = D)
  y
}

extract_resid <- function(fit) {
  if (is.null(fit)) return(NULL)
  r <- tryCatch(as.numeric(residuals(fit)), error = function(e) NULL)
  f <- tryCatch(as.numeric(fitted(fit)),    error = function(e) NULL)
  if (is.null(r) || is.null(f)) return(NULL)
  ok <- is.finite(r) & is.finite(f)
  list(r = r[ok], f = f[ok])
}

run_resid_tests <- function(r) {
  shapiro_p <- NA_real_
  if (length(r) >= 3 && length(r) <= 5000)
    shapiro_p <- tryCatch(shapiro.test(r)$p.value, error = function(e) NA_real_)
  jb_p   <- tryCatch(tseries::jarque.bera.test(r)$p.value,                             error = function(e) NA_real_)
  arch_p <- tryCatch(FinTS::ArchTest(r, lags = min(12, max(1, floor(length(r)/10))))$p.value, error = function(e) NA_real_)
  lb_p   <- tryCatch(Box.test(r, lag = min(24, length(r) - 1), type = "Ljung-Box")$p.value,  error = function(e) NA_real_)
  data.frame(
    Diagnostico   = c("Normalidad (Shapiro-Wilk)", "Normalidad (Jarque-Bera)",
                      "Homoscedasticidad (ARCH-LM)", "Incorrelacion (Ljung-Box)"),
    `p-valor`     = c(shapiro_p, jb_p, arch_p, lb_p),
    H0            = c("residuos normales", "residuos normales",
                      "no hay ARCH (varianza constante)", "no autocorrelacion"),
    check.names   = FALSE
  )
}

# ── NUEVO: estilo de graficos (normal / accesible B/N) ────────────────────────
gg_theme <- function(a11y = FALSE) {
  if (!a11y) return(theme_minimal(base_size = 14))
  theme_bw(base_size = 18) +
    theme(
      text             = element_text(colour = "black", face = "bold"),
      axis.text        = element_text(colour = "black", size = rel(1)),
      axis.ticks       = element_line(colour = "black", linewidth = 1),
      panel.border     = element_rect(colour = "black", linewidth = 1.5, fill = NA),
      panel.grid.major = element_line(colour = "grey75", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white", colour = NA),
      strip.background = element_rect(fill = "black", colour = "black"),
      strip.text       = element_text(colour = "white", face = "bold"),
      legend.key       = element_rect(fill = "white", colour = NA)
    )
}

# Fuerza en negro los colores fijos de las capas (p.ej. lineas azules de ggAcf)
# y engrosa lineas; las barras/histogramas pasan a gris oscuro con borde negro.
bw_layers <- function(p) {
  for (i in seq_along(p$layers)) {
    ly <- p$layers[[i]]
    if (!is.null(ly$aes_params$colour)) ly$aes_params$colour <- "black"
    if (inherits(ly$geom, c("GeomPath", "GeomHline", "GeomVline", "GeomSegment")))
      ly$aes_params$linewidth <- 1.2
    if (inherits(ly$geom, "GeomRect")) {
      ly$aes_params$fill   <- "grey35"
      ly$aes_params$colour <- "black"
    }
    p$layers[[i]] <- ly
  }
  p
}

style_plot <- function(p, a11y = FALSE) {
  if (a11y) bw_layers(p) + gg_theme(TRUE) else p + gg_theme(FALSE)
}

fc_plot <- function(fc, a11y, title) {
  p <- if (a11y)
    autoplot(fc, shadecols = c("grey45", "grey80"), fcol = "black", flwd = 1.3)
  else
    autoplot(fc)
  style_plot(p, a11y) + labs(title = title, x = NULL, y = NULL)
}

# ── NUEVO: CSS del modo accesible (alto contraste, blanco y negro) ───────────
a11y_css <- "
  .app-title { font-weight:800; letter-spacing:-0.02em; margin-bottom:0.25rem; }
  .small-muted { color:#6c757d; font-size:0.95rem; }
  .card { border-radius:16px; }

  /* ---------- MODO ACCESIBLE ---------- */
  body.a11y, body.a11y .container-fluid { background:#FFFFFF !important; color:#000000 !important; }
  body.a11y { font-size:20px !important; line-height:1.6; filter:none; }
  body.a11y * { color:#000000 !important; text-shadow:none !important; box-shadow:none !important; }
  body.a11y h1, body.a11y h2, body.a11y h3, body.a11y .app-title { font-weight:900 !important; }
  body.a11y .small-muted { font-size:1.05rem !important; font-weight:600; }
  body.a11y label, body.a11y .control-label { font-weight:700 !important; }

  /* Tarjetas */
  body.a11y .card { background:#FFFFFF !important; border:3px solid #000 !important; border-radius:6px; }
  body.a11y .card-header { background:#000 !important; border-bottom:3px solid #000 !important; }
  body.a11y .card-header, body.a11y .card-header * { color:#FFF !important; font-weight:800; }
  body.a11y hr { border-top:3px solid #000 !important; opacity:1; }

  /* Pestanas */
  body.a11y .nav-tabs { border-bottom:3px solid #000 !important; }
  body.a11y .nav-tabs .nav-link { background:#FFF !important; border:2px solid #000 !important;
                                  margin-right:4px; font-weight:700; text-decoration:underline; }
  body.a11y .nav-tabs .nav-link.active { background:#000 !important; color:#FFF !important; }
  body.a11y .nav-tabs .nav-link.active * { color:#FFF !important; }

  /* Botones: negro sobre blanco, invertidos al pasar/enfocar */
  body.a11y .btn { background:#000 !important; color:#FFF !important;
                   border:3px solid #000 !important; font-weight:800; border-radius:4px; }
  body.a11y .btn * { color:#FFF !important; }
  body.a11y .btn[class*='btn-outline'] { background:#FFF !important; color:#000 !important; }
  body.a11y .btn[class*='btn-outline'] * { color:#000 !important; }
  body.a11y .btn:hover, body.a11y .btn:focus { background:#FFF !important; color:#000 !important; }
  body.a11y .btn:hover *, body.a11y .btn:focus * { color:#000 !important; }

  /* Controles de formulario */
  body.a11y .form-control, body.a11y .form-select, body.a11y select,
  body.a11y .selectize-input, body.a11y .selectize-dropdown, body.a11y input[type='number'] {
    background:#FFF !important; border:3px solid #000 !important; border-radius:4px; font-size:1.1rem; }
  body.a11y .selectize-dropdown .active { background:#000 !important; color:#FFF !important; }
  body.a11y .selectize-input .item { background:#000 !important; color:#FFF !important; }
  body.a11y input[type='checkbox'], body.a11y input[type='radio'] {
    accent-color:#000; width:1.3em; height:1.3em; border:2px solid #000 !important; }
  body.a11y .form-check-input:checked { background-color:#000 !important; border-color:#000 !important; }
  body.a11y .input-group-btn .btn, body.a11y .btn-file { background:#000 !important; }

  /* Slider (ion.rangeSlider) */
  body.a11y .irs-line { background:#FFF !important; border:2px solid #000 !important; }
  body.a11y .irs-bar { background:#000 !important; border-color:#000 !important; }
  body.a11y .irs-handle { background:#FFF !important; border:3px solid #000 !important; }
  body.a11y .irs-single, body.a11y .irs-from, body.a11y .irs-to {
    background:#000 !important; color:#FFF !important; }
  body.a11y .irs-min, body.a11y .irs-max { background:#FFF !important; border:1px solid #000; }

  /* Tablas DT */
  body.a11y table.dataTable, body.a11y table.dataTable th, body.a11y table.dataTable td {
    background:#FFF !important; border:1px solid #000 !important; }
  body.a11y table.dataTable thead th { background:#000 !important; color:#FFF !important; }
  body.a11y .dataTables_paginate .paginate_button { border:2px solid #000 !important; }
  body.a11y .dataTables_paginate .paginate_button.current { background:#000 !important; color:#FFF !important; }

  /* Salidas de texto, mensajes y barra de progreso */
  body.a11y pre, body.a11y .shiny-text-output {
    background:#FFF !important; border:3px solid #000 !important; font-size:1rem; }
  body.a11y .text-success, body.a11y .text-danger { font-weight:800; text-decoration:underline; }
  body.a11y .shiny-output-error, body.a11y .shiny-output-error-validation {
    color:#000 !important; font-weight:800; border-left:6px solid #000; padding-left:8px; }
  body.a11y .progress-bar { background:#000 !important; }
  body.a11y .shiny-notification { background:#FFF !important; border:3px solid #000 !important; opacity:1; }

  /* Enlaces y foco visible */
  body.a11y a { text-decoration:underline !important; font-weight:700; }
  body.a11y :focus, body.a11y :focus-visible {
    outline:4px solid #000 !important; outline-offset:3px !important; }

  /* Graficos: red de seguridad en escala de grises */
  body.a11y .shiny-plot-output img { filter:grayscale(100%) contrast(1.2); border:3px solid #000; }
"

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   base_font = font_google("Inter"), base_font_size = "1rem"),
  tags$style(HTML(a11y_css)),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('setA11y', function(flag){
      if(flag){ document.body.classList.add('a11y'); }
      else    { document.body.classList.remove('a11y'); }
    });
  ")),
  titlePanel(div("ECONOMETRIA - Analisis de Series Temporales", class = "app-title")),
  fluidRow(column(12, card(card_body(
    checkboxInput("a11y_mode",
                  "Modo accesible: alto contraste en blanco y negro, texto grande y foco visible",
                  FALSE, width = "100%")
  )))),
  
  tabsetPanel(
    
    # ── 1) Datos ──────────────────────────────────────────────────────────────
    tabPanel("1) Datos",
             fluidRow(
               column(4, card(
                 card_header("Carga y configuracion"),
                 checkboxInput("use_example",
                               "Cargar datos de ejemplo (AirPassengers, mensual 1949-1960)", FALSE),
                 conditionalPanel("input.use_example == false",
                                  fileInput("file", "Sube un CSV", accept = ".csv"),
                                  checkboxInput("header", "Header", TRUE),
                                  radioButtons("sep", "Separador",
                                               choices = c("Coma" = ",", "Punto y coma" = ";", "Tab" = "\t"),
                                               inline = TRUE)
                 ),
                 conditionalPanel("input.use_example == true",
                                  div("Pasajeros mensuales de lineas aereas internacionales (miles), 1949-1960. ",
                                      "Box & Jenkins (1976).", class = "small-muted")
                 ),
                 uiOutput("col_select_ui"),
                 hr(),
                 conditionalPanel("input.doble_est == false",
                                  numericInput("freq_n", "Periodicidad (frecuencia)", value = 12, min = 1, step = 1),
                                  div("Ej.: 12=mensual, 4=trimestral, 52=semanal.", class = "small-muted")
                 ),
                 checkboxInput("doble_est", "Doble estacionalidad (msts)", FALSE),
                 conditionalPanel("input.doble_est == true",
                                  numericInput("s1", "Periodo estacional 1", value = 24, min = 2),
                                  numericInput("s2", "Periodo estacional 2", value = 168, min = 2)
                 ),
                 hr(),
                 sliderInput("train_prop", "Proporcion entrenamiento",
                             min = 0.6, max = 0.95, value = 0.8, step = 0.05),
                 numericInput("h", "Horizonte de pronostico (h)", value = 12, min = 1, step = 1)
               )),
               column(8, card(
                 card_header("Vista previa de los datos"),
                 DTOutput("preview"),
                 hr(),
                 checkboxInput("show_buys_ballot", "Mostrar grafico de Buys-Ballot", FALSE),
                 conditionalPanel("input.show_buys_ballot == true",
                                  plotOutput("buys_ballot_plot", height = 320)
                 ),
                 div("Nota: se usa tsibble con timestamp; se convierte a ts para forecasting.",
                     class = "small-muted")
               ))
             )
    ),
    
    # ── 2) Serie temporal y periodograma ──────────────────────────────────────
    tabPanel("2) Serie temporal",
             fluidRow(
               column(4, card(
                 card_header("Grafico de la serie"),
                 radioButtons("tsp_transf", "Transformacion",
                              choices = c("Ninguna"               = "none",
                                          "Logaritmo"             = "log",
                                          "Box-Cox (lambda auto)" = "boxcox"),
                              selected = "none"),
                 fluidRow(
                   column(6, numericInput("tsp_d", "d (dif. regular)",    value = 0, min = 0, max = 2, step = 1)),
                   column(6, numericInput("tsp_D", "D (dif. estacional)", value = 0, min = 0, max = 1, step = 1))
                 ),
                 checkboxInput("tsp_rollmean", "Media movil centrada (orden = periodicidad)", FALSE),
                 checkboxInput("tsp_loess",    "Tendencia suavizada (LOESS)", FALSE),
                 hr(),
                 card_header("Periodograma"),
                 radioButtons("pg_type", "Tipo",
                              choices = c("Bruto" = "raw", "Suavizado (Daniell)" = "smooth"),
                              selected = "raw", inline = TRUE),
                 conditionalPanel("input.pg_type == 'smooth'",
                                  numericInput("pg_span", "Amplitud del nucleo Daniell (impar)",
                                               value = 3, min = 3, step = 2)
                 ),
                 numericInput("pg_taper", "Taper (proporcion, 0-0.5)", value = 0.1,
                              min = 0, max = 0.5, step = 0.05),
                 checkboxInput("pg_log", "Eje Y en escala logaritmica", TRUE),
                 radioButtons("pg_xaxis", "Eje X",
                              choices = c("Frecuencia (ciclos/obs.)" = "freq",
                                          "Periodo (obs.)"           = "period"),
                              selected = "freq", inline = TRUE),
                 checkboxInput("pg_marks", "Marcar frecuencias estacionales k/s", TRUE),
                 div("El periodograma se calcula sobre la serie transformada y diferenciada.",
                     class = "small-muted")
               )),
               column(8, card(
                 card_header("Serie temporal"),
                 plotOutput("tsp_plot", height = 340),
                 hr(),
                 card_header("Periodograma"),
                 plotOutput("pg_plot", height = 340),
                 hr(),
                 div("Picos principales del periodograma (maximos locales)", class = "small-muted"),
                 DTOutput("pg_peaks")
               ))
             )
    ),
    
    # ── 3) Descomposicion ─────────────────────────────────────────────────────
    tabPanel("3) Descomposicion",
             fluidRow(
               column(4, card(
                 card_header("Opciones"),
                 radioButtons("decomp_type", "Tipo",
                              choices = c("Clasica (decompose)" = "classic", "STL" = "stl"), selected = "stl"),
                 checkboxInput("decomp_add", "Clasica: forzar aditiva (si no, multiplicativa)", TRUE),
                 checkboxInput("decomp_robust", "STL robust (mejor con outliers)", TRUE),
                 hr(),
                 actionButton("run_decomp", "Calcular descomposicion", class = "btn-primary")
               )),
               column(8, card(
                 card_header("Resultados"),
                 plotOutput("decomp_plot", height = 520),
                 hr(),
                 plotOutput("season_plot", height = 260)
               ))
             )
    ),
    
    # ── 4) Suavizado ──────────────────────────────────────────────────────────
    tabPanel("4) Suavizado (Holt / HW / ETS / BATS)",
             fluidRow(
               column(4, card(
                 card_header("Opciones de suavizado"),
                 radioButtons("smooth_method", "Metodo",
                              choices = c(
                                "Holt (tendencia)"         = "holt",
                                "Holt-Winters (estacional)" = "hw",
                                "BATS"                     = "bats",
                                "TBATS"                    = "tbats"
                              ), selected = "holt"),
                 checkboxInput("use_ets", "Usar espacios de estado (ETS)", FALSE),
                 conditionalPanel("input.smooth_method == 'hw' && !input.use_ets",
                                  selectInput("hw_seasonal", "Estacionalidad HW",
                                              choices = c("additive", "multiplicative"), selected = "additive")
                 ),
                 hr(),
                 actionButton("run_smooth", "Ajustar y pronosticar", class = "btn-primary")
               )),
               column(8, card(
                 card_header("Pronostico y metricas"),
                 plotOutput("smooth_fc_plot", height = 360),
                 hr(),
                 DTOutput("smooth_metrics"),
                 hr(),
                 verbatimTextOutput("smooth_summary")
               ))
             )
    ),
    
    # ── 5) ARIMA ──────────────────────────────────────────────────────────────
    tabPanel("5) ARIMA",
             fluidRow(
               column(4, card(
                 card_header("Opciones ARIMA"),
                 fluidRow(
                   column(6, actionButton("bj_show_id", "Calcular FAS/FAP", class = "btn-secondary w-100")),
                   column(6, actionButton("bj_fit",     "Ajustar ARIMA",    class = "btn-primary w-100"))
                 ),
                 hr(),
                 checkboxInput("bj_seasonal", "Considerar estacionalidad", TRUE),
                 hr(),
                 actionButton("bj_suggest_diff", "Proponer d y D (ndiffs/nsdiffs)",
                              class = "btn-outline-primary"),
                 uiOutput("bj_diff_suggest_ui"),
                 hr(),
                 numericInput("bj_d", "d (diferencia regular)",    value = 0, min = 0, step = 1),
                 numericInput("bj_D", "D (diferencia estacional)", value = 0, min = 0, step = 1),
                 hr(),
                 selectInput("bj_method", "Metodo de estimacion",
                             choices = c("ML", "CSS", "CSS-ML"), selected = "CSS-ML"),
                 hr(),
                 card_header("Ordenes (p,d,q) y (P,D,Q)"),
                 div(class = "small-muted", "Parte regular:"),
                 fluidRow(
                   column(6, numericInput("bj_p", "p (AR)", value = 1, min = 0)),
                   column(6, numericInput("bj_q", "q (MA)", value = 1, min = 0))
                 ),
                 div(class = "small-muted", "Parte estacional:"),
                 fluidRow(
                   column(6, numericInput("bj_P", "P (AR est.)", value = 0, min = 0)),
                   column(6, numericInput("bj_Q", "Q (MA est.)", value = 0, min = 0))
                 ),
                 hr(),
                 fluidRow(
                   column(6, numericInput("bj_const", "Constante (0/1)", value = 1, min = 0, max = 1)),
                   column(6, numericInput("bj_h",     "h (pronostico)",  value = 12, min = 1))
                 )
               )),
               column(8, card(
                 card_header("1) Identificacion: FAS (ACF) y FAP (PACF)"),
                 div("Arriba: serie original. Abajo: serie diferenciada (d, D).", class = "small-muted"),
                 fluidRow(
                   column(6, plotOutput("bj_acf_orig",  height = 260)),
                   column(6, plotOutput("bj_pacf_orig", height = 260))
                 ),
                 hr(),
                 fluidRow(
                   column(6, plotOutput("bj_acf_diff",  height = 260)),
                   column(6, plotOutput("bj_pacf_diff", height = 260))
                 ),
                 hr(),
                 card_header("2) Estimacion y Pronostico"),
                 plotOutput("bj_fc_plot", height = 360),
                 hr(),
                 DTOutput("bj_metrics"),
                 hr(),
                 verbatimTextOutput("bj_summary")
               ))
             )
    ),
    
    # ── 6) ARIMAX ─────────────────────────────────────────────────────────────
    tabPanel("6) ARIMAX",
             fluidRow(
               column(4, card(
                 card_header("Opciones ARIMAX"),
                 fluidRow(
                   column(6, actionButton("ax_show_id", "Calcular FAS/FAP", class = "btn-secondary w-100")),
                   column(6, actionButton("ax_fit",     "Ajustar ARIMAX",   class = "btn-primary w-100"))
                 ),
                 hr(),
                 checkboxInput("ax_seasonal", "Considerar estacionalidad", TRUE),
                 hr(),
                 actionButton("ax_suggest_diff", "Proponer d y D", class = "btn-outline-primary"),
                 uiOutput("ax_diff_suggest_ui"),
                 hr(),
                 numericInput("ax_d", "d (diferencia regular)",    value = 0, min = 0, step = 1),
                 numericInput("ax_D", "D (diferencia estacional)", value = 0, min = 0, step = 1),
                 hr(),
                 selectInput("ax_method", "Metodo de estimacion",
                             choices = c("ML", "CSS", "CSS-ML"), selected = "CSS-ML"),
                 hr(),
                 card_header("Ordenes (p,d,q) y (P,D,Q)"),
                 div(class = "small-muted", "Parte regular:"),
                 fluidRow(
                   column(6, numericInput("ax_p", "p (AR)", value = 1, min = 0)),
                   column(6, numericInput("ax_q", "q (MA)", value = 1, min = 0))
                 ),
                 div(class = "small-muted", "Parte estacional:"),
                 fluidRow(
                   column(6, numericInput("ax_P", "P (AR est.)", value = 0, min = 0)),
                   column(6, numericInput("ax_Q", "Q (MA est.)", value = 0, min = 0))
                 ),
                 hr(),
                 fluidRow(
                   column(6, numericInput("ax_const", "Constante (0/1)", value = 1, min = 0, max = 1)),
                   column(6, numericInput("ax_h",     "h (pronostico)",  value = 12, min = 1))
                 ),
                 hr(),
                 card_header("Regresores del CSV"),
                 uiOutput("ax_xreg_ui"),
                 conditionalPanel("output.ax_has_xreg == true",
                                  checkboxInput("ax_xreg_scale", "Estandarizar regresores (scale)", TRUE)
                 ),
                 hr(),
                 card_header("Variables de calendario (X-13 / TRAMO-SEATS)"),
                 checkboxInput("ax_use_calendar",
                               "Anadir variables de intervencion de calendario", FALSE),
                 conditionalPanel("input.ax_use_calendar == true",
                                  radioButtons("ax_cal_engine", "Motor",
                                               choices = c("X-13ARIMA-SEATS" = "x13", "TRAMO-SEATS" = "tramo"),
                                               selected = "x13", inline = TRUE),
                                  checkboxGroupInput("ax_cal_vars", "Variables a incluir",
                                                     choices = c(
                                                       "Trading day (dias habiles)" = "td",
                                                       "Easter (Semana Santa)"      = "easter",
                                                       "Ano bisiesto"               = "lom"
                                                     ), selected = c("td", "easter")),
                                  actionButton("ax_run_calendar", "Extraer variables de calendario",
                                               class = "btn-outline-secondary"),
                                  uiOutput("ax_calendar_status")
                 )
               )),
               column(8, card(
                 card_header("1) Identificacion: FAS (ACF) y FAP (PACF)"),
                 div("Arriba: serie original. Abajo: serie diferenciada (d, D).", class = "small-muted"),
                 fluidRow(
                   column(6, plotOutput("ax_acf_orig",  height = 260)),
                   column(6, plotOutput("ax_pacf_orig", height = 260))
                 ),
                 hr(),
                 fluidRow(
                   column(6, plotOutput("ax_acf_diff",  height = 260)),
                   column(6, plotOutput("ax_pacf_diff", height = 260))
                 ),
                 hr(),
                 card_header("2) Estimacion y Pronostico"),
                 plotOutput("ax_fc_plot", height = 360),
                 hr(),
                 DTOutput("ax_metrics"),
                 hr(),
                 verbatimTextOutput("ax_summary")
               ))
             )
    ),
    
    # ── 7) Validacion de residuos ─────────────────────────────────────────────
    tabPanel("7) Validacion de residuos",
             fluidRow(
               column(4, card(
                 card_header("Seleccionar modelo"),
                 radioButtons("val_model", "Modelo a validar",
                              choices = c(
                                "Suavizado (Holt/HW/ETS/BATS)" = "smooth",
                                "ARIMA"                         = "arima",
                                "ARIMAX"                        = "arimax"
                              ), selected = "arima"),
                 hr(),
                 div("Los graficos y tests se actualizan automaticamente al cambiar el modelo.",
                     class = "small-muted")
               )),
               column(8, card(
                 card_header("Diagnostico de residuos"),
                 fluidRow(
                   column(6, plotOutput("val_resid_ts",     height = 220)),
                   column(6, plotOutput("val_resid_qq",     height = 220))
                 ),
                 fluidRow(
                   column(6, plotOutput("val_resid_hist",   height = 220)),
                   column(6, plotOutput("val_resid_fitted", height = 220))
                 ),
                 fluidRow(
                   column(6, plotOutput("val_acf_resid",    height = 220)),
                   column(6, plotOutput("val_acf_resid2",   height = 220))
                 ),
                 hr(),
                 DTOutput("val_resid_tests")
               ))
             )
    )
    
  ) # end tabsetPanel
) # end fluidPage

# ── SERVER ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Accesibilidad
  a11y_on <- reactive(isTRUE(input$a11y_mode))
  observeEvent(input$a11y_mode, {
    session$sendCustomMessage("setA11y", isTRUE(input$a11y_mode))
  }, ignoreInit = TRUE)
  
  # ── Datos ────────────────────────────────────────────────────────────────────
  raw_df <- reactive({
    if (isTRUE(input$use_example)) {
      ap <- datasets::AirPassengers
      return(data.frame(
        fecha     = format(seq(as.Date("1949-01-01"), by = "month", length.out = length(ap)), "%Y-%m"),
        pasajeros = as.numeric(ap),
        stringsAsFactors = FALSE
      ))
    }
    req(input$file)
    read.csv(input$file$datapath, header = input$header,
             sep = input$sep, stringsAsFactors = FALSE)
  })
  
  # Datos de ejemplo: fijar periodicidad mensual y estacionalidad simple
  observeEvent(input$use_example, {
    if (isTRUE(input$use_example)) {
      updateCheckboxInput(session, "doble_est", value = FALSE)
      updateNumericInput(session, "freq_n", value = 12)
    }
  }, ignoreInit = TRUE)
  
  output$col_select_ui <- renderUI({
    df <- raw_df(); cols <- names(df)
    tagList(
      selectInput("time_col",  "Columna timestamp", choices = cols, selected = cols[1]),
      selectInput("value_col", "Columna valor (y)", choices = cols,
                  selected = cols[min(2, length(cols))])
    )
  })
  
  output$preview <- renderDT({
    datatable(head(raw_df(), 50), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  tsbl_data <- reactive({
    df <- raw_df(); req(input$time_col, input$value_col)
    validate(need(input$time_col != input$value_col,
                  "La columna timestamp y la columna valor deben ser distintas."))
    
    time_idx <- build_time_index(df[[input$time_col]])
    validate(need(!is.null(time_idx) && any(!is.na(time_idx)),
                  "No puedo parsear el timestamp. Prueba: 2026-02, 2026-02-05, 2026 Q1."))
    
    out <- data.frame(timestamp = time_idx,
                      value = suppressWarnings(as.numeric(df[[input$value_col]])))
    other <- setdiff(names(df), c(input$time_col, input$value_col))
    if (length(other) > 0) {
      extra <- df[, other, drop = FALSE]
      # Evitar colisiones de nombres con timestamp/value/row_id
      names(extra) <- make.unique(c("timestamp", "value", "row_id", names(extra)))[-(1:3)]
      out <- cbind(out, extra)
    }
    out <- out[!is.na(out$timestamp), , drop = FALSE]
    out <- out[order(out$timestamp), , drop = FALSE]
    rownames(out) <- NULL
    
    tsbl <- tryCatch({
      if (any(duplicated(out$timestamp))) {
        out$row_id <- seq_len(nrow(out))
        as_tsibble(out, index = timestamp, key = row_id)
      } else {
        as_tsibble(out, index = timestamp)
      }
    }, error = function(e) NULL)
    
    # Ultimo recurso: indice entero 1..n (la serie se analiza igual con ts)
    if (is.null(tsbl)) {
      showNotification(
        "El timestamp no es una fecha reconocible por tsibble; se usa un indice 1..n.",
        type = "warning", duration = 8)
      out$timestamp <- seq_len(nrow(out))
      out$row_id <- NULL
      tsbl <- as_tsibble(out, index = timestamp)
    }
    tsbl
  })
  
  ts_data <- reactive({
    tsbl <- tsbl_data()
    freq <- as.integer(input$freq_n)
    validate(need(freq >= 1, "La periodicidad debe ser >= 1."))
    to_ts(tsbl, value_col = "value", freq = freq)
  })
  
  # ── Serie temporal y periodograma ────────────────────────────────────────────
  # Serie transformada (log / Box-Cox) y diferenciada (d, D)
  tsp_series <- reactive({
    # Mensajes visibles en lugar de un panel en blanco
    validate(need(!is.null(input$file) || isTRUE(input$use_example),
                  "Carga primero un CSV o marca «Cargar datos de ejemplo» en la pestaña «1) Datos»."))
    validate(need(!is.null(input$time_col) && !is.null(input$value_col),
                  "Selecciona las columnas de tiempo y valor en la pestaña «1) Datos»."))
    x <- tryCatch(ts_data(), error = function(e) {
      if (inherits(e, "validation")) stop(e)
      if (inherits(e, "shiny.silent.error"))
        validate("Faltan datos de entrada: revisa la pestaña «1) Datos».")
      validate(paste("No se pudo construir la serie:", conditionMessage(e)))
    })
    validate(need(sum(is.finite(x)) >= 8,
                  "La columna de valores no contiene datos numericos suficientes (revisa la columna y el separador)."))
    d <- as.integer(input$tsp_d %||% 0); D <- as.integer(input$tsp_D %||% 0)
    validate(need(!is.na(d) && !is.na(D) && d >= 0 && D >= 0, "d y D deben ser >= 0."))
    validate(need(D == 0 || frequency(x) >= 2,
                  "La diferencia estacional requiere periodicidad >= 2."))
    lambda <- NULL; lab <- "y"
    if (input$tsp_transf == "log") {
      validate(need(all(x > 0, na.rm = TRUE), "El logaritmo requiere valores > 0."))
      x <- log(x); lab <- "log(y)"
    } else if (input$tsp_transf == "boxcox") {
      validate(need(all(x > 0, na.rm = TRUE), "Box-Cox requiere valores > 0."))
      lambda <- forecast::BoxCox.lambda(x)
      x <- forecast::BoxCox(x, lambda)
      lab <- paste0("Box-Cox(y), lambda = ", round(lambda, 3))
    }
    y <- apply_diffs(x, d = d, D = D)
    validate(need(length(y) >= 8, "Serie demasiado corta tras diferenciar."))
    if (d > 0 || D > 0) lab <- paste0(lab, "  [d = ", d, ", D = ", D, "]")
    list(y = y, lambda = lambda, lab = lab, d = d, D = D)
  })
  
  output$tsp_plot <- renderPlot({
    obj <- tsp_series(); y <- obj$y; a11y <- a11y_on()
    tt  <- as.numeric(time(y))
    df  <- data.frame(t = tt, v = as.numeric(y), Serie = "Serie")
    
    # Superposiciones opcionales
    if (isTRUE(input$tsp_rollmean)) {
      s <- frequency(y)
      validate(need(s >= 2, "La media movil estacional requiere periodicidad >= 2."))
      mm <- as.numeric(forecast::ma(y, order = s, centre = TRUE))
      df <- rbind(df, data.frame(t = tt, v = mm, Serie = paste0("Media movil (", s, ")")))
    }
    if (isTRUE(input$tsp_loess)) {
      lo <- tryCatch(stats::predict(stats::loess(as.numeric(y) ~ tt, span = 0.4)),
                     error = function(e) rep(NA_real_, length(tt)))
      df <- rbind(df, data.frame(t = tt, v = as.numeric(lo), Serie = "Tendencia LOESS"))
    }
    df$Serie <- factor(df$Serie, levels = unique(df$Serie))
    
    p <- ggplot(df[is.finite(df$v), ], aes(x = t, y = v, colour = Serie, linetype = Serie)) +
      geom_line()
    if (nlevels(df$Serie) > 1) {
      cols <- if (a11y) rep("black", nlevels(df$Serie))
      else c("grey20", "#c0392b", "#2c7fb8")[seq_len(nlevels(df$Serie))]
      p <- p + scale_colour_manual(values = cols) +
        scale_linetype_manual(values = c("solid", "dashed", "dotdash")[seq_len(nlevels(df$Serie))])
    } else {
      p <- p + scale_colour_manual(values = if (a11y) "black" else "grey20", guide = "none") +
        scale_linetype_manual(values = "solid", guide = "none")
    }
    style_plot(p, a11y) +
      labs(title = "Serie temporal", subtitle = obj$lab, x = "Tiempo", y = NULL,
           colour = NULL, linetype = NULL) +
      theme(legend.position = "bottom")
  })
  
  # Periodograma (frecuencia en ciclos por observacion, 0 - 0.5)
  pg_res <- reactive({
    obj <- tsp_series()
    y   <- as.numeric(obj$y)
    s   <- frequency(obj$y)
    taper <- input$pg_taper %||% 0.1
    validate(need(!is.na(taper) && taper >= 0 && taper <= 0.5, "El taper debe estar entre 0 y 0.5."))
    spans <- NULL
    if (input$pg_type == "smooth") {
      k <- max(3L, as.integer(input$pg_span %||% 3))
      if (k %% 2 == 0) k <- k + 1L
      validate(need(k < length(y) / 2, "Amplitud del nucleo demasiado grande para la serie."))
      spans <- k
    }
    sp <- stats::spec.pgram(y, spans = spans, taper = taper, detrend = TRUE,
                            fast = FALSE, plot = FALSE)
    df <- data.frame(freq = sp$freq, spec = sp$spec, period = 1 / sp$freq)
    
    # Frecuencias estacionales a marcar (fundamental y armonicos k/s)
    marks <- NULL
    seas  <- if (isTRUE(input$doble_est)) c(input$s1, input$s2) else s
    seas  <- unique(as.numeric(seas[is.finite(seas) & seas >= 2]))
    for (sx in seas) {
      kmax <- if (isTRUE(input$doble_est)) min(3, floor(sx / 2)) else min(6, floor(sx / 2))
      if (kmax >= 1)
        marks <- rbind(marks, data.frame(freq = seq_len(kmax) / sx,
                                         Estacionalidad = paste0("s = ", sx)))
    }
    list(df = df, marks = marks, s = s, smooth = !is.null(spans), lab = obj$lab)
  })
  
  output$pg_plot <- renderPlot({
    res <- pg_res(); df <- res$df; a11y <- a11y_on()
    use_period <- input$pg_xaxis == "period"
    df$x <- if (use_period) df$period else df$freq
    
    p <- ggplot(df, aes(x = x, y = spec)) +
      geom_line(colour = if (a11y) "black" else "#2c3e50")
    if (!a11y && !isTRUE(input$pg_log)) p <- p + geom_area(fill = "#2c3e50", alpha = 0.12)
    
    if (isTRUE(input$pg_marks) && !is.null(res$marks)) {
      mk <- res$marks
      mk$x <- if (use_period) 1 / mk$freq else mk$freq
      p <- p + geom_vline(data = mk, aes(xintercept = x, linetype = Estacionalidad),
                          colour = if (a11y) "black" else "#c0392b", linewidth = 0.5) +
        scale_linetype_manual(values = c("dashed", "dotted")[seq_len(length(unique(mk$Estacionalidad)))])
    }
    if (isTRUE(input$pg_log)) p <- p + scale_y_log10()
    if (use_period) p <- p + scale_x_log10()
    
    style_plot(p, a11y) +
      labs(title = if (res$smooth) "Periodograma suavizado (Daniell)" else "Periodograma",
           subtitle = res$lab,
           x = if (use_period) "Periodo (observaciones, escala log)" else "Frecuencia (ciclos por observacion)",
           y = if (isTRUE(input$pg_log)) "Densidad espectral (log)" else "Densidad espectral",
           linetype = NULL) +
      theme(legend.position = "bottom")
  })
  
  output$pg_peaks <- renderDT({
    df <- pg_res()$df
    n  <- nrow(df)
    validate(need(n >= 3, "No hay suficientes frecuencias para detectar picos."))
    is_peak <- c(df$spec[1] > df$spec[2],
                 df$spec[2:(n - 1)] > df$spec[1:(n - 2)] & df$spec[2:(n - 1)] > df$spec[3:n],
                 df$spec[n] > df$spec[n - 1])
    pk <- df[is_peak, , drop = FALSE]
    pk <- pk[order(-pk$spec), , drop = FALSE][seq_len(min(5, nrow(pk))), , drop = FALSE]
    out <- data.frame(
      `Frecuencia (ciclos/obs.)` = round(pk$freq, 4),
      `Periodo (obs.)`           = round(pk$period, 2),
      `Densidad espectral`       = signif(pk$spec, 4),
      `% potencia total`         = round(100 * pk$spec / sum(df$spec), 2),
      check.names = FALSE
    )
    datatable(out, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  
  output$buys_ballot_plot <- renderPlot({
    req(input$show_buys_ballot)
    x <- ts_data(); freq <- frequency(x); n <- length(x)
    validate(need(freq >= 2, "Buys-Ballot requiere frecuencia >= 2."))
    n_cyc <- floor(n / freq)
    validate(need(n_cyc >= 2, "Se necesitan al menos 2 ciclos completos."))
    mat <- matrix(x[1:(n_cyc * freq)], nrow = n_cyc, ncol = freq, byrow = TRUE)
    df_bb <- data.frame(
      Periodo = factor(rep(seq_len(freq), each = n_cyc),
                       labels = paste0("P", seq_len(freq))),
      Ciclo   = factor(rep(seq_len(n_cyc), times = freq)),
      Valor   = as.vector(mat)
    )
    p <- ggplot(df_bb, aes(x = Periodo, y = Valor, group = Ciclo, color = Ciclo)) +
      geom_line(alpha = 0.7) + geom_point(size = 1.5, alpha = 0.7) +
      stat_summary(aes(group = 1), fun = mean, geom = "line",
                   color = "black", linewidth = 1.2, linetype = "dashed")
    p <- style_plot(p, a11y_on())
    if (a11y_on()) p <- p + scale_colour_grey(start = 0.1, end = 0.7)
    p + labs(title = paste0("Grafico de Buys-Ballot (frecuencia = ", freq, ")"),
             x = "Periodo dentro del ciclo", y = NULL,
             caption = "Linea discontinua: media por periodo")
  })
  
  split_ts <- reactive({
    x <- ts_data(); n <- length(x)
    train_n <- max(2, floor(n * input$train_prop))
    train   <- x[1:train_n]
    test    <- if (train_n < n) x[(train_n + 1):n] else ts(numeric(0), frequency = frequency(x))
    list(train = train, test = test, n = n, train_n = train_n)
  })
  
  # ── Descomposicion ───────────────────────────────────────────────────────────
  decomp_res <- eventReactive(input$run_decomp, {
    x <- ts_data()
    validate(need(!any(is.na(x)),
                  paste0("La serie contiene ", sum(is.na(x)), " NA(s). Revisa datos o periodicidad.")))
    validate(need(length(x) >= 2 * frequency(x),
                  paste0("Se necesitan al menos 2 ciclos completos (", 2 * frequency(x), " obs).")))
    if (input$decomp_type == "classic") {
      validate(need(frequency(x) >= 2, "Descomposicion clasica requiere frecuencia >= 2."))
      decompose(x, type = if (input$decomp_add) "additive" else "multiplicative")
    } else {
      x_clean <- x; x_clean[!is.finite(x_clean)] <- NA
      validate(need(!any(is.na(x_clean)), "STL no admite NA/NaN/Inf."))
      validate(need(frequency(x_clean) >= 2, "STL requiere frecuencia >= 2."))
      validate(need(length(x_clean) >= 2 * frequency(x_clean) + 1,
                    paste0("STL necesita al menos ", 2 * frequency(x_clean) + 1, " observaciones.")))
      stl(x_clean, s.window = "periodic", robust = input$decomp_robust)
    }
  })
  
  output$decomp_plot <- renderPlot({
    req(decomp_res()); obj <- decomp_res()
    ttl <- if (inherits(obj, "stl")) "Descomposicion STL" else "Descomposicion clasica"
    style_plot(autoplot(obj), a11y_on()) + labs(title = ttl)
  })
  
  output$season_plot <- renderPlot({
    x <- ts_data()
    validate(need(frequency(x) >= 2, "Se requiere frecuencia >= 2 para seasonal plot."))
    p <- style_plot(ggseasonplot(x), a11y_on())
    if (a11y_on()) p <- p + scale_colour_grey(start = 0.1, end = 0.7)
    p + labs(title = "Patron estacional")
  })
  
  # ── Suavizado ────────────────────────────────────────────────────────────────
  smooth_fit <- eventReactive(input$run_smooth, {
    sp      <- split_ts()
    y_train <- ts(sp$train, frequency = input$freq_n)
    y_test  <- ts(sp$test,  frequency = input$freq_n)
    h       <- input$h
    validate(need(length(y_train) >= 5, "Serie demasiado corta para suavizado."))
    fit <- NULL; fc <- NULL
    if (isTRUE(input$use_ets)) {
      if (input$smooth_method == "holt") {
        fit <- ets(y_train, model = "AAN"); fc <- forecast(fit, h = h)
      } else if (input$smooth_method == "hw") {
        validate(need(frequency(y_train) >= 2, "ETS estacional requiere frecuencia >= 2."))
        fit <- ets(y_train); fc <- forecast(fit, h = h)
      } else {
        validate(need(FALSE, "ETS no aplica a BATS/TBATS. Desmarca la opcion ETS."))
      }
    } else if (input$smooth_method == "bats") {
      fit <- forecast::bats(y_train);  fc <- forecast(fit, h = h)
    } else if (input$smooth_method == "tbats") {
      fit <- forecast::tbats(y_train); fc <- forecast(fit, h = h)
    } else if (input$smooth_method == "holt") {
      fc <- holt(y_train, h = h); fit <- fc$model
    } else {
      validate(need(frequency(y_train) >= 2, "Holt-Winters requiere frecuencia >= 2."))
      fc <- hw(y_train, seasonal = input$hw_seasonal, h = h); fit <- fc$model
    }
    acc <- tryCatch(
      if (length(y_test) > 0) accuracy(fc, y_test) else accuracy(fc),
      error = function(e) accuracy(fc)
    )
    list(fit = fit, fc = fc, acc = acc)
  })
  
  output$smooth_fc_plot <- renderPlot({
    req(smooth_fit())
    fc_plot(smooth_fit()$fc, a11y_on(), "Pronostico (Suavizado)")
  })
  output$smooth_metrics <- renderDT({
    req(smooth_fit())
    datatable(round(as.data.frame(smooth_fit()$acc), 4),
              options = list(dom = "t", scrollX = TRUE))
  })
  output$smooth_summary <- renderPrint({ req(smooth_fit()); smooth_fit()$fit })
  
  # ── ARIMA ────────────────────────────────────────────────────────────────────
  bj_suggest <- eventReactive(input$bj_suggest_diff, {
    x <- ts_data()
    list(d = ndiffs(x),
         D = if (isTRUE(input$bj_seasonal) && frequency(x) >= 2) nsdiffs(x) else 0)
  })
  output$bj_diff_suggest_ui <- renderUI({
    if (is.null(bj_suggest())) return(NULL)
    s <- bj_suggest()
    tagList(
      div(class = "small-muted", paste0("Sugerencia: d = ", s$d, " | D = ", s$D, ".")),
      actionButton("bj_apply_suggest", "Aplicar sugerencia", class = "btn-outline-success")
    )
  })
  observeEvent(input$bj_apply_suggest, {
    req(bj_suggest()); s <- bj_suggest()
    updateNumericInput(session, "bj_d", value = s$d)
    updateNumericInput(session, "bj_D", value = s$D)
  })
  
  bj_id_series <- eventReactive(input$bj_show_id, {
    x <- ts_data(); d <- input$bj_d; D <- input$bj_D
    validate(need(d >= 0 && D >= 0, "d y D deben ser >= 0."))
    y <- apply_diffs(x, d = d, D = D)
    validate(need(length(y) > 10, "Serie demasiado corta tras diferenciar."))
    list(x = x, y = y, d = d, D = D)
  }, ignoreInit = FALSE)
  
  output$bj_acf_orig  <- renderPlot({
    style_plot(ggAcf(bj_id_series()$x), a11y_on()) + labs(title = "ACF serie original")
  })
  output$bj_pacf_orig <- renderPlot({
    style_plot(ggPacf(bj_id_series()$x), a11y_on()) + labs(title = "PACF serie original")
  })
  output$bj_acf_diff  <- renderPlot({
    obj <- bj_id_series()
    style_plot(ggAcf(obj$y), a11y_on()) +
      labs(title = paste0("ACF tras d = ", obj$d, ", D = ", obj$D))
  })
  output$bj_pacf_diff <- renderPlot({
    obj <- bj_id_series()
    style_plot(ggPacf(obj$y), a11y_on()) +
      labs(title = paste0("PACF tras d = ", obj$d, ", D = ", obj$D))
  })
  
  bj_fit <- eventReactive(input$bj_fit, {
    sp      <- split_ts()
    y_train <- sp$train
    h       <- input$bj_h
    d <- input$bj_d; D <- input$bj_D
    ord  <- c(as.integer(input$bj_p), as.integer(d), as.integer(input$bj_q))
    seas <- list(
      order  = c(as.integer(input$bj_P), as.integer(D), as.integer(input$bj_Q)),
      period = frequency(y_train)
    )
    # Intentar con el metodo elegido; si falla (p.ej. CSS no estacionario) usar ML
    fit <- tryCatch(
      forecast::Arima(y_train, order = ord, seasonal = seas,
                      include.mean = (input$bj_const == 1),
                      method = input$bj_method),
      error = function(e) {
        tryCatch(
          forecast::Arima(y_train, order = ord, seasonal = seas,
                          include.mean = (input$bj_const == 1),
                          method = "ML"),
          error = function(e2) stop(paste0(
            "No se pudo ajustar el modelo ARIMA(", paste(ord, collapse=","), ")",
            "(", paste(seas$order, collapse=","), "). ",
            "Prueba a cambiar los ordenes o diferencias. Error: ", conditionMessage(e2)
          ))
        )
      }
    )
    fc  <- forecast::forecast(fit, h = h)
    acc <- tryCatch(accuracy(fc), error = function(e) NULL)
    list(fit = fit, fc = fc, acc = acc)
  })
  
  output$bj_fc_plot <- renderPlot({
    req(bj_fit())
    fc_plot(bj_fit()$fc, a11y_on(), "Pronostico ARIMA")
  })
  output$bj_metrics <- renderDT({
    req(bj_fit()); acc <- bj_fit()$acc
    validate(need(!is.null(acc), "No se pudo calcular accuracy."))
    datatable(round(as.data.frame(acc), 4), options = list(dom = "t", scrollX = TRUE))
  })
  output$bj_summary <- renderPrint({ req(bj_fit()); bj_fit()$fit })
  
  # ── ARIMAX ───────────────────────────────────────────────────────────────────
  
  # UI de regresores del CSV
  output$ax_xreg_ui <- renderUI({
    req(tsbl_data())
    df       <- as.data.frame(tsbl_data())
    drop     <- c("timestamp", "value", "row_id")
    num_cols <- safe_req_numeric(df, setdiff(names(df), drop))
    if (length(num_cols) == 0)
      return(div("No hay columnas numericas adicionales en el CSV.",
                 class = "small-muted"))
    selectizeInput("ax_xreg_cols", "Regresores del CSV (opcional)",
                   choices  = num_cols, multiple = TRUE,
                   options  = list(placeholder = "Ninguno seleccionado"))
  })
  
  # Flag reactivo para mostrar/ocultar scale checkbox
  output$ax_has_xreg <- reactive({
    req(tsbl_data())
    df       <- as.data.frame(tsbl_data())
    drop     <- c("timestamp", "value", "row_id")
    num_cols <- safe_req_numeric(df, setdiff(names(df), drop))
    length(num_cols) > 0
  })
  outputOptions(output, "ax_has_xreg", suspendWhenHidden = FALSE)
  
  # Variables de calendario
  ax_calendar_xreg <- reactiveVal(NULL)
  
  observeEvent(input$ax_run_calendar, {
    x <- ts_data()
    validate(need(frequency(x) %in% c(4, 12),
                  "Variables de calendario solo disponibles para series mensuales (12) o trimestrales (4)."))
    vars_raw <- input$ax_cal_vars
    validate(need(length(vars_raw) > 0, "Selecciona al menos una variable de calendario."))
    
    withProgress(message = "Ejecutando ajuste estacional...", value = 0.5, {
      result <- tryCatch({
        engine <- input$ax_cal_engine
        
        reg_vars <- character(0)
        if ("td"     %in% vars_raw) reg_vars <- c(reg_vars, "td")
        if ("easter" %in% vars_raw) reg_vars <- c(reg_vars, "easter[8]")
        if ("lom"    %in% vars_raw) reg_vars <- c(reg_vars, "lom")
        
        m <- if (engine == "x13") {
          seasonal::seas(x,
                         regression.variables = reg_vars,
                         arima.model          = "(0 1 1)(0 1 1)",
                         transform.function   = "auto",
                         estimate.maxiter     = 1000L)
        } else {
          seasonal::seas(x,
                         regression.variables = reg_vars,
                         arima.model          = "(0 1 1)(0 1 1)",
                         transform.function   = "auto",
                         estimate.maxiter     = 1000L,
                         x11                  = "")
        }
        
        xreg_cal <- tryCatch({
          re <- seasonal::series(m, "regressioneffects")
          as.matrix(as.data.frame(re))
        }, error = function(e) {
          tryCatch({
            mf <- as.data.frame(seasonal::series(m, "regression.coefficients"))
            as.matrix(mf)
          }, error = function(e2) NULL)
        })
        
        list(ok = TRUE, xreg = xreg_cal,
             msg = paste0("Exito: ",
                          if (!is.null(xreg_cal)) ncol(xreg_cal) else 0,
                          " variable(s) extraidas."))
      }, error = function(e) {
        list(ok = FALSE, xreg = NULL, msg = paste("Error:", conditionMessage(e)))
      })
    })
    ax_calendar_xreg(result)
  })
  
  output$ax_calendar_status <- renderUI({
    res <- ax_calendar_xreg()
    if (is.null(res)) return(NULL)
    cls <- if (res$ok) "small-muted text-success" else "small-muted text-danger"
    # Prefijo textual: el estado no depende solo del color
    div(class = cls, paste(if (res$ok) "[OK]" else "[ERROR]", res$msg))
  })
  
  # d/D para ARIMAX
  ax_suggest <- eventReactive(input$ax_suggest_diff, {
    x <- ts_data()
    list(d = ndiffs(x),
         D = if (isTRUE(input$ax_seasonal) && frequency(x) >= 2) nsdiffs(x) else 0)
  })
  output$ax_diff_suggest_ui <- renderUI({
    if (is.null(ax_suggest())) return(NULL)
    s <- ax_suggest()
    tagList(
      div(class = "small-muted", paste0("Sugerencia: d = ", s$d, " | D = ", s$D, ".")),
      actionButton("ax_apply_suggest", "Aplicar sugerencia", class = "btn-outline-success")
    )
  })
  observeEvent(input$ax_apply_suggest, {
    req(ax_suggest()); s <- ax_suggest()
    updateNumericInput(session, "ax_d", value = s$d)
    updateNumericInput(session, "ax_D", value = s$D)
  })
  
  # ACF/PACF para ARIMAX
  ax_id_series <- eventReactive(input$ax_show_id, {
    x <- ts_data(); d <- input$ax_d; D <- input$ax_D
    validate(need(d >= 0 && D >= 0, "d y D deben ser >= 0."))
    y <- apply_diffs(x, d = d, D = D)
    validate(need(length(y) > 10, "Serie demasiado corta tras diferenciar."))
    list(x = x, y = y, d = d, D = D)
  }, ignoreInit = FALSE)
  
  output$ax_acf_orig  <- renderPlot({
    style_plot(ggAcf(ax_id_series()$x), a11y_on()) + labs(title = "ACF serie original")
  })
  output$ax_pacf_orig <- renderPlot({
    style_plot(ggPacf(ax_id_series()$x), a11y_on()) + labs(title = "PACF serie original")
  })
  output$ax_acf_diff  <- renderPlot({
    obj <- ax_id_series()
    style_plot(ggAcf(obj$y), a11y_on()) +
      labs(title = paste0("ACF tras d = ", obj$d, ", D = ", obj$D))
  })
  output$ax_pacf_diff <- renderPlot({
    obj <- ax_id_series()
    style_plot(ggPacf(obj$y), a11y_on()) +
      labs(title = paste0("PACF tras d = ", obj$d, ", D = ", obj$D))
  })
  
  # Ajuste ARIMAX
  ax_fit <- eventReactive(input$ax_fit, {
    sp      <- split_ts()
    y_train <- sp$train
    h       <- input$ax_h
    d <- input$ax_d; D <- input$ax_D
    ord  <- c(as.integer(input$ax_p), as.integer(d), as.integer(input$ax_q))
    seas <- list(
      order  = c(as.integer(input$ax_P), as.integer(D), as.integer(input$ax_Q)),
      period = frequency(y_train)
    )
    
    xreg_train  <- NULL
    xreg_future <- NULL
    
    # 1) Regresores del CSV
    csv_cols <- input$ax_xreg_cols
    if (!is.null(csv_cols) && length(csv_cols) > 0) {
      df <- as.data.frame(tsbl_data())
      X  <- as.matrix(df[, csv_cols, drop = FALSE])
      validate(need(nrow(X) >= sp$train_n,
                    "No hay suficientes filas en xreg para el entrenamiento."))
      xreg_train <- X[1:sp$train_n, , drop = FALSE]
      if (isTRUE(input$ax_xreg_scale)) xreg_train <- scale(xreg_train)
      last_row    <- xreg_train[nrow(xreg_train), , drop = FALSE]
      xreg_future <- matrix(rep(as.numeric(last_row), each = h), nrow = h, byrow = TRUE)
      colnames(xreg_future) <- colnames(xreg_train)
    }
    
    # 2) Variables de calendario
    cal <- ax_calendar_xreg()
    if (!is.null(cal) && isTRUE(cal$ok) && !is.null(cal$xreg) && ncol(cal$xreg) > 0) {
      cal_x <- cal$xreg
      validate(need(nrow(cal_x) >= sp$train_n,
                    "Variables de calendario: no hay suficientes filas para entrenamiento."))
      cal_train <- cal_x[1:sp$train_n, , drop = FALSE]
      cal_fut   <- matrix(rep(as.numeric(cal_train[nrow(cal_train), ]), each = h),
                          nrow = h, byrow = TRUE)
      colnames(cal_fut) <- colnames(cal_train)
      xreg_train  <- if (!is.null(xreg_train)) cbind(xreg_train,  cal_train) else cal_train
      xreg_future <- if (!is.null(xreg_future)) cbind(xreg_future, cal_fut)  else cal_fut
    }
    
    validate(need(!is.null(xreg_train) && ncol(xreg_train) > 0,
                  paste0("ARIMAX requiere al menos un regresor. ",
                         "Selecciona columnas del CSV o extrae variables de calendario.")))
    
    fit <- tryCatch(
      forecast::Arima(y_train, order = ord, seasonal = seas,
                      include.mean = (input$ax_const == 1),
                      method = input$ax_method,
                      xreg = xreg_train),
      error = function(e) {
        tryCatch(
          forecast::Arima(y_train, order = ord, seasonal = seas,
                          include.mean = (input$ax_const == 1),
                          method = "ML",
                          xreg = xreg_train),
          error = function(e2) stop(paste0(
            "No se pudo ajustar ARIMAX. Prueba otros ordenes. Error: ", conditionMessage(e2)
          ))
        )
      }
    )
    fc  <- forecast::forecast(fit, h = h, xreg = xreg_future)
    acc <- tryCatch(accuracy(fc), error = function(e) NULL)
    list(fit = fit, fc = fc, acc = acc)
  })
  
  output$ax_fc_plot <- renderPlot({
    req(ax_fit())
    fc_plot(ax_fit()$fc, a11y_on(), "Pronostico ARIMAX")
  })
  output$ax_metrics <- renderDT({
    req(ax_fit()); acc <- ax_fit()$acc
    validate(need(!is.null(acc), "No se pudo calcular accuracy."))
    datatable(round(as.data.frame(acc), 4), options = list(dom = "t", scrollX = TRUE))
  })
  output$ax_summary <- renderPrint({ req(ax_fit()); ax_fit()$fit })
  
  # ── Validacion unificada de residuos ─────────────────────────────────────────
  val_resid <- reactive({
    model <- input$val_model
    fit <- switch(model,
                  smooth = { req(smooth_fit()); smooth_fit()$fit },
                  arima  = { req(bj_fit());     bj_fit()$fit     },
                  arimax = { req(ax_fit());     ax_fit()$fit      }
    )
    res <- extract_resid(fit)
    validate(need(!is.null(res), "Ajusta primero el modelo seleccionado."))
    res
  })
  
  output$val_resid_ts <- renderPlot({
    obj <- val_resid()
    p <- ggplot(data.frame(t = seq_along(obj$r), r = obj$r), aes(x = t, y = r)) +
      geom_line() + geom_hline(yintercept = 0, linetype = 2, color = "red")
    style_plot(p, a11y_on()) + labs(title = "Residuos en el tiempo", x = NULL, y = NULL)
  })
  output$val_resid_qq <- renderPlot({
    obj <- val_resid()
    if (a11y_on()) {
      op <- par(cex = 1.3, lwd = 2, font = 2, font.lab = 2, font.main = 2, bg = "white")
      on.exit(par(op))
      qqnorm(obj$r, main = "QQ-plot de residuos", pch = 19, col = "black")
      qqline(obj$r, lwd = 3, col = "black")
    } else {
      qqnorm(obj$r, main = "QQ-plot de residuos"); qqline(obj$r)
    }
  })
  output$val_resid_hist <- renderPlot({
    obj <- val_resid()
    p <- ggplot(data.frame(r = obj$r), aes(x = r)) +
      geom_histogram(bins = 30, fill = "#3498db", alpha = if (a11y_on()) 1 else 0.7)
    style_plot(p, a11y_on()) + labs(title = "Histograma de residuos", x = NULL, y = NULL)
  })
  output$val_resid_fitted <- renderPlot({
    obj <- val_resid()
    p <- ggplot(data.frame(f = obj$f, r = obj$r), aes(x = f, y = r)) +
      geom_point(alpha = if (a11y_on()) 1 else 0.6) + geom_hline(yintercept = 0, linetype = 2)
    style_plot(p, a11y_on()) + labs(title = "Residuos vs Ajustados", x = "Ajustados", y = "Residuos")
  })
  output$val_acf_resid <- renderPlot({
    obj <- val_resid()
    style_plot(ggAcf(obj$r), a11y_on()) + labs(title = "ACF de residuos")
  })
  output$val_acf_resid2 <- renderPlot({
    obj <- val_resid()
    style_plot(ggAcf(obj$r^2), a11y_on()) + labs(title = "ACF de residuos^2 (ARCH)")
  })
  output$val_resid_tests <- renderDT({
    obj <- val_resid()
    df  <- run_resid_tests(obj$r)
    df[["p-valor"]] <- round(df[["p-valor"]], 6)
    datatable(df, options = list(dom = "t", scrollX = TRUE))
  })
  
} # end server

shinyApp(ui, server)