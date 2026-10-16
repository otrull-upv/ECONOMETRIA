# Guía de Usuario y Teórica — Aplicación de Series Temporales (Econometría)

Esta aplicación Shiny está diseñada para el análisis avanzado, modelización, diagnóstico y predicción de series temporales. Incorpora herramientas de accesibilidad (modo de alto contraste en blanco y negro, fuentes grandes y soporte completo para teclado).

---

## 1. Pestaña Datos y Configuración Inicial

### 1.1. Carga de Datos y Estructura Temporal
La aplicación permite cargar archivos en formato CSV o utilizar por defecto el conjunto de datos clásico `AirPassengers` (mensual, 1949–1960). 
* **Índice Temporal:** Se procesa de forma robusta mediante la librería `tsibble`, detectando automáticamente formatos mensuales (`YYYY-MM`), trimestrales (`YYYY Qq`), anuales (`YYYY`) o fechas completas (`POSIXct` / `Date`).
* **Conversión a Objeto de Serie Temporal (`ts`):** Para los algoritmos de modelización y pronóstico (`forecast`), las series se convierten internamente a la clase `ts` mediante la función:
  
  $
  Y_t = \{Y_1, Y_2, \dots, Y_n\}
  $

  con una frecuencia estacional especificada por el usuario ($s = 12$ para mensual, $s = 4$ para trimestral, etc.).

### 1.2. División Train / Test
Para evaluar la capacidad predictiva fuera de muestra, la serie se divide en dos subconjuntos según la proporción de entrenamiento $p \in [0.6, 0.95]$:
* **Entrenamiento (Train):** $t = 1, \dots, \lfloor n \cdot p \rfloor$
* **Validación (Test):** $t = \lfloor n \cdot p \rfloor + 1, \dots, n$

---

## 2. Pestaña Serie Temporal y Periodograma

### 2.1. Transformaciones y Diferenciación
* **Transformación de Box-Cox:** Utilizada para estabilizar la varianza cuando la amplitud estacional crece con el nivel de la serie:
  
  $$
  y_t^{(\lambda)} = \begin{cases} \frac{y_t^\lambda - 1}{\lambda} & \text{si } \lambda \neq 0 \\ \ln(y_t) & \text{si } \lambda = 0 \end{cases}
  $$

* **Diferenciación Regular y Estacional:** Permite eliminar tendencias deterministas o estocásticas y raíces unitarias.
  * Operador de diferencia regular de orden $d$:
    
    $$
    \Delta^d y_t = (1 - B)^d y_t
    $$

  * Operador de diferencia estacional de orden $D$ con periodicidad $s$:
    
    $$
    \Delta_s^D y_t = (1 - B^s)^D y_t
    $$

### 2.2. Periodograma Espectral
El periodograma analiza la descomposición de la varianza de la serie en el dominio de las frecuencias, identificando ciclos ocultos o componentes estacionales dominantes.
* **Densidad Espectral Estimada:** Se calcula mediante la transformada de Fourier discreta de las autocovarianzas muestrales con aplicación previa de un *taper*:
  
  $$
  I(\omega) = \frac{1}{n} \left| \sum_{t=1}^n (y_t - \bar{y}) e^{-i \omega t} \right|^2
  $$

  para frecuencias $\omega \in [0, \pi]$. Los picos locales en $I(\omega)$ señalan periodicidades significativas equivalentes a un periodo temporal de $T = \frac{2\pi}{\omega}$ (o $\frac{1}{\omega}$ en ciclos por observación).

---

## 3. Pestaña Descomposición de Series Temporales

### 3.1. Descomposición Clásica
Descompone la serie $Y_t$ en tendencia ($T_t$), componente estacional ($S_t$) y residuo o ruido irregular ($E_t$):
* **Modelo Aditivo:** $Y_t = T_t + S_t + E_t$
* **Modelo Multiplicativo:** $Y_t = T_t \times S_t \times E_t$

### 3.2. Descomposición STL (Seasonal and Trend decomposition using LOESS)
Método robusto basado en suavizado local ponderado (LOESS). Permite aislar iterativamente la tendencia y la estacionalidad minimizando el impacto de valores atípicos mediante pesos de robustez local.

---

## 4. Pestaña Suavizado Exponencial y Espacios de Estado (ETS / BATS / TBATS)

* **Holt (Tendencia Lineal):** Modela el nivel ($\ell_t$) y la tendencia lineal amortiguada o constante ($b_t$) mediante ecuaciones de actualización exponencial.
* **Holt-Winters (Estacionalidad Aditiva / Multiplicativa):** Añade una ecuación para la componente estacional ($s_t$), permitiendo capturar tanto tendencia como patrones estacionales complejos.
* **BATS / TBATS:** Modelos de espacio de estado basados en cajas exponenciales, transformaciones Box-Cox y armonicos trigonométricos para manejar múltiples estacionalidades complejas o no enteros.

---

## 5. Pestaña Modelos ARIMA (Box-Jenkins)

La metodología ARIMA($p, d, q$) $\times (P, D, q)_s$ modela una serie estacionaria mediante procesos autorregresivos y de medias móviles aplicados sobre la serie diferenciada $\nabla^d \nabla_s^D y_t$:

$$
\phi_p(B) \tilde{\Phi}_P(B^s) (1 - B)^d (1 - B^s)^D Y_t = c + \theta_q(B) \tilde{\Theta}_Q(B^s) \varepsilon_t
$$

* **Identificación (ACF y PACF):**
  * **Función de Autocorrelación (ACF):** Mide la correlación lineal entre $Y_t$ y $Y_{t-k}$:
    
    $$
    \rho_k = \frac{\sum_{t=k+1}^n (Y_t - \bar{Y})(Y_{t-k} - \bar{Y})}{\sum_{t=1}^n (Y_t - \bar{Y})^2}
    $$

  * **Función de Autocorrelación Parcial (PACF):** Mide la correlación entre $Y_t$ y $Y_{t-k}$ eliminando los efectos intermedios de los retardos $1$ a $k-1$.

---

## 6. Pestaña Modelos ARIMAX y Variables de Calendario

### 6.1. ARIMAX con Regresores Exógenos
Extiende el modelo ARIMA incorporando efectos explicativos externos $X_{jt}$:

$$
\Phi(B)(1-B)^d Y_t = \sum_{j=1}^k \beta_j X_{jt} + \Theta(B)\varepsilon_t
$$

### 6.2. Variables de Calendario (X-13ARIMA-SEATS / TRAMO-SEATS)
Permite extraer y modelar de forma automática efectos de calendario que distorsionan la serie:
* **Trading Days (TD):** Controla el número de días laborables o fines de semana específicos en cada mes/trimestre.
* **Easter (Efecto Semana Santa):** Modela desplazamientos de festividades móviles.
* **Leap Year (Año Bisiesto):** Capta el impacto del 29 de febrero.

---

## 7. Pestaña Validación de Residuos

Para garantizar que el modelo captura toda la estructura sistemática y que los errores $\varepsilon_t$ constituyen ruido blanco, la aplicación evalúa un panel completo de contrastes estadísticos:

* **Normalidad:**
  * **Shapiro–Wilk:** Evalúa concordancia con la distribución normal en muestras finitas:
    
    $$
    W = \frac{\left( \sum_{i=1}^n a_i e_{(i)} \right)^2}{\sum_{i=1}^n (e_i - \bar{e})^2}
    $$

  * **Jarque–Bera:** Contraste asintótico basado en asimetría ($S$) y curtosis ($K$):
    
    $$
    \text{JB} = \frac{n}{6} \left( S^2 + \frac{(K - 3)^2}{4} \right) \sim \chi^2_2
    $$

* **Homoscedasticidad condicional (Efectos ARCH):**
  * **Contraste ARCH-LM:** Evalúa la presencia de trazas de volatilidad condicional en los residuos al cuadrado $e_t^2$:
    
    $$
    LM = n \cdot R^2 \sim \chi^2_m
    $$

* **Incorrelación serial (Ruido Blanco):**
  * **Contraste de Ljung–Box:** Prueba conjunta de autocorrelación hasta el retardo $m$:
    
    $$
    Q = n(n+2) \sum_{k=1}^m \frac{\hat{\rho}_k^2}{n - k} \sim \chi^2_m
    $$
