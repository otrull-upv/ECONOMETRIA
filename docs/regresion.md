# Guía de Usuario y Accesibilidad — Aplicación Shiny de Econometría

Esta aplicación Shiny está diseñada para el análisis econométrico avanzado, permitiendo cargar datos, ajustar diversos modelos de regresión, evaluar supuestos econométricos fundamentales (multicolinealidad, normalidad, heterocedasticidad, autocorrelación) y comparar modelos predictivos con facilidad. La interfaz ha sido concebida no solo para la ejecución técnica, sino también para una experiencia de uso clara, accesible y sostenible.

## 1. Características de Accesibilidad (A11y)

Con el fin de garantizar una experiencia inclusiva y cumplir con los estándares de accesibilidad web, la aplicación incorpora las siguientes herramientas:

* **Enlace de salto rápido ("Skip link"):** Al pulsar la tecla `Tab` al cargar la página, aparece un enlace flotante que permite saltar directamente al contenido principal (`#main-content`), evitando la necesidad de recorrer todo el menú lateral en lectores de pantalla y usuarios de teclado.

* **Modo Accesible (Alto Contraste y Letra Grande):** En la parte superior de la barra lateral, encontrarás una casilla de verificación titulada **"Modo accesible (alto contraste y letra grande)"**. Al activarla, se aplican varias mejoras visuales:

  * El tamaño de la fuente aumenta de forma global a un mínimo de $18\,\text{px}$ con una mayor altura de línea.

  * El esquema de colores cambia a alto contraste puro (fondo blanco y texto negro).

  * Los botones y campos de formulario adquieren bordes y líneas de enfoque más gruesas ($2--3\,\text{px}$) y visibles.

* **Indicadores de Enfoque (Focus Outlines):** Todos los elementos interactivos (botones, enlaces, campos de texto y selectores) muestran un contorno azul brillante ($3\,\text{px}$) al recibir el foco, lo que mejora enormemente la navegación por teclado.

* **Etiquetas para Lectores de Pantalla:** Se han incorporado clases ocultas `.sr-only` y atributos ARIA (`aria-label`, `role="img"`) en los bloques gráficos para describir adecuadamente el contenido visual del análisis.

* **Reducción de Movimiento:** La aplicación respeta la preferencia del sistema `prefers-reduced-motion`, desactivando o minimizando las transiciones y animaciones para usuarios sensibles al movimiento.

## 2. Guía de Uso del Panel Lateral (Controles)

1. **Carga de Datos:**

   * Sube tu propio archivo de datos en formato `.csv`, indicando si posee cabecera y seleccionando el separador adecuado (coma, punto y coma o tabulación).

   * O bien, marca la opción **"Usar dataset de ejemplo (mtcars)"** para realizar pruebas rápidas de inmediato.

2. **Selección de Variables:**

   * **Variable respuesta ($Y$):** Selecciona la columna que actuará como variable dependiente.

   * **Predictores ($X$):** Selecciona una o varias variables explicativas.

   * **Constante (Intercepto):** Puedes decidir incluir o excluir el término independiente ($\beta_0$) en el modelo.

3. **Tipo de Modelo:**

   * **Regresión lineal clásica (`lm`)**

   * **Cochrane–Orcutt AR(1)** para corrección de autocorrelación serial.

   * **Modelos penalizados (`ridge`, `lasso`, `Elastic Net`)** mediante validación cruzada (`cv.glmnet`).

   * **Regresión logística (`glm binomial`)** para variables respuesta binarias.

4. **Opciones adicionales:**

   * Estandarizar variables numéricas mediante puntuaciones $z$-score.

   * Eliminar automáticamente filas con valores perdidos (`NA`).

   * Botón principal **"Ajustar modelo"** para actualizar los cálculos.

5. **Predicción (Train / Test):**

   * Configura el porcentaje de entrenamiento mediante el control deslizante y pulsa **"Entrenar / Test"** para evaluar el rendimiento predictivo fuera de muestra.

   * Utiliza los botones de descarga para exportar el resumen completo del modelo en formato texto o las predicciones en CSV.

## 3. Pestañas de Análisis, Diagnóstico y Fórmulas Econométricas

Esta sección detalla los fundamentos matemáticos y estadísticos de las pestañas de diagnóstico integradas en la aplicación:

### 3.1. Datos y Exploración
Permite visualizar la estructura interna del dataset y analizar la relación bivariada mediante diagramas de dispersión con ajuste no paramétrico `loess`. La ecuación general del modelo estimado es:

$$
Y_i = \beta_0 + \beta_1 X_{1i} + \beta_2 X_{2i} + \cdots + \beta_k X_{ki} + e_i
$$

### 3.2. Modelo y Estimación
Muestra los coeficientes estimados por Mínimos Cuadrados Ordinarios (MCO), los cuales minimizan la suma de errores al cuadrado:

$$
\min_{\beta} \sum_{i=1}^{n} e_i^2 = \sum_{i=1}^{n} (Y_i - \hat{Y}_i)^2
$$

El coeficiente de determinación $R^2$ evalúa la bondad de ajuste global:

$$
R^2 = 1 - \frac{\sum_{i=1}^{n} e_i^2}{\sum_{i=1}^{n} (Y_i - \bar{Y})^2}
$$

### 3.3. Multicolinealidad
Cuando las variables explicativas presentan alta correlación lineal entre sí, las varianzas de los estimadores MCO se inflaman. Se evalúa mediante el Factor de Inflación de Varianza (VIF):

$$
\text{VIF}_j = \frac{1}{1 - R_j^2}
$$

Donde $R_j^2$ es el coeficiente de determinación de una regresión auxiliar de la variable $j$-ésima sobre el resto de predictores. Valores de $\text{VIF} > 10$ (o estrictamente $> 5$ en literatura aplicada) suelen indicar multicolinealidad severa.

### 3.4. Normalidad de los Residuos
Para garantizar la validez de las pruebas de hipótesis en muestras finitas, se analiza si los residuos siguen una distribución normal $e_i \sim \mathcal{N}(0, \sigma^2)$.

* **Contraste de Shapiro–Wilk:** Evalúa la concordancia entre los estadísticos de orden muestrales y los valores esperados de una normal estándar:

$$
    W = \frac{\left( \sum_{i=1}^{n} a_i e_{(i)} \right)^2}
   {\sum_{i=1}^{n} (e_i - \overline{e})^2}
$$

* **Contraste de Jarque–Bera:** Mide el sesgo y la curtosis de los residuos en comparación con la distribución normal:

$$
  \text{JB} = \frac{n}{6} \left( S^2 + \frac{(K - 3)^2}{4} \right)
$$

  donde $S$ es el sesgo muestral y $K$ la curtosis.

### 3.5. Heterocedasticidad
La presencia de varianza no constante en los errores ($\text{Var}(e_i \mid X) = \sigma_i^2 \neq \text{constante}$) invalida los errores estándar tradicionales.

* **Contraste de Breusch–Pagan:** Evalúa si la varianza de los errores depende de los predictores mediante una regresión auxiliar de los residuos al cuadrado $e_i^2$ sobre las variables independientes:

$$
  \text{LM} = n \cdot R^2_{e^2} \sim \chi^2_k
$$

* **Gráfico Scale-Location:** Representa la raíz cuadrada de los residuos estandarizados en valor absoluto frente a los valores ajustados:

$$
  \sqrt{|e_i^*|} = \sqrt{\left| \frac{\hat{e}_i}{\hat{\sigma}\sqrt{1 - h_{ii}}} \right|}
$$

### 3.6. Autocorrelación Serial
Frequente en series temporales, donde los errores están correlacionados entre periodos ($\text{Cov}(e_t, e_s) \neq 0$).

* **Estadístico de Durbin–Watson:** Detecta autocorrelación de primer orden:

$$
  \text{DW} = \frac{\sum_{t=2}^{n} (e_t - e_{t-1})^2}{\sum_{t=1}^{n} e_t^2} \approx 2(1 - \hat{\rho})
$$

* **Contraste de Breusch–Godfrey:** Permite probar autocorrelación de orden superior $p$:

$$
  e_t = \rho_1 e_{t-1} + \rho_2 e_{t-2} + \cdots + \rho_p e_{t-p} + \beta_0 + \sum_{j=1}^{k} \beta_j X_{jt} + v_t
$$

  El estadístico de prueba es $(n - p)R^2 \sim \chi^2_p$.

### 3.7. Métricas de Predicción (Train / Test)
Para evaluar la capacidad predictiva fuera de muestra, se calcula la Raíz del Error Cuadrático Medio (RMSE) en el conjunto de prueba:

$$
\text{RMSE} = \sqrt{\frac{1}{n_{\text{test}}} \sum_{i \in \text{test}} (Y_i - \hat{Y}_i)^2 }
$$

### 3.8. Restricciones Lineales
Permite contrastar hipótesis conjuntas sobre los coeficientes del modelo de la forma $R\beta = q$ mediante el estadístico $F$ de restricciones lineales:

$$
F = \frac{(\mathrm{SSR}_R - \mathrm{SSR}_{UR})/q}{\mathrm{SSR}_{UR}/(n - k - 1)} \sim F\bigl(q,\, n - k - 1\bigr)
$$

Donde $\mathrm{SSR}_R$ y $\mathrm{SSR}_{UR}$ representan la suma de residuos al cuadrado del modelo restringido y no restringido, respectivamente.
