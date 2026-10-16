# ECONOMETRIA

Versión: 0.90 Beta

Este repositorio contiene código y materiales orientados a la realización de prácticas de la asignatura de Econometría de la Universidad Politécnica de Valencia (UPV).

## Descripción

El proyecto está estructurado en dos unidades didácticas (UD), cada una con su propia aplicación y enfoque específico dentro de la asignatura:

- UD1: Modelos de regresión múltiple
  - Aplicación dedicada a la estimación, interpretación y diagnóstico de modelos de regresión múltiple.
  - Incluye ejemplos de relaciones entre variables, ajuste del modelo y evaluación de la calidad de la predicción.

- UD2: Series temporales
  - Aplicación dedicada al análisis y modelado de series temporales.
  - Incluye conceptos de tendencia, estacionalidad, autocorrelación, previsión y otros métodos habituales en econometría temporal.

Cada unidad didáctica tiene su propia aplicación y su propio conjunto de prácticas, datos y ejercicios.

## Objetivo del repositorio

El objetivo de este repositorio es servir como soporte para la realización de las prácticas de Econometría, facilitando la experimentación con modelos estadísticos y herramientas de análisis aplicados a problemas reales.

## Estructura del proyecto

```text
ECONOMETRIA/
├── README.md
├── apps/
│   ├── regresion.R
│   ├── time_series.R
│   ├── ML_regression.R
│   └── ML_timeseries.R
├── requirements.txt
├── LICENSE
└── docs/
```

## Requisitos

Para poder ejecutar el código, normalmente será necesario:

- Un entorno de programación compatible (R) con el complemento Shiny.
- Dependencias indicadas en el archivo `requirements.txt` o en la documentación de cada unidad.
- Datos de ejemplo o instrucciones para su descarga.

## Instalación

1. Clonar el repositorio:

```bash
git clone https://github.com/otrull-upv/ECONOMETRIA.git
cd ECONOMETRIA
```
2. Consultar la documentación específica de cada unidad didáctica para ejecutar la aplicación correspondiente.

## Uso

Cada unidad didáctica incluye su propia aplicación y su propia documentación. El flujo habitual es:

- abrir la aplicación de la UD correspondiente, ya sea desde R o desde el fichero
- cargar los datos de ejemplo o los datos del ejercicio,
- ejecutar el análisis estadístico o econométrico solicitado,
- interpretar los resultados,
- documentar las conclusiones en el informe o notebook asociado.

## Buenas prácticas

- Documentar cada ejercicio con comentarios y explicaciones.
- Guardar resultados, gráficos y salidas en carpetas adecuadas.
- Usar datos de ejemplo cuando sea necesario para validar procedimientos.

## Contribuciones

Las contribuciones son bienvenidas siempre que mantengan el enfoque docente y académico del repositorio. Se recomienda:

- crear ramas para cada cambio,
- documentar adecuadamente los ajustes,
- mantener la estructura del proyecto,
- revisar que el material siga siendo útil para prácticas de econometría.

## Licencia

Este repositorio se ofrece con fines docentes y académicos. Si se aplica una licencia específica, deberá indicarse en el correspondiente archivo del proyecto. En caso de dudas, consultar con la persona responsable del material o la universidad.

## Historial de versiones

- 0.90 Beta — Primera versión del README con la estructura general del repositorio y la organización por unidades didácticas.

## Nota

Este proyecto está pensado como material de apoyo para la asignatura de Econometría de la UPV y puede adaptarse a distintos ejercicios, prácticas y metodologías didácticas.
