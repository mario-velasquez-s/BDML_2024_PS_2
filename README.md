# BDML_2024_PS_2
Entrega de Big Data y Machine Learning | Problem Set 2

# BDML 2024

# Problem Set 2 

## Sobre el Proyecto:

Este proyecto tiene como objetivo desarrollar un modelo predictivo que pueda clasificar con precisión los hogares colombianos en categorías de pobreza utilizando variables socioeconómicas. La iniciativa aborda la necesidad global de identificar y reducir la pobreza aprovechando técnicas de aprendizaje automático para señalar los predictores críticos de pobreza entre los hogares. Este enfoque apoya el Objetivo de Desarrollo Sostenible de las Naciones Unidas para erradicar la pobreza en todo el mundo. Es importante destacar que el desafío de la pobreza es complejo, requiriendo evaluaciones que van más allá de las medidas monetarias para incluir un conjunto más amplio de indicadores de vulnerabilidad.

La investigación realizada en este proyecto se basa en estudios anteriores, como los de Sabogal et al. (2021), que utilizaron herramientas de aprendizaje automático como el algoritmo XGBoost para predecir la pobreza basada en datos del Departamento Administrativo Nacional de Estadística (DANE) para el período 2016-2019. Este estudio extiende esas metodologías incorporando una gama de modelos predictivos, incluyendo regresión lineal, modelos logísticos y árboles de decisión, optimizados además a través de diversas técnicas de equilibrio de datos para mejorar la precisión y robustez del modelo. Este esfuerzo predictivo no solo sirve como un ejercicio académico sino también como una herramienta práctica para guiar la asignación de recursos y la formulación de políticas hacia la reducción de la pobreza.

## Paquetes Necesarios y Prerrequisitos:

El proyecto requiere los siguientes paquetes de R:
- **pacman**: Herramienta de gestión de paquetes utilizada para cargar y administrar otros paquetes de R.
- **Paquetes para Manipulación de Datos**: `rio`, `tidyverse`, `dplyr`, `visdat`, `fastDummies`, `Hmisc`, `labelled`
- **Paquetes para Visualización de Datos**: `ggplot2`, `gridExtra`, `corrplot`, `rpart.plot`, `gtsummary`
- **Paquetes para Análisis Estadístico y Modelado**: `skimr`, `stargazer`, `MASS`, `fixest`, `zoo`, `glmnet`, `gbm`, `xgboost`
- **Paquetes de Aprendizaje Automático**: `caret`, `ranger`, `MLmetrics`, `rpart`
- **Paquetes para Equilibrar Datos**: `smotefamily`, `ROSE`
- **Herramientas Adicionales**: `rvest`, `httr`, `leaps`, `xtable`, `Metrics`, `doParallel`

### Instrucciones de Instalación

1. **Instalar R**: Descarga e instala R desde el [sitio web de CRAN](https://cran.r-project.org/).
2. **Instalar Paquetes**:
   - Puedes instalar cualquier paquete faltante utilizando el siguiente comando de R:
     ```R
     install.packages("packageName")
     ```
   - Alternativamente, el script proporcionado con el proyecto verifica automáticamente e instala cualquier paquete faltante de la lista requerida.

### Ejecución del Proyecto
Una vez que todos los paquetes necesarios estén instalados, puedes ejecutar los scripts del proyecto. Cada script incluye comentarios detallados explicando el propósito del código y cómo contribuye a los objetivos del proyecto.


## Uso:

### Configuración y Manipulación Inicial de Datos
El script comienza limpiando el entorno y configurando los paquetes necesarios utilizando el paquete `pacman` para una gestión eficiente. Luego, determina el sistema operativo del usuario y establece el directorio de trabajo correspondiente para manejar archivos de datos específicos según las configuraciones del usuario.

### Generación del DataFrame
Las primeras 480 líneas del script son cruciales, ya que incluyen la importación de datos, funciones de preprocesamiento y la imputación de datos faltantes. Estas líneas deben ejecutarse primero para generar el DataFrame requerido para los análisis subsiguientes.

### Secciones Independientes

Después de la fila 480, cada sección se ejecuta de manera independiente:
   - **Sección 2.1, 2.2, etc.**, están diseñadas para operar de manera independiente, asumiendo que se han ejecutado las primeras 480 filas.
   - Los usuarios pueden ejecutar estas secciones por separado para realizar análisis específicos, visualizaciones o modelado sin tener que volver a ejecutar todo el script.

Este enfoque modular ayuda en la gestión eficiente de scripts largos, especialmente cuando se centra en partes específicas del análisis de datos o cuando se necesita volver a ejecutar solo ciertas secciones debido a actualizaciones iterativas en el flujo de trabajo de análisis.

## Frameworks, Librerías y Programas:

- Los scripts del proyecto fueron llevados a cabo en R.
- El documento final que consolida las conclusiones y el análisis fue realizado en LaTex.

## Autores:

- María Camila Arias
- Martín Velásquez
- Mario Velásquez
- Daniela Vlasak
