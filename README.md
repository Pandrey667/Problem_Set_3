# Problem Set 3: Making Money with ML?

**Equipo:** Yenny Castillo, Fabian Vidal, Andrey Rincón, Carlos Alape  
**Universidad de los Andes – Maestría en Economía**

Este repositorio presenta el desarrollo completo del Problem Set 3, cuyo objetivo es construir y comparar modelos predictivos para estimar el precio de oferta de viviendas en la zona de Chapinero, Bogotá. El proyecto emplea información de Properati enriquecida con fuentes externas para asegurar la robustez de los resultados y la relevancia de las recomendaciones para aplicaciones reales en el mercado inmobiliario.

## Estructura del Repositorio

- **document/**: Contiene el PDF final con introducción, datos, modelos, resultados y conclusiones.
- **scripts/**: Incluye los códigos fuente en R organizados por etapas del proceso. Se recomienda ejecutarlos en el siguiente orden:
 1. `1.Limpieza de Training.R` - Preprocesamiento del conjunto de entrenamiento
 2. `1.Limpieza de Testing.R` - Preprocesamiento del conjunto de prueba
 3. `2.Estadisticas Descriptivas.R` - Analisis de estadistica Descriptiva
 4. `3. XGBoost_Modelo Ganador.R` - Entrenamiento detallado del modelo ganador
Otros Modelos
 5. `3. CART.R`
 6. `3. OLS-Ridge`
 7. `3. RF , CART , XGBost , Red Neuronal`
 8. `3. RF , GMB , Super Learner`
 9. `3. 3.Elastic Net, Random Forest y XG Boost con VC Espacial`


- **stores/**: Archivos CSV con los conjuntos de datos de entrenamiento y prueba. Ademas los datos de fuentes externas de Datos Abiertos Bogota y el ShapeFile de coordenadas. 
- **views/**: Visualizaciones: Mapas y diagramas de barras de mediciones de MAE. 

## Modelos Implementados

Se compararon múltiples estrategias de modelado y validación:
- **XGBoost** (modelo ganador): validación cruzada en dos etapas (aleatoria y espacial), destaca por su robustez y menor MAE.
- **Random Forest** y **Elastic Net**: muestran desempeño competitivo, especialmente con validación espacial.
- **Ridge y CART**: útiles para comparar linealidades y efectos de regularización.
- **Gradient Boosting Machine , Red Neuronal MLP y SuperLearner**: permiten contrastar el poder predictivo de ensamblajes y redes poco profundas frente a métodos de árboles optimizados.

Las métricas principales incluyen **MAE**, **RMSE** y desempeño en Kaggle (evaluando error fuera de muestra y generalización).


| Modelo                | MAE Test        | RMSE Test      | MAE Kaggle      |
|-----------------------|-----------------|---------------|-----------------|
| Redes neuronales      | 245,729,670     | 309,030,826   | 340,115,955     |
| OLS-Ridge             | 205,171,801     | 287,731,542   | 265,571,420     |
| CART                  | 205,171,801     | 287,731,542   | 248,255,420     |
| Random Forest         | 146,052,767     | 193,585,019   | 247,039,507     |
| Gradient Boosting Machine                | 152,493,262     | 212,582,468   | 222,057,769     |
| SuperLearner          | 141,946,030     | 195,582,202   | 260,196,139     |
| Elastic Net           | 172,837,162     | 251,996,502   | 254,519,525     |
| Random Forest CV espacial       | 101,228,886     | 162,352,357   | 218,810,463     |
| XGBoost II            | 98,704,429      | 155,788,519   | 188,895,296     |
| **XGBoost ganador**   | 149,832,428     | 219,633,506   | **186,141,249** |

**Nota:** El menor MAE en Kaggle corresponde a **XGBoost ganador**, demostrando su capacidad superior de generalización y validación espacial rigurosa en este entorno competitivo.


## Resultados Principales

- El modelo XGBoost validado espacialmente logró el MAE más bajo y mejor generalización, superando a otros métodos en datos internos y en la competencia de Kaggle.
- Las variables más relevantes fueron el área del inmueble, longitud y latitud, interacción área-estrato, número de habitaciones y baños, y medidas de accesibilidad urbana (distancias a servicios).
- Incorporar fuentes externas (OpenStreetMap, criminalidad, estrato) y realizar una ingeniería de variables es esencial para aumentar la precisión y robustez del modelo.
- El enfoque es replicable y puede actualizarse periódicamente para reflejar cambios de mercado y mejorar la utilidad práctica de la predicción.
