# Modelos para Series Temporales

Este repositorio incluye scripts para la descripción y ajuste de modelos de series de tiempo, tanto para datos univariados como multivariados. El objetivo es proporcionar herramientas para analizar y predecir series temporales utilizando diversos enfoques.

## Modelos Univariados: SARIMAX

- Los modelos de la familia **SARIMAX** se utilizan para ajustes univariados. Estos modelos incorporan variables regresoras que son útiles para captar la variabilidad en ciertos momentos específicos.
- Limitación: Es posible que no siempre se disponga de valores futuros para las regresoras, lo que puede dificultar las predicciones a largo plazo.
- 📂 [Ver Script de Modelo VAR en R](https://github.com/NicoGottig/series-temporales/blob/main/Script%20varios/arma-visitas-cinear.R)
## Modelos Multivariados: VAR

- Los modelos **VAR (Vector Autoregressive)** son generalizaciones multivariadas de los modelos AR y permiten analizar la dinámica entre múltiples series temporales.
- Son útiles tanto para entender las relaciones a corto plazo como para identificar la **cointegración** y las relaciones de equilibrio a largo plazo entre las series.
- 📂 [Ver Script de Modelo VAR en R](https://github.com/NicoGottig/series-temporales/blob/main/pronosticos%20de%20inflacion/var-final-tp3.R)

## Modelos de Pronóstico Avanzados

- Se incluyen scripts en Python para el pronóstico de inflación a 6 meses, utilizando modelos avanzados:
  - **Neural Prophet**: Un modelo de predicción que combina las capacidades de los modelos autoregresivos tradicionales con técnicas de aprendizaje automático.
  - **Redes LSTM (Long Short-Term Memory)**: Una arquitectura de red neuronal recurrente eficaz para capturar patrones de largo plazo en series temporales.
  - 📂 [Ver Red Neuronal para Inflación](https://github.com/NicoGottig/series-temporales/blob/main/pronosticos%20de%20inflacion/scripts%20python/redes-neuronales-inflacion.ipynb)

---

## Objetivo del Proyecto

El objetivo es proporcionar un conjunto de herramientas robustas para el análisis y pronóstico de series temporales. Estos modelos son aplicables en diversos contextos económicos y financieros.
