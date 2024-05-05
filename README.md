# Modelos para series temporales
El repositorio incluye scripts para la descripción y el ajuste de modelos de series de tiempo para datos univariados y multivariados.

+ Los modelos de la familia SARIMAX proponen ajustes univariados, con variables regresoras que en general son útiles para captar la variabilidad en ciertos momentos. El problema de estas regresoras es podemos no contar con sus valores en el futuro.
+ Como alternativa, los modelos VAR son generalizaciones multivariadas de los modelos AR, y sirven para conocer la dinámica entre las series en el corto y en el largo plazo (cointegración).
+ Por último, se presentan scripts en python para el pronóstico de inflación a 6 meses. Se incluyen modelos de Neural Prophet y redes LSTM.
  
