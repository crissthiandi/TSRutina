# TSRutina

Asistente para el análisis de series de tiempo, permite mantener un flujo de trabajo mecánico y basado en la metodología definida en un curso introductorio.

Temas tratados:
* Prueba de Estacionariedad
* Suavizamientos 
  * Promedio movil Simple
  * Promedio movil Ponderado
  * Exponencial
  * Holt
  * Holt Winter
* Ajustes a modelos ARIMA(p,i,q)
  * Consideramos las aproximaciónes:
  * ARIMA(0,0,q)=Ma(q)
  * ARIMA(p,0,0)=Ar(p)

Contiene un generador de graficas de los ajustes al igual que un metodo de escritura en disco duro, pendiente capacidad de ajuste de calidad y dimensiones.

## Ejmplo de uso

Primeros pasos, instalación y uso:
```
library('TSRutina')
```

