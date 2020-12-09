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

## Ejemplo de uso

### Instalación

Primeros pasos, instalación y uso:
```python
#Previos
install.packages('devtools')
library('devtools')
#TSRutina
install_github("crissthiandi/TSRutina@v2.0",force = TRUE)
library('TSRutina')
```
### Rutina init()

La idea principal es solo correr una función a la cual se le pasan la menor cantidad de parametros posibles y que con ello la función realice todo el proceso de analisis. ```init()``` es la función encargada de esta tarea.

Tomemos la base de datos **sunspot.year** la cual tiene las manchas solares anuales entre los años 1749 y 1983.
![image](img/manchasts.png)






