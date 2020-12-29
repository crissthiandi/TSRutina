# TSRutina V3.0 Taby

Asistente para el análisis de series de tiempo, permite mantener un flujo de trabajo mecánico y basado en la metodología definida acontinuación:

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
  * ARIMA(p,0,q)=ARMA(p,q)

El asistente contiene un generador de graficas de los Suavizamientos anteriores que permite almacenar los graficos en el directorio de trabajo, pendiente capacidad de ajuste de calidad y dimensiones en las imagenes PNG.

Se realizan multiples pruebas estadisticas que permite tomar decisiones sobre si la serie se ajusta o no a la lista de supuestos de algun modelo.

Esta paqueteria tiene una documentación de la mayoria de las funciónes, si se tien duda de su uso o que hacen algunas funciones no dudes en usar la documentación que se instala junto a la paqueteria. Ejemplo:
```python
#acceder a la documentación de la función init()
library('TSRutina')
?init
```

## Ejemplo de uso y salidas

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

La idea principal es solo correr una función a la cual *se le pasaran la menor cantidad de parametros posibles*. Parametros con los que la función realizara todo el proceso de analisis. ```init()``` es la función encargada de esta tarea.

Tomemos la base de datos **sunspot.year** la cual tiene las manchas solares anuales entre los años 1749 y 1983.
![image](img/manchasts.png)

luego llamemos a ```init(sunspot.year)``` y obtenemos una analisis de estos datos.
**Pendiente a documentación** :c




