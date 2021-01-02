#Trabajo hecho y en mantenimiento por @crissthiandi <albertocenaa@gmail.com>

#' Carga paquetes para un analisis de series de tiempo
#'
#' Proximamento en desuso en las funciones de TSRutina pero ideal para usarse en procesos
#' escribir codigo para analisis de series de tiempo
#'
#' @return Mensage si todo sale bien
#' @export
#'
#' @examples
#'
#' paquetes.tsrutina()
paquetes.tsrutina <- function(){
  require('tseries',warn.conflicts = F,quietly = F)
  library('lmtest',warn.conflicts = F,quietly = F)
  library('pracma',warn.conflicts = F,quietly = F)
  library('ggfortify',warn.conflicts = F,quietly = F)
  library('forecast',warn.conflicts = F,quietly = F,)
  library('tseries',warn.conflicts = F,quietly = F)
  library('greybox',warn.conflicts = F,quietly = F)
  library('readr',warn.conflicts = F,quietly = F)
  message("Se han cargado los paquetes necesarios")

}

#' Pruebas de una serie de tiempo
#'
#' Esta funcion incluye el calculo y la decision de dos pruebas estadisticas a una serie de tiempo.
#'
#' Realiza las siguientes pruebas:
#' La prueba de estacionalidad (Dickey-Fuller)
#'
#' La prueba de autocorrelacion (Durbin-Watson)
#'
#' Se debe meter un dataframe de dos columnas, la primera el tiempo y la segunda el valor de la serie
#' Tambien soporta el uso de objetos TimeSeries
#'
#' @param datos Data.frame o objeto TS a analizar
#' @param frecuencia Frecuencia de los datos, en caso de TS sobrescribe los valores
#' @param init_ Indicador para verificar los datos [True/False]
#'
#' @return Solo arroja la decicion a tomar, por defecto con respecto a p=0.05
#' @export
#'
#' @importFrom lmtest dwtest
#' @importFrom tseries adf.test
#'
#' @examples
#'  base=data.frame(tiempo=seq(Sys.Date(),by="days",length=20),valores=1:20*3+runif(1))
#'  serie_tiempo_pruebas(datos=base,frecuencia=4)
#'
serie_tiempo_pruebas <-function(datos,frecuencia=NULL,init_=FALSE,msg=TRUE){
    if(!init_){
      paquetes.tsrutina()
      pausa()
      conditional.tsrutina(datos)
    }
    pausa()
    if(!is.ts(datos)){

        if(is.data.frame(datos)){
            base<-datos
            names(base)<-c("x","y")

            base2<-base
            base2$xx<-1:length(base$x)

            regresion<-lm(y~xx,base2)
            cat("\n Prueba de presencia de autocorrelación Durbin-Watson Test \n")
            cat("\n (Prueba aplicada los residuos de una regresión lineal) \n")
            prueba<-dwtest(regresion)
            print(prueba)
            if(msg){
              p_valor<-readline('Inserte un p valor, (intro para p=0.05):  \n')
            }else{
              p_valor<-0.05
            }

            if(p_valor==""){
                    p_valor<-0.05
            }else{
                    print(sprintf("El valor de p= %s",p_valor))
                    p_valor<-as.numeric(p_valor)
            }


            if(prueba$p.value>0.05){
                message("No se puede rechazar \"H0 = No hay presencia de autocorrelacion en los residuos\" \n")
            }else{
                message("Se rechaza H0, se obta por \"H1 = Hay presencia de autocorrelacion en los residuos\" \n")
            }


            sprintf("\n \n")

            #Probar Estacionariedad para proceder a los modelos no probabilisticos
            #Prueba Dickey-Fuller:
            #H0: Hay presencia de una raiz unitaria en la serie de tiempo
            #H1: La serie de tiempo es Estacionaria.
            #No se rechaza H0 pues pvalor=0.467 es mayor que 0.05 (No se rechaza H0)
            basets<-ts(base$y,frequency=frecuencia)
            cat("\n Prueba de presencia de Estacionalidad Dickey-Fuller Test \n")

            prueba<-adf.test(basets)
            print(prueba)
            if(msg){#si no hay mensaje entonces predeterminado
              p_valor<-readline('Inserte un p valor, (intro para p=0.05) \n')
            }else{
              p_valor<-0.05
            }

            if(p_valor==""){
                p_valor<-0.05
            }else{
                print(sprintf("El valor de p= %s",p_valor))
                p_valor<-as.numeric(p_valor)
            }

                if(prueba$p.value>p_valor){
                    message("No se puede rechazar H0 = Hay presencia de una raiz unitaria \n")
                }else{
                    message("Se rechaza H0, se obta por H1 = La serie de tiempo es Estacionaria \n")
                }
        }else{
            stop("El objeto debe ser data frame")
        }

    }else{



        if(is.ts(datos)){
          elementos = tratamiento.ts_set(datos)
          frecuencia=ifelse(is.null(frecuencia),elementos$frecu,frecuencia)
          serie_tiempo_pruebas(elementos$data,frecuencia,init_ = TRUE)
          }else{
          stop("El objeto debe ser un data frame con dos elementos o una serie de tiempo")
        }
    }
}

#' Primer filtro de las funciones en TSRutina
#'
#' @param datos objeto a verificar
#'
#' @description Verifica si un data.frame tiene dos columnas y si la segunda de estas
#' es de caracter numerico, si es serie de tiempo no hace nada, retorna NULL.
#'
#' @return NULL or stop() event
#'
#' @examples
#'
#' conditional.tsrutina(datos)
#'
conditional.tsrutina <- function(datos){
  if(is.ts(datos)){
    return(NULL)
  }

  if(ncol(datos)!=2)
    stop("Los datos deben tener solo dos columnas, tiempo y valor en ese orden")
  if(!is.numeric(datos[,2]))
    stop("La segunda columna deben ser los valores, la primera el tiempo")
  message("\n Primera columna => variable de Tiempo
      \n Segunda columna => variable de valor \n")
}

#' From ts to data.frame
#'
#' @param datosts objeto ts a tranformar en data.frame class
#'
#' @description Convierte un objeto serie de tiempo a data.frame con dos columnas
#'   x => variable fecha
#'   y => variable valor
#'   La variable fecha inicia con el inicio de la serie y termina con el numero de
#'   frecuencias que se pueden hacer
#'
#' @return a data.frame object
#' @export
#'
#' @examples
#'
#' tratamiento.ts_set(sunspot.year)
#'
tratamiento.ts_set <- function(datosts){
  datos_conver=as.data.frame(datosts)
  year=start(datosts)[1L]
  mes=start(datosts)[2L]
  dia=start(datosts)[3L]
  dia=ifelse(test = is.na(dia),yes = 1,no = dia)
  fecha_inicio=as.Date(paste(dia,mes,year, sep = "-"),format = "%d-%m-%Y")
  frecuencia=frequency(datosts)
  avance=switch(as.character(frecuencia),
    "1" = 'year',
    "12" = 'month',
    "4" = '3 month',
    "54" = '7 days',
    "3" = '4 month'
  )


  fecha_secuencia=seq(fecha_inicio, by=avance, length=nrow(datos_conver))
  datos_conver=data.frame(x = fecha_secuencia,y = datos_conver$x)
  elementos=list()
  elementos$data=datos_conver
  elementos$frecu=frequency(datosts)
  elementos$inicio=start(datosts)
  elementos$fin=end(datosts)

  return(elementos)
}

#' Tratamiento de fechas TSR
#'
#' Tratamiento para que una fecha tenga el formato Date. De no tener un formato convertible se retorna NA
#' values
#'
#' @param fecha_vector Vector de valores con las fechas en formato caracter o Date
#'     Formatos soportador:  "\%d/\%m/\%Y", "\%Y-\%m-\%d" y "\%d-\%m-\%Y"
#'
#' @return Vector en formato fecha estandar de R
#' @export
#'
#' @examples
#' tiempo=seq(Sys.Date(),by="days",length=20)
#' tratamiento.fechas.TRS(tiempo)
#'
tratamiento.fechas.TRS <- function(fecha_vector){

  fecha_vector_tratamiento<-as.Date(fecha_vector,format("%d/%m/%Y"))  #Y debe ser mayuscula
  if(is.na(fecha_vector_tratamiento[1])){
    fecha_vector_tratamiento=as.Date(fecha_vector,format("%d-%m-%Y"))
  }
  if(is.na(fecha_vector_tratamiento[1])){
    fecha_vector_tratamiento=as.Date(fecha_vector,format("%Y-%m-%d"))
  }
  if(is.na(fecha_vector_tratamiento[1])){
    message("\n Si la variable tiempo es fecha, use el formato dia-mes-year: \n
            \"%Y-%m-%d\" ")
  }

  return(fecha_vector_tratamiento)
}

#' Checa los datos en rutina TSR
#'
#' Imprime un head de los datos y te pregunta si todo esta bien.
#'
#' Ideal para usarse en las rutinas de TSRutina que deben preguntar si se interpretaron
#' bien los datos.
#'
#' @param datos Data.Frame de 2 columnas, fecha y valores respectivamente. (se hace tratamiento de fechas con \code{\link{tratamiento.fechas.TRS}})
#' @param frecuencia Frecuencia de la serie de tiempo
#' @param inicio Inicio de la serie de tiempo
#'
#' @return Una lista \code{\link{list}} que contiene dos elementos, la base de datos tratada y un objeto TimeSeries
#' @export
#'
#' @examples
#' base=data.frame(tiempo=seq(Sys.Date(),by="days",length=20),valores=1:20*3+runif(1))
#' checar_datos(datos=base,frecuencia=4,inicio=2010)
#'
checar_datos <- function(datos,frecuencia,inicio,msg=TRUE) {
  names(datos)<-c("x","y")
  datos$x <- tratamiento.fechas.TRS(datos$x)
  print(head(datos))
  if(msg){
    message("\n ¿Estan bien los datos a usar? \n
        Si hay un error [Esc] \n De lo contrario [Enter] para continuar")
    continuar<-readline(": \t")

    if(!continuar=="")
      stop("Corrige el error")
  }

  #creando el objeto series
  datosts<-ts(data = datos$y,frequency =  frecuencia,start=inicio)

  return(list(datos=datos,datosts=datosts))
}

#' Pausa la consola al imprimir resultados
#'
#' Realiza una pausa tanto indeterminada como por una cantidad de segundos
#'
#' Cuando \bold{duracion} no es definido la pausa es indefinida y se espera la entrada de "stop" o Esc para terminar la pausa
#'
#' @param duracion Tiempo de pausa en segundos, default Inf (infinito)
#'
#' @return
#' @export
#'
#' @examples
#'
#' #para 10 segundos
#' pausa(10)
#'
pausa <-function(duracion = Inf){

        if (is.infinite(duracion)) {
            arg <- "*"
            while (arg != "") {
                cat("\n")
                arg <- readline("[Intro] to continue | [stop/Esc] to exit... ")
                if (arg == "stop") {
                    stop("El programa finalizo", call. = FALSE)
                }
            }
        } else {
            cat("Pause of", duracion, "seconds\n")
            Sys.sleep(duracion)
        }
        invisible(NULL)
}

#' @title Rutinas en una serie de tiempo
#'
#' @description Realiza una rutina para una base de datos en la que se
#'   incluyen ajustes por:
#'   \itemize{\item{Regresion lineal}{}}
#'   \itemize{\item{Promedio movil simple}{}}
#'   \itemize{\item{Promedio movil ponderado}{}}
#'   \itemize{\item{Suavizamiento exponencial simple}{}}
#'   \itemize{\item{Suav. expon. doble o de Holt}{}}
#'   \itemize{\item{Suavizamiento Holt-Winter}{}}
#'
#' @param datos Dataframe de no mas de 2 columnas, en el orden primero tiempo y
#'    luego valor el tiempo va en formato fecha, y tiene que ser en el orden
#'    dia-mes-year. La versión 2.1 soporta objetos de la clase TS (time series)
#' @param frecuencia Este es el periodo de la serie, trimestral = 3, cuatrimestral = 4
#'   ,mensual = 12, etc.
#' @param  inicio Este es el year a iniciar la serie de tiempo
#'
#' @return La salida no es como tal un objeto, si no una serie de impresiones de varios
#'   varios analisis.
#'   \itemize{\item{Plost}{  Arroja una lista de plots que ayudan a ver el comportamiento de la serie y como ciertos ajustes se aproximan mejor a ella}}
#'   \itemize{\item{Resumenes}{  Arroja ciertos resumenes de ciertos ajustes o pruebas que se hacen}}
#'   \itemize{\item{Modelo}{  Modelo con el menor MSE(Error cuadratico medio)}}
#'
#' @details Se usa una salida interactiva en la que el usuario debe agregar ciertos
#'   datos o tomar ciertas decisiones durante la rutina.
#'
#' @author Cristhian Diaz
#' @export
#'
#' @import ggplot2
#' @importFrom forecast ggseasonplot
#' @importFrom pracma movavg
#' @importFrom forecast ses hw holt forecast
#' @importFrom greybox MSE
#'
#' @examples
#' serie_tiempo_rutina(sunspot.year,5)
#'
#' base=data.frame(tiempo=seq(Sys.Date(),by="days",length=20),valores=1:20*3+runif(1))
#' serie_tiempo_rutina(datos=base,frecuencia=4,inicio=2010)
#'
serie_tiempo_rutina<-function(datos,frecuencia=NULL,inicio=NULL,init_=FALSE){

    if(!init_){
      paquetes.tsrutina()
      pausa()
      conditional.tsrutina(datos)
    }

    if(is.ts(datos)){
      elementos=tratamiento.ts_set(datos)
      datos=elementos$data
      frecuencia=ifelse(is.null(frecuencia),elementos$frecu,frecuencia)
      inicio=ifelse(is.null(inicio),elementos$inicio,inicio)

    }


    #verificar si los elementos se ven bien
    elementos=checar_datos(datos,frecuencia,inicio)
    datos=elementos$datos
    datosts=elementos$datosts


    #Graficos para ver si es estacional
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo Visualización")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))
    )
    pausa()
    print(
    autoplot(stl(datosts, s.window = "periodic"), ts.colour="blue",
             main="Ruido + Estacionalidad + Tendencia + SerieTemporal ")
    )
    pausa()
    if(frecuencia==1){
      message("La frecuencia de la serie de tiempo es 1, usaremos frecuencia 12 para los siguientes 2 graficos")
    }
    seasonplot(datosts,col=rainbow(length(datos$y)/frecuencia),year.labels=TRUE,xlab="Tiempo",
               ylab="Serie de tiempo",main = "Grafico Estacional de la Serie Temp.")
    pausa()

    boxplot(datosts~cycle(datosts),xlab = "Frecuencias",ylab = "Valores",main="Boxplot por cada valor de la frecuencia")

    pausa()

    #Modelo de regresion lineal
    datos$periodos<-1:length(datos$x)
    datos_rl<-lm(y~x, data=datos)
    print(summary(datos_rl))
    #Se puede ver cuales variables son significativas en el modelo
    pausa()

    #Grafico del modelo de regresion lineal
    plot(datos$periodos,datos$y,type = "l",
         xlab="Periodos",ylab="Valor de la serie",
         main="Regresión Lineal")
    lines(datos_rl$fitted.values, col="blue")
    pausa()

    #Promedio Movil simple

    promo<-movavg(datosts, n=frecuencia, type="s")

    plot(datos$periodos,datos$y,type = "l",
         xlab="Periodos",ylab="Valor de la serie",
         main="Promedio movil simple")
    lines(promo,col="blue")
    pausa()

    #Promedio Movil ponderado

    promopo<-movavg(datosts, n=frecuencia, type="w")

    plot(datos$periodos,datos$y,type = "l",
         xlab="Periodos",ylab="Valor de la serie",
         main="Promedio movil ponderado")
    lines(promopo,col="blue")
    pausa()

    #Exponential Smoothing

    pesoses<-ses(datos$y)

    summary(pesoses)
    plot(datos$periodos,datos$y,type = "l",
         xlab="Periodos",ylab="Valor de la serie",
         main="Suavizamiento Exponencial")
    lines(datos$periodos,pesoses$fitted, col="blue")
    pausa()


    #Holt's Exponential Smoothing

    pesoholt<- holt(datos$y)

    summary(pesoholt)
    plot(datos$periodos,datos$y,type = "l",
         xlab="Periodos",ylab="Valor de la serie",
         main="Suavizamiento Exponencial holt")
    lines(datos$periodos,pesoholt$fitted, col="blue")
    pausa()

    #Holt-Winters' Exponential Smoothing

    pesohw<- hw(datosts)

    summary(pesohw)
    #asignamos valores ajustados a una columna
    datos$Ajustadohw<-as.numeric(pesohw$fitted)

    plot(datos$periodos,datos$y,type = "l",
         xlab="Periodos",ylab="Valor de la serie",
         main="Suavizamiento Exponencial holt-winter")
    lines(datos$periodos, datos$Ajustadohw, col="red", type="l")
    pausa()
    #Ahora, para elegir el mejor modelo de "suavizamiento",
    #usaremos el MSE (error cuadratico medio).

    print("MSE del modelo de Regresion Lineal")
    print(MSE(datos$y, datos_rl$fitted.values))

    print('MSE del modelo de Promedios Moviles')

    print('Simple')
    print(MSE(datos$y, promo))

    print('Ponderado')
    print(MSE(datos$y, promopo))

    print('MSE del modelo de Exponential Smoothing')
    print(MSE(datos$y, pesoses$fitted))

    print('MSE del modelo de Holt')
    print(MSE(datos$y, pesoholt$fitted))

    print('MSE del modelo Holt-Winters')
    print(MSE(datos$y, datos$Ajustado))

    print('el error minimo se tiene con')
    a<-which.min(c(MSE(datos$y, datos_rl$fitted.values),
                MSE(datos$y, promo),
                MSE(datos$y, promopo),
                MSE(datos$y, pesoses$fitted),
                MSE(datos$y, pesoholt$fitted),
                MSE(datos$y, datos$Ajustado)))
    switch(a,
        '1' = {print('Regresion lineal')
            pausa()
            pronostico<-forecast(datos_rl$fitted.values,h=5,level=c(80,95))
            plot(pronostico)},
        '2' = {print('Promedio movil simple')
            pausa()
            pronostico<-forecast(promo,h=5,level=c(80,95))
            plot(pronostico)},
        '3' = {print('Promedio ponderado')
            pausa()
            pronostico<-forecast(promopo,h=5,level=c(80,95))
            plot(pronostico)},
        '4' = {print('Exponencial simple')
            pausa()
            pronostico<-forecast(pesoses,h=5,level=c(80,95))
            plot(pronostico)},
        '5' = {print('Suavizamiento de Holt')
            pausa()
            pronostico<-forecast(pesoholt,h=5,level=c(80,95))
            plot(pronostico)},
        '6' = {print('Suavizamiento de Holt-Winter')
            pausa()
            pronostico<-forecast(pesohw,h=5,level=c(80,95))
            plot(pronostico)}
    )
    pausa()

    print('Supuesto de Normalidad')
    prueba<-shapiro.test(pesohw$residuals)
    print(prueba)
    if(prueba$p.value>0.05){
        message("No se puede rechazar H0:Hay Normalidad")
    }else{
        message("Se rechaza H0, se obta por H1: No hay normalidad")
    }

}

#' Depurador de dispositivos graficos
#'
#' Depura el espacio de dispositivos graficos atravez de dev.off() y dev.new(). Checando dev.list().
#' @return NULL return
#' @export
#'
#' @examples
#' dev.TRS()
#' #Se limpia los datos, consulte dev.list()
dev.TRS <- function(){
  n=length(dev.list())

  if(n < 2){
    dev.new()
    dev.TRS()
  }

  if(n > 2){
    for (i in 1:(n-2)){
      dev.off(i+3)
    }
  }

}

#' Grafica de ajuste de TimeSeries en PNG
#'
#' Obten archivos .png de tus graficos de serie de tiempo en tu directorio
#'
#' Se usa una salida interactiva para el proceso
#'
#' @param datos Dataframe de no mas de 2 columnas, en el orden primero tiempo y
#'    luego valor el tiempo va en formato fecha, y tiene que ser en el orden
#'    dia-mes-year. La versión 2.1 soporta objetos de la clase TS (time series)
#' @param frecuencia  Este es el periodo de la serie, trimestral = 3, cuatrimestral = 4
#'    ,mensual = 12, etc.
#' @param inicio Este es el year a iniciar la serie de tiempo
#' @param init_ Boleano, True/False indica di se vericaran los datos
#'
#' @return  La salida no es como tal un objeto, si no una serie de impresiones de varios
#'    analisis.
#'    \itemize{\item{Plost}{  Arroja una lista de plots que ayudan a ver el comportamiento de la serie y como ciertos ajustes se aproximan mejor a ella}}
#'    \itemize{\item{PNG}{ Imagenes grabadas en el directorio de trabajo}}
#'
#' @author Cristhian Diaz
#'
#' @export
#'
#' @import ggplot2
#' @importFrom forecast ggseasonplot seasonplot
#' @importFrom pracma movavg
#' @importFrom forecast ses hw holt forecast
#' @importFrom greybox MSE
#'
#'
#' @examples
#' serie_tiempo_rutina(sunspot.year,5)
#'
#' base=data.frame(tiempo=seq(Sys.Date(),by="days",length=20),valores=1:20*3+runif(1))
#' serie_tiempo_rutina(datos=base,frecuencia=4,inicio=2010)
#'
serie_tiempo_plots<-function(datos,frecuencia=NULL,inicio=NULL,init_=FALSE){
    if(!init_){
      paquetes.tsrutina()
      pausa()
      conditional.tsrutina(datos)
    }

    if(is.ts(datos)){
      elementos=tratamiento.ts_set(datos)
      datos=elementos$data
      frecuencia=ifelse(is.null(frecuencia),elementos$frecu,frecuencia)
      inicio=ifelse(is.null(inicio),elementos$inicio,inicio)

    }

    #verificar si los elementos se ven bien
    elementos=checar_datos(datos,frecuencia,inicio)
    datos=elementos$datos
    datosts=elementos$datosts


    #tratamiento para errores en mostrar graficos
    dev.TRS()


    #Graficos para ver si es estacional

    png("serie_de_tiempo.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego
    print(
    ggplot(datos, aes(x, y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo Visualización")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))
    )
    dev.off(4)
    cat("Serie de tiempo serie_de_tiempo.png fue creada y guardada \n")

    message("¿Deseas ver en el grafico o seguir generando los siguientes graficos? \n")
    imprime = readline(" [Sí/Intro]:")

    if(imprime %in% c("si","Sí","SI","yes","YES","Si","Yes","s","1")){
      cat("Graficando...")
      dev.TRS() #Regulador de device graphics
      print(
      ggplot(datos, aes(x, y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo Visualización")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))
      )
      pausa()
    }



    png("st_descomposicion.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego
    print(
      autoplot(stl(datosts, s.window = "periodic"), ts.colour="blue",
               main = "Descomposición de Serie de tiempo")
      )
    cat("Descomposición de Serie de tiempo st_descomposicion.png fue creada y guardada \n")

    dev.off(4)
    message("¿Deseas ver en el grafico o seguir generando los siguientes graficos? \n")
    imprime = readline(" [Sí/Intro]:")

    if(imprime %in% c("si","Sí","SI","yes","YES","Si","Yes","s","1")){
      cat("Graficando...")
      print(
        autoplot(stl(datosts, s.window = "periodic"), ts.colour="blue",
                 main = "Descomposición de Serie de tiempo")
      )
      pausa()
    }


    png("st_seasonplot.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego
    print(seasonplot(datosts,col=rainbow(length(datos$y)/frecuencia),year.labels=TRUE,
               xlab="Periodos",ylab="Serie de tiempo",
               main = "Grafico Estacional de la serie de tiempo"))
    cat("Temporalidad de Serie de tiempo st_seasonplot.png fue creada y guardada \n")
    dev.off()
    message("¿Deseas ver en el grafico o seguir generando los siguientes graficos? \n")
    imprime = readline(" [Sí/Intro]:")

    if(imprime %in% c("si","Sí","SI","yes","YES","Si","Yes","s","1")){
      cat("Graficando...")
      print(
      seasonplot(datosts,col=rainbow(length(datos$y)/frecuencia),year.labels=TRUE,
                 xlab="Periodos",ylab="Serie de tiempo",
                 main = "Grafico Estacional de la serie de tiempo")
      )
      pausa()
      }
    #Se aprecia que no hay estacionalidad

    png("st_boxes_plot.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego
    print(boxplot(datosts~cycle(datosts),xlab = "Frecuencias",ylab = "Valores",main="Boxplot por cada valor de la frecuencia"))
    cat("Temporalidad de Serie de tiempo st_boxes_plot.png fue creada y guardada \n")
    dev.off()
    message("¿Deseas ver el grafico o seguir generando los siguientes graficos? \n")
    imprime = readline(" [Sí/Intro]:")

    if(imprime %in% c("si","Sí","SI","yes","YES","Si","Yes","s","1")){
      cat("Graficando...")
      print(
        boxplot(datosts~cycle(datosts),xlab = "Frecuencias",ylab = "Valores",main="Boxplot por cada valor de la frecuencia")
      )
      pausa()
    }

    #Modelo de regresion lineal
    datos$periodos<-1:length(datos$x)
    datos_rl<-lm(y~periodos, data=datos)

    #Grafico del modelo de regresion lineal
    png("st_regresion_lineal.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego

    datos$lineal<-datos_rl$fitted.values
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo + Regresión lineal")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,lineal), col="blue")
    )
    cat("Ajuste de regresión lineal st_regresion_lineal.png fue creada y guardada \n")
    dev.off()
    message("¿Deseas ver en el grafico o seguir generando los siguientes graficos? \n")
    imprime = readline(" [Sí/Intro]:")

    if(imprime %in% c("si","Sí","SI","yes","YES","Si","Yes","s","1")){
      cat("Graficando...")
      print(
      ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo + Regresión lineal")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,lineal), col="blue")
      )
      pausa()
    }

    #abline(pesorl,col="blue")


    #Promedio Movil simple
    promo<-movavg(datosts, n=frecuencia, type="s")
    png("st_prom_movil_simple.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego
    datos$promo<-promo
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo + Promedio movil simple")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,promo), col="blue")
    )
    cat("Promedio Movil simple st_prom_movil_simple.png fue creada y guardada \n")

    dev.off()

    message("¿Deseas ver en el grafico o seguir generando los siguientes graficos? \n")
    imprime = readline(" [Sí/Intro]:")

    if(imprime %in% c("si","Sí","SI","yes","YES","Si","Yes","s","1")){
      cat("Graficando...")
      print(
        ggplot(datos, aes(x,y)) +
          geom_point()+geom_line()+
          ggtitle("Serie de tiempo + Promedio movil simple")+
          theme(plot.title = element_text(color = "Black",hjust = 0.5))+
          geom_line(aes(x,promo), col="blue")
      )
      pausa()
    }





    #Promedio Movil ponderado

    promopo<-movavg(datosts, n=frecuencia, type="w")

    png("st_prom_movil_ponderado.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego

    datos$promopo<-promopo
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo + Promedio ponderado")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,promopo), col="blue")
    )
    cat("Promedio Movil ponderado st_prom_movil_ponderado.png fue creada y guardada \n")

    dev.off()

    message("¿Deseas ver en el grafico o seguir generando los siguientes graficos? \n")
    imprime = readline(" [Sí/Intro]:")

    if(imprime %in% c("si","Sí","SI","yes","YES","Si","Yes","s","1")){
      cat("Graficando...")
      print(
        ggplot(datos, aes(x,y)) +
          geom_point()+geom_line()+
          ggtitle("Serie de tiempo + Promedio ponderado")+
          theme(plot.title = element_text(color = "Black",hjust = 0.5))+
          geom_line(aes(x,promopo), col="blue")
      )
      pausa()
    }







    #Exponential Smoothing
    pesoses<-ses(datos$y)
    #summary(pesoses)
    png("st_ajuste_exponencial.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego

    datos$sess<-pesoses$fitted
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo + Ajuste exponencial")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,sess), col="blue")
    )
    cat("Suavizamiento Exponencial st_ajuste_exponencial.png fue creada y guardada \n")

    dev.off()

    message("¿Deseas ver en el grafico o seguir generando los siguientes graficos? \n")
    imprime = readline(" [Sí/Intro]:")

    if(imprime %in% c("si","Sí","SI","yes","YES","Si","Yes","s","1")){
      cat("Graficando...")
      print(
        ggplot(datos, aes(x,y)) +
          geom_point()+geom_line()+
          ggtitle("Serie de tiempo + Ajuste exponencial")+
          theme(plot.title = element_text(color = "Black",hjust = 0.5))+
          geom_line(aes(x,sess), col="blue")
      )
      pausa()
    }






    #Holt's Exponential Smoothing
    pesoholt<- holt(datos$y)
    #summary(pesoholt)
    png("st_holt_exponencial.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego

    datos$holtt<-pesoholt$fitted
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo + Ajuste de Holt")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,holtt), col="blue")
    )
    cat("Suavizamiento Holt st_holt_exponencial.png fue creada y guardada \n")
    dev.off()

    message("¿Deseas ver en el grafico o seguir generando los siguientes graficos? \n")
    imprime = readline(" [Sí/Intro]:")

    if(imprime %in% c("si","Sí","SI","yes","YES","Si","Yes","s","1")){
      cat("Graficando...")
      print(
        ggplot(datos, aes(x,y)) +
          geom_point()+geom_line()+
          ggtitle("Serie de tiempo + Ajuste de Holt")+
          theme(plot.title = element_text(color = "Black",hjust = 0.5))+
          geom_line(aes(x,holtt), col="blue")
      )
      pausa()
    }





    #Holt-Winters' Exponential Smoothing
    pesohw<- hw(datosts,seasonal = "additive",h=10,level = 95)
    #pesohw$model

    #asignamos valores ajustados a una columna
    png("st_holt-winter_exponencial.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego

    datos$Ajustado<-as.numeric(pesohw$fitted)
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo + Holt winter")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,Ajustado), col="blue")
    )
    cat("Suavizamiento Holt-Winters st_holt-winter_exponencial.png fue creada y guardada \n")

    dev.off()

    message("¿Deseas ver en el grafico o seguir generando los siguientes graficos? \n")
    imprime = readline(" [Sí/Intro]:")

    if(imprime %in% c("si","Sí","SI","yes","YES","Si","Yes","s","1")){
      cat("Graficando...")
      print(
        ggplot(datos, aes(x,y)) +
          geom_point()+geom_line()+
          ggtitle("Serie de tiempo + Holt Winter")+
          theme(plot.title = element_text(color = "Black",hjust = 0.5))+
          geom_line(aes(x,Ajustado), col="blue")
      )
      pausa()
    }



    a<-which.min(c(MSE(datos$y, datos_rl$fitted.values),
                  MSE(datos$y, promo),
                  MSE(datos$y, promopo),
                  MSE(datos$y, pesoses$fitted),
                  MSE(datos$y, pesoholt$fitted),
                  MSE(datos$y, datos$Ajustado)))
    print('El ajuste con menor MSE es: ')
    switch(a,
           '1' = {print('Regresion lineal')
               pausa()
               pronosticado<-datos_rl$fitted.values},
           '2' = {print('Promedio movil simple')
               pausa()
               pronosticado<-promo},
           '3' = {print('Promedio ponderado')
               pausa()
               pronosticado<-promopo},
           '4' = {print('Exponencial simple')
               pausa()
               pronosticado<-pesoses$model},
           '5' = {print('Ajuste de Holt')
               pausa()
               pronosticado<-pesoholt$model},
           '6' = {print('Holt-Winter')
               pausa()
               pronosticado<-pesohw$model}
    )
    pausa()


    #Promedio Movil ponderado
    pronostico<-forecast(pronosticado,h=20,level=c(80,95))

    png("st_pronostico_ponderado_80-90.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego
    print(
    autoplot(pronostico)
    )
    cat("Pronostico para el mejor ajuste st_pronostico_ponderado_80-90.png fue creada y guardada \n")

    dev.off()

    message("¿Deseas ver en el grafico o seguir generando los siguientes graficos? \n")
    imprime = readline(" [Sí/Intro]:")

    if(imprime %in% c("si","Sí","SI","yes","YES","Si","Yes","s","1")){
      cat("Graficando...")
      print(
        autoplot(pronostico)
      )
    }




}

#' Recomendaciones a un grafico ACF o PACF
#'
#' Obten un recomendación a tu ajuste de series de tiempo ACF o PACF
#'
#' El valor IC se calcula siguiendo la metodologia de la paqueteria Stats en a función acf(). Se retorna el valor positivo talque el intervalo se forma con (IC,-IC)
#'
#' @param objeto_cf objeto ACF o PACF
#' @param print_IC Indicador True/False para mostrar valos absoluto de intervalo de confianza ver details
#'
#' @return Valor del ultimo lag significativo de la funcion de autocorrelacion
#' @export
#'
#' @examples
#' base=data.frame(x=seq(Sys.Date(),by="days",length=200),y=1:20*3+runif(20))
#' recomendacion_autocorrelaciones(acf(base$y,plot = FALSE))
#'
recomendacion_autocorrelaciones <- function(objeto_cf,print_IC=FALSE) {
  llamada=match.call()
  ruta=match(c("objeto_cf"),names(llamada))


  if(ruta!=2){#chequeo de que se agregaron bien los parametros
    #stopifnot(ruta!=2)
    message("Objeto_cf no encontrado o hay más de un parametro en la función")
    stop()
  }

  #Zona de plot not True
  a=llamada[[2]]
  a=as.character(a)
  if(length(a)!=3 | as.logical(a[3])){
    message("EL objeto debe ser un ACF o PACF con parametro plot = FALSE")
    message("Ver el ejemplo en la documentación")
    ?recomendacion_autocorrelaciones
  }
  #si la salida es un vector de 3 elementos entonces hay dos parametros
  #serie=tryCatch(get(objeto_cf$series),error= function(e){message(e," \nSe busca otra entrada..."); return(NULL)})
  serie=get("base",envir = parent.frame())
  #se corrigio el uso de envir

  #en caso de serie NULL
  # if(is.null(serie)){
  #   vec=strsplit(a[2],split = "$",fixed = TRUE)
  #   message("\nSe encontro la base de datos llamada: ",vec[[1]][1])
  #   message("\nDentro de ella se encontro el vector llamado: ",vec[[1]][2])
  #   serie=eval(str2lang(a[2]))
  #   cat("\nLos primeros 6 valores de este vector son:\n")
  #   print(head(serie))
  # }
  serie=ts(serie$y)

  order_=NULL
  if(objeto_cf$type=="partial"){
    matriz=matriz_eacf(serie,ar.max = 1, ma.max = 15,print_matrix = FALSE)
    matriz=matriz$symbol=="o"
    for(i in 1:15){
      if(matriz[1,i]==1){
        order_=i
        cat("\nProponemos MA(q) con q=:",order_)
        break
      }
    }
    #parche para casos donde no hay valor a proponer
    if(is.null(order_)){
      cat("El valor de la q propuesta es mayor a 15...")
      order_=16
    }
  }
  if(objeto_cf$type=="correlation"){
    matriz=TSRutina::matriz_eacf(serie,ar.max = 15,ma.max = 1,print_matrix = FALSE)
    matriz=matriz$symbol=="o"
    for(i in 1:15){
      if(matriz[i,1]==1){
        order_=i
        cat("\nProponemos AR(p) con p=:",order_)
        break
      }
    }
    if(is.null(order_)){
      cat("El valor de la p propuesta es mayor a 15...")
      order_=16
    }
  }



  #obtener los intervalos de confianza dando el objeto
  IC=intervalo_confianza_acf(objeto_cf)
  #mayores=abs(objeto_cf$acf)>IC
  #cat("\nLos siguientes elementos son propuestas de r: ")
  #posibles_lags=objeto_cf$lag[mayores]
  #cat(posibles_lags)
  #cat("\nProponemos que r sea:",posibles_lags[length(posibles_lags)])

  if(print_IC){
    cat("\nEl IC de modelo es: ",IC)
  }

  return(invisible(order_))
}

#' Recomendación de modelo ARMA
#'
#' Usando una matriz eacf de TSA paqueteria se propone un posible vector con los valores de p y q de un modelos ARMA(p,q)
#'
#' Se utiliza un metodo de busqueda de esquinas para proponer el valor de
#' p,q para ARMA(p,q).
#'
#' @param time_series Objeto Serie de tiempo
#' @param print_matrix Indicador, imprimir o no matriz de eacf
#'
#' @return Vector de longitud 2, primera entrada valor de \bold{p}, segunda valor de \bold{q}
#' @export
#'
#' @examples
#' recomendaciones_arma(AirPassengers)
recomendaciones_arma <- function(time_series,print_matrix=TRUE) {
  x=time_series

  modelo_arma <- matriz_eacf(x,7,7,print_matrix)

  matriz_true_false <- modelo_arma$symbol=="o"
  matriz_true_false=matriz_true_false[-1,][,-1]

  for (i in 1:7) {
    if(sum(matriz_true_false[,i])>0){
      for (j in 1:7) {
        zz=matriz_true_false[,i]
        if(zz[j]==1){
          #se analizan vecinos
          izquierda=matriz_true_false[j,i+1]
          abajo=matriz_true_false[j+1,i]
          diagonal=matriz_true_false[j+1,i+1]
          #condicion algun vecino o diagonal no null
          if(izquierda+abajo+diagonal >2 | diagonal>0){
            vec=c(i,j)
            return(vec)
          }
        }
      }
    }
  }
}

#' Ajuste de Modelo ARIMA a tu Serie de tiempo
#'
#' Ajusta un modelo arima usando un asistente que te mostrar si tu serie se ajusta a un modelo ARIMA en particular
#'
#' A diferencia de otras funciones esta utiliza el conocimiento del experto para ajustar el mejor modelo ARIMA a la serie de tiempo estudiada, mientras propone modelos que el asistente considera utiles.
#'
#' @param datos Dataframe de no mas de 2 columnas, en el orden primero tiempo y
#'    luego valor el tiempo va en formato fecha, y tiene que ser en el orden
#'    dia-mes-year. La versión 2.1 soporta objetos de la clase TS (time series)
#' @param frecuencia  Este es el periodo de la serie, trimestral = 3, cuatrimestral = 4
#'    ,mensual = 12, etc.
#' @param inicio Este es el year a iniciar la serie de tiempo
#' @param init_ Boleano, True/False indica di se vericaran los datos
#'
#' @return La salida no es como tal un objeto, si no una serie de impresiones de varios
#'    analisis. El mejor basando en criterio AIC.
#' @export
#'
#' @examples
#' serie_tiempo_ARIMA(sunspot.year,5)
#'
#' base=data.frame(tiempo=seq(Sys.Date(),by="days",length=20),valores=1:20*3+runif(20))
#' serie_tiempo_ARIMA(datos=base,frecuencia=4,inicio=2010)
#'
serie_tiempo_ARIMA<-function(datos,frecuencia=NULL,inicio=NULL,init_=FALSE,msg=TRUE){
  if(!init_){
    paquetes.tsrutina()
    pausa()
    conditional.tsrutina(datos)
  }

  if(is.ts(datos)){
      elementos=tratamiento.ts_set(datos)
      datos=elementos$data
      frecuencia=ifelse(is.null(frecuencia),elementos$frecu,frecuencia)
      inicio=ifelse(is.null(inicio),elementos$inicio,inicio)
  }

  if(!is.data.frame(datos)){
    stop("El objeto debe ser un data frame con dos elementos")
  }


  #verificar si los elementos se ven bien
  elementos=checar_datos(datos,frecuencia,inicio)
  datos=elementos$datos
  #datosts=elementos$datosts #No se usa en la rutina


  base=datos
  ban=TRUE
  numero_diferenciaciones=0
  while(ban){

    prueba<-adf.test(base$y)
    print(prueba)
    if(msg){
      p_valor<-readline('\nInserte un p valor (intro para p=0.05): ')
    }else{
      p_valor<-0.05
    }

    if(p_valor==""){
      p_valor<-0.05
    }else{
      print(sprintf("\nEl valor de p= %s \n",p_valor))
      p_valor<-as.numeric(p_valor)
    }

    if(prueba$p.value>p_valor){
      cat("No se puede rechazar H0:Hay presencia de una raiz unitaria\n")
      cat("No es estacionaria")
      pausa()
      differenciado<-diff(base$y,lag = 1,differences = 1)
      base=base[-nrow(base),]
      base$y=differenciado
      numero_diferenciaciones=numero_diferenciaciones+1 #contador de diferenciaciones

      message('\nSe ha diferencio la base de datos para obtener estacionalidad\n')
    }else{
      cat("Se rechaza H0, se obta por H1: La serie de tiempo es Estacionaria\n")
      ban=FALSE
    }
    pausa()
  }
  #plotea el acf y analizas
  print(acf(base$y,main="Autocorrelación, Analiza el valor de r en MA(r)"))

  #función de recomendación
  rec=recomendacion_autocorrelaciones(acf(base$y,plot = FALSE))

  if(msg){
    ma<-readline('Que MA(r) sospechas?, inserte el valor de r: ')
  }else{
    ma<-""
  }

  ma<-if(ma==""){
    c(0,numero_diferenciaciones,as.numeric(rec))
  }else{
    c(0,numero_diferenciaciones,as.numeric(ma))
  }
  pausa()
  #plotea el pacf
  print(pacf(base$y,main="Autocorrelación Parcial,Analiza el valor de p en AR(p)"))
  rec=recomendacion_autocorrelaciones(pacf(base$y,plot = FALSE))

  if(msg){
    ra<-readline('Que AR(p) sospechas?, inserte el valor de p: ')
  }else{
    ra<-""
  }
  ra<-if(ra==""){
    c(as.numeric(rec),numero_diferenciaciones,0)
  }else{
    c(as.numeric(ra),numero_diferenciaciones,0)
  }
  pausa()
  #imprime arimas
  print(arima(base$y,order = ma))
  pausa()
  print(arima(base$y,order = ra))
  pausa()
  #compara arimas
  mama<-arima(base$y,order = ma)
  rara<-arima(base$y,order = ra)
  if(mama$aic<rara$aic){
    print(sprintf('El modelo con menor AIC es el MA(%s)',ma[3]))
  }else{
    print(sprintf('El modelo con menor AIC es el RA(%s)',ra[3]))
  }
  pausa()

  cat("Prueba de Box-Pierce and Ljung-Box Test")
  for (i in 1:2) {
    modelo=list(mama,rara)
    if(mama[["call"]][["order"]]=="ma"){
      message("\nAnalisis de correlación en el modelo para Ma(r)")
    }else{
      message("\nAnalisis de correlación en el modelo para RA(p)")
    }

    box_test=Box.test(modelo[[i]]$residuals, type ="Ljung-Box")
    print(box_test)
    cat("Box.test(), el p_valor > 0.05 entonces no hay correlacion ruido blanco")

    if(msg){
      p_valor<-readline('Inserte un p valor, (intro para p=0.05):  \n')
    }else{
      p_valor<-0.05
    }

    if(p_valor==""){
      p_valor<-0.05
    }else{
      print(sprintf("El valor de p= %s",p_valor))
      p_valor<-as.numeric(p_valor)
    }


    if(prueba$p.value>0.05){
      cat("No se puede rechazar H0 = No hay presencia de autocorrelacion ")
    }else{
      cat("Se rechaza H0, se obta por H1 = Hay presencia de autocorrelacion")
    }


    sprintf("\n \n")
  }

  #analisis ARMA si diferecias es mayor a cero, ARMA=ARIMA
  datosts=ts(base$y)

  rec=recomendaciones_arma(datosts) #checar como el objeto obtiene los symbol de la lista
  cat("\nSe recomienda el modelo ARMA(p,q) con p=",rec[1]," q=",rec[2])
  if(msg){
    cat("\nQue ARMA(p,q) sospechas?, inserte el valor de p,q separado por comas: ")
    arma_pq<-readline('Ejemplo: 3,4 \t')
  }else{
    arma_pq<-""
  }

  arma_order<-if(arma_pq==""){
    c(as.numeric(rec[1]),numero_diferenciaciones,as.numeric(rec[2]))
  }else{
    #dividir la entrada
    w=strsplit(arma_pq,",")
    arma_p=as.numeric(w[[1]][1])
    arma_q=as.numeric(w[[1]][2])
    c(arma_p,numero_diferenciaciones,arma_q)
  }
  #se imprime el modelo arma
  print(arima(base$y,order = arma_order))
  pausa()

  return(NULL)
}

#' Rutina Principal de TSRutina
#'
#' Realiza todas las rutinas de la paqueteria TSRutina, ver detalles para más información
#'
#' Se hace un ajuste por metodo no ARIMA y se determina cual de estos modelos es el de menor
#' Error en los residuos. Ver \code{\link{serie_tiempo_rutina}}
#'
#' Se realiza las rutinas que verifican si la serie es o no estacionaria, si existe correlación
#' , luego se ajusta un modelo ARIMA y se guardan los graficos importantes en el directorio
#'
#' @param datos Datos para el analisis de serie de tiempo suport (data.frame and ts class)
#' @param frecuencia Frecuencia de la serie de tiempo, sirve para
#'   reescribir la frecuencia cuando datos es un objeto ts
#' @param inicio Inicio de la serie de tiempo, igual que frecuencia
#'   sobreescribe valores de objetos ts
#' @param init_ (True or False) validar el parametro datos
#' @param ... Not work
#'
#' @return La salida no es como tal un objeto, si no una serie de impresiones de varios
#'   analisis. La siguiente lista detalla alguno de ellos:
#'   \itemize{\item{Plost}{  Arroja una lista de plots que ayudan a ver el comportamiento de la serie y como ciertos ajustes se aproximan mejor a ella}}
#'   \itemize{\item{Resumenes}{  Arroja ciertos resumenes de ciertos ajustes o pruebas que se hacen}}
#'   \itemize{\item{Modelo}{  Modelo con el menor MSE(Error cuadratico medio)}}
#'
#' @export
#'
#' @examples
#'  base=data.frame(tiempo=seq(Sys.Date(),by="days",length=20),valores=1:20*3+runif(1))
#'  init(datos=base,frecuencia=4,inicio=2010)
init <- function(datos,frecuencia=NULL,inicio=NULL,init_=TRUE,msg=TRUE,...){
  paquetes.tsrutina()
  pausa()
  conditional.tsrutina(datos)

  if(is.ts(datos)){
    elementos=tratamiento.ts_set(datos)
    datos=elementos$data
    frecuencia=ifelse(is.null(frecuencia),elementos$frecu,frecuencia)
    inicio=ifelse(is.null(inicio),elementos$inicio,inicio)
  }

  message("\n Inicio de rutina para tratamiento de una Serie de tiempo \n")
  serie_tiempo_rutina(datos = datos,frecuencia = frecuencia,inicio = inicio,init_ = init_,...)
  message("\n Inicio de pruebas para tratamiento de una Serie de tiempo \n")
  serie_tiempo_pruebas(datos = datos,frecuencia = frecuencia,init_ = init_,msg)
  message("\n Ajuste de un modelo ARIMA para tratamiento de una Serie de tiempo \n")
  serie_tiempo_ARIMA(datos = datos,frecuencia = frecuencia,inicio = inicio,init_ = init_,msg)
  message("\n Varios suavizamientos de una Serie de tiempo creación en workdir \n")
  serie_tiempo_plots(datos = datos,frecuencia = frecuencia,inicio = inicio,init_ = init_)

}


#' Realiza analisis de manera directa
#'
#' Esta función considera que las recomendaciones de la paqueteria seran tomandas como los valores a usar
#'
#' Esta función comparte los mismos detalles que \code{\link{init}}
#'
#' @param datos Datos para el analisis de serie de tiempo suport (data.frame and ts class)
#' @param frecuencia Frecuencia de la serie de tiempo, sirve para
#'   reescribir la frecuencia cuando datos es un objeto ts
#' @param inicio Inicio de la serie de tiempo, igual que frecuencia
#'   sobreescribe valores de objetos ts
#' @param init_ (True or False) validar el parametro datos
#' @param ... Not work
#'
#' @return La salida no es como tal un objeto, si no una serie de impresiones de varios
#'   analisis. La siguiente lista detalla alguno de ellos:
#'   \itemize{\item{Plost}{  Arroja una lista de plots que ayudan a ver el comportamiento de la serie y como ciertos ajustes se aproximan mejor a ella}}
#'   \itemize{\item{Resumenes}{  Arroja ciertos resumenes de ciertos ajustes o pruebas que se hacen}}
#'   \itemize{\item{Modelo}{  Modelo con el menor MSE(Error cuadratico medio)}}
#'
#' @export
#'
#' @examples
#'  base=data.frame(tiempo=seq(Sys.Date(),by="days",length=20),valores=1:20*3+runif(1))
#'  Ajuste_rapido(datos=base,frecuencia=4,inicio=2010)
#'
Ajuste_rapido <- function(datos,frecuencia=NULL,inicio=NULL,init_=TRUE,msg=FALSE,...){
 #Función para hacer el proceso de forma directa con las sugerencias como respuestas
  #Probablemente se deba agregar en el futuro la opción de reporte
  init(datos,frecuencia,inicio,init_,msg)
}

Ajuste_ARIMA_rapido <- function(datos,frecuencia=NULL,inicio=NULL,init_=TRUE,msg=FALSE,...){
  paquetes.tsrutina()
  pausa()
  conditional.tsrutina(datos)

  if(is.ts(datos)){
    elementos=tratamiento.ts_set(datos)
    datos=elementos$data
    frecuencia=ifelse(is.null(frecuencia),elementos$frecu,frecuencia)
    inicio=ifelse(is.null(inicio),elementos$inicio,inicio)
  }

  #solo se hace ajuste arima y prueba de estacionalidad

  serie_tiempo_pruebas(datos,frecuencia,init_,msg)
  serie_tiempo_ARIMA(datos,frecuencia,inicio,init_,msg)
}
