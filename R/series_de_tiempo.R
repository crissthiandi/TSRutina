
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
serie_tiempo_pruebas <-function(datos,frecuencia=NULL,init_=FALSE){
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
            p_valor<-readline('Inserte un p valor, (intro para p=0.05):  \n')

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
            p_valor<-readline('Inserte un p valor, (intro para p=0.05) \n')

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
checar_datos <- function(datos,frecuencia,inicio) {
  names(datos)<-c("x","y")
  datos$x <- tratamiento.fechas.TRS(datos$x)
  print(head(datos))
  message("\n ¿Estan bien los datos a usar? \n
      Si hay un error [Esc] \n De lo contrario [Enter] para continuar")
  continuar<-readline(": \t")

  if(!continuar=="")
    stop("Corrige el error")

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

    seasonplot(datosts,col=rainbow(length(datos$y)/frecuencia),year.labels=TRUE,xlab="Mes",
               ylab="Serie de tiempo",main = "Grafico Estacional de la Serie Temp.")

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
    lines(promo,col="orange")
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
    lines(datos$periodos,pesoses$fitted, col="red")
    pausa()


    #Holt's Exponential Smoothing

    pesoholt<- holt(datos$y)

    summary(pesoholt)
    plot(datos$periodos,datos$y,type = "l",
         xlab="Periodos",ylab="Valor de la serie",
         main="Suavizamiento Exponencial holt")
    lines(datos$periodos,pesoholt$fitted, col="green")
    pausa()

    #Holt-Winters' Exponential Smoothing

    pesohw<- hw(datosts,seasonal = "additive",h=10,level = 95)

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
            pronostico<-forecast(datos_rl,h=5,level=c(80,95))
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

serie_tiempo_ARIMA<-function(datos,frecuencia=NULL,inicio=NULL,init_=FALSE){
  if(!init_){
    paquetes.tsrutina()
    pausa()
    conditional.tsrutina(datos)
  }

  if(!is.ts(datos)){

    if(is.data.frame(datos)){


  #verificar si los elementos se ven bien
  elementos=checar_datos(datos,frecuencia,inicio)
  datos=elementos$datos
  #datosts=elementos$datosts #No se usa en la rutina


  base=datos
  ban=TRUE
  while(ban){

    prueba<-adf.test(base$y)
    print(prueba)
    p_valor<-readline('Inserte un p valor (intro para p=0.05)')

    if(p_valor==""){
      p_valor<-0.05
    }else{
      print(sprintf("El valor de p= %s",p_valor))
      p_valor<-as.numeric(p_valor)
    }

    if(prueba$p.value>p_valor){
      print("No se puede rechazar H0:Hay presencia de una raiz unitaria")
      print("No es estacionaria")
      pausa()
      differenciado<-diff(base$y,lag = 1,differences = 1)
      base=base[-nrow(base),]
      base$y=differenciado

      message('Se diferencio la base de datos')
    }else{
      print("Se rechaza H0, se obta por H1: La serie de tiempo es Estacionaria")
      ban=FALSE
    }
    pausa()
  }
  #plotea el acf y analizas
  print(acf(base$y,main="Autocorrelación"))
  ma<-readline('Que MA(r) sospechas?, inserte el valor de r:     ')
  ma<-c(0,0,as.numeric(ma))
  pausa()
  #plotea el pacf
  print(pacf(base$y,main="Autocorrelación Parcial"))
  ra<-readline('Que AR(p) sospechas?, inserte el valor de p:     ')
  ra<-c(0,0,as.numeric(ra))
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
    box_test=Box.test(modelo[[i]]$residuals, type ="Ljung-Box")
    print(box_test)
    cat("Box.test(), el p_valor > 0.05 entonces no hay correlacion ruido blanco")
    p_valor<-readline('Inserte un p valor, (intro para p=0.05):  \n')

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


    }else{
      stop("El objeto debe ser data frame")
    }

  }else{

    if(is.ts(datos)){
      elementos=tratamiento.ts_set(datos)
      datos=elementos$data
      frecuencia=ifelse(is.null(frecuencia),elementos$frecu,frecuencia)
      inicio=ifelse(is.null(inicio),elementos$inicio,inicio)
      serie_tiempo_ARIMA(datos,frecuencia,inicio,init_ = TRUE)
    }
    stop("El objeto debe ser un data frame con dos elementos")
  }

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
init <- function(datos,frecuencia=NULL,inicio=NULL,init_=TRUE,...){
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
  serie_tiempo_pruebas(datos = datos,frecuencia = frecuencia,init_ = init_)
  message("\n Ajuste de un modelo ARIMA para tratamiento de una Serie de tiempo \n")
  serie_tiempo_ARIMA(datos = datos,frecuencia = frecuencia,inicio = inicio,init_ = init_)
  message("\n Varios suavizamientos de una Serie de tiempo creación en workdir \n")
  serie_tiempo_plots(datos = datos,frecuencia = frecuencia,inicio = inicio,init_ = init_)

}
