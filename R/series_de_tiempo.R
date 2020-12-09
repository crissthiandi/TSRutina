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

serie_tiempo_pruebas <-function(datos,frecuencia,init_=FALSE){
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
            cat("\n (Prueba aplicada a una regresión lineal) \n")
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
                message("No se puede rechazar H0 = No hay presencia de autocorrelacion \n")
            }else{
                message("Se rechaza H0, se obta por H1 = Hay presencia de autocorrelacion \n")
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
          serie_tiempo_pruebas(elementos$data,elementos$frecu,init_ = TRUE)
          }else{
          stop("El objeto debe ser un data frame con dos elementos o una serie de tiempo")
        }
    }
}

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

tratamiento.ts_set <- function(datosts){
  datos_conver=as.data.frame(datosts)
  datos_conver=data.frame(x = row(datos_conver),y = datos_conver$x)
  elementos=list()
  elementos$data=datos_conver
  elementos$frecu=frequency(datosts)
  elementos$inicio=start(datosts)
  elementos$fin=end(datosts)

  return(elementos)
}

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

pausa <-function(duracion = Inf){

        if (is.infinite(duracion)) {
            arg <- "*"
            while (arg != "") {
                cat("\n")
                arg <- readline("  [Intro to continue / 'stop' to exit]: ")
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

serie_tiempo_rutina<-function(datos,frecuencia,inicio,init_=FALSE){

    if(!init_){
      paquetes.tsrutina()
      pausa()
      conditional.tsrutina(datos)
    }

    names(datos)<-c("x","y")
    datos$x <- tratamiento.fechas.TRS(datos$x)
    print(head(datos))
    message("\n ¿Estan bien los datos a usar? \n
    Si hay un error [Esc] \n De lo contrario [Enter para continuar]")
    continuar<-readline(": \t")

    if(!continuar=="")
        stop("Corrige el error")

    #creando el objeto series
    datosts<-ts(data = datos$y,frequency =  frecuencia,start=inicio)


    #print(adf.test(datosts))
    #pausa()


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

serie_tiempo_plots<-function(datos,frecuencia,inicio,init_=FALSE){
    if(!init_){
      paquetes.tsrutina()
      pausa()
      conditional.tsrutina(datos)
    }

    names(datos)<-c("x","y")
    datos$x <- tratamiento.fechas.TRS(datos$x)
    print(head(datos))
    message("\n ¿Estan bien los datos a usar? \n
    Si hay un error [Esc] \n De lo contrario [Enter para continuar]")
    continuar<-readline(": \t")

    if(!continuar=="")
        stop("Corrige el error")

    #tratamiento para errores en mostrar graficos
    dev.TRS()

    #creando el objeto series
    datosts<-ts(data = datos$y,frequency =  frecuencia,start=inicio)

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

serie_tiempo_ARIMA<-function(datos,frecuencia=4,inicio=2010,init_=FALSE){
  if(!init_){
    paquetes.tsrutina()
    pausa()
    conditional.tsrutina(datos)
  }

  if(!is.ts(datos)){

    if(is.data.frame(datos)){


  names(datos)<-c("x","y")
  datos$x <- tratamiento.fechas.TRS(datos$x)
  if(is.na(datos$x)){
    datos$x=tratamiento.fechas.TSR(datos$x)
  }
  print(head(datos))
  message("\n ¿Estan bien los datos a usar? \n
    Si hay un error [Esc] \n De lo contrario [Enter para continuar]")
  continuar<-readline(": \t")

  if(!continuar=="")
    stop("Corrige el error")

  #creando el objeto series
  datosts <- ts(data = datos$y,frequency =  frecuencia, start=inicio)

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
    stop("El objeto debe ser un data frame con dos elementos")
  }

}


init <- function(datos,frecuencia,inicio,init_=TRUE,...){
  paquetes.tsrutina()
  pausa()
  conditional.tsrutina(datos)
  if(is.ts(datos)){
    elemento = tratamiento.ts_set(datos)
    datos = elemento$data
    frecuencia = elemento$frecu
    inicio = elemento$inicio
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
