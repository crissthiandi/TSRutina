paquetes.tsrutina <- function(){
  require('tseries',warn.conflicts = F,quietly = F)
  library('lmtest',warn.conflicts = F,quietly = F)
  library('pracma',warn.conflicts = F,quietly = F)
  library('ggfortify',warn.conflicts = F,quietly = F)
  library('forecast',warn.conflicts = F,quietly = F,)
  library('tseries',warn.conflicts = F,quietly = F)
  library('greybox',warn.conflicts = F,quietly = F)
}

serie_tiempo_pruebas <-function(datos,frecuencia){
    paquetes.tsrutina()

    message("Se han cargado los paquetes necesarios")
    pausa()

    conditional.tsrutina(datos)

    if(!is.ts(datos) | ncol(datos) != 2){

        if(is.data.frame(datos)){
            base<-datos
            names(base)<-c("x","y")

            base2<-base
            base2$xx<-1:length(base$x)

            regresion<-lm(y~xx,base2)
            prueba<-dwtest(regresion)
            print(prueba)
            p_valor<-readline('Inserte un p valor, (intro para p=0.05) \n')

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

            #Probar Estacionariedad para proceder a los modelos no probabilisticos
            #Prueba Dickey-Fuller:
            #H0: Hay presencia de una raiz unitaria en la serie de tiempo
            #H1: La serie de tiempo es Estacionaria.
            #No se rechaza H0 pues pvalor=0.467 es mayor que 0.05 (No se rechaza H0)
            basets<-ts(base$y,frequency=frecuencia)

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
                    cat("No se puede rechazar H0:Hay presencia de una raiz unitaria")
                }else{
                    cat("Se rechaza H0, se obta por H1: La serie de tiempo es Estacionaria")
                }

            cat("Prueba de Box-Pierce and Ljung-Box Test")
            cat("Box.test(), el pvalor>0.05 entonces no hay correlacion ruido blanco")
        }else{
            stop("El objeto debe ser data frame")
        }

    }else{
        stop("El objeto debe ser un data frame con dos elementos")
    }

}

conditional.tsrutina <- function(datos){
  cat("si el tiempo es fecha, use el orden dia-mes-year")
  cat("se han cargado las librerias")
  pausa()

  if(ncol(datos)!=2)
    stop("Los datos deben tener solo dos columnas, tiempo y valor en ese orden")
  if(!is.numeric(datos[,2]))
    stop("La segunda columna deben ser los valores, la primera el tiempo")
}

pausa <-function(duracion = Inf){

        if (is.infinite(duracion)) {
            arg <- "*"
            while (arg != "") {
                arg <- readline("Pause. [ to continue / 'stop' to exit] ")
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

serie_tiempo_rutina<-function(datos,frecuencia,inicio){
    paquetes.tsrutina()
    conditional.tsrutina(datos)

    names(datos)<-c("x","y")
    datos$x<-as.Date(datos$x,format("%d/%m/%Y"))  #Y debe ser mayuscula
    print(head(datos))
    continuar<-readline(" Estan bien los datos a usar? si hay un error, corrige,
             si no los hay, enter para continuar: ")

    if(!continuar=="")
        stop("Corrige el error")

    #creando el objeto series
    datosts<-ts(data = datos$y,frequency =  frecuencia,start=inicio)


    print(adf.test(datosts))
    pausa()


    #Graficos para ver si es estacional
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))
    )
    pausa()
    print(
    autoplot(stl(datosts, s.window = "periodic"), ts.colour="blue")
    )
    pausa()

    seasonplot(datosts,col=rainbow(length(datos$y)/frecuencia),year.labels=TRUE,xlab="Mes",
               ylab="Serie de tiempo",main = "Grafico Estacional de la S.T.")

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
         main="Regresion Lineal")
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
         main="Ajuste Exponencial")
    lines(datos$periodos,pesoses$fitted, col="red")
    pausa()


    #Holt's Exponential Smoothing

    pesoholt<- holt(datos$y)

    summary(pesoholt)
    plot(datos$periodos,datos$y,type = "l",
         xlab="Periodos",ylab="Valor de la serie",
         main="Ajuste Exponencial holt")
    lines(datos$periodos,pesoholt$fitted, col="green")
    pausa()

    #Holt-Winters' Exponential Smoothing

    pesohw<- hw(datosts,seasonal = "additive",h=10,level = 95)

    summary(pesohw)
    #asignamos valores ajustados a una columna
    datos$Ajustadohw<-as.numeric(pesohw$fitted)

    plot(datos$periodos,datos$y,type = "l",
         xlab="Periodos",ylab="Valor de la serie",
         main="Ajuste Exponencial holt-winter")
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
        '5' = {print('Ajuste de Holt')
            pausa()
            pronostico<-forecast(pesoholt,h=5,level=c(80,95))
            plot(pronostico)},
        '6' = {print('Holt-Winter')
            pausa()
            pronostico<-forecast(datos_rl,h=5,level=c(80,95))
            plot(pronostico)}
    )
    pausa()

    print('Supuesto de Normalidad')
    prueba<-shapiro.test(pesohw$residuals)
    print(prueba)
    if(prueba$p.value>0.05){
        print("No se puede rechazar H0:Hay Normalidad")
    }else{
        print("Se rechaza H0, se obta por H1: No hay normalidad")
    }



}

serie_tiempo_plots<-function(datos,frecuencia,inicio){
    paquetes.tsrutina()

    conditional.tsrutina(datos)

    names(datos)<-c("x","y")
    datos$x<-as.Date(datos$x,format("%d/%m/%Y"))  #Y debe ser mayuscula
    print(head(datos))
    continuar<-readline(" Estan bien los datos a usar? si hay un error,
    corrige, si no los hay, enter para continuar: ")

    if(!continuar=="")
        stop("Corrige el error")

    #creando el objeto series
    datosts<-ts(data = datos$y,frequency =  frecuencia,start=inicio)

    #Graficos para ver si es estacional
    png("serie_de_tiempo.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego


    print(
    ggplot(datos, aes(x, y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))
    )

    dev.off()
    pausa()

    png("st_descomposicion.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego
    print(autoplot(stl(datosts, s.window = "periodic"), ts.colour="blue"))
    dev.off()
    pausa()

    png("st_seasonplot.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego
    print(seasonplot(datosts,col=rainbow(length(datos$y)/frecuencia),year.labels=TRUE,
               xlab="Periodos",ylab="Serie de tiempo",
               main = "Grafico Estacional de la serie de tiempo"))
    dev.off()
    pausa()
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
        ggtitle("Serie de tiempo")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,lineal), col="blue")
    )
    dev.off()
    pausa()
    #abline(pesorl,col="blue")


    #Promedio Movil simple
    promo<-movavg(datosts, n=frecuencia, type="s")
    png("st_prom_movil_simple.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego
    datos$promo<-promo
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,promo), col="blue")
    )
    dev.off()
    pausa()





    #Promedio Movil ponderado

    promopo<-movavg(datosts, n=frecuencia, type="w")

    png("st_prom_movil_ponderado.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego

    datos$promopo<-promopo
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,promopo), col="blue")
    )
    dev.off()
    pausa()








    #Exponential Smoothing
    pesoses<-ses(datos$y)
    #summary(pesoses)
    png("st_ajuste_exponencial.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego

    datos$sess<-pesoses$fitted
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,sess), col="blue")
    )
    dev.off()

    pausa()






    #Holt's Exponential Smoothing
    pesoholt<- holt(datos$y)
    #summary(pesoholt)
    png("st_holt_exponencial.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego

    datos$holtt<-pesoholt$fitted
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,holtt), col="blue")
    )
    dev.off()
    pausa()




    #Holt-Winters' Exponential Smoothing
    pesohw<- hw(datosts,seasonal = "additive",h=10,level = 95)
    #pesohw$model

    #asignamos valores ajustados a una columna
    png("st_holt-winter_exponencial.png",width = 720,height = 480,units = "px") #se guarda una imagen, esta se plotea luego luego

    datos$Ajustado<-as.numeric(pesohw$fitted)
    print(
    ggplot(datos, aes(x,y)) +
        geom_point()+geom_line()+
        ggtitle("Serie de tiempo")+
        theme(plot.title = element_text(color = "Black",hjust = 0.5))+
        geom_line(aes(x,Ajustado), col="blue")
    )
    dev.off()
    pausa()

    a<-which.min(c(MSE(datos$y, datos_rl$fitted.values),
                  MSE(datos$y, promo),
                  MSE(datos$y, promopo),
                  MSE(datos$y, pesoses$fitted),
                  MSE(datos$y, pesoholt$fitted),
                  MSE(datos$y, datos$Ajustado)))
    print('El modelo elegido fue: ')
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
    dev.off()
}

serie_tiempo_ARIMA<-function(datos,frecuencia=4,inicio=2010){
  paquetes.tsrutina()
  conditional.tsrutina(datos)
  pausa()

  if(!is.ts(datos)){

    if(is.data.frame(datos)){


  names(datos)<-c("x","y")
  datos$x<-as.Date(datos$x,format("%d/%m/%Y"))  #Y debe ser mayuscula
  print(head(datos))
  continuar<-readline(" Estan bien los datos a usar? si hay un error,
    corrige, si no los hay, enter para continuar: ")

  if(!continuar=="")
    stop("Corrige el error")

  #creando el objeto series
  datosts<-ts(data = datos$y,frequency =  frecuencia,start=inicio)

  base=datos
  p=0
  while(p==0){

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
      base<-diff(base$y,lag = 1,differences = 1)

      print('se diferencio la base de datos')
    }else{
      print("Se rechaza H0, se obta por H1: La serie de tiempo es Estacionaria")
      p=1
    }
    pausa()
  }
  #plotea el acf y analizas
  print(acf(base))
  ma<-readline('Que MA(r) sospechas?, inserte el valor de r:     ')
  ma<-c(0,0,as.numeric(ma))
  pausa()
  #plotea el pacf
  print(acf(base))
  ra<-readline('Que AR(p) sospechas?, inserte el valor de p:     ')
  ra<-c(0,0,as.numeric(ra))
  pausa()
  #imprime arimas
  print(arima(base,order = ma))
  pausa()
  print(arima(base,order = ra))
  pausa()
  #compara arimas
  mama<-arima(base,order = ma)
  rara<-arima(base,order = ra)
  if(mama$aic<rara$aic){
    print(sprintf('El modelo con menor AIC es el MA(%s)',ma))
  }else{
    print(sprintf('El modelo con menor AIC es el RA(%s)',ra))
  }
  pausa()

    }else{
      stop("El objeto debe ser data frame")
    }

  }else{
    stop("El objeto debe ser un data frame con dos elementos")
  }

}
