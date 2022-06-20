# Dudas a @crissthiandi <albertocenaa@gmail.com>

#' Data to prophet format
#'
#' Agrega el formato para hacer modelado prophet
#' nombres de variable ds and y
#'
#' @param datos Datos para el análisis de serie de tiempo solo Data.frame
#'
#' @return Dataframe con formato para prophet
#'
#' @export
#' @encoding UTF-8
#'
#' @examples
#'
#' datos <- read.csv("https://raw.githubusercontent.com/crissthiandi/datos/master/Series_tiempo/sunspot_month_dataframe.csv")
#'
#' data_to_prophet(datos)
data_to_prophet <- function(datos,...){
  ## ordena cual es fecha, checar primer elemento
  if(is.numeric(datos[1,1])){
    aux <- datos[2]
    aux[2] <- datos[1]
    datos <- aux
  }
  # captura las correcciones de checar datos
  lista <- checar_datos(datos)
  datos <- lista$datos


  if(any(datos %>% names() != c("ds","y"))){
    # Cambiando nombre de la base de datos a ds y y
    names(datos) <- c("ds","y")
    cat(crayon::cyan("\n Se tomo la primera columna como tiempo y\n la segunda como valores de la serie de tiempo."))
  }

  invisible(datos)

}

#' Outliers to prophet
#'
#' Modifica los datos que seran outliers, soporta vector de rangos.
#' Es necesario usar formato de entrada prophet, de no hacerlo se ajustara usando \code{\link{data_to_prophet}}
#'
#'
#' @param datos Datos para el análisis de serie de tiempo solo Data.frame
#' @param from [as.Date] Univalor o vector de fechas para hacer intervalos de outliers
#' @param to [as.Date] Univalor o vector de fechas para hacer intervalos de outliers
#' @param ... Not work
#'
#' @details Si se ingresa vector de fechas, from y to deben ser del mismo tamaño.
#'
#' @return Data.frame con outliers etiquetados con NA
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples
#'
#' datos <- read.csv("https://raw.githubusercontent.com/crissthiandi/datos/master/Series_tiempo/sunspot_month_dataframe.csv")
#'
#' outliers_to_prophet(datos,as.Date('2002-02-01'),as.Date('2007-06-01'))
#'
#' # Si se usan varios rangos
#' from <- c(as.Date('1800-06-01'),as.Date('2004-03-01'))
#' to <- c(as.Date('1805-06-01'),as.Date('2007-03-01'))
#' outliers_to_prophet(datos,from,to)
outliers_to_prophet <- function(datos,from,to,...){
  # valida los datos
  datos <- data_to_prophet(datos)
  # mismo tamaño entre from y to
  n <- length(from)
  stopifnot(length(from) == length(to))
  # igual que n pero de esta forma el mensaje de error es más claro
  # Validar para cada rango
  for (i in 1:n) {
    outliers <- with(
      datos,
      {
        (ds >= from[i]
         & ds <= to[i])
      }
    )
    datos$y[outliers] = NA # filtrar datos
  }

  invisible(datos)
}

#' Holydays to fit prophet hasta 2014
#'
#' Histórico de fechas donde hay eventos "relevantes" en México para retail. La lista completa se incluye en details.
#'
#'
#' @param filter filtro de cuales eventos serán usados, por default "all", para alguna lista en particular usar por ejemplo c("Navidad","BuenFin").
#'
#' @details Los eventos relevantes son:
#'    \itemize{\item{Navidad:}{ Todo Diciembre centrado al 23 de diciembre.}}
#'    \itemize{\item{Buen Fin:}{ Varia con los años}}
#'    \itemize{\item{Black Friday:}{ Tercer jueves de noviembre}}
#'    \itemize{\item{Cyber Monday:}{ Lunes despues del Black Friday}}
#'    \itemize{\item{Hot Sale:}{ Varia entre años}}
#'    \itemize{\item{Independencia:}{ 16 de septiembre MX}}
#'    \itemize{\item{Halloween:}{ Finales de octubre, Noche de halloween}}
#'    \itemize{\item{Pre_Halloween:}{ Compras a inicio de octubre, efecto de temporada previa a halloween}}
#'    \itemize{\item{San_valentin:}{ 14 de Febrero}}
#'    \itemize{\item{caida_25_diciembre:}{ Efecto de "pausa" economica por dia despues a navidad}}
#'    \itemize{\item{Puente natalicio Benito Juarez:}{Puente oficial por parte del estado Mexicano}}
#'    \itemize{\item{Vacaciones semana santa}{Efecto por las ventas en vacaciones de semana santa}}
#'
#' @return Data.frame con fechas y rangos de efectos de promociones.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples
#'
#' holydays_to_prophet()
holydays_to_prophet <- function(datos,from,to,...){


  buen_fin <- tibble::tibble(
    holiday = "Buen_fin",
    ds = as.Date(c("2022-11-19","2021-11-13","2019-11-16","2018-11-17",
                   "2017-11-18","2016-11-19","2015-11-14","2014-11-14")),
    lower_window = c(-2,-4,rep(-2,6)),
    upper_window = 3
  )

  Navidad <- tibble::tibble(
    holiday = "Navidad",
    ds = as.Date(c("2022-12-15","2021-12-15","2019-12-15","2018-12-15",
                   "2017-12-15","2016-12-15","2015-12-15","2014-12-15")),
    lower_window = -15,
    upper_window = 16
  )

  Noche_buena <- tibble::tibble(
    holiday = "Noche_buena",
    ds = as.Date(c("2022-12-23","2021-12-23","2019-12-23","2018-12-23",
                   "2017-12-23","2016-12-23","2015-12-23","2014-12-23")),
    lower_window = -3,
    upper_window = 1
  )

  Black_friday <- tibble::tibble(
    holiday = "BlackFriday",
    ds = as.Date(c("2022-11-25","2021-11-26","2019-11-29","2018-11-23",
                   "2017-11-24","2016-11-25","2015-11-27","2014-11-28")),
    lower_window = 0,
    upper_window = 1
  )

  CyberMonday <- tibble::tibble(
    holiday = "CyberMonday",
    ds = as.Date(c("2022-11-28","2021-11-29","2019-12-02","2018-11-26",
                   "2017-11-27","2016-11-28","2015-11-30","2014-12-01")),
    lower_window = 0,
    upper_window = 1
  )

  Hot_sale <- tibble::tibble(
    holiday = "Hot_sale",
    ds = as.Date(c("2022-05-28","2021-05-27","2019-05-29","2018-05-30",
                   "2017-05-30","2016-05-31","2015-05-30","2014-09-06")),
    lower_window = c(-5,-6,-3,-3,-2,-2,-2,-2),
    upper_window = c(3,4,2,2,3,3,3,3)
  )

  Independencia <- tibble::tibble(
    holiday = "independencia",
    ds = as.Date(c("2022-09-16","2021-09-16","2019-09-16","2018-09-16",
                   "2017-09-16","2016-09-16","2015-09-16","2014-09-16")),
    lower_window = -2,
    upper_window = 1
  )

  Halloween <- tibble::tibble(
    holiday = "Halloween",
    ds = as.Date(c("2022-10-31","2021-10-31","2019-10-31","2018-10-31",
                   "2017-10-31","2016-10-31","2015-10-31","2014-10-31")),
    lower_window = -2,
    upper_window = 1
  )

  Pre_Halloween <- tibble::tibble(
    holiday = "Pre_Halloween",
    ds = as.Date(c("2022-10-10","2021-10-10","2019-10-10","2018-10-10",
                   "2017-10-10","2016-10-10","2015-10-10","2014-10-10")),
    lower_window = -7,
    upper_window = 7
  )

  San_valentin <- tibble::tibble(
    holiday = "San_valentin",
    ds = as.Date(c("2022-02-14","2021-02-14","2020-02-14","2019-02-14",
                   "2018-02-14","2017-02-14","2016-02-14","2015-02-14","2014-02-14")),
    lower_window = -2,
    upper_window = 1
  )

  caida_25_diciembre <- tibble::tibble(
    holiday = "caida_25_diciembre",
    ds = as.Date(c("2022-12-25","2021-12-25","2019-12-25","2018-12-25",
                   "2017-12-25","2016-12-25","2015-12-25","2014-12-25")),
    lower_window = 0,
    upper_window = 0
  )

  # quien sale el primer dia del año (?)
  caida_Primero_dia_del_anio <- tibble::tibble(
    holiday = "primer_dia_del_anio",
    ds = as.Date(c("2022-01-01","2021-01-01","2019-01-01","2018-01-01",
                   "2017-01-01","2016-01-01","2015-01-01","2014-01-01")),
    lower_window = 0,
    upper_window = 1
  )

  puente_natalicio_Benito_juarez <- tibble::tibble(
    holiday = "natalicio_BJ",
    ds = as.Date(c("2022-03-21","2021-03-15","2020-03-16","2019-03-18",
                   "2018-03-19",
                   "2017-03-20","2016-03-21","2015-03-16","2014-03-17")),
    lower_window = c(-3,-3,-3,-3,-3,-3,-3,-3,-3),
    upper_window = c(0,0,0,0,0,0,0,0,0)
  )

  # 2014 - Viernes 11 de Abril y deberán regresar a clase el día lunes 28 de Abril. ==>
  # as.Date("2014-04-12") - as.Date("2014-04-27")
  # as.Date("2014-04-12") + 7 # 7- 8
  #
  # 2015 - Del lunes 30 de marzo al viernes 10 de abril. ==>
  # as.Date("2015-03-28") - as.Date("2015-04-12")
  # as.Date("2015-03-28") + 7 # 7-8
  #
  # 2016 - 22 de marzo al 5 de abril
  # as.Date("2016-03-22") - as.Date("2016-04-05")
  # as.Date("2016-03-22") +7 # 7-7
  # base::weekdays.Date(as.Date("2016-03-22")) # martes
  # base::weekdays.Date(as.Date("2016-04-05")) # martes
  #
  # 2017 - Del 10 al 21 de abril
  # as.Date("2017-04-10") - as.Date("2017-04-21")
  # base::weekdays.Date(as.Date("2017-04-10")) # lunes
  # as.Date("2017-04-08") - as.Date("2017-04-23")
  # as.Date("2017-04-08") + 7 # 7- 8
  #
  # 2018 - Del 26 de Marzo al 6 de Abril
  # as.Date("2018-03-26") - as.Date("2018-04-06")
  # base::weekdays.Date(as.Date("2018-03-26")) # lunes
  # as.Date("2018-03-24") - as.Date("2018-04-08")
  # as.Date("2018-03-24") + 7 # 7 - 8
  #
  # 2019 - Del 15 de Abril al 26 de Abril
  # as.Date("2019-04-15") - as.Date("2019-04-26")
  # base::weekdays.Date(as.Date("2019-04-15")) # lunes
  # as.Date("2019-04-13") - as.Date("2019-04-28")
  # as.Date("2019-04-13") + 7 # 7 - 8
  #
  # 2022 - el lunes 11 de abril y termina el viernes 22 de abril
  # as.Date("2022-04-11") - as.Date("2022-04-22")
  # base::weekdays.Date(as.Date("2022-04-11")) # lunes
  # base::weekdays.Date(as.Date("2022-04-09")) # domingo
  # as.Date("2022-04-09") - as.Date("2022-04-24")
  # as.Date("2022-04-09") + 7 # 7 - 8
  #

  Vacaciones_semana_santa <- tibble::tibble(
    holiday = "Semana_Santa",
    ds = as.Date(c(
      "2014-04-19","2015-04-04","2016-03-29","2017-04-15",
      "2018-03-31","2019-04-20","2022-04-16"
    )),
    lower_window = c(-7,-7,-7,-7,-7,-7,-7),
    upper_window = c(8,8,7,8,8,8,8)
  )

  Dia_de_las_madres <- tibble::tibble(
    holiday = "Dia_de_la_madre",
    ds = as.Date(paste(2014:2022,"05",10,sep = "-")),
    lower_window = -7,
    upper_window = 1
  )



  Festivos <- rbind(buen_fin,Navidad,Black_friday,CyberMonday,
                    Hot_sale,Independencia,Halloween,Pre_Halloween,
                    Noche_buena,San_valentin,caida_25_diciembre,
                    puente_natalicio_Benito_juarez,
                    Vacaciones_semana_santa,
                    caida_Primero_dia_del_anio,
                    Dia_de_las_madres)

  invisible(Festivos)
}

#' Training model
#'
#' Entrena modelo Prophet
#'
#'
#' @param Datos Datos para entrenamiento
#' @param modelo (opcional) by default NULL. Modelos de la class prophet que sera entrenado, ver detalles.
#' @param Days_to_forecast by default 45 ¿cuantos días serán pronosticados?
#' @param Festivos data.frame con las fechas de eventos festivos
#'
#' @details De no dar un modelo se ajusta el modelo más general acotado a los
#' parámetros que el equipo de prophet asigno. Para más detalles leer paper del modelo.
#'
#' \link{https://peerj.com/preprints/3190.pdf}
#'
#'
#' @return lista con datos entrenados y gráfico de la clase dyplot
#'
#' @encoding UTF-8
#'
#' @importFrom prophet prophet
#' @importFrom prophet fit.prophet
#' @importFrom prophet make_future_dataframe
#' @importFrom prophet dyplot.prophet
#'
#' @export
#'
#' @examples
#'
#' # Sin modelo
#' entrenando_ando(Datos)
#'
#' # Con modelo
#' entrenando_ando(Datos,Modelo)
entrenando_ando <- function(datos,Modelo = NULL,Days_to_forecast,Festivos){

  ## DEFINE MODELO GENERAL
  framework_model <- if(is.null(Modelo)){
    prophet::prophet(
      seasonality.mode = "multiplicative",
      growth = "linear",
      n.changepoints = 150,
      holidays = Festivos,
      fit = FALSE
    )
  }else{
    Modelo
  }


  fit_model <- prophet:::fit.prophet(framework_model,df = datos)

  predicciones_days <- prophet::make_future_dataframe(m = fit_model,
                                          periods = Days_to_forecast,
                                          freq = "days")
  # Prediction
  predicciones <- prophet:::predict.prophet(object = fit_model,
                             df = predicciones_days)

  ## Graficar
  p_de_plot <- prophet::dyplot.prophet(x=fit_model,fcst = predicciones)

  invisible(list(
    "Grafico" = p_de_plot,
    "Predicciones" = predicciones,
    "fit_model" = fit_model
  ))
}
