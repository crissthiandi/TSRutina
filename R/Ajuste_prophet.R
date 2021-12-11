
#' Datos to prophet format
#'
#' Agrega el formato para hacer modelado con modelo prophet
#'
#'
#' @param datos Datos para el análisis de serie de tiempo solo Data.frame
#'
#' @return Dataframe con formato para prophet
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @examples
#'
#' datos <- read.csv("https://raw.githubusercontent.com/crissthiandi/datos/master/Series_tiempo/sunspot_month_dataframe.csv")
#'
#' data_to_prophet(datos)
#'
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
#' Es necesario usar formato prophet o se ajustara usando \code{\link{data_to_prophet}}
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
#'
#'
outliers_to_prophet <- function(datos,from,to,...){
  # valida los datos
  datos <- data_to_prophet(datos)
  # mismo tamaño entre from y to
  n <- length(from)
  stopifnot(n==length(to))
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
#' Histórico de fechas donde hay eventos "relevantes" en México a nivel retail. La lista completa se incluye en details.
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
#'
#'
holydays_to_prophet <- function(datos,from,to,...){

  #Ajuste de fechas
  buen_fin <- data_frame(
    holiday = "Buen_fin",
    ds = as.Date(c("2021-11-13","2019-11-16","2018-11-17",
                   "2017-11-18","2016-11-19","2015-11-14","2014-11-15")),
    lower_window = c(-4,rep(-2,5)),
    upper_window = 3
  )

  Navidad <- data_frame(
    holiday = "Navidad",
    ds = as.Date(c("2021-12-23","2019-12-23","2018-12-23",
                   "2017-12-23","2016-12-23","2015-12-23","2014-12-23")),
    lower_window = -23,
    upper_window = 10
  )

  Black_friday <- data_frame(
    holiday = "BlackFriday",
    ds = as.Date(c("2021-11-26","2019-11-29","2018-11-23",
                   "2017-11-24","2016-11-25","2015-11-27","2014-11-28")),
    lower_window = 0,
    upper_window = 1
  )

  CyberMonday <- data_frame(
    holiday = "CyberMonday",
    ds = as.Date(c("2021-11-29","2019-12-02","2018-11-26",
                   "2017-11-27","2016-11-28","2015-11-30","2014-12-01")),
    lower_window = 0,
    upper_window = 1
  )

  # TODO agregar efectos desde 2014 hasta 2021 de hot sales
  Hot_sale <- data_frame(
    holiday = "Hot_sale",
    ds = as.Date(c("2021-05-27","2019-05-29","2018-05-30",
                   "2017-05-30","2016-05-31","2015-05-30","2014-09-06")),
    lower_window = c(-6,-3,-3,-2,-2,-2,-2),
    upper_window = c(4,2,2,3,3,3,3)
  )

  Festivos <- rbind(buen_fin,Navidad,Black_friday,CyberMonday,Hot_sale)

  invisible(Festivos)
}

