
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

