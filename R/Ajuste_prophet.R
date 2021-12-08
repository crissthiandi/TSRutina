
#' Datos to prophet format
#'
#' Agrega el formato para hacer modelado con modelo prophet
#'
#'
#' @param datos Datos para el análisis de serie de tiempo solo Data.frame
#' @param date_format [En caso de errores] Formato de Fecha ingresado ejemplo "Y-mm-dd"
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
data_to_prophet <- function(datos,date_format=NULL,...){
  if(any(datos %>% names() != c("ds","y"))){
    # Cambiando nombre de la base de datos a ds y y
    names(datos) <- c("ds","y")
    cat(crayon::cyan("\n Se tomo la primera columna como tiempo y\n la segunda como valores de la serie de tiempo."))
  }

  invisible(datos)

}

