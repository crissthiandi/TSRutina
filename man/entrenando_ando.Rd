% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ajuste_prophet.R
\encoding{UTF-8}
\name{entrenando_ando}
\alias{entrenando_ando}
\title{Training model}
\usage{
entrenando_ando(datos, Modelo = NULL, Days_to_forecast, Festivos)
}
\arguments{
\item{Days_to_forecast}{by default 45. ¿cuantos días serán pronosticados?}

\item{Festivos}{data.frame con las fechas de eventos festivos}

\item{Datos}{Datos para entrenamiento}

\item{modelo}{(opcional) by default NULL. Modelos de la class prophet que sera entrenado ver detalles}
}
\value{
lista con datos entrenados y grafico dyplot
}
\description{
Entrena modelo Prophet
}
\details{
De no dar un modelo se ajusta el modelo más general acotado a los parametros que el equipo de prophet asigno. Para más detalles leer paper del modelo.

\link{https://peerj.com/preprints/3190.pdf}
}
\examples{

# Sin modelo
entrenando_ando(Datos)

# Con modelo
entrenando_ando(Datos,Modelo)
}
