\name{serie_tiempo_ARIMA}
\alias{serie_tiempo_ARIMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MA(q) o AR(p)
%%  ~~function to do ... ~~
}
\description{
Realiza un analisis con diferenciaciones a una serie de tiempo y obtiene entre los modelos MA(q) o AR(p), el mejor basandoce en criterio \code{\link{AIC}}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
serie_tiempo_ARIMA(datos, frecuencia = 4, incio = 2020)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{datos}{ Dataframe de no mas de 2 columnas, en el orden de primero \bold{tiempo} y luego \bold{valor}. El tiempo va en formato fecha, y tiene que ser en el orden \bold{dia-mes-year}
%%     ~~Describe \code{x} here~~
}
\item{
  frecuencia
  }{ Este es el periodo de la serie, trimestral=3, cuatrimestral=4, mensual=12, etc.}
  \item{inicio}{ Este es el year a iniciar la serie de tiempo}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
se usa una salida interactiva para el proceso
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Cristhian Diaz Chable

Sabrina Guevara Cervera
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
serie_tiempo_ARIMA(datos=base)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
