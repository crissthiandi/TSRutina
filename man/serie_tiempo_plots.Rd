\name{serie_tiempo_plots}
\alias{serie_tiempo_plots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Graficas en PNG de tu S.T.
%%  ~~function to do ... ~~
}
\description{
Obten archivos .png de tus graficos de serie de tiempo en tu directorio

Usa y se estiliza con

\code{\link{ggplot2:::ggplot}}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
serie_tiempo_plots(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{datos}{ Sera el dataframe de no mas de 2 columnas, en el orden primero tiempo y luego valor el tiempo va en formato fecha, y tiene que ser en el orden dia-mes-year
%%     ~~Describe \code{x} here~~
}
\item{
  frecuencia
  }{ Este es el periodo de la serie, trimestral=3, cuatrimestral=4, mensual=12, etc.}
  \item{inicio}{ Este es el year a iniciar la serie de tiempo}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Se usa una salida interactiva para el proceso
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
  \item{Graficos}{ Un conjunto de imagenes .png se generan en el directorio \code{\link{getwd}}. Se guardan con los nombres de que representan como titulo del archivo}
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
\code{\link{ggplot}}

\code{\link{getwd}}
}
\examples{
##---- Should be DIRECTLY executable !! ----

base=data.frame(tiempo=1:20,valores=1:20*3+5)
serie_tiempo_plots(datos=base,frecuencia=4,inicio=2010)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
