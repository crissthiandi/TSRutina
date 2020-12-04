\name{serie_tiempo_rutina}
\alias{serie_tiempo_rutina}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rutinas en una serie de tiempo
%%  ~~function to do ... ~~
}
\description{Realiza una rutina para una base de datos en la que se incluyen ajustes por

\itemize{\item{Regresion lineal}{}}

\itemize{\item{Promedio movil simple}{}}

\itemize{\item{Promedio movil ponderado}{}}

\itemize{\item{Suavizamiento exponencial simple}{}}

\itemize{\item{Suav. expon. doble o de Holt}{}}

\itemize{\item{Suavizamiento Holt-Winter}{}}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
serie_tiempo_rutina(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{ Dataframe de no mas de 2 columnas, en el orden primero \bold{tiempo} y luego valor el \bold{tiempo} va en formato fecha, y tiene que ser en el orden \bold{dia-mes-year}
%%     ~~Describe \code{x} here~~
}
\item{
  frecuencia
  }{ Este es el periodo de la serie, trimestral=3, cuatrimestral=4, mensual=12, etc.}
  \item{inicio}{ Este es el year a iniciar la serie de tiempo}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Se usa una salida interactiva en la que el usuario debe agregar ciertos datos o tomar ciertas decisiones durante la rutina.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{ Plots}{ Arroja una lista de plots que ayudan a ver el comportamiento de la serie y como ciertos ajustes se aproximan mejor a ella}
\item{Resumenes}{ Arroja ciertos resumenes de ciertos ajustes o pruebas que se hacen}
\item{Seleccion de modelo}{ Se arroja cual es el modelo con menor MSE (Error cuadratico medio)}
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

base=data.frame(tiempo=1:20,valores=1:20*3+5)

serie_tiempo_rutina(datos=base,frecuencia=4,inicio=2010)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
