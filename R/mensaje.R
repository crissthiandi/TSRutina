
#' Mensaje con retorno
#'
#' Similar a message solo que retorna el mensaje.
#'
#' @param ... Conjunto de elementos caracter a mostrar en el mensaje
#' @param domain See \code{\link{message}}
#' @param appendLF See \code{\link{message}}
#'
#' @return Cadena de texto del mensaje impreso
#' @export
#'
#' @examples
#' a <- mensaje("Hola"," Cristhiandi")
#' ### Aparece impreso "Hola Cristhiandi" en formato message
#' print(a) ## imprime cadena "Hola Cristhiandi"
#'
#'
mensaje <- function (..., domain = NULL, appendLF = TRUE)
{
  args <- list(...)
  cond <- if (length(args) == 1L && inherits(args[[1L]], "condition")) {
    if (nargs() > 1L)
      warning("additional arguments ignored in message()")
    args[[1L]]
  }
  else {
    msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
    call <- sys.call()
    simpleMessage(msg, call)
  }
  defaultHandler <- function(c) {
    cat(conditionMessage(c), file = stderr(), sep = "")
  }
  withRestarts({
    signalCondition(cond)
    defaultHandler(cond)
  }, muffleMessage = function() NULL)
  return(invisible(msg))
}
