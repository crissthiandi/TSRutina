#Trabajo hecho y en mantenimiento por @crissthiandi <albertocenaa@gmail.com>

#' Obten el intervalo de confianza de un objeto ACF o PACF
#'
#' Atravez del metodo usado en Stats se obtiene el valor de los intervalos de confianza
#'
#'
#' Esta pensado para su uso en Rutinas de la paqueteria TSRutina, aunque su uso como función independiente
#' deberia ser sencillo, se recomienda usarse solo si se conoce sobre el codigo y el tema.
#'
#' @param x Objeto ACF o PACF
#' @param ci Valor de Alpha, por defecto 0.95
#' @param type Tipo de linea en la grafica (No usar, deshabilitado)
#' @param xlab (No usar, deshabilitado)
#' @param ylab (No usar, deshabilitado)
#' @param ylim (No usar, deshabilitado)
#' @param main (No usar, deshabilitado)
#' @param ci.col (No usar, deshabilitado)
#' @param ci.type Tipo de intervalo de confianza por defecto es white
#' @param ... Parametros que se pueden pasar a el grafico
#'
#' @return Valor numerico que representa el limite del intervalo de confianza
#' @export
#'
#' @examples
#' base=data.frame(x=seq(Sys.Date(),by="days",length=20),y=(rexp(50)+1)*sin(1:50))
#' p=acf(base$y,plot=FALSE)
#' intervalo_confianza_acf(p) #0.4382613
intervalo_confianza_acf=function (x, ci = 0.95, type = "h", xlab = "Lag", ylab = NULL,
                                  ylim = NULL, main = NULL, ci.col = "blue", ci.type = c("white",
                                                                                         "ma"), max.mfrow = 6, ask = Npgs > 1 && dev.interactive(),
                                  mar = if (nser > 2) c(3, 2, 2, 0.8) else par("mar"), oma = if (nser >
                                                                                                 2) c(1, 1.2, 1, 1) else par("oma"), mgp = if (nser >
                                                                                                                                               2) c(1.5, 0.6, 0) else par("mgp"), xpd = par("xpd"),
                                  cex.main = if (nser > 2) 1 else par("cex.main"), verbose = getOption("verbose"),
                                  ...)
{
  ci.type <- match.arg(ci.type)
  if ((nser <- ncol(x$lag)) < 1L)
    stop("x$lag must have at least 1 column")
  if (is.null(ylab))
    ylab <- switch(x$type, correlation = "ACF", covariance = "ACF (cov)",
                   partial = "Partial ACF")
  if (is.null(snames <- x$snames))
    snames <- paste("Series ", if (nser == 1L)
      x$series
      else 1L:nser)
  with.ci <- ci > 0 && x$type != "covariance"
  with.ci.ma <- with.ci && ci.type == "ma" && x$type == "correlation"
  if (with.ci.ma && x$lag[1L, 1L, 1L] != 0L) {
    warning("can use ci.type=\"ma\" only if first lag is 0")
    with.ci.ma <- FALSE
  }
  clim0 <- if (with.ci)
    qnorm((1 + ci)/2)/sqrt(x$n.used)
  else c(0, 0)
  Npgs <- 1L
  nr <- nser
  if (nser > 1L) {
    if (nser > max.mfrow) {
      Npgs <- ceiling(nser/max.mfrow)
      nr <- ceiling(nser/Npgs)
    }
    # opar <- par(mfrow = rep(nr, 2L), mar = mar, oma = oma,
    #             mgp = mgp, ask = ask, xpd = xpd, cex.main = cex.main)
    # on.exit(par(opar))
    if (verbose) {
      message("par(*) : ", appendLF = FALSE, domain = NA)
      str(par("mfrow", "cex", "cex.main", "cex.axis",
              "cex.lab", "cex.sub"))
    }
  }
  if (is.null(ylim)) {
    ylim <- range(x$acf[, 1L:nser, 1L:nser], na.rm = TRUE)
    if (with.ci)
      ylim <- range(c(-clim0, clim0, ylim))
    if (with.ci.ma) {
      for (i in 1L:nser) {
        clim <- clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1,
                                                   i, i]^2)))
        ylim <- range(c(-clim, clim, ylim))
      }
    }
  }
  for (I in 1L:Npgs) for (J in 1L:Npgs) {
    dev.hold()
    iind <- (I - 1) * nr + 1L:nr
    jind <- (J - 1) * nr + 1L:nr
    if (verbose)
      message(gettextf("Page [%d,%d]: i =%s; j =%s", I,
                       J, paste(iind, collapse = ","), paste(jind,
                                                             collapse = ",")), domain = NA)
    for (i in iind) for (j in jind) if (max(i, j) > nser) {
      frame()
      box(col = "light gray")
    }
    else {
      clim <- if (with.ci.ma && i == j)
        clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1, i, j]^2)))
      else clim0
      # plot(x$lag[, i, j], x$acf[, i, j], type = type,
      #      xlab = xlab, ylab = if (j == 1)
      #        ylab
      #      else "", ylim = ylim, ...)
      # abline(h = 0)
      if (with.ci && ci.type == "white")
        clim=clim
      # abline(h = c(clim, -clim), col = ci.col, lty = 2)
      else if (with.ci.ma && i == j) {
        clim <- clim[-length(clim)]
        # lines(x$lag[-1, i, j], clim, col = ci.col, lty = 2)
        # lines(x$lag[-1, i, j], -clim, col = ci.col,
        # lty = 2)
      }
      # title(if (!is.null(main))
      # main
      #else if (i == j)
      # snames[i]
      #else paste(sn.abbr[i], "&", sn.abbr[j]), line = if (nser >
      #  2)
      # 1
      #else 2)
    }
    if (Npgs > 1) {
      mtext(paste("[", I, ",", J, "]"), side = 1, line = -0.2,
            adj = 1, col = "dark gray", cex = 1, outer = TRUE)
    }
    dev.flush()
  }
  return(clim)

}
