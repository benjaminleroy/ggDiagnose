# this function mimics the `plot.Gam` function from the `gam` package,
# *not* the `plot.gam` from the `mgcv` package. (however thought to make
# 2 types of `gam`s is annoying)

# ggDiagnose.Gam

#' Grab term labels from \code{Gam} object
#'
#' Inner function "lifted" from \code{gam}.
#'
#' @param object \code{Gam} object from package \code{gam}
#' @param ... extra parameters (not used)
#'
#' @return term names
labels.Gam <- function(object,...){
  attr(object$terms, "term.labels")
}



#' Diagnostic plot for Gam object (ggplot based)
#'
#' this function mimics the \code{\link[gam]{plot.Gam}} function from the
#' \code{\link[gam]} package, *not* the \code{\link[mgcv]{plot.gam}} from the
#' \code{\link[mgcv]} package.
#'
#' (however thought to make 2 types of \code{gam}s is annoying.) *remove when
#' have both.*
#'
#' @param x \code{Gam} object from \code{gam} library
#' @param residuals if TRUE, partial deviance residuals are plotted along with
#' the fitted terms—default is FALSE. If residuals is a vector with the same
#' length as each fitted term in x, then these are taken to be the overall
#' residuals to be used for constructing the partial residuals.
#' @param rugplot if TRUE (the default), a univariate histogram or rugplot is
#' displayed along the base of each plot, showing the occurrence of each ‘x’;
#' ties are broken by jittering (see parameter option below.
#' @param se if TRUE (the default), upper and lower pointwise
#' twice-standard-error curves are included for each plot.
#' @param terms subsets of the terms can be selected.
#' @param ... (extra parameters, passed to dfCompile.Gam).
#' @param jitter if TRUE (the default), rug points and plotted residual points
#' are jittered. (Note \code{ggplot2}'s jitter is different than base plot.)
#' @param point.shape shape of points (the default is 1, an open circle).
#' @param color color for curves (or points if variable is discrete).
#' @param se.color color of se band.
#' @param se.alpha opacity of ribbon of se band if variable is continuous.
#' @param se.linetype linetype of se band if variable is discrete.
#' @param show.plot if TRUE, a display the graphics (group of graphics in this
#' case) is returned.
#' @param return if TRUE, a list of graphics and the data frame to make
#' the majority of graphics is returned.
#' @param nrow integer number of rows in the displayed graphic. (Default is
#' \code{"sqrt"}, which makes the \code{show.plot} output on a square grid)
#'
#' @return depending on \code{show.plot} and \code{return} it
#' will return the visualization of the graphics and/or a list
#' of both the data frame used the make the majority of the graphic and
#' a list of each individual graphic.
#' @export
#'
#' @examples
#' libary(gam)
#' gam.object <- gam::gam(Sepal.Length ~ gam::s(Sepal.Width) + Species,
#'                        data = iris)
#'
#' par(mfrow = c(1,2))
#' plot(gam.object, se = TRUE, residuals = TRUE)
#'
#' ggDiagnose(gam.object, residuals = TRUE) # se = TRUE by default
ggDiagnose.Gam <- function(x,  residuals = NULL, rugplot = TRUE, se = TRUE,
                           terms =  labels.Gam(x),
                           ...,
                           jitter = TRUE, point.shape = 1,
                           color = "black", se.color = "black",
                           se.alpha = .2, se.linetype = 2,
                           show.plot = TRUE,
                           return = FALSE,
                           nrow = "sqrt"){


  if (!any(show.plot, return)) {
    return(NULL)
  }
  missing.packages <- look.for.missing.packages(c("gam"))
  # ^also requires ggplot2, base, dyplr, gridExtra

  if (length(missing.packages) > 0) {
    stop(paste0(c("Package(s) '",paste0(missing.packages, collapse = "', '"),
                  "' needed for this function to work. Please install them/it."),
                collapse = ""))
  }
  # removing interaction terms for graphics ----------------
  # from orginal function
  # *also collects type of variable (continuous or discrete in mode)
  Terms <- object$terms
  a <- attributes(Terms)
  Call <- object$call
  all.terms <- labels(Terms)
  xvars <- parse(text = all.terms)
  names(xvars) <- all.terms
  terms <- sapply(terms,match.arg, all.terms)
  Interactions <- a$order > 1
  if (any(Interactions)) {
    all.terms <- all.terms[!Interactions]
    TM <- match(terms, all.terms, 0)
    if (!all(TM)) {
      terms <- terms[TM > 0]
      warning("No terms saved for \"a:b\" style interaction terms")
    }
  }
  xvars <- xvars[terms]
  xnames <- as.list(terms)
  names(xnames) <- terms
  modes <- sapply(xvars, mode)
  # end of code from original function -----------


  if (jitter) {
    position = "jitter"
  } else {
    position = "identity"
  }

  actual.var.names <- sapply(terms,function(x) all.vars(formula(paste("~",x))))

  completed.df <- dfCompile.Gam(x, residuals = residuals,
                                terms = terms,...)

  # plot residuals?
  plot.residuals <- FALSE
  if (!is.null(residuals)) {
    if (length(residuals) > 1) {
      plot.residuals <- TRUE
    } else if (residuals) {
      plot.residuals <- TRUE
    }

  }

  gglist <- list()
  for (term in terms) {
    var.name <- actual.var.names[term]
    y.clean <- gsub(x = term,
                    pattern = "::|\\)|\\(",
                    replacement = ".")
    y.name <- paste0(".smooth.", y.clean)
    y.se.lower <- paste0(".se", y.name, ".lower")
    y.se.upper <- paste0(".se", y.name, ".upper")
    if (term %in% terms[modes != "name"]) { # continous variable
      gglist[[term]] <-
        ggplot2::ggplot(completed.df,
                        ggplot2::aes_string(x = var.name,
                                            y = y.name)) +
        ggplot2::geom_line() +
        ggplot2::labs(y = term)
      if (se) {
        gglist[[term]] <- gglist[[term]] +
          ggplot2::geom_ribbon(
            ggplot2::aes_string(x = var.name,
                                ymin = y.se.lower,
                                ymax = y.se.upper),
            alpha = se.alpha)
      }
      if (rugplot) {
        gglist[[term]] <- gglist[[term]] +
          ggplot2::geom_rug(sides = "b", position = position)
      }

      if (plot.residuals) {
        gglist[[term]] <- gglist[[term]] +
          ggplot2::geom_point(ggplot2::aes_string(x = var.name,
                                                  y = paste0(y.name,
                                                             " - .resid")),
                              shape = point.shape)
      }
    } else {# modes = "name" -> discrete variable
      discrete <- completed.df[,c(var.name, y.name, y.se.lower, y.se.upper)] %>%
        dplyr::distinct()

      gglist[[term]] <-
        ggplot2::ggplot(discrete, aes_string(x = var.name, y = y.name)) +
        ggplot2::geom_point() +
        ggplot2::labs(y = paste("partial for",term))

      if (se) {
        gglist[[term]] <- gglist[[term]] +
          ggplot2::geom_errorbar(
            ggplot2::aes_string(x = var.name,
                                ymin = y.se.lower,
                                ymax = y.se.upper),
            width = .5,
            linetype = se.linetype)
      }

      if (rugplot) {
        gglist[[term]] <- gglist[[term]] +
          ggplot2::geom_rug(data = completed.df,
                            ggplot2::aes_string(x = var.name),
                            sides = "b", position = position)
      }

      if (plot.residuals) {
        gglist[[term]] <- gglist[[term]] +
          ggplot2::geom_point(data = completed.df,
                              ggplot2::aes_string(x = var.name,
                                                  y = paste0(y.name,
                                                             " - .resid")),
                              position = position,
                              shape = point.shape)
      }
    }

  }


  if (show.plot) {
    if (nrow == "sqrt") {
      nrow = floor(sqrt(length(gglist)))
    }
    gridExtra::grid.arrange(grobs = gglist, nrow = nrow)
  }

  if (return) {
    return(list(data = output.df, gglist = gglist))
  }

}




#' Creates an augmented data frame for \code{Gam} objects (for \pkg{ggplot2}
#' visuals)
#'
#' Though a similar idea to \code{\link[broom]{broom::augment}} this returns
#' very different values.
#'
#' @param x \code{Gam} object from \code{gam} library
#' @param residuals if TRUE, partial deviance residuals are plotted along with
#' the fitted terms—default is FALSE. If residuals is a vector with the same
#' length as each fitted term in x, then these are taken to be the overall
#' residuals to be used for constructing the partial residuals.
#' @param terms subsets of the terms can be selected.
#' @param ... (extra parameters, passed to dfCompile.Gam).
#'
#' @return augmented data.frame, see \code{details} for more information
#' @details
#' \describe{
#'   \item{original data frame}{original data frame used to create lm or glm
#'   object}
#'   \item{.resid}{residuals between predicted verse actual \code{y} values}
#'  }
#'  For each variable in the model we have the following variables:
#' \describe{
#'   \item{.smooth._}{part of the predicted value from variable \code{_}
#'   for each observation}
#'   \item{se.smooth._.lower}{lower pointwise twice-standard-error values for
#'   each point relative to the part of the predicted value from variable
#'   \code{_}}
#'   \item{se.smooth._.upper}{the "upper" version of \code{se.smooth._.lower}}
#' }
#'
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' gam.object <- gam::gam(Sepal.Length ~ gam::s(Sepal.Width) + Species,
#'                        data = iris)
#'
#' dfCompile.Gam(gam.object) %>% head
dfCompile.Gam <- function(x, residuals = NULL, #newdata = NULL,
                          terms = labels.Gam(x), ...){
  # pkg requirements (for this function)
  missing.packages <- look.for.missing.packages(c("gam"))
  # ^also requires base, dyplr

  if (length(missing.packages) > 0) {
    stop(paste0(c("Package(s) '",paste0(missing.packages, collapse = "', '"),
                  "' needed for this function to work. Please install them/it."),
                collapse = ""))
  }

  # residuals
  Residuals <- resid(x)
  if (!is.null(residuals)) {
    if (length(residuals) == 1) {
      residuals <- Residuals # even if "residuals = FALSE"
    } # else use provided residuals
  } else {
    residuals <- Residuals
  }


  output.df <- data.frame(x$data,
                          .resid = residuals)
  pred <- predict(x, type = "terms", terms = terms,
                  se.fit = TRUE)


  fits <- data.frame(pred$fit)
  names(fits) <- paste0(".smooth.", colnames(pred$fit))

  se.fits <- data.frame(pred$se.fit)
  names(se.fits) <-  paste0(".se.smooth.", colnames(pred$se.fit))

  se.df <- cbind(fits + 2 * se.fits,
                 fits - 2 * se.fits)

  names(se.df) <- as.vector(sapply(c(".upper", ".lower"),
                                   function(x) paste0(names(se.fits),x)))



  output.df <- cbind(output.df,
                     fits,
                     se.df )

  names(output.df) <- gsub(x = names(output.df),
                           pattern = "::|\\)|\\(",
                           replacement = ".")

  return(output.df)
}
