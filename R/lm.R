# plot.lm, plot.glm, plot.rlm replacement

#' Diagnostic plot for lm and glm objects (ggplot based)
#'
#' This function leverages code from the \code{\link[stats]{plot.lm}} function
#' from the \pkg{stats} library. It allows for the same imput (except when
#' related directly to `par` functionality, which makes no sense to include).
#' We also include "newer" functionality.
#'
#' This function can deal with \code{\link[stats]{lm}} and
#' \code{\link[stats]{glm}} objects.
#'
#' @param x \code{lm} or \code{glm} object
#' @param which which plots you'd like to create
#' @param caption title per plot
#' @param sub.caption caption for bottom of multiple plot visual (defaults
#' to the formula of the model.)
#' @param main title for the mulitple plot visual
#' @param ... extra attributes (currently not used)
#' @param id.n id the n observations with largest residuals
#' @param labels.id labels for all observations
#' @param qqline logic for whether a qqline should be drawn (a line between the
#' 25 and 75 quantiles in the Q-Q plot)
#' @param cook.levels levels of cooks distance to visualize in the leverage vs
#' standardized residual graphic
#' @param show.plot logic to display the graphics (group of graphics in this
#' case)
#' @param return logic to return list of graphics and the data frame to make
#' the majority of graphics
#' @param shape shape of points (the default is 1, an open circle)
#' @param nrow number of rows in the displayed graphic
#' @param smooth.color color for smoothing lines
#' @param dashed.color color for dashed line (a vector of length 6 is expected)
#'
#' @return depending on \code{show.plot} and \code{return} it
#' will return the visualization of the graphics and/or a list
#' of both the data frame used the make the majority of the graphic and
#' a list of each individual graphic.
#' @export
#'
#' @examples
#' lm.object <- lm(Sepal.Length ~., data = iris)
#'
#' par(mfrow = c(2,3))
#' plot(lm.object, which = 1:6)
#'
#' ggDiagnose.lm(lm.object, which = 1:6)
#'
#' @seealso see \code{\link{dfCompile.lm}} for data creation.
ggDiagnose.lm <- function(x, which = c(1L:3L,5L), ## was which = 1L:4L,
                          caption = list("Residuals vs Fitted", "Normal Q-Q",
                                         "Scale-Location", "Cook's distance",
                                         "Residuals vs Leverage",
                                         expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))),
                          sub.caption = NULL, main = NULL,
                          ...,
                          id.n = 3, labels.id = factor(names(stats::residuals(x)),
                                                       levels = names(stats::residuals(x))),
                          qqline = TRUE, cook.levels = c(0.5, 1.0),
                          # new attributes
                          show.plot = TRUE, return = FALSE,
                          shape = 1,
                          nrow = min(2, length(which)), smooth.color = "blue",
                          dashed.color = c("red", "blue", NA, NA, "red", "black")){

  # ATTN: look at difference in .std.resid and .std.resid2

  # so that CRAN checks pass
  .resid <- .yhat <- .std.resid <- .sqrt.abs.resid <- .cooksd <- NULL
  .index <- .facval <- .std.pearson.resid <- .leverage <- NULL
  legend <- group <- .logit.leverage <- NULL
  .weights <- y <- v <- label <- NULL

  # pkg requirements (for this function)
  missing.packages <- look.for.missing.packages(c("stats",
                                                  "grDevices",
                                                  "graphics",
                                                  "scales"))
  # ^also requires ggplot2, base, dyplr, gridExtra

  if (length(missing.packages) > 0) {
    stop(paste0(c("Package(s) '",paste0(missing.packages, collapse = "', '"),
              "' needed for this function to work. Please install them/it."),
              collapse = ""))
  }

  if (!any(show.plot, return)) {
    return(NULL)
  }

  if (!is.null(x$family) & any(x$family[[1]] %in% c("binomial", "quasibinomial"))) {
    warning("binomial glm are not well diagnosted with these plots.")
  }

  if (!inherits(x, "lm")) {
    stop("use only with \"lm\" objects") # glm, rlm objects also inherit this
  }

  if (!is.numeric(which) || any(which < 1) || any(which > 6)) {
    stop("'which' must be in 1:6")
  }

  isGlm <- inherits(x, "glm")
  show <- rep(FALSE, 6)
  show[which] <- TRUE

  n <- nrow(x$model)

  if (is.null(labels.id)) {
    labels.id <- factor(paste(1L:n),
                        levels = paste(1L:n))
  }

  expanded.df <- dfCompile.lm(x, labels.id = labels.id) %>%
    dplyr::filter(.weights != 0)

  if (any(show[2L:3L])) {
    ylab23 <- ifelse(isGlm,
                     "Std. deviance resid.",
                     "Standardized residuals")
  }

  if (any(show[5L:6L])) { # using 'leverages'
    range.hat <- range(expanded.df$.leverage, na.rm = TRUE) # though should never have NA
    isConst.hat <- all(range.hat == 0) ||
      diff(range.hat) < 1e-10 * mean(expanded.df$.leverage, na.rm = TRUE)
  }
  if (any(show[c(1L, 3L)])) {
    l.fit <- ifelse(isGlm, "Predicted values", "Fitted values")
  }


  if (id.n > 0L) { ## label the largest residuals
    # ATTN: could to be updated: (need to focus on adj.x and label.pos, etc)
    text.id <- function(df, x.string, y.string, ind.string, id.n,
                        ggbase = ggplot2::ggplot()) {
      df.inner <- df[df[,ind.string] <= id.n,]
      ggout <- ggbase +
        ggplot2::geom_text(data = df.inner,
                           ggplot2::aes_string(x = x.string,
                                               y = y.string,
                                               label = ".labels.id"))
      return(ggout)
    }
  }
  # ATTN: needs to be updated:
  getCaption <- function(k) { # allow caption = "" , plotmath etc
    ifelse(length(caption) < k, NA_character_,
           grDevices::as.graphicsAnnot(caption[[k]]))
  }

  # ATTN: will need to deal with this...
  if (is.null(sub.caption)) { ## construct a default:
    cal <- x$call
    if (!is.na(m.f <- match("formula", names(cal)))) {
      cal <- cal[c(1, m.f)]
      names(cal)[2L] <- "" # drop	" formula = "
    }
    cc <- deparse(cal, 80) # (80, 75) are ``parameters''
    nc <- nchar(cc[1L], "c")
    abbr <- length(cc) > 1 || nc > 75
    sub.caption <-
      ifelse(abbr, paste(substr(cc[1L], 1L, min(75L, nc)), "..."), cc[1L])
  }

  ggout.list <- list()
  ##---------- Do the individual plots : ----------
  if (show[1L]) {
    ggout.list$residual.vs.yhat <-
      ggplot2::ggplot(expanded.df,
                     ggplot2::aes(x = .yhat,
                         y = .resid)) +
      ggplot2::geom_point(shape = shape) +
      ggplot2::labs(x = l.fit, y = "Residuals",
           title = getCaption(1)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2,
                          color = dashed.color[1]) +
      ggplot2::geom_smooth(se = F, color = "blue")
    #ATTN: probably need to come back for the title & captions, etc
    if (id.n > 0) {
      ggout.list$residual.vs.yhat <- text.id(df = expanded.df,
                                               x.string = ".yhat",
                                               y.string = ".resid",
                                               ind.string = ".ordering.resid",
                                               id.n = id.n,
                                               ggbase = ggout.list$residual.vs.yhat)

    }
  }
  if (show[2L]) { ## Normal QQ
    ggout.list$qqnorm <- ggplot2::ggplot(expanded.df) +
      ggplot2::geom_qq(ggplot2::aes(sample = .std.resid),
                       distribution = stats::qnorm,
                       shape = shape) +
      ggplot2::labs(title = getCaption(2),
           y = ylab23,
           x = "Theoretical Quantiles")

    if (qqline) {
      ggout.list$qqnorm <- ggout.list$qqnorm +
        ggplot2::geom_qq_line(ggplot2::aes(sample = .std.resid),
                     distribution = stats::qnorm,
                     linetype = 2, col = dashed.color[2L])
    }
    if (id.n > 0) {
      qq <- stats::qqnorm(expanded.df$.std.resid,plot.it = FALSE) %>%
        data.frame() %>%
        dplyr::mutate(.ordering.resid = expanded.df$.ordering.resid,
               .labels.id = expanded.df$.labels.id)

      ggout.list$qqnorm <- text.id(df = qq,
              x.string = "x",
              y.string = "y",
              id.n = id.n,
              ind.string = ".ordering.resid",
              ggbase =  ggout.list$qqnorm)
    }
  }
  if (show[3L]) {
    yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = as.name(ylab23))))

    ggout.list$sqrt.abs.resid <-
      ggplot2::ggplot(expanded.df,
             ggplot2::aes(x = .yhat,
                 y = .sqrt.abs.resid)) +
      ggplot2::geom_point(shape = shape) +
      ggplot2::labs(x = l.fit,
           y = yl,
           title = getCaption(3)) +
      ggplot2::geom_smooth(se = FALSE, color = "blue")
    if (id.n > 0) {
      ggout.list$sqrt.abs.resid <- text.id(df = expanded.df,
                                             x.string = ".yhat",
                                             y.string = ".sqrt.abs.resid",
                                             ind.string = ".ordering.std.resid",
                                             id.n = id.n,
                                             ggbase = ggout.list$sqrt.abs.resid)
    }


  }
  if (show[4L]) { ## Cook's Distances
    ggout.list$cooks <- ggplot2::ggplot(expanded.df,
                                        ggplot2::aes(y = .cooksd,
                                            x = .index)) +
      ggplot2::geom_segment(ggplot2::aes(x = .index,
                                xend = .index,
                                y = 0,
                                yend = .cooksd)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      ggplot2::labs(x = "Observation Number",
                    y = "Cook's distance",
                    title = getCaption(4))
    if (id.n > 0) {
      ggout.list$cooks <- text.id(df = expanded.df,
                                  x.string = ".index",
                                  y.string = ".cooksd",
                                  ind.string = ".ordering.cooks",
                                  id.n = id.n,
                                  ggbase =  ggout.list$cooks)
    }
  }
  if (show[5L]) {
    ylab5 <- ifelse(isGlm, "Std. Pearson resid.", "Standardized residuals")

    if (isConst.hat) { ## leverages are all the same
      if (missing(caption)) { # set different default
        caption[[5L]] <- "Constant Leverage:\n Residuals vs Factor Levels"
      }
      ## plot against factor-level combinations instead
      aterms <- attributes(stats::terms(x))
      ## classes w/o response
      dcl <- aterms$dataClasses[ -aterms$response ] #
      facvars <- names(dcl)[dcl %in% c("factor", "ordered")]
      mf <- stats::model.frame(x)[facvars]# better than x$model
      if (ncol(mf) > 0) {
        dm <- data.matrix(mf)
        ## #{levels} for each of the factors:
        nf <- length(nlev <- unlist(unname(lapply(x$xlevels, length))))
        ff <- if (nf == 1) { #can't do a ifelse here
          1
          }else {
            rev(cumprod(c(1, nlev[nf:2])))
          }
        facval <- (dm - 1) %*% ff
        xx <- facval # for use in do.plot section.

        data.vis <-
          data.frame(.facval = facval,
                     .std.pearson.resid = expanded.df$.std.pearson.resid,
                     .show.cooks = expanded.df$.ordering.cooks,
                     .labels.id = expanded.df$.labels.id)

        ggout.list$residual.vs.leverage <- ggplot2::ggplot(data.vis,
                                                      ggplot2::aes(x = .facval,
                                                      y = .std.pearson.resid)) +
          ggplot2::geom_point(shape = shape) +
          ggplot2::geom_vline(data = data.frame(v = ff[1L]*(0:nlev[1L]) - 1/2),
                              ggplot2::aes(xintercept = v),
                              color = dashed.color[5],
                              linetype = "F4") +
          ggplot2::scale_x_continuous(breaks = ff[1L]*(1L:nlev[1L] - 1/2) - 1/2,
                                      labels = x$xlevels[[1L]]) +
          ggplot2::labs(x = paste0("Factor Level Combinations\n(Major: ",
                                   facvars[1L],")")) +
          ggplot2::geom_hline(yintercept = 0, lty = 2,
                              color = dashed.color[5]) +
          ggplot2::geom_smooth(se = FALSE, color = "blue")

        if (id.n > 0) {
          ggout.list$residual.vs.leverage <- text.id(df = data.vis,
                                        x.string = ".facval",
                                        y.string = ".std.pearson.resid",
                                        ind.string = ".ordering.cooks",
                                        id.n = id.n,
                                        ggbase =  ggout.list$residual.vs.leverage)
        }
      } else {# no factors
        message(gettextf("hat values (leverages) are all = %s\n and there are no factor predictors; no plot no. 5",
                         format(mean(range.hat))),
                domain = NA)
      }

    } else {## Residual vs Leverage
      ## omit hatvalues of 1.
      ylim <- range(expanded.df[expanded.df$.non.extreme.leverage,".std.pearson.resid"])
      xlim <- range(expanded.df[expanded.df$.non.extreme.leverage,".leverage"])

      ggout.list$residual.vs.leverage <- ggplot2::ggplot(expanded.df[expanded.df$.non.extreme.leverage,],
                                      ggplot2::aes(x = .leverage,
                                          y = .std.pearson.resid)) +
        ggplot2::geom_point(shape = shape) +
        ggplot2::geom_vline(xintercept = 0, linetype = 2,
                            color = dashed.color[5L]) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2,
                            color = dashed.color[5L]) +
        ggplot2::geom_smooth(se = FALSE, color = "blue") +
        ggplot2::labs(x = "Leverage",
             y = ylab5,
             title = getCaption(5))

      if (length(cook.levels)) {
        data.cooks <- data.frame(`.leverage` = -1,
                                 `.std.pearson.resid` = 0,
                                 legend = "blank", group = "blank") %>%
          dplyr::mutate(legend = as.character(legend),
                 group = as.character(group))

        p <- length(stats::coef(x))

        hh <- seq.int(min(range.hat[1L], range.hat[2L]/100), range.hat[2L],
                      length.out = 101)

        for (crit in cook.levels) {
          cl.h <- sqrt(crit * p * (1 - hh) / hh)

          data.cooks <- rbind(data.cooks,
                              data.frame(`.leverage` = rep(hh, times = 2),
                                         `.std.pearson.resid` = c(cl.h, -cl.h),
                                         legend = "Cook's distance",
                                         group = c(rep(paste0(crit,".upper"), length(hh)),
                                                     rep(paste0(crit,".lower"), length(hh)))
                              )
          )
        }
        data.cooks <- data.cooks[-1,]

        ggout.list$residual.vs.leverage <- ggout.list$residual.vs.leverage +
          ggplot2::geom_path(data = data.cooks,
                    ggplot2::aes(x = .leverage,
                        y = .std.pearson.resid,
                        group = group),
                    color = dashed.color[5L], linetype = 2,
                    na.rm = TRUE) +
          ggplot2::xlim(c(0,xlim[2])) +
          ggplot2::ylim(ylim)
      }
      if (id.n > 0) {
      ggout.list$residual.vs.leverage <- text.id(df = expanded.df[expanded.df$.non.extreme.leverage,],
              x.string = ".leverage",
              y.string = ".std.pearson.resid",
              ind.string = ".ordering.cooks",
              id.n = id.n,
              ggbase =  ggout.list$residual.vs.leverage)
      }

    }
  }
  if (show[6L]) {

    ylim <- c(0, max(expanded.df$.cooksd))
    xlim <- c(0, max(expanded.df$.logit.leverage))

    athat <- pretty(expanded.df$.leverage)

    ggout.list$cooks.vs.logit.leverage <-
      ggplot2::ggplot(expanded.df,
                      ggplot2::aes(x = .logit.leverage,
                                   y = .cooksd)) +
      ggplot2::geom_point(shape = shape) +
      ggplot2::geom_smooth(se = FALSE) +
      ggplot2::ylim(ylim) + #xlim(xlim) +
      ggplot2::labs(y = "Cook's distance",
                    x =  expression("Leverage  " * h[ii]),
                    title = getCaption(6)) +
      ggplot2::scale_x_continuous(limits = xlim,
                                  breaks = athat,
                                  labels = paste(athat))
    ## Label axis with h_ii values

    p <- length(stats::coef(x))
    bval <- pretty(sqrt(p * expanded.df$.cooksd /
                          expanded.df$.logit.leverage), 5)

    xmax <- xlim[2]
    ymax <- ylim[2]
    for (i in seq_along(bval)) {
      bi2 <- bval[i]^2
      if (ymax > bi2*xmax) {
        xi <- xmax - graphics::strwidth(" ",units = "figure")/3
        yi <- bi2*xi
        ggout.list$cooks.vs.logit.leverage <-
          ggout.list$cooks.vs.logit.leverage +
          ggplot2::geom_abline(intercept = 0,slope = bi2, linetype = 2,
                               color = dashed.color[6L]) +
          ggplot2::geom_label(data = data.frame(x = xi, y = yi,
                                                label = paste(bval[i])),
                              ggplot2::aes(x = x, y = y, label = label),
                              size = 4, color = dashed.color[6L])
      } else {
        yi <- ymax - graphics::strheight(" ", units = "figure")/5
        xi <- yi/bi2
        slope <- yi/xi
        ggout.list$cooks.vs.logit.leverage <-
          ggout.list$cooks.vs.logit.leverage +
          ggplot2::geom_abline(intercept = 0,slope =  slope, linetype = 2,
                               color = dashed.color[6L]) +
          ggplot2::geom_label(data = data.frame(x = xi,
                                                y = ymax - 0.5*graphics::strheight(" ",
                                                                         units = "figure"),
                                                label = paste(bval[i])),
                             ggplot2::aes(x = x, y = y, label = label),
                             size = 4, color = dashed.color[6L])
      }
    }


    if (id.n > 0) {
      ggout.list$cooks.vs.logit.leverage <- text.id(df = expanded.df,
                                                    x.string = ".logit.leverage",
                                                    y.string = ".cooksd",
                                                    ind.string = ".ordering.cooks",
                                                    id.n = id.n,
                                                    ggbase = ggout.list$cooks.vs.logit.leverage)
    }
  }

  if (show.plot) {
    gridExtra::grid.arrange(grobs = ggout.list, nrow = nrow, top = main,
                            bottom = sub.caption)
  }

  if (return) {
    return(list(data = expanded.df, gglist = ggout.list))
  }

}


#' Creates an augmented data frame for lm and glm objects (for \pkg{ggplot2}
#' visuals)
#'
#' Similar to a extended version of \code{broom::}\code{\link[broom]{augment}} for \code{lm}
#' and \code{glm} objects but with prepared for the diagnostic plots.
#'
#' @param x \code{lm} or \code{glm} object
#' @param labels.id labels for all observations
#'
#' @return augmented data.frame, see \code{details} for more information
#'
#' @details
#' \describe{
#'   \item{original data frame}{original data frame used to create lm or glm
#'   object}
#'   \item{.index}{row number 1 to \code{nrow(original data)}}
#'   \item{.labels.id}{provides labels or strings with same names as
#'   \code{.index}}
#'   \item{.weights}{weights from the model for each observation}
#'   \item{.yhat}{predicted values in \code{y} terms
#'   (not probablities, logit probabilities, log transformed, etc), as such,
#'   for glm this is different then \code{fitted(x)}}
#'   \item{.resid}{residuals between \code{.yhat} and \code{y}}
#'   \item{.leverage}{leverage for each observation, corresponds to the diagonal
#'   of the "hat" matrix (\eqn{diag(X(X^TX)^{-1}X)}. }
#'   \item{.cooksd}{Cook's Distance, if \code{lm}, then we use the estimated
#'   standard deviation to calculate the value. Cook's Distance is the a
#'   "leave-one-out" based diagnostic for linear and generalized linear models
#'   discussed in Belsley, Kuh and Welsch (1980), Cook and Weisberg (1982), etc}
#'   \item{.weighted.resid}{residuals weighted by the \code{.weights}, (i.e.
#'   \eqn{\sqrt{ .weights } \cdot .resid})}
#'   \item{.std.resid}{the standardized residuals using weighted.residuals
#'   and scaled by leverage and the estimated standard deviation, (i.e.
#'   \eqn{\frac{\code{.weighted.resid}}{std.deviation * (1 - \code{.leverage})}}
#'   )}
#'   \item{.sqrt.abs.resid}{the square-root of the absolute value of the
#'   standardized residuals}
#'   \item{.pearson.resid}{pearson residuals, residuals divided by the square
#'   root the variance}
#'   \item{.std.pearson.resid}{standardized pearson residuals, (i.e.
#'   \eqn{\frac{\code{.pearson.resid}}{std.deviation * (1- \code{.leverage})}})}
#'   \item{.logit.leverage}{logit of the leverage
#'   (i.e. \eqn{log(\frac{x}{1-x})})}
#'   \item{.ordering.resid}{Index ordering of residuals (in absolute value)}
#'   \item{.ordering.std.resid}{Index ordering of standardized residuals
#'   (in absolute value)}
#'   \item{.ordering.cooks}{Index ordering of cook's distance}
#'   \item{.non.extreme.leverage}{logical vector if leverage != 1
#'   (for extreme cases)}
#'   }
#'
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' lm.object <- lm(Sepal.Length ~., data = iris)
#'
#' dfCompile.lm(lm.object) %>% head
dfCompile.lm <- function(x, labels.id = factor(names(stats::residuals(x)),
                                             levels = names(stats::residuals(x)))) {

  # so that CRAN checks pass
  .std.resid <- .weights <- .resid <- .cooksd <- .leverage <- NULL

  # pkg requirements (for this function)
  missing.packages <- look.for.missing.packages(c("stats",
                                                  "scales"))
  # ^also requires base, dyplr

  if (length(missing.packages) > 0) {
    stop(paste0(c("Package(s) '",paste0(missing.packages, collapse = "', '"),
                  "' needed for this function to work. Please install them/it."),
                collapse = ""))
  }


  isGlm <- inherits(x, "glm")

  dropInf <- function(x, h) {
    if (any(isInf <- h >= 1.0)) {
      warning(gettextf("not plotting observations with leverage one:\n  %s",
                       paste(which(isInf), collapse = ", ")),
              call. = FALSE, domain = NA)
      x[isInf] <- NaN
    }
    x
  }

  # standard deviation
  s <- if (inherits(x, "rlm")) {
    x$s
  } else {
    if (isGlm) {
      sqrt(base::summary(x)$dispersion)
    }else{
      sqrt(stats::deviance(x)/stats::df.residual(x))
    }
  }

  output.df <- data.frame(x$model)
  names(output.df) = names(x$model)

  output.df <- output.df %>%
    dplyr::mutate(.index = 1:nrow(x$model),
                  .labels.id = labels.id,
                  .weights = if (is.null(stats::weights(x))) {
                      1
                    } else {
                      stats::weights(x)
                    },
                  .yhat = stats::predict(x), #!= fitted() for glm
                  .resid = stats::residuals(x),
                  .leverage = stats::lm.influence(x, do.coef = FALSE)$hat,
                  .pearson.resid = stats::residuals(x, "pearson")
    )

  output.df <- output.df %>%
    dplyr::mutate(.cooksd = if (isGlm) {
                              stats::cooks.distance(x)
                            } else {
                              stats::cooks.distance(x, sd = s,
                                                    res = output.df$.resid)
                            # note it isn't the same as basic
                            # cooks.distance(x)
                            },
                  .weighted.resid = sqrt(.weights) * .resid,
                  .std.pearson.resid = dropInf(output.df$.pearson.resid /
                                          (s * sqrt(1 - output.df$.leverage)),
                                          output.df$.leverage),
                  .logit.leverage = dropInf(output.df$.leverage /
                                              (1 - output.df$.leverage),
                                              output.df$.leverage)
    )

  output.df <- output.df %>%
    dplyr::mutate(.std.resid = dropInf(
                output.df$.weighted.resid/(s * sqrt(1 - output.df$.leverage)),
                output.df$.leverage),
                  .non.extreme.leverage = .leverage < 1)

  output.df <- output.df %>%
    dplyr::mutate(.sqrt.abs.resid = sqrt(abs(.std.resid)))

  n <- nrow(output.df)

  output.df$.ordering.cooks <- n
  output.df$.ordering.resid <- n
  output.df$.ordering.std.resid <- n

  output.df$.ordering.cooks[order(output.df$.cooksd, decreasing = TRUE)] <- 1:n
  output.df$.ordering.resid[order(abs(output.df$.resid),
                                  decreasing = TRUE)] <- 1:n
  output.df$.ordering.std.resid[order(abs(output.df$.std.resid),
                                      decreasing = TRUE)] <- 1:n

  # just prioritizing columns
  output.df <- output.df[,c(names(x$model),
                            ".index", ".labels.id",
                            ".weights",
                            ".yhat", ".resid", ".leverage", ".cooksd",
                            ".weighted.resid", ".std.resid", ".sqrt.abs.resid",
                            ".pearson.resid", ".std.pearson.resid",
                            ".logit.leverage",
                            ".ordering.resid", ".ordering.std.resid",
                            ".ordering.cooks",
                            ".non.extreme.leverage")]

  return(output.df)


}

