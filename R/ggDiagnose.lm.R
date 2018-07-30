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
#' @param show_plot logic to display the graphics (group of graphics in this
#' case)
#' @param return logic to return list of graphics and the data frame to make
#' the majority of graphics
#' @param shape shape of points (default is 1, open circle)
#' @param nrow number of rows in the displayed graphic
#' @param smooth_color color for smoothing lines
#' @param dashed_color color for dashed line (a vector of length 6 is expected)
#'
#' @return depending on \code{show_plot} and \code{return} it
#' will return the visualization of the graphics and/or a list
#' of both the data frame used the make the majority of the graphic and
#' a list of each individual graphic.
#' @export
#'
#' @example
#' lm_object <- lm(Sepal.Length ~., data = iris)
#' ggDiagnose.lm(lm_object, which = 1:6)
#' plot(lm_object, which = 1:6)
ggDiagnose.lm <- function(x, which = c(1L:3L,5L), ## was which = 1L:4L,
                          caption = list("Residuals vs Fitted", "Normal Q-Q",
                                         "Scale-Location", "Cook's distance",
                                         "Residuals vs Leverage",
                                         expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))),
                          sub.caption = NULL, main = NULL,
                          ...,
                          id.n = 3, labels.id = factor(names(residuals(x)),
                                                       levels = names(residuals(x))),
                          qqline = TRUE, cook.levels = c(0.5, 1.0),
                          # new attributes
                          show_plot = TRUE, return = FALSE,
                          shape = 1,
                          nrow = 2, smooth_color = "blue",
                          dashed_color = c("red", "blue", NA, NA, "red", "black")){

  # ATTN: look at difference in .std.resid and .std.resid2

  # make a warning message if any of the base functionality that we don't use
  # is used.

  # rewrite with a mutate for all desired added elements, check augment with glms and rlm

  # pkg requirements (for this function)
  missing_packages <- look_for_missing_packages(c("stats",
                                                  "grDevices",
                                                  "graphics"))
  # ^also requires ggplot2, base, dyplr, gridExtra

  if (length(missing_packages) > 0) {
    stop(paste0(c("Package(s) '",paste0(missing_packages, collapse = "', '"),
              "' needed for this function to work. Please install them/it."),
              collapse = ""))
  }

  if (!any(show_plot, return)) {
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

  expanded_df <- augment.lm(x, labels.id = labels.id) %>% filter(.weights != 0)

  if (any(show[2L:3L])) {
    ylab23 <- ifelse(isGlm,
                     "Std. deviance resid.",
                     "Standardized residuals")
  }

  if (any(show[5L:6L])) { # using 'leverages'
    range_hat <- range(expanded_df$.leverage, na.rm = TRUE) # though should never have NA
    isConst.hat <- all(range_hat == 0) ||
      diff(range_hat) < 1e-10 * mean(expanded_df$.leverage, na.rm = TRUE)
  }
  if (any(show[c(1L, 3L)])) {
    l.fit <- ifelse(isGlm, "Predicted values", "Fitted values")
  }


  if (id.n > 0L) { ## label the largest residuals
    # ATTN: needs to be updated: (need to focus on adj.x and label.pos, etc)
    text.id <- function(df, x_string, y_string, ind_string, id.n,
                        ggbase = ggplot2::ggplot()) {
      df_inner <- df[df[,ind_string] <= id.n,]
      ggout <- ggbase +
        ggplot2::geom_text(data = df_inner,
                           ggplot2::aes_string(x = x_string,
                                               y = y_string,
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

  ggout_list <- list()
  ##---------- Do the individual plots : ----------
  if (show[1L]) {
    ggout_list$residual_vs_yhat <-
      ggplot2::ggplot(expanded_df,
                     ggplot2::aes(x = `.yhat`,
                         y = `.resid`)) +
      ggplot2::geom_point(shape = shape) +
      ggplot2::labs(x = l.fit, y = "Residuals",
           title = getCaption(1)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2,
                          color = dashed_color[1]) +
      ggplot2::geom_smooth(se = F, color = "blue")
    #ATTN: probably need to come back for the title & captions, etc
    if (id.n > 0) {
      ggout_list$residual_vs_yhat <- text.id(df = expanded_df,
                                               x_string = ".yhat",
                                               y_string = ".resid",
                                               ind_string = ".ordering.resid",
                                               id.n = id.n,
                                               ggbase = ggout_list$residual_vs_yhat)

    }
  }
  if (show[2L]) { ## Normal QQ
    ggout_list$qqnorm <- ggplot2::ggplot(expanded_df) +
      ggplot2::geom_qq(ggplot2::aes(sample = .std.resid),
                       distribution = stats::qnorm,
                       shape = shape) +
      ggplot2::labs(title = getCaption(2),
           y = ylab23,
           x = "Theoretical Quantiles")

    if (qqline) {
      ggout_list$qqnorm <- ggout_list$qqnorm +
        ggplot2::geom_qq_line(ggplot2::aes(sample = .std.resid),
                     distribution = stats::qnorm,
                     linetype = 2, col = dashed_color[2L])
    }
    if (id.n > 0) {
      qq <- stats::qqnorm(expanded_df$.std.resid,plot.it = FALSE) %>%
        data.frame() %>%
        mutate(.ordering.resid = expanded_df$.ordering.resid,
               .labels.id = expanded_df$.labels.id)

      ggout_list$qqnorm <- text.id(df = qq,
              x_string = "x",
              y_string = "y",
              id.n = id.n,
              ind_string = ".ordering.resid",
              ggbase =  ggout_list$qqnorm)
    }
  }
  if (show[3L]) {
    yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = as.name(ylab23))))

    ggout_list$sqrt_abs_resid <-
      ggplot2::ggplot(expanded_df,
             ggplot2::aes(x = .yhat,
                 y = .sqrt.abs.resid)) +
      ggplot2::geom_point(shape = shape) +
      ggplot2::labs(x = l.fit,
           y = yl,
           title = getCaption(3)) +
      ggplot2::geom_smooth(se = FALSE, color = "blue")
    if (id.n > 0) {
      ggout_list$sqrt_abs_resid <- text.id(df = expanded_df,
                                             x_string = ".yhat",
                                             y_string = ".sqrt.abs.resid",
                                             ind_string = ".ordering.std.resid",
                                             id.n = id.n,
                                             ggbase = ggout_list$sqrt_abs_resid)
    }


  }
  if (show[4L]) { ## Cook's Distances
    ggout_list$cooks <- ggplot2::ggplot(expanded_df,
                                        aes(y = .cooksd,
                                            x = .index)) +
      ggplot2::geom_segment(aes(x = .index,
                                xend = .index,
                                y = 0,
                                yend = .cooksd)) +
      ggplot2::theme(axis.text.x = element_text(angle = 90)) +
      ggplot2::labs(x = "Observation Number",
                    y = "Cook's distance",
                    title = getCaption(4))
    if (id.n > 0) {
      ggout_list$cooks <- text.id(df = expanded_df,
                                  x_string = ".index",
                                  y_string = ".cooksd",
                                  ind_string = ".ordering.cooks",
                                  id.n = id.n,
                                  ggbase =  ggout_list$cooks)
    }
  }
  if (show[5L]) {
    ylab5 <- ifelse(isGlm, "Std. Pearson resid.", "Standardized residuals")

    if (isConst.hat) { ## leverages are all the same
      if (missing(caption)) { # set different default
        caption[[5L]] <- "Constant Leverage:\n Residuals vs Factor Levels"
      }
      ## plot against factor-level combinations instead
      aterms <- attributes(terms(x))
      ## classes w/o response
      dcl <- aterms$dataClasses[ -aterms$response ] #
      facvars <- names(dcl)[dcl %in% c("factor", "ordered")]
      mf <- model.frame(x)[facvars]# better than x$model
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

        data_vis <-
          data.frame(.facval = facval,
                     .std.pearson.resid = expanded_df$.std.pearson.resid,
                     .show.cooks = expanded_df$.ordering.cooks,
                     .labels.id = expanded_df$.labels.id)

        ggout_list$residual_vs_leverage <- ggplot2::ggplot(data_vis,
                                                      ggplot2::aes(x = .facval,
                                                      y = .std.pearson.resid)) +
          ggplot2::geom_point(shape = shape) +
          ggplot2::geom_vline(data = data.frame(v = ff[1L]*(0:nlev[1L]) - 1/2),
                              ggplot2::aes(xintercept = v),
                              color = dashed_color[5],
                              linetype = "F4") +
          ggplot2::scale_x_continuous(breaks = ff[1L]*(1L:nlev[1L] - 1/2) - 1/2,
                                      labels = x$xlevels[[1L]]) +
          ggplot2::labs(x = paste0("Factor Level Combinations\n(Major: ",
                                   facvars[1L],")")) +
          ggplot2::geom_hline(yintercept = 0, lty = 2,
                              color = dashed_color[5]) +
          ggplot2::geom_smooth(se = FALSE, color = "blue")

        if (id.n > 0) {
          ggout_list$residual_vs_leverage <- text.id(df = data_vis,
                                        x_string = ".facval",
                                        y_string = ".std.pearson.resid",
                                        ind_string = ".ordering.cooks",
                                        id.n = id.n,
                                        ggbase =  ggout_list$residual_vs_leverage)
        }
      } else {# no factors
        message(gettextf("hat values (leverages) are all = %s\n and there are no factor predictors; no plot no. 5",
                         format(mean(range_hat))),
                domain = NA)
      }

    } else {## Residual vs Leverage
      ## omit hatvalues of 1.
      ylim <- range(expanded_df[expanded_df$.non.extreme.leverage,".std.pearson.resid"])
      xlim <- range(expanded_df[expanded_df$.non.extreme.leverage,".leverage"])

      ggout_list$residual_vs_leverage <- ggplot2::ggplot(expanded_df[expanded_df$.non.extreme.leverage,],
                                      ggplot2::aes(x = .leverage,
                                          y = .std.pearson.resid)) +
        ggplot2::geom_point(shape = shape) +
        ggplot2::geom_vline(xintercept = 0, linetype = 2,
                            color = dashed_color[5L]) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2,
                            color = dashed_color[5L]) +
        ggplot2::geom_smooth(se = FALSE, color = "blue") +
        ggplot2::labs(x = "Leverage",
             y = ylab5,
             title = getCaption(5))

      if (length(cook.levels)) {
        data_cooks <- data.frame(`.leverage` = -1,
                                 `.std.pearson.resid` = 0,
                                 legend = "blank", group = "blank") %>%
          dplyr::mutate(legend = as.character(legend),
                 group = as.character(group))

        p <- length(coef(x))

        hh <- seq.int(min(range_hat[1L], range_hat[2L]/100), range_hat[2L],
                      length.out = 101)

        for (crit in cook.levels) {
          cl.h <- sqrt(crit * p * (1 - hh) / hh)

          data_cooks <- rbind(data_cooks,
                              data.frame(`.leverage` = rep(hh, times = 2),
                                         `.std.pearson.resid` = c(cl.h, -cl.h),
                                         legend = "Cook's distance",
                                         group = c(rep(paste0(crit,"_upper"), length(hh)),
                                                     rep(paste0(crit,"_lower"), length(hh)))
                              )
          )
        }
        data_cooks <- data_cooks[-1,]

        ggout_list$residual_vs_leverage <- ggout_list$residual_vs_leverage +
          ggplot2::geom_path(data = data_cooks,
                    ggplot2::aes(x = .leverage,
                        y = .std.pearson.resid,
                        group = group),
                    color = dashed_color[5L], linetype = 2) +
          ggplot2::xlim(c(0,xlim[2])) +
          ggplot2::ylim(ylim)
      }
      #print(expanded_df$.show.leverage)
      if (id.n > 0) {
      ggout_list$residual_vs_leverage <- text.id(df = expanded_df[expanded_df$.non.extreme.leverage,],
              x_string = ".leverage",
              y_string = ".std.pearson.resid",
              ind_string = ".ordering.cooks",
              id.n = id.n,
              ggbase =  ggout_list$residual_vs_leverage)
      }

    }
  }
  if (show[6L]) {

    ylim <- c(0, max(expanded_df$.cooksd))
    xlim <- c(0, max(expanded_df$.logit.leverage))

    athat <- pretty(expanded_df$.leverage)

    ggout_list$cooks_vs_logit_leverage <-
      ggplot2::ggplot(expanded_df,
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
    bval <- pretty(sqrt(p * expanded_df$.cooksd /
                          expanded_df$.logit.leverage), 5)

    xmax <- xlim[2]
    ymax <- ylim[2]
    for (i in seq_along(bval)) {
      bi2 <- bval[i]^2
      if (ymax > bi2*xmax) {
        xi <- xmax - graphics::strwidth(" ",units = "figure")/3
        yi <- bi2*xi
        ggout_list$cooks_vs_logit_leverage <-
          ggout_list$cooks_vs_logit_leverage +
          ggplot2::geom_abline(intercept = 0,slope =  bi2, linetype = 2,
                               color = dashed_color[6L]) +
          ggplot2::geom_label(data = data.frame(x = xi, y = yi,
                                                label = paste(bval[i])),
                              ggplot2::aes(x = x, y = y, label = label),
                              size = 4, color = dashed_color[6L])
      } else {
        yi <- ymax - graphics::strheight(" ", units = "figure")/5
        xi <- yi/bi2
        slope <- yi/xi
        ggout_list$cooks_vs_logit_leverage <-
          ggout_list$cooks_vs_logit_leverage +
          ggplot2::geom_abline(intercept = 0,slope =  slope, linetype = 2,
                               color = dashed_color[6L]) +
          ggplot2::geom_label(data = data.frame(x = xi,
                                                y = ymax - 0.5*graphics::strheight(" ",
                                                                         units = "figure"),
                                                label = paste(bval[i])),
                             ggplot2::aes(x = x, y = y, label = label),
                             size = 4, color = dashed_color[6L])
      }
    }


    if (id.n > 0) {
      ggout_list$cooks_vs_logit_leverage <- text.id(df = expanded_df,
                                                    x_string = ".logit.leverage",
                                                    y_string = ".cooksd",
                                                    ind_string = ".ordering.cooks",
                                                    id.n = id.n,
                                                    ggbase = ggout_list$cooks_vs_logit_leverage)
    }
  }

  if (show_plot) {
    gridExtra::grid.arrange(grobs = ggout_list, nrow = nrow, top = main,
                            bottom = sub.caption)
  }

  if (return) {
    return(list(data = expanded_df, gglist = ggout_list))
  }

}

  ## The following should be in the example, but more of a
  ## "how to do it yourself" <- maybe with the data.frame that we create instead?
  ##
  ## that is to say- I'm not sure if we should show example code

  # x <- lm(Sepal.Length ~., data = iris)
  #
  # df <- broom::augment(x)
  # res_vs_fit <- ggplot(df,
  #                      aes(x = .fitted, y = .resid)) +
  #   geom_point(pch = 1) +
  #   geom_smooth(se = F, color = "red", size = .5) +
  #   labs(x = "Fitted values",
  #        y = "Residuals",
  #        title = "Residuals vs Fitted")
  #
  # qq_norm <- ggplot(df, aes(sample = .resid)) +
  #   geom_qq(distribution = stats::qnorm) +
  # geom_qq_line(linetype = "dashed") +
  #   labs(x = "Theoretical Quantiles",
  #        y = "Standardized Residuals",
  #        title = "Normal Q-Q")
  #
  # sqrt_st_res_vs_fit <- ggplot(diag_data, aes(x = fitted, y = sqrt_st_res)) +
  #   geom_point() + geom_smooth(se = F)
  #
  # st_resid_vs_leverage <- ggplot(diag_data, aes(x = leverage, y = st_resid,
  #                                               color = cooks)) +
  #   geom_point() +
  #   geom_smooth(se = F)
  #
  #
  # grid.arrange(fit_vs_y, ggplot(),
  #              res_vs_fit, qq_norm,
  #              sqrt_st_res_vs_fit, st_resid_vs_leverage, nrow = 3)


# quartz(); ggDiagnose.lm(lm(Sepal.Width ~ ., data = iris),which = 1:6, smooth_color = "red", dashed_color = rep("orange",6))
# quartz(); par(mfrow = c(2,3)); plot(lm(Sepal.Width ~ ., data = iris),which = 1:6)
#
#
# ggDiagnose.lm(lm(Sepal.Width ~ ., data = iris))


#' Creates an augmented data frame for lm and glm objects (for \pkg{ggplot2}
#' visuals)
#'
#' Similar to a extended version of \code{\link[broom]{broom::augment}} for \code{lm}
#' and \code{glm} objects but with prepared for the diagnostic plots.
#'
#' @param x \code{lm} or \code{glm} object
#' @param labels.id labels for all observations
#'
#' @return augmented data.frame, see \code{details} for more information
#'
#' @details
#' \itemize{
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
#'   \item{.leverage}{leverage for each observation, corresponds to the
#'   $diag(XX^T)$ NOTE: GIVE BETTER DESCRIPTION}
#'   \item{.cooksd}{Cook's Distance, if \code{lm}, then we use the estimated
#'   standard deviation to calculate the value }
#'   \item{.weighted.resid}{residuals weighted by the \code{.weights}, (i.e.
#'   $\sqrt{\code{.weights}} \cdot \code{.resid}$)}
#'   \item{.std.resid}{the standardized residuals using weighted.residuals
#'   and scaled by leverage and the estimated standard deviation, (i.e.
#'   $\frac{\code{.weighted.resid}}{std.deviation \cdot (1 - \code{.leverage})}
#'   )$}
#'   \item{.sqrt.abs.resid}{the square-root of the absolute value of the
#'   standardized residuals}
#'   \item{.pearson.resid}{pearson residuals, residuals weighted using the
#'   pearson formula NOTE: GIVE BETTER DESCRIPTION}
#'   \item{.std.pearson.resid}{standardized pearson residuals, (i.e.
#'   $\frac{\code{.pearson.resid}}{std.deviation \cdot (1- \code{.leverage})})}
#'   \item{.logit.leverage}{logit (i.e. $log(\frac{x}{1-x})$) of the leverage}
#'   \item{.ordering.resid}{Index ordering of residuals (in absolute value)}
#'   \item{.ordering.std.resid}{Index ordering of above standardized residuals
#'   (in absolute value)}
#'   \item{.ordering.cooks}{Index ordering of largest cook's distance}
#'   \item{.non.extreme.leverage}{logical vector if leverage != 1}
#'   }
#' @export
#'
#' @examples
#' lm_object <- lm(Sepal.Length ~., data = iris)
#' augment.lm(lm_object)
augment.lm <- function(x, labels.id = factor(names(residuals(x)),
                                             levels = names(residuals(x)))) {

  # pkg requirements (for this function)
  missing_packages <- look_for_missing_packages(c("stats"))
  # ^also requires base, dyplr

  if (length(missing_packages) > 0) {
    stop(paste0(c("Package(s) '",paste0(missing_packages, collapse = "', '"),
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

  output_df <- data.frame(x$model) %>%
    dplyr::mutate(.index = 1:nrow(x$model),
                  .labels.id = labels.id,
                  .weights = if (is.null(stats::weights(x))) {
                      1
                    } else {
                      stats::weights(x)
                    },
                  .yhat = predict(x), #!= fitted() for glm
                  .resid = stats::residuals(x),
                  .leverage = stats::lm.influence(x, do.coef = FALSE)$hat,
                  .pearson.resid = stats::residuals(x, "pearson")
    )

  output_df <- output_df %>%
    dplyr::mutate(.cooksd = if (isGlm) {
                              stats::cooks.distance(x)
                            } else {
                              stats::cooks.distance(x, sd = s,
                                                    res = output_df$.resid)
                            # note it isn't the same as basic
                            # cooks.distance(x)
                            },
                  .weighted.resid = sqrt(.weights) * .resid,
                  .std.pearson.resid = dropInf(output_df$.pearson.resid /
                                          (s * sqrt(1 - output_df$.leverage)),
                                          output_df$.leverage),
                  .logit.leverage = dropInf(expanded_df$.leverage /
                                              (1 - expanded_df$.leverage),
                                              expanded_df$.leverage)
    )

  output_df <- output_df %>%
    dplyr::mutate(.std.resid = dropInf(
                output_df$.weighted.resid/(s * sqrt(1 - output_df$.leverage)),
                output_df$.leverage),
                  .non.extreme.leverage = .leverage < 1)

  output_df <- output_df %>%
    dplyr::mutate(.sqrt.abs.resid = sqrt(abs(.std.resid)))

  n <- nrow(output_df)

  output_df$.ordering.cooks <- n
  output_df$.ordering.resid <- n
  output_df$.ordering.std.resid <- n

  output_df$.ordering.cooks[order(output_df$.cooksd, decreasing = TRUE)] <- 1:n
  output_df$.ordering.resid[order(abs(output_df$.resid),
                                  decreasing = TRUE)] <- 1:n
  output_df$.ordering.std.resid[order(abs(output_df$.std.resid),
                                      decreasing = TRUE)] <- 1:n

  # just prioritizing columns
  output_df <- output_df[,c(names(x$model),
                            ".index", ".labels.id",
                            ".weights",
                            ".yhat", ".resid", ".leverage", ".cooksd",
                            ".weighted.resid", ".std.resid", ".sqrt.abs.resid",
                            ".pearson.resid", ".std.pearson.resid",
                            ".logit.leverage",
                            ".ordering.resid", ".ordering.std.resid",
                            ".ordering.cooks",
                            ".non.extreme.leverage")]

  return(output_df)


}

# x <- lm(Sepal.Length ~., data = iris)
# broom::augment(x) %>% names
