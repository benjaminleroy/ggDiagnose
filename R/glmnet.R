# plot.glmnet, plot.cv.glmnet replacements

#' Diagnostic plot for cv.glmnet object (ggplot based)
#'
#' This function creates a \code{\link[ggplot2]{ggplot}} graphic version of
#' \code{\link[glmnet]{plot.cv.glmnet}} for \code{\link[glmnet]{cv.glmnet}}
#' objects.
#'
#' @param x \code{cv.glmnet} object from library \code{\link[glmnet]{glmnet}}
#' @param sign.lambda what sign of \code{lambda} you'd like \eqn{\\pm 1}
#' @param color color of CV MSE average dot
#' @param dashed.color color of dashed lines for \code{1se} and \code{min}
#' \code{lambda} values
#' @param show.plot logic to display the plot
#' @param return logic to return list the graphic and the data frame to make
#' the majority of the graphic
#' @param ... extra attributes (currently not used)
#'
#' @return depending on \code{show.plot} and \code{return} it
#' will return the visualization of the graphic and/or a list
#' of both the data frame used the make the majority of the graphic and
#' the graphic object itself.
#'
#' @seealso \code{\link{dfCompile.cv.glmnet}} for data creation.
#'
#' @export
#'
#' @examples
#' cv.glmnet.object <- cv.glmnet(y = iris$Sepal.Length,
#'                               x = model.matrix(Sepal.Length~., data = iris))
#'
#' plot(cv.glmnet.object)
#'
#' ggDiagnose(cv.glmnet.object)
ggDiagnose.cv.glmnet <- function(x, sign.lambda = 1, color = "red",
                                 dashed.color = "red",
                                 show.plot = TRUE, return = FALSE, ...){

  missing.packages <- look.for.missing.packages(c("glmnet"))
  # ^also requires ggplot2

  if (length(missing.packages) > 0) {
    stop(paste0(c("Package(s) '",paste0(missing.packages, collapse = "', '"),
                  "' needed for this function to work. Please install them/it."),
                collapse = ""))
  }

  if (!any(show.plot, return)) {
    return(NULL)
  }

  xlabel <- "log(Lambda)"
  if (sign.lambda < 0) {
    xlabel <- paste("-", xlabel, sep = "")
  }

  expanded.df <- dfCompile.cv.glmnet(x)

  lambda.min <- x$lambda.min
  lambda.1se <- x$lambda.1se

  inner.function <- function(log.lambda){
    which <- sapply(log.lambda, function(x) which(x == expanded.df$.log.lambda))
    return(expanded.df$number.non.zero[expanded.df$.log.lambda == .log.lambda])
  }

  ggout <- ggplot2::ggplot(expanded.df, ggplot2::aes(y = cross.validated.error,
                          x = sign.lambda*.log.lambda)) +
    ggplot2::geom_point(color = color) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = cross.validation.upper.error,
                      ymax = cross.validation.lower.error)) +
    ggplot2::geom_vline(data = data.frame(x.line =
                                            c(sign.lambda*log(lambda.min),
                                              sign.lambda*log(lambda.1se))),
                        ggplot2::aes(xintercept = x.line),
               linetype = 2,
               color = dashed.color[1]) +
    ggplot2::labs(x = xlabel,
         y = x$name)

  if (sign.lambda == 1) {
    major.breaks <- expanded.df$.log.lambda[nrow(expanded.df):1][
      findInterval(as.numeric(ggplot2::ggplot_build(ggout)$layout$panel_params[[1]]$x.labels),
                   expanded.df$.log.lambda[nrow(expanded.df):1])
      ]
  } else if (sig.lambda == -1) {
    major.breaks <- expanded.df$.log.lambda[
      findInterval(as.numeric(ggplot2::ggplot_build(ggout)$layout$panel_params[[1]]$x.labels),
                                 -1*expanded.df$.log.lambda)
      ]
  }


  ggout <- ggout + ggplot2::scale_x_continuous(sec.axis =
                                                 ggplot2::sec_axis(~ . * 1,
                                          breaks = major.breaks,
                                          labels =
                                            sapply(major.breaks,
                                                   function(x) {
                                                  expanded.df$number.non.zero[
                                     expanded.df$.log.lambda == sign.lambda*x]}),
                                    name = "# non-zero coefficients"))

  if (show.plot) {
    print(ggout)
  }
  if (return) {
    return(list(data = expanded.df, ggout = ggout))
  }
}


#' Create visualization matrix for a cv.glmnet object
#'
#' @param x cv.glmnet object from library `glmnet`
#'
#' @return data frame, see details.
#'
#' @details
#' data frame returned has the following columns:
#' \describe{
#'   \item{.log.lambda}{the log of the values of \code{lambda} used in the fit}
#'   \item{cross.validated.error}{error accumulated over across k folds of the
#'   cross validation for each \code{.log.lambda}}
#'   \item{cross.validation.upper.error}{\code{cross.validation.error} plus an
#'   estimate of the standard error of the \code{cross.validation.error}
#'   relative to the K-fold CV}
#'   \item{cross.validation.lower.error}{\code{cross.validation.error} minus an
#'   estimate of the standard error of the \code{cross.validation.error}
#'   relative to the K-fold CV}
#'   \item{number.non.zero}{number of non-zero coeffiecients at each
#'   \code{.log.lambda}}
#'   }
#' @export
#'
#' @seealso \code{broom}'s \code{\link[broom]{tidy}} and use with the
#' same object.
#'
#' @examples
#' cv.glmnet.object <- cv.glmnet(y = iris$Sepal.Length,
#'                               x = model.matrix(Sepal.Length~., data = iris))
#'
#' dfCompile.cv.glmnet(cv.glmnet.object) %>% head
dfCompile.cv.glmnet <- function(x){
  expanded.df <- data.frame(
    `cross.validated.error` = x$cvm,
    `cross.validation.upper.error` = x$cvup,
    `cross.validation.lower.error` = x$cvlo,
    `number.non.zero` = x$nz,
    `.log.lambda` = log(x$lambda)
  )

  return(expanded.df)
}

#' Diagnostic plot for glmnet object (ggplot based)
#'
#' #' This function creates a \code{\link[ggplot2]{ggplot}} graphic version of
#' \code{\link[glmnet]{plot.glmnet}} for \code{\link[glmnet]{glmnet}}
#' objects.
#'
#' @param x \code{glmnet} object from \code{\link[glmnet]{glmnet}} library
#' @param xvar string for x axis variable (see details)
#' @param label logic to label each beta value's line with an integer value
#' @param show.plot logic to display the plot
#' @param return logic to return list the graphic and the data frame to make
#' the majority of the graphic
#' @param ... extra attributes (currently not used)
#'
#' @return depending on \code{show.plot} and \code{return} it
#' will return the visualization of the graphic and/or a list
#' of both the data frame used the make the majority of the graphic and
#' the graphic object itself.
#' @export
#'
#' @details
#' For \code{xvar} you have the option of selecting
#' \describe{
#'   \item{\code{"norm"}}{The \eqn{L_1} norm of the beta values for each
#'   specific \code{lambda}. That is, \eqn{\sum_{i=1}^p beta_i(\lambda)}}
#'   \item{\code{"lambda"}}{The \code{lambda} value examined}
#'   \item{\code{"dev"}}{The deviance ratio, the fraction of (null) deviance
#'   explained by the model at each specific \code{lambda} examined. The
#'   deviance is defined to be \eqn{2*(loglike_sat - loglike)}, where
#'   \eqn{loglike_sat} is the log-likelihood for the saturated model
#'   (a model with a free parameter per observation). Hence
#'   \eqn{.dev=1-dev/nulldev}.}
#' }
#'
#' This will be the value of the variable on the x axis.
#'
#' @seealso see \code{\link{dfCompile.glmnet}} for data creation.
#'
#' @examples
#' glmnet.object <- glmnet(y = iris$Sepal.Length,
#'                         x = model.matrix(Sepal.Length~., data = iris))
#'
#' plot(glmnet.object)
#'
#' ggDiagnose.glmnet(glmnet.object)
ggDiagnose.glmnet <- function(x, xvar = c("norm","lambda","dev"), label = FALSE,
                              show.plot = TRUE, return = FALSE, ...){

  missing.packages <- look.for.missing.packages(c("glmnet"))
  # ^also requires ggplot2, dplyr, reshape2

  if (length(missing.packages) > 0) {
    stop(paste0(c("Package(s) '",paste0(missing.packages, collapse = "', '"),
                  "' needed for this function to work. Please install them/it."),
                collapse = ""))
  }

  xvar <- match.arg(xvar)
  which <- glmnet::nonzeroCoef(x$beta)
  nwhich <- length(which)
  switch(nwhich + 1, #we add one to make switch work
         "0" = {
           warning("No plot produced since all coefficients zero")
           return()
         },
         "1" = warning("1 or less nonzero coefficients; glmnet plot is not meaningful")
  )
  vis.df <- dfCompile.glmnet(x)



  if (label) {
    vis.df.last <- vis.df %>%
      filter(.log.lambda == min(.log.lambda)) %>%
      dplyr::mutate(.variable.num = as.numeric(factor(variable,
                                                     levels = rownames(x$beta))))
    }

  xvar <- xvar[1]
  if (xvar == "norm") {
    xvar <- ".norm"
    xlabel <- "L1 Norm"
    approx.f <- 1
  } else if (xvar == "lambda") {
    xvar <- ".log.lambda"
    xlabel <- "Log Lambda"
    approx.f <- 0
  } else if (xvar == "dev") {
    xvar <- ".dev"
    xlabel <- "Fraction Deviance Explained"
    approx.f <- 1
  }

  ggout <- ggplot2::ggplot(vis.df, aes_string(x = xvar,
                            y = "beta.value",
                            color = "variable")) +
    ggplot2::geom_line() + ggplot2::theme(legend.position = "none")  +
    ggplot2::labs(x = xlabel,
         y = "Coefficients")

  if (label) {
    ggout <- ggout + ggplot2::geom_label(data = vis.df.last,
                                         ggplot2::aes_string(x = xvar,
                                           y = "beta.value",
                                           color = "variable",
                                           label = "variable.num"),
                                hjust = 1, size = 3,
                                alpha = .3)
  }


  # breaks for upper x axis labels
  major.breaks <- sort(unique(vis.df[nrow(vis.df):1,xvar]))[
    findInterval(
      as.numeric(
        ggplot2::ggplot_build(ggout)$layout$panel_params[[1]]$x.labels),
      sort(unique(vis.df[nrow(vis.df):1,xvar])))]


  ggout <- ggout + ggplot2::scale_x_continuous(sec.axis =
                                                 ggplot2::sec_axis(~ . * 1,
                                                 breaks = major.breaks,
                                                 labels =
                                                   sapply(major.breaks,
                                                    function(x) {
                                                    vis.df$.number.non.zero[
                                                      vis.df[,xvar] == x]})[1,],
                                              name = "# non-zero coefficients"))

  if (show.plot) {
    print(ggout)
  }
  if (return) {
    return(list(data = vis.df, ggout = ggout))
  }
  }

#' Create visualization matrix for a glmnet object
#'
#' For the graphic one could consider this just a smart "melting" (
#' \code{reshape2}'s \code{\link[reshape2]{melt}}) of the beta matrix in the
#' \code{glmnet} object.
#'
#' @param x \code{glmnet} object from \code{glmnet} library
#'
#' @return data frame, see details.
#'
#' @details
#' data frame returned has the following columns:
#' \describe{
#' \item{.log.lambda}{\eqn{log(lambda)} for the lambda values examined}
#'   \item{variable}{name of variable in the model}
#'   \item{beta.value}{for the specific \code{variable} when \eqn{log(lambda)}
#'   was \code{.log.lambda}}
#'   \item{.norm}{The \eqn{L_1} norm of the beta values for specific each
#'   \code{.log.lambda}. That is, \eqn{\sum_{i=1}^p beta_i(\lambda)}}
#'   \item{.dev}{deviance ratio, the fraction of (null) deviance explained by
#'   the model. The deviance is defined to be \eqn{2*(loglike_sat - loglike)},
#'   where \eqn{loglike_sat} is the log-likelihood for the saturated model
#'   (a model with a free parameter per observation). Hence
#'   \eqn{.dev=1-dev/nulldev}.}
#'   \item{.number.non.zero}{number of beta for that \code{.log.lambda} value
#'   that were non-zero}
#'   }
#'
#' @seealso \code{broom}'s \code{\link[broom]{tidy}} and use with the
#' same object.
#'
#' @export
#'
#' @examples
#' library(tidyverse)
#' glmnet.object <- glmnet(y = iris$Sepal.Length,
#'                         x = model.matrix(Sepal.Length~., data = iris))
#'
#' dfCompile.glmnet(glmnet.object) %>% head
dfCompile.glmnet <- function(x){
  beta <- as.matrix(x$beta[,,drop = FALSE])
  vis.df <- beta %>% t %>% data.frame %>%
    dplyr::mutate(.norm = apply(abs(beta), 2, sum),
                  .log.lambda = log(x$lambda),
                  .dev = x$dev.ratio,
                  .number.non.zero = apply(beta != 0, 2, sum)) %>%
    reshape2::melt(id.vars = c(".norm",
                               ".log.lambda",
                               ".dev",
                               ".number.non.zero")) %>%
    dplyr::rename(beta.value = value) %>%
    dplyr::select(.log.lambda, variable, beta.value,
           .norm, .dev, .number.non.zero)

  return(vis.df)
}
