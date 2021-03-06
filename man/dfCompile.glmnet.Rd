% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glmnet.R
\name{dfCompile.glmnet}
\alias{dfCompile.glmnet}
\title{Create visualization matrix for a glmnet object}
\usage{
dfCompile.glmnet(x)
}
\arguments{
\item{x}{\code{glmnet} object from \code{glmnet} library}
}
\value{
data frame, see details.
}
\description{
For the graphic one could consider this just a smart "melting" (
\code{reshape2}'s \code{\link[reshape2]{melt}}) of the beta matrix in the
\code{glmnet} object.
}
\details{
data frame returned has the following columns:
\describe{
\item{.log.lambda}{\eqn{log(lambda)} for the lambda values examined}
  \item{variable}{name of variable in the model}
  \item{beta.value}{for the specific \code{variable} when \eqn{log(lambda)}
  was \code{.log.lambda}}
  \item{.norm}{The \eqn{L_1} norm of the beta values for specific each
  \code{.log.lambda}. That is, \eqn{\sum_{i=1}^p beta_i(\lambda)}}
  \item{.dev}{deviance ratio, the fraction of (null) deviance explained by
  the model. The deviance is defined to be \eqn{2*(loglike_sat - loglike)},
  where \eqn{loglike_sat} is the log-likelihood for the saturated model
  (a model with a free parameter per observation). Hence
  \eqn{.dev=1-dev/nulldev}.}
  \item{.number.non.zero}{number of beta for that \code{.log.lambda} value
  that were non-zero}
  }
}
\examples{
library(tidyverse)
library(glmnet)
glmnet.object <- glmnet(y = iris$Sepal.Length,
                        x = model.matrix(Sepal.Length~., data = iris))

dfCompile.glmnet(glmnet.object) \%>\% head
}
\seealso{
\code{broom}'s \code{\link[broom]{tidy}} and use with the
same object.
}
