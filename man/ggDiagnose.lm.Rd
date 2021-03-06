% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm.R
\name{ggDiagnose.lm}
\alias{ggDiagnose.lm}
\title{Diagnostic plot for lm and glm objects (ggplot based)}
\usage{
ggDiagnose.lm(x, which = c(1L:3L, 5L),
  caption = list("Residuals vs Fitted", "Normal Q-Q", "Scale-Location",
  "Cook's distance", "Residuals vs Leverage",
  expression("Cook's dist vs Leverage  " * h[ii]/(1 - h[ii]))),
  sub.caption = NULL, main = NULL, ..., id.n = 3,
  labels.id = factor(names(stats::residuals(x)), levels =
  names(stats::residuals(x))), qqline = TRUE, cook.levels = c(0.5, 1),
  show.plot = TRUE, return = FALSE, shape = 1, nrow = min(2,
  length(which)), smooth.color = "blue", dashed.color = c("red",
  "blue", NA, NA, "red", "black"))
}
\arguments{
\item{x}{\code{lm} or \code{glm} object}

\item{which}{which plots you'd like to create}

\item{caption}{title per plot}

\item{sub.caption}{caption for bottom of multiple plot visual (defaults
to the formula of the model.)}

\item{main}{title for the mulitple plot visual}

\item{...}{extra attributes (currently not used)}

\item{id.n}{id the n observations with largest residuals}

\item{labels.id}{labels for all observations}

\item{qqline}{logic for whether a qqline should be drawn (a line between the
25 and 75 quantiles in the Q-Q plot)}

\item{cook.levels}{levels of cooks distance to visualize in the leverage vs
standardized residual graphic}

\item{show.plot}{logic to display the graphics (group of graphics in this
case)}

\item{return}{logic to return list of graphics and the data frame to make
the majority of graphics}

\item{shape}{shape of points (the default is 1, an open circle)}

\item{nrow}{number of rows in the displayed graphic}

\item{smooth.color}{color for smoothing lines}

\item{dashed.color}{color for dashed line (a vector of length 6 is expected)}
}
\value{
depending on \code{show.plot} and \code{return} it
will return the visualization of the graphics and/or a list
of both the data frame used the make the majority of the graphic and
a list of each individual graphic.
}
\description{
This function leverages code from the \code{\link[stats]{plot.lm}} function
from the \pkg{stats} library. It allows for the same imput (except when
related directly to `par` functionality, which makes no sense to include).
We also include "newer" functionality.
}
\details{
This function can deal with \code{\link[stats]{lm}} and
\code{\link[stats]{glm}} objects.
}
\examples{
lm.object <- lm(Sepal.Length ~., data = iris)

par(mfrow = c(2,3))
plot(lm.object, which = 1:6)

ggDiagnose.lm(lm.object, which = 1:6)

}
\seealso{
see \code{\link{dfCompile.lm}} for data creation.
}
