# plot.tree from the tree package
# we will  use ggdendro for tree plotting

#' Diagnostic plot for \code{tree} object (\code{ggplot2} based)
#'
#' Utilizing work from Andie de Vries's function
#' \code{\link[ggdendro]{dendro_data.tree}} from the \pkg{ggdendro} package
#' which is used in the \code{dfCompile} to create a list of data frames.
#'
#' @param x an object of class \code{tree} from the \pkg{tree}.
#' @param type character string. If "uniform", the branches are of uniform
#' length (only shows depth). Otherwise they are proportional to the decrease
#' in impurity.
#' @param split.labels if TRUE (the default), non-leaf nodes are labeled with
#' splitting rule
#' @param node.labels if TRUE, leaves are labeled with predicted value
#' (the default is FALSE).
#' @param text.size integer size for \code{link[ggplot2]{geom_text}} for labels.
#' @param ... extra attributes (currently not used)
#' @param show.plot logic to display the graphics (group of graphics in this
#' case)
#' @param return logic to return list of graphics and the data frame to make
#' the majority of graphics
#'
#' @return depending on \code{show.plot} and \code{return} it
#' will return the visualization of the graphic and/or a list
#' of both the list of data frames used the make the graphic and the
#' individual graphic object.
#' @export
#'
#' @examples
#' library(tree)
#'
#' tree.object <- tree(Species ~., data = iris)
#'
#' plot(tree.object)
#'
#' ggDiagnose.tree(tree, split.labels = FALSE)
ggDiagnose.tree <- function(x, type = c("proportional", "uniform"),
                            split.labels = TRUE, node.labels = FALSE,
                            text.size = 3,
                            ...,
                            show.plot = TRUE, return = FALSE) {
  missing.packages <- look.for.missing.packages(c("ggdendro"))
  # ^also requires ggplot2, base, dyplr

  if (length(missing.packages) > 0) {
    stop(paste0(c("Package(s) '",paste0(missing.packages, collapse = "', '"),
                  "' needed for this function to work. Please install them/it."),
                collapse = ""))
  }

  compiled.df.list <- dfCompile.tree(x, type)

  ggout <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = compiled.df.list$segments,
                          ggplot2::aes(x = x, y = y,
                                       xend = xend, yend = yend)) +
    ggdendro::theme_dendro()

  if (split.labels) {
    ggout <- ggout + ggplot2::geom_text(data = compiled.df.list$labels,
                                        ggplot2::aes(x = x, y = y,
                                                     label = label),
                                        vjust = 0, size = text.size)
  }

  if (node.labels) {
    ggout <- ggout + ggplot2::geom_label(data = compiled.df.list$leaf_labels,
                                        ggplot2::aes(x = x, y = y,
                                                     label = label),
                                        vjust = 1, size = text.size)
  }

  if (show.plot) {
    plot(ggout)
  }

  if (return) {
    return(list(data = compiled.df.list, gglist = ggout))
  }

  }

#' Creates a list of data frames for \code{tree} objects (for \pkg{ggplot2}
#' visuals)
#'
#' This function just a wrapper for \pkg{ggdendro}'s
#' \code{\link[ggdendro]{dendro_data}} (look at \code{help(dendro_data.tree)}),
#' with slight modifications.
#'
#' @param x
#'
#' @return returns a list of 3 data frames, see details
#'
#' @details
#' \describe{
#'   \item{segments}{a data frame containing the line segment data}
#'   \item{labels}{a data frame containing the label text data}
#'   \item{leaf_labels}{a data frame containing the leaf label text data}
#'   }
#'
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' tree.object <- tree::tree(Species ~., data = iris)
#'
#' compile.df.list <- dfCompile.tree(tree.object)
#' compile.df.list %>% names
#'
#' for (df.name in names(dfCompile.tree)) print(compile.df.list[["df.name"]])
dfCompile.tree <- function(x, type = c("proportional", "uniform")){
  missing.packages <- look.for.missing.packages(c("ggdendro"))
  # ^also requires base, dyplr

  if (length(missing.packages) > 0) {
    stop(paste0(c("Package(s) '",paste0(missing.packages, collapse = "', '"),
                  "' needed for this function to work. Please install them/it."),
                collapse = ""))
  }

  frame <- x$frame
  frame_splits <- frame[frame$var != "<leaf>",]
  frame_leaf <- frame[frame$var == "<leaf>",]

  data_out <- ggdendro::dendro_data(x, type)

  data_out$labels <- data_out$labels %>%
    mutate(label = paste0(label,frame_splits$splits[,"cutleft"]))

  return(data_out)
}
