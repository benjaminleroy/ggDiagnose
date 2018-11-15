# matrix visualization 
# takes the place of "heatmap" and "image" for a matrix (not the same thing
# as plotting a matrix

# data example
# iris_c <- iris %>% select(-Species) %>% cor()

# image function first (reordering and dendrogram second)

#' Diagnostic plot for \code{matrix} object (\code{ggplot2} based)
#' 
#' Creates ggplot based visual of either \code{\link[graphics]{image}} or 
#' \code{\link[stats]{heatmap}} for matrix objects.
#'
#' @param x matrix object
#' @param type string of either "image" or "heatmap" the tells the function
#' whether to display the matrix directly as an image or to reorder the columns
#' and create a dendrogram. Note, the heatmap is a more general case of the 
#' image function call
#' @param dendro_height proportional height of dendrogram compared to image
#' if dendrograms a displayed.
#' @param ... used to pass additional heatmap parameters, see details in 
#' \code{\link{dfCompile.matrix}}.
#' @param show.plot logic to display the graphic
#' @param return logic to return list of graphics and the list of data frames to
#' make the majority of the graphics
#'
#' @return depending on \code{show.plot} and \code{return} it
#' will return the visualization of the graphic and/or a list
#' of both the list of data frames used the make the graphic and the
#' individual graphic object.
#' 
#' @export
#'
#' @examples
#' library(tidyverse)
#' data(iris)
#' iris_c <- iris %>% select(-Species) %>% cor %>% as.matrix 
#' 
#' ggDiagnose.matrix(iris_c, type = "heatmap")
ggDiagnose.matrix <- function(x, type = "image",
                              dendro_height = .25,
                              ...,
                              show.plot = TRUE, return = FALSE){
  # so that CRAN checks pass
  .var1 <- .var2 <- value <- NULL
  y <- yend <- xend <- NULL
  
  n_row <- nrow(x)
  n_col <- ncol(x)
  
  missing.packages <- look.for.missing.packages(c("ggdendro", "graphics", 
                                                  "stats"))
  # ^also requires ggplot2, dyplr
  
  if (length(missing.packages) > 0) {
    stop(paste0(c("Package(s) '",paste0(missing.packages, collapse = "', '"),
                 "' needed for this function to work. Please install them/it."),
                collapse = ""))
  }
  
  data_list <- dfCompile.matrix(x = x, type = type, ...)
  
  data <- data_list$df
  dendro_row <- data_list$dendro_row
  dendro_col <- data_list$dendro_col

  ggobj <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = data, 
                       ggplot2::aes(x = .var1, y = .var2, fill = value)) +
    ggplot2::labs(x = "Variable 1", y = "Variable 2")
  
  
  if (type == "heatmap") {
    if (!(is.null(dendro_col))) {
      dendro_c_mult = n_row/max(dendro_col$segments$y) * dendro_height
      ggobj <- ggobj +
        ggplot2::geom_segment(data = dendro_row$segments, 
                              ggplot2::aes(y = x, x = -y * dendro_c_mult + .45, 
                                          yend = xend, 
                                          xend = -yend * dendro_c_mult + .45)) 
    }
    if (!(is.null(dendro_row))) {
      dendro_r_mult = n_col/max(dendro_row$segments$y) * dendro_height
      ggobj <- ggobj +
        ggplot2::geom_segment(data = dendro_col$segments, 
                              ggplot2::aes(x = x, 
                                           y = y * dendro_r_mult + n_row + .55, 
                                           xend = xend, 
                                           yend = yend * dendro_r_mult + 
                                                  n_row + .55))
    }

    ggobj <- ggobj +
      ggplot2::scale_y_discrete(position = "right") +
      ggplot2::theme_minimal() + 
      ggplot2::theme(panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(x= "",y = "")
  }
  
  if (show.plot) {
    graphics::plot(ggobj)
  }
  
  if (return) {
    return(list(data = data_list, ggout = ggobj))
  }
}

#' Creates a list of data frames for \code{tree} objects (for \pkg{ggplot2}
#' visuals)
#' 
#' TODO: get the function for reorder.dendrogram
#'
#' @param x matrix object
#' @param type string of either "image" or "heatmap" the tells the function
#' whether to display the matrix directly as an image or to reorder the columns
#' and create a dendrogram. Note, the heatmap is a more general case of the 
#' image function call
#' @param ... additional parameters (see Details section)
#'
#' @details 
#' If wishing to use the \code{heatmap} functionality, the following 
#' parameters can be inputted to customize the heatmap. 
#'  \describe{
#'   \item{Rowv}{determines if and how the row dendrogram should be computed 
#'   and reordered. Either a \code{\link[stats]{dendrogram}} or a vector of 
#'   values used to reorder the row dendrogram or NA to suppress any row 
#'   dendrogram (and reordering) or by default, NULL, see more details below
#'   this list.}
#'   \item{Colv}{determines if and how the column dendrogram should be 
#'   reordered. Has the same options as the Rowv argument above and 
#'   additionally when x is a square matrix, Colv = "Rowv" means that columns 
#'   should be treated identically to the rows (and so if there is to be no row 
#'   dendrogram there will not be a column one either).}
#'   \item{distfun}{function used to compute the distance (dissimilarity) 
#'   between both rows and columns. Defaults to \code{\link[stats]{dist}}.}
#'   \item{hclustfun}{function used to compute the hierarchical clustering when 
#'   Rowv or Colv are not dendrograms. Defaults to \code{\link[stats]{hclust}}.
#'   Should take as argument a result of distfun and return an object to which 
#'   \code{\link[stats]{as.dendrogram}} can be applied}
#'   \item{reorderfun}{function(d, w) of dendrogram and weights for reordering 
#'   the row and column dendrograms. The default uses 
#'   \code{\link[stats]{reorder.dendrogram}}}
#'   \item{scale}{character indicating if the values should be centered and 
#'   scaled in either the row direction ("row") or the column direction ("col"),
#'   or none ("none"). The default is "row" if symm false, and "none" otherwise.
#'   }
#'   \item{symm}{logical indicating if x should be treated symmetrically; can 
#'   only be true when x is a square matrix. Default is FALSE.}
#'   \item{na.rm}{logical indicating whether NA's should be removed in for 
#'   ordering by using row or column sum. default is TRUE.}
#'
#' @return returns a list of 1 data frame and 2 lists
#' \describe{
#'   \item{df}{contains a 'melted' data frame to visualize the matrix}
#'   \item{dendro_row}{a list of information about the row dendrogram in the 
#'   form of \code{\link[ggdendro]{dendro_data}}, namely a (1) a data frame
#'   called "segments" that contains containing the line segment data to
#'   create the dendrogram, (2) a data frame containing the label text data
#'   called "labels" - assoicated with the leafs, (3) empty list element
#'   labeled "leaf_labels", and (4) a "class" element detailing the type of 
#'   object it is representing (in this case always "dendrogram")}
#'   \item{dendro_col}{a similar list object like \code{dendro_row} but for the
#'   column dendrogram}.
#'   }
#'   
#' Note, if no dendrograms are required to be made, the entry for 
#' \code{dendro_row} and \code{dendro_col} is stored as NULL
#' @export
#'
#' @examples
#' library(tidyverse)
#' data(iris)
#' iris_c <- iris %>% select(-Species) %>% cor
#' 
#' data_list <- dfCompile.matrix(iris_c, type = "heatmap")
#' 
#' data_list2 <- dfCompile.matrix(iris_c, type = "image")
dfCompile.matrix <- function(x, type = "image", 
                             ...){
  
  # so that CRAN checks pass
  Var1 <- Var2 <- NULL
  
  dots <- list(...)
  x_dim <- dim(x)
  if (length(x_dim) != 2 || !is.numeric(x)) {
    stop("'x' must be a numeric matrix")
  }
  n_row = x_dim[1L]
  n_col = x_dim[2L]
  
  if (n_row <= 1 || n_col <= 1) {
    stop("'x' must have at least 2 rows and 2 columns")
  }
  
  if (!(type %in% c("image","heatmap"))) {
    stop(paste0("type (inputted ", type, ") must be either 'image' or ",
                "'heatmap'."))
  }
  if (type == "heatmap") {
    
    Rowv <- dots[["Rowv"]]
    Colv <- dots[["Rowv"]]
    doRdend <- !identical(Rowv,NA)
    doCdend <- !identical(Colv,NA)
    
    distfun <- dots[["distfun"]]
    hclustfun <- dots[["hclustfun"]]
    reorderfun <- dots[["reorderfun"]]
    scale <- dots[["scale"]]
    symm <- dots[["symm"]]
    na.rm <- dots[["na.rm"]]
    
    if (is.null(symm)) {
      symm <- FALSE
    }
    
    if (is.null(Colv)) {
      if (symm) {
        doCdend <- FALSE
      }
    }
    
    if (is.null(distfun)) {
      distfun <- stats::dist
    }
    if (is.null(hclustfun)) {
      hclustfun <- stats::hclust
    }
    if (FALSE){#is.null(reorderfun)) {
      #reorderfun <- stats::reorder.dendrogram # come back to
    }
    
    if (is.null(scale)) {
      if (symm) {
        scale <- "none"
      } else {
        scale <- "row"
      }
    }
    if (!(scale %in% c("row","col","none"))) {
      stop(paste0("scale (inputted ", type, ") must be either 'row', ",
                  "'col', or 'none'."))
    }
    
    if (is.null(na.rm)){
      na.rm <- TRUE
    }
    
    if (is.null(Rowv)) {
      Rowv <- rowMeans(x, na.rm = na.rm)
    }
    if (is.null(Colv)) {
      Colv <- colMeans(x, na.rm = na.rm)    
    }
    
    if (doRdend) {
      if (inherits(Rowv, "dendrogram")) {
        ddr <- Rowv
      } else {
        hcr <- hclustfun(distfun(x))
        ddr <- stats::as.dendrogram(hcr)
        if (FALSE){#(!is.logical(Rowv) || Rowv) {
          #ddr <- stats::reorderfun(ddr, Rowv)
        }
      }
      if (n_row != length(rowInd <- stats::order.dendrogram(ddr))) {
        stop("row dendrogram ordering gave index of wrong length")
      }
      dendro_row <- ggdendro::dendro_data(ddr)
      
    } else {
      rowInd <- 1L:n_row
      dendro_row <- NULL
    }
    
    if (doCdend) {
      if (inherits(Colv, "dendrogram")) {
        ddc <- Colv
      } else {
        if (identical(Colv, "Rowv")) {
        if (n_row != n_col) {
          stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
        }
        ddc <- ddr
      } else {
        hcc <- hclustfun(distfun(if (symm) {
            x
          } else {
            t(x)
          }))
        ddc <- stats::as.dendrogram(hcc)
        if (FALSE){#(!is.logical(Colv) || Colv) {
          ddc <- reorderfun(ddc, Colv)
        }
      }
      }
      if (n_col != length(colInd <- stats::order.dendrogram(ddc))) {
        stop("column dendrogram ordering gave index of wrong length")
      }
      dendro_col <- ggdendro::dendro_data(ddc)
    } else {
      colInd <- 1L:n_col
      dendro_col <- NULL
    }
    
    x <- x[rowInd, colInd]
    
  } 
  
  data <- x %>% reshape2::melt() %>%
    dplyr::rename(.var1 = Var1, .var2 = Var2)

  if (type == "image"){
    dendro_row = NULL
    dendro_col = NULL
  }
  
  return(list(df = data, dendro_row = dendro_row, dendro_col = dendro_col))
}



  
    
