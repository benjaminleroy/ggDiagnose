#' Diagnostic plot for objects (ggplot based)
#'
#' @param x object to visualize
#' @param show.plot logic to display the plot
#' @param return logic to return list the graphic and the data frame to make
#' the majority of the graphic
#' @param ... additional parameters, see \code{ggDiagnose.__} for more
#' information
#'
#' @return depending on \code{show.plot} and \code{return} it
#' will return the visualization of the graphic and/or a list
#' of both the data frame used the make the majority of the graphic and
#' the graphic object itself.
#' @export
ggDiagnose <- function(x, show.plot = TRUE, return = FALSE, ...){
  type.x <- class(x)

  potential.function.names <- paste0("ggDiagnose.",type.x)
  function.exists <- sapply(potential.function.names,
                            function(func.name){
                                     exists(func.name,
                                            where = 'package:ggDiagnose',
                                            mode = 'function')})
  if (any(function.exists)) {
    function.name <- potential.function.names[function.exists][1]

    return(eval(parse(text = paste0(function.name,"(x, show.plot = ",show.plot,
                                    ", return = ",return,", ...)"))))
  } else {
    warning("no ggDiagnose function found for this object")
  }

}


#' Diagnostic plot for objects (ggplot based)
#'
#' @param x object to visualize
#' @param ... additional parameters, see \code{dfCompile.__} for more
#' information
#'
#' @return data frame with ready to visualize
#' @export
dfCompile <- function(x, ...){
  type.x <- class(x)

  potential.function.names <- paste0("dfCompile.",type.x)
  function.exists <- sapply(potential.function.names,
                            function(func.name){
                              exists(func.name,
                                     where = 'package:ggDiagnose',
                                     mode = 'function')})
  if (any(function.exists)) {
    function.name <- potential.function.names[function.exists][1]

    return(eval(parse(text = paste0(function.name,"(x, ...)"))))
  } else {
    warning("no dfCompile function found for this object")
  }

}
