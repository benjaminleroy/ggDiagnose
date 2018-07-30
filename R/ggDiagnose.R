#' Diagnostic plot for objects (ggplot based)
#'
#' @param x object to visualize
#' @param show_plot logic to display the plot
#' @param return logic to return list the graphic and the data frame to make
#' the majority of the graphic
#' @param ... additional parameters, see \code{ggDiagnose.__} for more
#' information
#'
#' @return depending on \code{show_plot} and \code{return} it
#' will return the visualization of the graphic and/or a list
#' of both the data frame used the make the majority of the graphic and
#' the graphic object itself.
#' @export
ggDiagnose <- function(x, show_plot = TRUE, return = FALSE, ...){
  type_x <- class(x)

  potential_function_names <- paste0("ggDiagnose.",type_x)
  function_exists <- sapply(potential_function_names,
                            function(func_name){
                                     exists(func_name,
                                            where = 'package:ggDiagnose',
                                            mode = 'function')})
  if (any(function_exists)) {
    function_name <- potential_function_names[function_exists][1]

    return(eval(parse(text = paste0(function_name,"(x, show_plot = ",show_plot,
                                    ", return = ",return,", ...)"))))
  } else {
    warning("no ggDiagnose function found for this object")
  }

}


#' Diagnostic plot for objects (ggplot based)
#'
#' @param x object to visualize
#' @param ... additional parameters, see \code{augment.__} for more
#' information
#'
#' @return data frame with ready to visualize
#' @export
augment <- function(x, ...){
  type_x <- class(x)

  potential_function_names <- paste0("augment.",type_x)
  function_exists <- sapply(potential_function_names,
                            function(func_name){
                              exists(func_name,
                                     where = 'package:ggDiagnose',
                                     mode = 'function')})
  if (any(function_exists)) {
    function_name <- potential_function_names[function_exists][1]

    return(eval(parse(text = paste0(function_name,"(x, show_plot = ",show_plot,
                                    ", return = ",return,", ...)"))))
  } else {
    warning("no augment function found for this object")
  }

}
