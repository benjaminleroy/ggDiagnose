# inner functions for checking package dependencies

#' Inner function to check if package names are in the namespace
#'
#' @param package.names list of package names to check are in the Namespace
#'
#' @return vector of packages that are missing
look.for.missing.packages <- function(package.names) {
  missing.packages <- c()

  for (package.name in package.names) {
    if (!requireNamespace(package.name, quietly = TRUE)) {
      missing.packages <- c(missing.packages, package.name)
    }
  }
  return(missing.packages)
}



