# inner functions for checking package dependencies

#' Inner function to check if package names are in the namespace
#'
#' @param package_names list of package names to check are in the Namespace
#'
#' @return vector of packages that are missing
look_for_missing_packages <- function(package_names) {
  missing_packages <- c()

  for (package_name in package_names) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
      missing_packages <- c(missing_packages, package_name)
    }
  }
  return(missing_packages)
}



