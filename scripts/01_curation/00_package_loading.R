load_packages <- function(x){
  installed_packages <- x %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  invisible(
    lapply(x, library, character.only = TRUE))
}