load_packages <- function(x){
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == F)){
    install.packages(packages[!installed_packages])
  }
  
  outdated_packages <- packages %in% rownames(old.packages())
  if (any(outdated_packages == T)){
    install.packages(packages[outdated_packages])
    }
  
  invisible(lapply(packages, library, character.only = TRUE))
}
