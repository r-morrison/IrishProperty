# install required packages

all_required <- c(
  "ggplot2",
  "dplyr",
  "stringr"
)

install_package_if_necessary <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dep = TRUE)
  }
}

install_required_packages <- function(required_packages) {
  lapply(required_packages, install_package_if_necessary)
}

invisible(install_required_packages(required_packages))