library(rvest)

get_cran_version <- function() {
  url <- "https://cran.r-project.org/bin/windows/base/"
  page <- read_html(url)
  text <- html_text(page)
  version_string <- substr(text, 12,16)
  
  return(version_string)
}

compare_versions <- function() {
  cran_version <- get_cran_version()
  my_version <- substr(R.version.string, 11,15)
  
  if (cran_version == my_version) {
    message("Your R version is up to date.")
  } else {
    message(sprintf("Your R version is outdated. The latest version is %s.", cran_version))
  }
}
