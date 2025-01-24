compare_versions <- function() {
  # Compares your current R Version to the CRAN-version.
  library(rvest)
  url <- "https://cran.r-project.org/bin/windows/base/"
  page <- read_html(url)
  text <- html_text(page)
  cran_version <- substr(text, 12,16)
  my_version <- substr(R.version.string, 11,15)
  
  if (cran_version == my_version) {
    message("Your R version is up to date.")
  } else {
    message(sprintf("Your R version is outdated. The latest version is %s.", cran_version))
  }
}
compare_versions()
