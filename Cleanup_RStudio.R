# from: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Cleanup_RStudio.R

library(pacman)
p_load(rstudioapi)

# Remove all objects from the workspace
rm(list = ls())

# Clear all plots
graphics.off()

clear_viewer_pane <- function() {
  dir <- tempfile()
  dir.create(dir)
  TextFile <- file.path(dir, "blank.html")
  writeLines("", con = TextFile)
  rstudioapi::viewer(TextFile) 
}

clear_viewer_pane()

# Clear Console
cat("\014")


