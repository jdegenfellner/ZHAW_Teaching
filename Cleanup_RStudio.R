# from: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Cleanup_RStudio.R

# Remove all objects from the workspace
rm(list = ls())

# Clear all plots
graphics.off()

# Clear Console
cat("\014")