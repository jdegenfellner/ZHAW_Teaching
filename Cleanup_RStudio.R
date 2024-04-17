# Cleanup.R

# Clear Console
cat("\014")

# Remove all objects from the workspace
rm(list = ls())

# Clear all plots
graphics.off()