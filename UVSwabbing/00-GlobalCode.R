### Global Configuration and Setup ####
# Run this script FIRST before any other scripts
# This script loads libraries and defines all global variables

rm(list = ls())

# === LOAD LIBRARIES ===
library(dplyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(stringr)

# === COLOUR PALETTE ===
ColourPalette <- c("red", "orange", "yellow2", "limegreen", "darkgreen", "skyblue", "blue", "violet", "darkmagenta", "lightpink", "hotpink")
ColourPalette2 <- c("red", "limegreen", "blue")

# === DIRECTORY PATHS ===
# Base directory
Base.dir <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Shared with Oliver/"

# Source images
SourceImages.dir <- paste0(Base.dir, "Images/")

# Organized images (output of 01-OrganizeImages.R)
OrganizedImages.dir <- paste0(Base.dir, "OrganizedImages/")

# ImageJ results (where you save ImageJ macro outputs)
ImageJResults.dir <- paste0(Base.dir, "ImageJResults/")

# Combined threshold results (output of 02-CombineResults.R)
ThresholdResults.dir <- paste0(Base.dir, "ThresholdResults/")

# Analysis output directory
AnalysisOutput.dir <- paste0(Base.dir, "Analysis/")
BlankThresholdResults.dir <- paste0(AnalysisOutput.dir, "Blank Threshold Analysis/")
BeforeThresholdResults.dir <- paste0(AnalysisOutput.dir, "Unswabbed Threshold Analysis/")

# === CREATE OUTPUT DIRECTORIES ===
dir.create(file.path(OrganizedImages.dir), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(ImageJResults.dir), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(ThresholdResults.dir), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(AnalysisOutput.dir), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(BlankThresholdResults.dir), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(BeforeThresholdResults.dir), recursive = TRUE, showWarnings = FALSE)

# === PRINT SETUP INFO ===
cat("=== GLOBAL SETUP COMPLETE ===\n")
cat("Libraries loaded: dplyr, ggplot2, tidyverse, reshape2, stringr\n")
cat("\nDirectories configured:\n")
cat(paste0("  Source Images:      ", SourceImages.dir, "\n"))
cat(paste0("  Organized Images:   ", OrganizedImages.dir, "\n"))
cat(paste0("  ImageJ Results:     ", ImageJResults.dir, "\n"))
cat(paste0("  Threshold Results:  ", ThresholdResults.dir, "\n"))
cat(paste0("  Analysis Output:   ", AnalysisOutput.dir, "\n"))
cat("\nReady to run numbered scripts.\n")
