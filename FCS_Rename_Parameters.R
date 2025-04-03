# Script to rename parameters in .fcs files
# Authors: Michael de Kok & Sara Garcia Garcia
# Date: 18-11-2024
# Last Updated: 03-04-2025

############################## USER PARAMETERS ############################## 
setwd("") # Change to location of this file
INPUT_FOLDER     <- "Input"
OUTPUT_FOLDER    <- "Output"
PARAMETER_TABLE <- "FCS_Rename_Parameters.xlsx"
#############################################################################

# Install Packages (only first time)
if (!require("BiocManager", quietly = TRUE)) {install.packages("BiocManager")}
#BiocManager::install(c("readxl", "flowCore)) # Uncomment to install packages

# Load Packages
library(readxl)
library(flowCore)

# Read Parameter File
parameter_table <- read_xlsx(path = PARAMETER_TABLE)
old_parameters  <- parameter_table$Input
new_parameters  <- parameter_table$Output
names(new_parameters) <- old_parameters

# List FCS Files and set up lists
fcs_files  <- list.files(path = INPUT_FOLDER, pattern = ".fcs", full.names = T)
fcs_input  <- list()
fcs_output <- list()

# Helper Functions for replacing names
rename_fcs_parameters <- function(fcs, names.map) {
  ret <- fcs
  old.names <- colnames(ret)
  new.names <- old.names
  ignored <- character()
  for (i in 1:length(old.names)) {
    if (old.names[i] %in% names(names.map)) {
      j <- which(names(names.map) == old.names[i])
      new.names[i] <- unname(names.map[j])
    } else {
      ignored <- c(ignored, old.names[i])}}
  ignored <- paste("\n", ignored, sep = "")
  stopifnot(!any(duplicated(new.names)))
  colnames(ret) <- new.names
  return(ret)
}

rename_fcs_spillover <- function(fcs, names.map) {
  ret <- fcs
  old.names <- colnames(ret@description$SPILL)
  new.names <- old.names
  ignored <- character()
  for (i in 1:length(old.names)) {
    if (old.names[i] %in% names(names.map)) {
      j <- which(names(names.map) == old.names[i])
      new.names[i] <- unname(names.map[j])
    } else {
      ignored <- c(ignored, old.names[i])}}
  if (length(ignored) > 0) {
    ignored <- paste("\n", ignored, sep = "")
    cat("\nWarning: Ignored the following FCS Parameters while converting as no match was found:", 
        ignored, "\nPlease set these as the 'Input' parameter values in the sheet for all parameters that need it!",
        "\nKeep in mind that if FlowJo shows parameters as '670_30', you need to put '670/30' in the sheet.")
  }
  stopifnot(!any(duplicated(new.names)))
  colnames(ret@description$SPILL) <- new.names
  return(ret)
}

# For each FCS File
for (file in fcs_files) {
  # Print Progress
  cat("\n\nConverting File: ", file, " (", which(fcs_files == file), " out of ", length(fcs_files), ")", sep = "")
  
  # Read FCS File
  fcs_input[[file]] <- read.FCS(filename = file)
  
  # Change Parameter names
  fcs_output[[file]] <- rename_fcs_parameters(fcs = fcs_input[[file]], 
                                              names.map = new_parameters)
  
  # Change Spillover names
  fcs_output[[file]] <- rename_fcs_spillover(fcs = fcs_output[[file]], 
                                             names.map = new_parameters)
  
  # Save FCS File
  filename <- sub(basename(file), pattern = ".fcs", replacement = "_conv.fcs")
  filename <- file.path(OUTPUT_FOLDER, filename)
  write.FCS(x = fcs_output[[file]], filename = filename)
}

cat("Conversion completed.")
