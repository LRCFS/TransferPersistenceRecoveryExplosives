###########################################################################
#
# Detection and quantitation of novel benzodiazepines in tablets, powders,
# blotters and infused materials from Scottish prisons  - Copyright (C) 2021
#
# Leverhulme Research Centre for Forensic Science

# Centre for Forensic Science, Department of Pure and Applied Chemistry,
# University of Strathclyde, Royal College, 204 George Street, Glasgow

# Victoria Marland, Robert Reid, Andrew Brandon, Kevin Hill, Fiona Cruickshanks,
# Craig McKenzie, Caitlyn Norman, Niamh Nic Daéid, Herve Menard

# Website: https://github.com/LRCFS/GcMsDataProcess
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
###########################################################################
#
# This general code to run first
#
###########################################################################

#Clear all lists from memory to avoid unintentional errors

rm(list=ls()) 

#############################################################
#####                 File requirement                  #####
#############################################################
# load the libraries
library(readxl)
library(xlsx)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(hablar)
library(ggplot2)
library(tidyverse)
library(scales)
library(zoo)
library(hyperSpec)
library(reshape2)
library(baseline)
library(smooth)
library(IDPmisc)
library(lubridate)

#############################################################
#####                Folder & Files                     #####
#############################################################

# set files extensions
extensionCSV <- ".csv"
extensionXLSX <- ".xlsx"
extensionMS1 <- ".ms1"

#Specify the parent folder of the GC Data
DataFolder <- "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/GC Data/Troubleshooting/PETN Test"
ParentFolder <- sub(".*/", "", DataFolder)

# This is where the .D files from the instrument will be placed  
GcDataConverterMs.dir <- paste0(DataFolder,"/GcDataConverterMs/")
dir.create(file.path(paste0(DataFolder, "/RawData/")))

# For the code dealing with the MS1 files conversion
# This is where the MS1 TIC files converted using ProteoWizard - MSConvert will be placed  
GcDataConverterMs.dir <- paste0(DataFolder,"/GcDataConverterMs/")
dir.create(file.path(GcDataConverterMs.dir),recursive = TRUE)

# This is where the MS1 TIC files are saved after their content is reordered in specific columns and rows  
GcDataConvertedRcodeTIC.dir <- paste0(DataFolder,"/GcDataConvertedRcode/TIC/")
dir.create(file.path(GcDataConvertedRcodeTIC.dir),recursive = TRUE) # will create folder if not already there.

# This is where the MS1 SIM files are saved after their content is reordered in specific columns and rows  
GcDataConvertedRcodeSIM.dir <- paste0(DataFolder,"/GcDataConvertedRcode/SIM/")
dir.create(file.path(GcDataConvertedRcodeSIM.dir),recursive = TRUE) # will create folder if not already there.

# For code dealing in background subtraction
# where the generated figures are saved, create folder if not existing
GCSampleTrace.dir <- paste0(DataFolder,"/Results/GCSampleTrace/")
dir.create(file.path(GCSampleTrace.dir),recursive = TRUE) # will create folder if not already there.

# where the output data is saved
GcData.dir <- paste0(DataFolder,"/GcData/")
dir.create(file.path(GcData.dir),recursive = TRUE) # will create folder if not already there.
 
# This is for the code for processing the data
Metadata.dir <- paste0(DataFolder,"/Metadata/")
dir.create(file.path(Metadata.dir),recursive = TRUE) # will create folder if not already there.

MetadataOutput.dir <- paste0(DataFolder,"/Results/FiguresOutput/")
dir.create(file.path(MetadataOutput.dir),recursive = TRUE) # will create folder if not already there.

Results.dir <- paste0(DataFolder,"/Results/")
dir.create(file.path(Results.dir),recursive = TRUE) # will create folder if not already there.

Backup.dir <- paste0(DataFolder,"/Results/Backup/")
dir.create(file.path(Backup.dir),recursive = TRUE) # will create folder if not already there.

#############################################################
#####           Constants and thresholds                #####
#############################################################

# threshold <- 0.2 # % peak height of max intensity TIC
# angular.vertor <- 0.7
# # m/z accepted precision
# Mass.precision <- 0.2

#smooth.loop <- 10 # number of smoothing repeat applied to the data
SignalMaxThresholdTIC <- 100000  # minimum peak height after background removal for TIC
                                # or there will be no peaks
SignalMaxThresholdSIM <- 80 # minimum peak height after background removal for SIM
                              # or there will be no peaks
rolling.average <- 3 # value for rolling average
IS.Exp.Range <- 466.5  # the retention time Internal Standard is expected at
PETN.Exp.Range <- 392.5  # the retention time PETN is expected at

#############################################################
#####                     Sample Volumes                #####
#############################################################

#Total Volume of GCMS vial in uL
VialVol <- 100

#Volume of sample in GCMS vial in uL
AliquotVol <- 90

#Calculate dilution factor
Dilution <- AliquotVol / VialVol

#Total sample volume in uL
SampleVol <- 1000

#Volume of standard deposited on surface in uL
DepositVol <- 50

#Concentration of standard in ng/uL
DepositConc <- 200

#Mass deposited on surface
DepositMass <- DepositVol * DepositConc

#############################################################
#####                   Functions                       #####
#############################################################

source("Code/ModPeaks.R")

#############################################################
#####                       Codes                       #####
#############################################################

# This codes should be run in order as each create necessary outputs for the next codes.

# This script changes the converted *.D Agilent data using Proteo Wizard - MsConvert
# The converted data file format is *.ms1 and placed in the GcDataConverterMs folder
# The Msconverter can handle bath process, the R code will run one file after another and save the peak area results in GcData folder

# This only need to be done once per ms1 files and export is saved to a new folder: GcDataConvertedRcode
# source("Code/MsFilesReorganiser.R")

# This code takes the files reordered from the previous code and calculate the areas for the relevant peaks
# IMPORTANT NOTE: before running this file, make sure the correct retention times for the Internal Standard and PETN are correct !
# otherwise, NA will appear for peak areas and it will not process with the Metadata
# source("Code/MsFilePeakExtract.R")

# This script use the results of the previous code (or self entered) in GcData and combined it to the Metadata
# source("Code/Metadata.R")
