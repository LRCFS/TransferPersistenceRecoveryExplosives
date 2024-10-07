###########################################################################
#
# TBC - Copyright (C) 2021
#
# Leverhulme Research Centre for Forensic Science

# Centre for Forensic Science, Department of Pure and Applied Chemistry,
# University of Strathclyde, Royal College, 204 George Street, Glasgow
# Chiron AS, Stiklestadveien 1, NO-7041 Trondheim, Norway

# Caitlyn Norman, Dorothy Xi Yue Lim , Taylor Henderson, Fabio Casali,
# Niamh Nic Daéid, Lorna Nisbet, Hervé Ménard

# Website: https://github.com/HerveMenard/CollaborationNetwork-DrugsEvidence
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

# To clean the Global environment
rm(list=ls()) 

#############################################################
#####                 File requirement                  #####
#############################################################
# The files to be imported are generated from Scopus and Web Of Science databases
# The columns will need to contain:
# Year; Title; Source.title; Authors; AuthorID; DE; DES; EID; SO; DT

#############################################################
#####                 Load Libraries                    #####
#############################################################

library(plyr)
library(dplyr)
library(expss)
library(stringr)
library(tidyr)
library(ggplot2)
library(maps)
library(countrycode)
library(RColorBrewer)
library(tidyverse)
library(reshape2)
library(tikzDevice)
library(Cairo)
library(extrafont)
library(ggmap)
library(mgsub)
library(httr2)
library(pdftools)
library(tm)
library(RWeka)
library(ggtext)
library(ggsci)

#############################################################
#####                Load Functions                     #####
#############################################################

source("Functions/SearchAndReplace.R")

# function to replace accented characters with unaccented equivalents 
# adapted from https://stackoverflow.com/questions/15253954/replace-multiple-letters-with-accents-with-gsub
removeDiacritics <- function(string) {
  chartr(
    "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöøùúûüýÿ",
    "SZszYAAAAAACEEEEIIIIDNOOOOOOUUUUYaaaaaaceeeeiiiidnoooooouuuuyy", 
    string
  )
}

#############################################################
#####          Create Folders and Filenames             #####
#############################################################

# set extension and Citation
extension <- ".csv"
cit.path.INTERPOL <- "INTERPOL/"
cit.path.SCOPUS <- "Scopus/"

# where the generated figures are saved, create folder if not existing
Results.dir <- "Results/"
if (!dir.exists("Results/")){
  dir.create(file.path(Results.dir),recursive = TRUE)
}
Figure.dir <- "Figures/"
if (!dir.exists("Figures/")){
  dir.create(file.path(Figures.dir),recursive = TRUE)
}
Papers.dir <- "Papers/"
if (!dir.exists("Papers/")){
  dir.create(file.path(Figures.dir),recursive = TRUE)
}

# filename for figure export
FigureName <- "Fig1_Scopus_Keyword_"
TableName <- "Table1_Scopus_Keyword_"

#############################################################
#####                    Load Countries                 #####
#############################################################

# get city/country data
data(world.cities)

#############################################################
##### Country and Institution figures
# Number of individual country appearing on the figures
NumberCountry <- 20

#############################################################
#####     This section is used to determine which       #####
#####           keywords will be searched               #####
#############################################################

#     Select one of the following three options
# KeywordEntries <- "Author_Keywords"
# KeywordEntries <- "Database_Keywords"
KeywordEntries <- "All_Keywords"

#############################################################
#####                  Data cleansing                   #####
#############################################################

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
#read the corrected list of keywords and combine it to the original list

#Load Keyword correction list
KeywordCorrectionList <- read.csv("CorrectionLists/KeywordsCorrectionFull.txt", sep="\t", header=TRUE)
KeywordCorrectionList <- as.data.frame(KeywordCorrectionList)

#############################################################
#####           Load Explosive Corpus                   #####
#############################################################

# Load the Corpus of interest to search in the Interpol output entries
ExplosiveList <- read.csv("ExplosiveDatabase.csv", header = TRUE)

ExplosiveList$UncorrectedNoSpecials <- ExplosiveList$Uncorrected.Explosive
ExplosiveList$UncorrectedNoSpecials <- paste0(" ", ExplosiveList$UncorrectedNoSpecials)
ExplosiveList$UncorrectedNoSpecials<- gsub('[^[:alnum:] ]', ' ', ExplosiveList$UncorrectedNoSpecials)
ExplosiveList$UncorrectedNoSpecials <- gsub("\\s+", " ", ExplosiveList$UncorrectedNoSpecials)

# Remove trailing (right) whitespace and make lowercase
ExplosiveList$UncorrectedNoSpecials <- trimws(ExplosiveList$UncorrectedNoSpecials, which = c("right"))
formatC
ExplosiveList$UncorrectedNoSpecials <- toupper(ExplosiveList$UncorrectedNoSpecials)

ExplosiveList$UncorrectedNoSpaces <- ExplosiveList$UncorrectedNoSpecials
ExplosiveList$UncorrectedNoSpaces <- trimws(ExplosiveList$UncorrectedNoSpaces, which = c("both"))

ExplosiveList$Colour <- ExplosiveList$Explosive.Type
ExplosiveList$Colour <-gsub('No Practical Use','tomato',ExplosiveList$Colour)
ExplosiveList$Colour <- replace_na(ExplosiveList$Colour,'black')

# Convert to Object
ExplosiveListString <- ExplosiveList$UncorrectedNoSpecials

#############################################################
#####           Figure Settings                         #####
#############################################################
#############################################################
##### Figure colours
# assign text colour
textcol <- "black"

#############################################################
##### Keyword Figures
# the number of keywords (top most frequent) appearing in the figure
number <- 100   # target number of keywords apearing in the keyword figure
maximum <- 105  # maximum number of keywords appearing in the keyword figure

# colour palette for the Keyword figure
pal <- c("#990000","#FF5D00","#FFB900","#FFFF00","#ACFF00","#00CC00","#33FFFF","#008BFF","#0000FF","#8968CD","#551A8B")

#############################################################
#####  This is the code for generating Keyword figures  #####
#############################################################

# for (file in filenames){
# # remove the extension and path of the file in column reference  
#   name <- gsub(extension, "", file)
name <- "_Scopus"
Count <- number

#############################################################
#####                       Codes                       #####
#############################################################
#These codes must be run first to prepare the data correctly for figure generation
 source("Code/Interpol Data Prep.R")
 source("Code/Scopus Data Prep.R")

# These codes can be run subsequently or independently as each create necessary outputs for the next codes.

# # Figure 1, INTERPOL Keywords as a function of year
source("Code/Figure1_Interpol_Keywords.R")
#
# # Figure 2, Scopus Keywords as a function of year
 source("Code/Figure2_Scopus_Keywords.R")
#
# # Figure 3.1, Interpol - Country affiliations
 source("Code/Figure3.1_Interpol_Country_Affiliation.R")

# # Figure 3.2, Scopus - Country affiliations
 source("Code/Figure3.2_Scopus_Country_Affiliation.R")

#These codes must be run prior first to extract explosive keywords
source("Code/Interpol Explosive Keywords.R")
source("Code/Scopus Explosive Keywords.R")

# # Figure 4.1, Interpol Explosive Country
 source("Code/Figure4.1_Interpol_Explosive_Country.R")

# # Figure 4.2, Scopus Explosive Country
 source("Code/Figure4.2_Scopus_Explosive_Country.R")

# # Figure 5.1, Interpol Explosive Year
 source("Code/Figure5.1_Interpol_Explosive_Year.R")

# # Figure 5.2, Scopus Explosive Year
 source("Code/Figure5.2_Scopus_Explosive_Year.R")

# # Figure 6, Full Text Mining Comparison
 source("Code/Figure6_Full_Text_Mining.R")

# # Figure 7, Scopus Country Network
 source("Code/Figure7_CountryNetwork.R")

# # Figure 8, Scopus - Country collaboration percentage
 source("Code/Figure8_InternationalCollaboration.R")
