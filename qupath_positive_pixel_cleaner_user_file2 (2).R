# This script is intended to be used on data output from QuPath (v0.2.0-m2) to calculate average %AO across tiles in an ROI. Prior to using this script, QuPath txt file output will need to be copy/pasted into Microsoft Excel and saved as an Excel or CSV file.
    # Author: Claire S. Peterson. 07/29/2019


rm(list = ls()) # Clear workspace environment
# Install required packages
if (!require('stringr')) install.packages('stringr'); library('stringr') #package to parse strings
if (!require('readxl')) install.packages('readxl'); library('readxl') #package to import excel files
if (!require('dplyr')) install.packages('dplyr'); library('dplyr') #package for cleaning data


# Set location of excel file containing data and should contain the path to the directory
setwd("U:/DLB MRI Data")  


# Import desired data
    # (Un)Comment the following line if you (don't) want to use a TXT file - NOT RECOMMENDED AS THIS TENDS TO BE MORE DIFFICULT
# df <- read.delim("TXT_FILE_NAME_HERE.txt", row.names = NULL)
# colnames(TXT)[colnames(TXT)=="row.names"] <- "File.Name"    #if you import a txt file, you need to rename the first column with this line

    # (Un)Comment the following line if you (don't) want to use an Excel file... I used Excel files
# df <- read_excel("EXCEL_FILE_NAME_HERE.xlsx")
at8 = read_excel("AT8.xlsx")
nab = read_excel("NAB228.xlsx")    
R13 = read_excel("R13.xlsx")    

    # (Un)comment the following line if you (don't) WANT to use a CSV file. 
# df <- read.csv("CSV_FILE_NAME_HERE.csv")

# Imports outside function (stored in a different file) which cleans QuPath Tile data. The full name (including ".R") should be enclosed between double quotations. Note that this file needs to exist in the same directory as the data or R returns an error.
      # If user does not want source file of outside function to be stored with data, then user must specify where the source R file exists. Use the "setwd("PATH_NAME_HERE")" function to specify path to source file.
source("qupath_positive_pixel_tile_cleaner (2).R")

# Use function on dataframes and name the output df_tile_avg. The dataframe should be between ( )
df_clean <- qupath_positive_pixel_tile_cleaner(df)
at8.clean = qupath_positive_pixel_tile_cleaner(at8)
nab.clea = qupath_positive_pixel_tile_cleaner(nab)
r13.clean = qupath_positive_pixel_tile_cleaner(R13)


# Create new CSV files in current directory of summary dataframes and name them whatever is between "". .csv is necessary for a csv file to be written

write.csv(at8.clean, "AT8_Clean.csv")
write.csv(nab.clea, "Nab228_Clean.csv")
write.csv(r13.clean, "R13_Clean.csv")

write.csv(df_clean, "OUTPUT_FILE_NAME_HERE.csv")
