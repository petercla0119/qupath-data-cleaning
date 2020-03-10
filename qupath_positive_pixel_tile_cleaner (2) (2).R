# Updated 11/2019/2019 
    # selects columns to keep by header name - not by indices 
qupath_positive_pixel_tile_cleaner <- function(df) {
  require(dplyr)
  require(stringr)
  columns.to.keep <- c("(F|f)ile(\\s|\\.)(N|n)ame|Parent|Name|^ROI$|^Positive.pixel.area|(.*?)of.total.ROI(.*)") # Create expression of strings to recognize
  df <- df[,grepl(columns.to.keep, names(df))] # Remove unnecessary columns

  # Create Regular Expression pattern that will recognize SlideID
  slideid_pattern <- "^[0-9]{2,4}(?# Autopsy Year matching 2-4 digits between 0-9)\\-([0-9]{1,3})(?# Autopsy case number matching 1-3 digits between 0-1)\\-([0-9]{1,2})([EF])(?# Block number matching)([\\-|\\s][L|R][\\s|\\.][A-Z]{3,4}|[\\-|\\s][A-Z]{3,4})(?# matches one of two situations: 1. hyphen/space followed by L/R followed by hyphen/period followed by 3-4 characters from A-Z OR 2. hyphen/space followed by 3-4 characters from A-Z)"
  # For QuPath validation all regions should be 3-4 characters long, however for library {3,4} should be modified to {2,6} as this will match a character length between 2-6 (e.g. IF or DLPFC) 
  
  df$slideID <- stringr::str_extract_all(as.matrix(df[,1]), slideid_pattern, simplify = TRUE)  #Create a new column in df to contain SlideID
  df$autopsyID <- stringr::str_extract_all(as.matrix(df[,1]), "^([0-9]{2,4})\\-([0-9]{1,3})", simplify = TRUE)  #Create a new column in df to contain AutopsyID
  df$region <- stringr::str_extract_all(as.matrix(df[,1]), "([A-Z]{3,6})", simplify = TRUE)  #Create a new column in df to contain Region
  df$hemisphere <- NA
  df$hemisphere <- ifelse(grepl("\\-L", df$slideID), "L", ifelse(grepl("\\-R", df$slideID), "R",  "NA"))
 #Create a new column in df to contain hemisphere
  colnames(df)[grep ("(.*?)of.total.ROI(.*)", names(df))] <- "percent.AO" # Rename the %AO column to avoid special characters using grep()
  # removes rows with Line in the ROI region
  df$percent.AO <- as.numeric(df$percent.AO)
  # Creates new data frame which should only contain Tiles
  cleaned_df <- df %>%
    filter(!ROI == "Line") %>%
    filter(ROI == "Rectangle") %>%
    group_by(slideID)
  # gm_df <- df %>% 
  #   filter(Parent == "Greatest GM Sampling zone")
  # _df <- df %>% 
  #   filter(Parent == "Greatest GM Sampling zone")
  summarized_df <- cleaned_df %>% 
    group_by(slideID, autopsyID, region, Parent, hemisphere) %>% # will crash R if a column (e.g. hemisphere) doesn't contain anything
    filter(!Parent == "Image") %>% 
    summarise(avg.percent.AO = mean(percent.AO))
  summarized_df$analysis_region <- NA
  summarized_df$analysis_region[summarized_df$Parent == "Greatest GM Sampling zone"] <- "GM"
  summarized_df$analysis_region[summarized_df$Parent == "WM"] <- "WM"

  # Creates output print statement to summarize the number of unique SlideIDs, AutopsyIDs and regions calculated 
  a <- length(unique(summarized_df$slideID))
  b <- length(unique(summarized_df$autopsyID))
  c <- length(unique(summarized_df$region))
  show(sprintf("There are unique %i SlideIDs with %s unique AutopsyIDs and %s different region(s)", a, b, c))
  
  return(summarized_df)
  
}
