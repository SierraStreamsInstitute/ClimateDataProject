#### Google Earth Engine CSV Reformatting Script ####
## Sierra Streams Institute
## Author: WS
## Modified: 8/13/2020

## This is a script meant to turn CSVs output from Dot's GEE scripts
## into longform compatible with analysis. 

# To move forward thru the script, press Ctrl+Enter- HOWEVER, read notes carefully,
# as you will need to change certain things as directed

# This is built for NDVI, NBR, and NLCD scripts. It should work for any CSV
# produced by a direvative of Dorothy' LANDSAT 7 Band calculator script. I encourage 
# the reader to edit as they see fit to make this script more robust to
#  other kinds of GEE (or otherwise) CSV output

#### START ####

# Set working directory- this will be specific to your own local clone
# of the repository. 

setwd("C:/Users/Wes/github/ClimateDataProject/LandCover")

# Load desired packages
library(reshape2)
library(dplyr)
library(magrittr)

# Read in desired datframes
# Most likely NDVI, NBR, NLCD

NLCD.raw <- read.csv("LULC/NLCD/catchment_results2 - catchment_results.csv")
NDVI.raw <- read.csv("NDVI/draft_ndvi_results3 - draft_ndvi_results3.csv")
NBR.raw <- read.csv("NBR/draft_nbr_results.csv")

### Formatter Function Description ###

# x = desired dataframe
# y = quantitative variable abbreviation, 
#     as appears in dataframe, and special notation:

# User must use what are called "regular expressions"- there are many, 
# and you will exert control over how this function interacts with your dataframe.
# For example, a ^ symbol will search your DF for column names STARTING 
# with your input, no symbol in front of the search term is CONTAINING, 
# $ is end of the string, etc. 

# So searching NDVI, we use "^NDVI" because are quantitative vars start with NDIV
# for NLCD we use "area" because it is nested in the middle of variable names

# Link for more info on regular expressions: 
# https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

# what y should be is highly depdnent on your dataframe, 
# it needs to be a string universal + unique among quantitative vars. 

# string:  a sequence of characters (in R, strings appear on the code-side in quotes)

# Note: Below function necessary for 'Year' column formatting in formatter function 
        # (takes string 'x' and returns 'n' charachters from the right)
        substrRight <- function(x, n){
          substr(x, nchar(x)-n+1, nchar(x))
        } 

#### LANDSAT Band Calculation CSV Formating Function ####

# Again, to run this function, enter your dataframe and your search term, 
# and save it to a new dataframe
# eg.

# NLCD <- satForm(NLCD.raw,"area")
# NDVI <- satForm(NDVI.raw,"^NDVI")
# NBR <- satForm(NBR.raw,"^NBR")

satForm <- function(x,y){
  
  # Rename gridcode to Site
  colnames(x)[which(names(x) == "gridcode")] <- "Site"    
  
  # First, make a vector that finds all of the column names starting with y*  
  VAR.query <- colnames(x)[c(grepl(y, colnames(x)))]
  
  # Then extract a dataframe of only variable values and "Gridcode"
  VAR.data <- x[c("Site",VAR.query)]

  # "Reshape" into longform
  VAR.long <- melt(VAR.data, id.vars = "Site")
  
  # # Add columns for year and stat type based of variable name
  VAR.long <- VAR.long %>%
    mutate(
      Year = gsub("[^[:digit:].]","",VAR.long$variable),
      Stat = gsub(".*_(.*)\\_.*", "\\1", VAR.long$variable)
    )
  
  ## Check if NLCD script- extract year from var name,
  if("raw.forest.area.2016" %in% VAR.long$variable){
      VAR.long$Year <- substrRight(VAR.long$Year,4)
      
      ## and extract land cover type from var name
      VAR.long <-  mutate(VAR.long,
                          Stat = gsub("(?:[^.]+\\.){1}([^.]+).*", "\\1", VAR.long$variable)
      )} else {
        # If not NLCD script, continue on as normal
        VAR.long
      }
  
    VAR.fin <- VAR.long %>%
                        # then remove variable column and the redundant "0" Years values
                        subset(Year > 0, select= c("Site","Year","Stat","value"))
  # If NBR or NDVI script, turn 1-20 into corresponding years
   if("mean" %in% VAR.fin$Stat){
     for (i in 1:length(VAR.fin$Year)){
       # For 1-9, paste '200' in front
       if(VAR.fin$Year[i] %in% c(1,2,3,4,5,6,7,8,9)){
         VAR.fin$Year[i] <- paste0("200",VAR.fin$Year[i])
       }else{
       # otherwise (10-20), paste 20 in front
         VAR.fin$Year[i] <- paste0('20',VAR.fin$Year[i])
       }
     }
  
   } else {
    # If not, leave as is 
     VAR.fin
   }
# Return our new dataframe
# with the Stat/Type, Year, Site, and Value for NDVI, NBR, NLCD (or, any band type)    
  VAR.fin
} 

# Run the function- examples below- replace with your own code
NLCD <- satForm(NLCD.raw,"area")
NDVI <- satForm(NDVI.raw,"^NDVI")
NBR <- satForm(NBR.raw,"^NBR")


# Functions for splitting NDVI dataframe into seperate 'Max' 'Mean' 'Min'
# just run your dataframe into these functions and save as seperate dataframe,
# e.g. maxNDVI <o maxFun(NDVI)

# Max
maxFun <- function(x){
  subset(x,Stat=='max')[,-3] 
}

# Min
minFun <- function(x){
  subset(x,Stat=='min')[,-3] 
}

# Mean
meanFun <- function(x){
  subset(x,Stat=='mean')[,-3] 
}

