# Save all of these files in a folder called "DailyDQ" and update all file paths to that folder

rm(list = ls())
source("~/User_Info.R")

# Setup ############################################################################################################################################

# Set working directory for data visualization output
setwd("~/DailyDQ")

# Set dates 
arrived_earliest_date <- "2018-10-01"           
arrived_latest_date <- Sys.Date()+1             
plot_earliest_date <- as.POSIXct("2017-10-01", format = "%Y-%m-%d", tz = "America/Chicago")           
earliest_file <- "2018-10-01"

arrived_earliest_date_no_dash <- paste0("'", gsub("-", "", arrived_earliest_date), "'")
arrived_latest_date_no_dash <- paste0("'", gsub("-", "", arrived_latest_date), "'")

# Check if required packages are installed. If not, install them.
packages <- c("plyr", "RODBC", "dplyr", "stringr", "lubridate", "ggplot2","httr","readxl","tidyr","scales","hms","xlsx")
if (length(setdiff(packages, rownames(installed.packages() ) ) ) > 0)   {
  install.packages(setdiff(packages, rownames(installed.packages() ) ) )  
}
lapply(packages, library, character.only = TRUE)

# detach plyr temporarily to correctly use some function in the following code
library(plyr)
library(dplyr)
detach("package:plyr", unload = TRUE) # will produce an error about that namespace cannot be unloaded, you can ignore it

# Extraction ########################################################################################################################################

# Connect to the Adminer Database and create queries.
# Inner Join AZ_PR_Raw and AZ_PR_Processed on Message_ID.
# Left Outer Join AZ_PR_Processed and AZ_MFT on Sending_Facility_ID.
# Update tables to correct names (i.e. AZ_PR_Raw to whatever your raw table name is)
# We are only interested in the message after processing.

NSSP <- odbcConnect("BioSense_Platform", rawToChar(username), rawToChar(password))

# From the Production table
my_queryPR <- paste("select Raw.File_Name, Raw.Arrived_Date_Time, Pro.Recorded_Date_Time, Pro.Sending_Facility_ID, Pro.Visit_ID, Pro.Trigger_Event, Pro.C_Biosense_Facility_ID, Pro.C_Visit_Date",  
                  "from AZ_PR_Raw as Raw inner join AZ_PR_Processed as Pro",
                  "on Raw.Message_ID = Pro.Message_ID",
                  "where cast(Raw.Arrived_Date_Time as date) between", 
                  arrived_earliest_date_no_dash, "and", arrived_latest_date_no_dash)

# From the Staging table
my_queryST <- paste("select Raw.File_Name, Raw.Arrived_Date_Time, Pro.Recorded_Date_Time, Pro.Sending_Facility_ID, Pro.Visit_ID, Pro.Trigger_Event, Pro.C_Biosense_Facility_ID, Pro.C_Visit_Date",  
                  "from AZ_ST_Raw as Raw inner join AZ_ST_Processed as Pro",
                  "on Raw.Message_ID = Pro.Message_ID",
                  "where cast(Raw.Arrived_Date_Time as date) between", 
                  arrived_earliest_date_no_dash, "and", arrived_latest_date_no_dash)

# From the MFT
my_query2 <- "select Sending_Facility_ID, Parent_Organization, Facility_Name from AZ_MFT"

df_filesPR <- sqlQuery(NSSP, my_queryPR, stringsAsFactors = FALSE, as.is = TRUE)
df_filesST <- sqlQuery(NSSP, my_queryST, stringsAsFactors = FALSE, as.is = TRUE)
MFT <- sqlQuery(NSSP, my_query2, stringsAsFactors = FALSE, as.is = TRUE)

odbcCloseAll()

rm(password);rm(username);rm(UserID)
