# Data Management #######################################################################################################################################################################
# Load historical data set
# Update this data set and the file path every month
load("/DailyDQ/Aug17 - Sep18 max message counts.RData")
load("/DailyDQ/Aug17 - Sep18 max visit counts.RData")

# Deduplicate MFT by unique Sending_Facility_ID.
MFT_tidy <-  MFT %>% 
  tbl_df() %>% 
  distinct(Sending_Facility_ID, .keep_all = TRUE)   
# We had one facility with a change in Sending_Facility_ID, so this hard codes the info in to make sure it's included in the cleaned MFT
new_BTT_row <- data.frame("1508809427","Banner Tucson","Banner-University Medical Center-South Campus")
names(new_BTT_row) <- names(MFT_tidy)
MFT_tidy <- rbind(MFT_tidy,new_BTT_row)

# Correct Parent_Organization based on Facility_Name.
# Arrived Date Time is UTC, so transform to Arrived_Date_CDT (CDT)
# Recorded Date Time is MST

df_files_orgPR <- df_filesPR %>% 
  tbl_df() %>% 
  left_join(MFT_tidy, by = "Sending_Facility_ID")  %>%
  mutate(Parent_Organization = ifelse(Facility_Name %in% c("Banner-University Medical Center-Tucson Campus",
                                                           "Banner-University Medical Center-South Campus"),
                                      "Banner Tucson", Parent_Organization)) %>% ##UPDATE AFTER ~10/01 when Banner Tucson switches to Cerner
  mutate(Arrived_Date_Time = as.POSIXct(Arrived_Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") ) %>% 
  mutate(Arrived_Date_CDT_Time = format(Arrived_Date_Time, tz = "America/Chicago") ) %>% 
  mutate(Arrived_Time_CDT = strftime(Arrived_Date_Time, format="%H:%M:%S", tz = "America/Chicago")) %>%
  mutate(Arrived_Date_CDT = as.factor(date(Arrived_Date_CDT_Time) ) ) %>%
  mutate(Recorded_Date = date(Recorded_Date_Time) ) %>% 
  filter(Arrived_Date_Time >= arrived_earliest_date  
         & Arrived_Date_CDT_Time <= arrived_latest_date) 

df_files_orgST <- df_filesST %>% 
  tbl_df() %>% 
  left_join(MFT_tidy, by = "Sending_Facility_ID")  %>%
  mutate(Parent_Organization = ifelse(Facility_Name %in% c("Banner-University Medical Center-Tucson Campus",
                                                           "Banner-University Medical Center-South Campus"),
                                      "Banner Tucson", Parent_Organization)) %>% 
  mutate(Arrived_Date_Time = as.POSIXct(Arrived_Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") ) %>% 
  mutate(Arrived_Date_CDT_Time = format(Arrived_Date_Time, tz = "America/Chicago") ) %>% 
  mutate(Arrived_Time_CDT = strftime(Arrived_Date_Time, format="%H:%M:%S", tz = "America/Chicago")) %>%
  mutate(Arrived_Date_CDT = as.factor(date(Arrived_Date_CDT_Time) ) ) %>%
  mutate(Recorded_Date = date(Recorded_Date_Time) ) %>% 
  filter(Arrived_Date_CDT_Time >= arrived_earliest_date  
         & Arrived_Date_CDT_Time <= arrived_latest_date) 


# IF DUPLICATE FILES HAVE BEEN SENT (run "Check Duplicate.R" script first) HERE IS HOW TO EXCLUDE THEM FROM THIS VISUALIZATION
# Each file must be removed individually
# You only need to list the files that were extracted as part of this script (not historic data, that will be removed separately)
df_files_orgPR <- filter(df_files_orgPR, (Arrived_Date_Time != "2018-10-29 20:10:38"))

# Bind PR and ST files into one data set
df_files_org <- bind_rows(df_files_orgPR,df_files_orgST)
