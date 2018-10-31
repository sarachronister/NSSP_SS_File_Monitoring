# Task 1: Number of messages per File_Name ########################################################################################################################################

# List of distinct file names
file_names <- df_files_org %>%
  distinct(File_Name, Arrived_Date_CDT, Parent_Organization, Arrived_Time_CDT) %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT))

# Group by File_Name and calculate total number of messages per file
message_total_by_file <- df_files_org %>% # Update to correct hospital group
  group_by(File_Name, Parent_Organization, Arrived_Date_CDT, Arrived_Time_CDT) %>% 
  summarise(message_total = n()) %>% 
  ungroup(Arrived_Date_CDT) %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT)) 

# Add a line with the number of messages and the time the file arrived
message_total_file_w_line <- message_total_by_file %>%
  ungroup(Parent_Organization) %>%
  mutate(first_line = paste(message_total_by_file$message_total,"messages")) %>%
  mutate(File_Title = paste(Arrived_Date_CDT,":",File_Name))

# Group by File_Name and Event_Date and calculate total number of messages per file and event date
message_per_event_by_file <- df_files_org  %>% 
  group_by(Parent_Organization, Recorded_Date, File_Name, Arrived_Date_CDT)  %>% 
  summarise(messages = n(),
            Arrived_Date_CDT_Time = first(Arrived_Date_CDT_Time) ) %>%
  group_by(Parent_Organization,Recorded_Date) %>%
  mutate(cum_messages = cumsum(messages)) %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT))

# max_message_cum is loaded from the historical data (see MergeMFT_CleanDates.R which loads in the first two lines)
test_back_merge <- max_message_cum %>%
  select(names(message_per_event_by_file)) %>%
  bind_rows(message_per_event_by_file)

#
# Same as above but for visit (ED registration) counts instead of messages
#

# Group by File_Name and calculate total number of visits per file
# Filter to include only A04 triggers (ED registrations) and unique Visit_ID
visit_total_by_file <- df_files_org %>%
  filter(Trigger_Event == "A04") %>%
  distinct(Visit_ID, .keep_all = TRUE) %>%
  group_by(File_Name, Parent_Organization, Arrived_Date_CDT, Arrived_Time_CDT) %>% 
  summarise(visit_total = n()) %>% 
  ungroup(Arrived_Date_CDT) %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT)) %>%
  full_join(file_names, by = c("File_Name","Parent_Organization", "Arrived_Date_CDT","Arrived_Time_CDT"))

no_visits <- which(is.na(visit_total_by_file$visit_total))
visit_total_by_file$visit_total[no_visits] <- 0

# Add a line with the number of visits per file
visit_total_file_w_line <- visit_total_by_file %>%
  ungroup(Parent_Organization) %>%
  mutate(first_line = paste(visit_total_by_file$visit_total,"ED registrations")) %>%
  mutate(File_Title = paste(Arrived_Date_CDT,":",File_Name))

# Group by File_Name and Visit Date 
# and calculate total number of visits per file and visit date
visit_per_event_by_file <- df_files_org  %>% # Update to correct hospital group
  filter(Trigger_Event == "A04") %>%
  select(-Trigger_Event) %>%
  group_by(Parent_Organization, C_Visit_Date, File_Name, Arrived_Date_CDT)  %>% 
  summarise(visits = n(),
            Arrived_Date_CDT_Time = first(Arrived_Date_CDT_Time) ) %>%
  group_by(Parent_Organization,C_Visit_Date) %>%
  mutate(cum_visits = cumsum(visits)) %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT))

# max_visit_cum is loaded from the historical data (see MergeMFT_CleanDates.R which loads in the first two lines)
test_back_merge_visit <- max_visit_cum %>%
  select(names(visit_per_event_by_file)) %>%
  bind_rows(visit_per_event_by_file)

