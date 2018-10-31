# Dot plot ############################################################################################################################################
# For connected dot plot showing arrival times for each file in the date range of interest (plot_earliest_file and later)
load("/DailyDQ/Threshold base.RData")
messages_by_arrived <- fill_titles_lines %>%
  full_join(file_range, by = "Arrived_Date_CDT") %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT)) %>%
  arrange(Arrived_Date_CDT) %>%
  mutate(Arrived_Time_CDT = as.hms(Arrived_Time_CDT)) 


# Overlapping bar charts ############################################################################################################################################

message_per_event_by_file <- cum_update %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT)) %>%
  full_join(threshold_base, by = "Parent_Organization") %>%
  select(-starts_with("Mean"), -starts_with("SD"), -visit_threshold_line) %>%
  mutate(low = ifelse(message_total < message_threshold_line,1,0))

message_total_by_file <- fill_titles_lines %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT)) 


visit_per_event_by_file <- cum_update_visit %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT)) %>%
  ungroup(C_Visit_Date) %>%
  mutate(C_Visit_Date = as.Date(C_Visit_Date)) %>%
  full_join(threshold_base, by = "Parent_Organization") %>%
  select(-starts_with("Mean"), -starts_with("SD"), -message_threshold_line) %>%
  mutate(low = ifelse(visit_total < visit_threshold_line,1,0))
 
visit_total_by_file <- fill_titles_lines_visit %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT))
