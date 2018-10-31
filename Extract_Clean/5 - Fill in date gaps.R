# Task 2: Fill in missing event date and arrived date gaps ########################################################################################################################################
# Gaps must be filled by file then by parent_org

## Fill in missing date gaps
arrived_earliest_date <-as.POSIXct(arrived_earliest_date,format="%Y-%m-%d")
arrived_latest_date <-as.POSIXct(arrived_latest_date,format="%Y-%m-%d")
# Date range of event dates to be plotted
plot_range <- data.frame(seq.Date(as.Date(plot_earliest_date),as.Date(arrived_latest_date),by="day"))
colnames(plot_range) <- "date"
day_num <- data.frame(seq(from = 1, to = nrow(plot_range), by = 1))
colnames(day_num) <- "seq_order"

# Date range for FILES (Arrived_Date_CDT)
file_dates <- seq.POSIXt(as.POSIXct(arrived_earliest_date, format="%Y-%m-%d %H:%M:%S"), as.POSIXct(arrived_latest_date, format="%Y-%m-%d %H:%M:%S"), by = "day")
file_range <- data.frame(Arrived_Date_CDT = as.Date(file_dates))

# Create a data frame of dates mapped to a sequence number. The sequence number act as place holders and will be used to convert the loop to dates.
date_map <- bind_cols(plot_range,day_num)
date_mes = NULL
date_arr = NULL

# For loop to assign each expected event date to each expected arrived date. Object grows by one each iteration, store in object created in line above
# Logic: Each day you need to have a spot for each previous day all the way back to the start date.
# If start date is 10/1, then on 10/5 you need 5 spots (for 10/1, 10/2, 10/3, 10/4, and 10/5)
for (i in 1:nrow(plot_range)) {
  # Assigns start and end positions based on sequence order number
  row_start <- sum(1:(i-1))+1 
  row_end <- sum(1:i)
  # fills in sequence numbers to the correct positions in the vector
  date_mes[row_start:row_end] <- (seq(from = 1, to = i, by = 1))
  date_arr[row_start:row_end] <- (rep(i,times = i))
}

# Map dates to sequence numbers
date_mes_frame <- data.frame(date_mes)
colnames(date_mes_frame) <- "seq_order"
date_arr_frame <- data.frame(date_arr)
colnames(date_arr_frame) <- "seq_order"
mes_mapped <- left_join(date_mes_frame,date_map, by = "seq_order") %>%
  select(date)
colnames(mes_mapped) <- "Recorded_Date"
arr_mapped <- left_join(date_arr_frame,date_map, by = "seq_order") %>%
  select(date)
colnames(arr_mapped) <- "Arrived_Date_CDT"
visit_mapped <- left_join(date_mes_frame,date_map, by = "seq_order") %>%
  select(date)
colnames(visit_mapped) <- "C_Visit_Date"

# Join event dates to arrived dates, in the correct order
gap_fill <- bind_cols(mes_mapped,arr_mapped) %>%
  filter(Arrived_Date_CDT >= earliest_file)
gap_fill$Recorded_Date <- as.Date(gap_fill$Recorded_Date)
gap_fill$Arrived_Date_CDT <- as.Date(gap_fill$Arrived_Date_CDT) 

# Join visit dates to arrived dates, in the correct order
gap_fill_visit <- bind_cols(visit_mapped,arr_mapped) %>%
  filter(Arrived_Date_CDT >= earliest_file)
#gap_fill_visit$C_Visit_Date <- as.Date.character(gap_fill$C_Visit_Date)
gap_fill_visit$Arrived_Date_CDT <- as.Date(gap_fill$Arrived_Date_CDT)

## Fill gaps by each parent organization ########################################################################################################################################

# List of parent organizations in the current AZ MFT
org_list <- (unique(MFT_tidy$Parent_Organization))

#
# For message counts 
#

# Empty list to store output of fill loop
filled_list <- list()

# Fill loop for file gaps and event date gaps
for (j in (1:length(org_list))) {
  
  # Fills file gaps
  file_fill <- message_total_file_w_line %>%
    as_tibble() %>%
    filter(Parent_Organization == org_list[j]) %>%
    select(File_Name, Parent_Organization, Arrived_Date_CDT) %>%
    full_join(file_range, by = "Arrived_Date_CDT") %>%
    mutate(no_file = "No file arrived") %>%
    mutate(parent_org = org_list[j])
  ind1 <- which(is.na(file_fill$File_Name))
  file_fill[ind1,"File_Name"] <- file_fill[ind1, "no_file"]
  file_fill[ind1,"Parent_Organization"] <- file_fill[ind1,"parent_org"]
  file_fill_final <- file_fill %>%
    select(File_Name, Parent_Organization, Arrived_Date_CDT)
  
  # Fills event date gaps
  gap_fill_w_file <- gap_fill %>%
    full_join(file_fill_final, by = "Arrived_Date_CDT")
  
  # List of new filled data frames for each parent organization
  list_output <- list(gap_fill_w_file = gap_fill_w_file)
  filled_list[j] <- list_output
}

# Concatenates the list created from the for loop into a data frame
all_orgs_filled <- plyr::ldply(filled_list, data.frame)

# Find unique event dates by organization, file name, etc and merge with file_range for later merge with filled range
files_org <- test_back_merge %>% # created in line 140
  distinct(File_Name, Parent_Organization, Arrived_Date_CDT, Arrived_Date_CDT_Time, Recorded_Date, messages, cum_messages) %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT)) %>%
  full_join(file_range, by = "Arrived_Date_CDT")

files_org <- files_org %>%
  group_by(Recorded_Date,File_Name,cum_messages)

joins <- c("Arrived_Date_CDT", "File_Name", "Recorded_Date")

test_fill <- full_join(files_org,all_orgs_filled, by = joins) %>%
  tbl_df() %>%
  mutate(Recorded_Date = as.Date(Recorded_Date))
ind_file <- which(is.na(test_fill$Arrived_Date_CDT_Time))
test_fill[ind_file, "messages"] <- 0
test_fill[ind_file,"Parent_Organization.x"] <- test_fill[ind_file,"Parent_Organization.y"]

ind_rm_na1 <- which(is.na(test_fill$Recorded_Date)&(is.na(test_fill$File_Name)))
test_fill <- test_fill[-ind_rm_na1,] %>%
  arrange(Recorded_Date, Arrived_Date_CDT, Parent_Organization.x)
test_fill_final <- test_fill %>%
  select(colnames(test_fill[,1:8])) %>%
  mutate(File_Title = paste(Arrived_Date_CDT,":",File_Name)) %>%
  distinct()

first_line_df <- message_total_by_file %>%
  ungroup(Parent_Organization) %>%
  mutate(first_line = paste(message_total_by_file$message_total,"messages")) %>%
  select(File_Name, first_line, message_total, Arrived_Time_CDT)

cum_update <- test_fill_final %>%
  group_by(Parent_Organization.x, Recorded_Date) %>%
  arrange(File_Title) %>%
  mutate(cum_messages = cumsum(messages)) %>%
  rename(Parent_Organization = Parent_Organization.x) %>%
  full_join(first_line_df, by = "File_Name")

ind_first_line_fill <- which(is.na(cum_update$first_line))
cum_update[ind_first_line_fill,"first_line"] <- "0 messages"

cum_update <- subset(cum_update, select = -(Parent_Organization.y)) 

# Convert 0 messages back to NA, better for plotting
ind_0 <- which(cum_update$messages == 0)
cum_update$messages[ind_0] <- NA

fill_titles_lines <- cum_update %>%
  ungroup(Recorded_Date) %>%
  select(File_Title, Parent_Organization, first_line, message_total, Arrived_Date_CDT, Arrived_Date_CDT_Time, Arrived_Time_CDT) %>%
  distinct(File_Title, Parent_Organization, .keep_all = TRUE) 

#
# For visit counts 
#

# Empty list to store output of fill loop for visit counts
filled_list_visit <- list()

# Fill loop for file gaps and visit date gaps
for (j in (1:length(org_list))) {
  
  # Fills file gaps
  file_fill_visit <- visit_total_file_w_line %>%
    as_tibble() %>%
    filter(Parent_Organization == org_list[j]) %>%
    select(File_Name, Parent_Organization, Arrived_Date_CDT) %>%
    full_join(file_range, by = "Arrived_Date_CDT") %>%
    mutate(no_file = "No file arrived") %>%
    mutate(parent_org = org_list[j])
  ind_v <- which(is.na(file_fill_visit$File_Name))
  file_fill_visit[ind_v,"File_Name"] <- file_fill_visit[ind_v, "no_file"]
  file_fill_visit[ind_v,"Parent_Organization"] <- file_fill_visit[ind_v,"parent_org"]
  file_fill_final_visit <- file_fill_visit %>%
    select(File_Name, Parent_Organization, Arrived_Date_CDT)
  
  # Fills visit date gaps
  gap_fill_w_file_visit <- gap_fill_visit %>%
    full_join(file_fill_final_visit, by = "Arrived_Date_CDT")
  
  # List of new filled data frames for each parent organization
  list_output_visit <- list(gap_fill_w_file_visit = gap_fill_w_file_visit)
  filled_list_visit[j] <- list_output_visit
}

# Concatenates the list created from the for loop into a data frame for visits
all_orgs_filled_visit <- plyr::ldply(filled_list_visit, data.frame)

# Find unique event dates by organization, file name, etc and merge with file_range for later merge with filled range
files_org_visit <- test_back_merge_visit %>% 
  distinct(File_Name, Parent_Organization, Arrived_Date_CDT, Arrived_Date_CDT_Time, C_Visit_Date, visits, cum_visits) %>%
  mutate(Arrived_Date_CDT = as.Date(Arrived_Date_CDT)) %>%
  ungroup(C_Visit_Date) %>%
  mutate(C_Visit_Date = as.Date(C_Visit_Date)) %>%
  full_join(file_range, by = "Arrived_Date_CDT")

joins_visit <- c("Arrived_Date_CDT", "File_Name", "C_Visit_Date")

visit_fill <- full_join(files_org_visit,all_orgs_filled_visit, by = joins_visit) %>%
  tbl_df() %>%
  mutate(C_Visit_Date = as.character.Date(C_Visit_Date))
ind_file <- which(is.na(visit_fill$Arrived_Date_CDT_Time))
visit_fill[ind_file, "visits"] <- 0
visit_fill[ind_file,"Parent_Organization.x"] <- visit_fill[ind_file,"Parent_Organization.y"]

ind_rm_naV <- which(is.na(visit_fill$C_Visit_Date)&(is.na(visit_fill$File_Name)))
visit_fill <- visit_fill[-ind_rm_naV,] %>%
  arrange(C_Visit_Date, Arrived_Date_CDT, Parent_Organization.x)
visit_fill_final <- visit_fill %>%
  select(colnames(visit_fill[,1:8])) %>%
  mutate(File_Title = paste(Arrived_Date_CDT,":",File_Name)) %>%
  distinct()

#colnames(visit_fill_final)[2] <- "File_Name"

first_line_visit <- visit_total_by_file %>%
  ungroup(Parent_Organization) %>%
  mutate(first_line = paste(visit_total_by_file$visit_total,"ED registrations")) %>%
  select(File_Name, first_line, visit_total, Arrived_Time_CDT)

cum_update_visit <- visit_fill_final %>%
  group_by(Parent_Organization.x, C_Visit_Date) %>%
  arrange(File_Title) %>%
  mutate(cum_visits = cumsum(visits)) %>%
  rename(Parent_Organization = Parent_Organization.x) %>%
  full_join(first_line_visit, by = "File_Name")

ind_first_line_fillV <- which(is.na(cum_update_visit$first_line))
cum_update_visit[ind_first_line_fillV,"first_line"] <- "0 ED registrations"

cum_update_visit <- subset(cum_update_visit, select = -(Parent_Organization.y)) 

# Convert 0 messages back to NA, better for plotting

ind_V <- which(cum_update_visit$visits == 0)
cum_update_visit$visits[ind_V] <- NA

fill_titles_lines_visit <- cum_update_visit %>%
  ungroup(C_Visit_Date) %>%
  select(File_Title, Parent_Organization, first_line, visit_total, Arrived_Date_CDT, Arrived_Date_CDT_Time, Arrived_Time_CDT) %>%
  distinct(File_Title, Parent_Organization, .keep_all = TRUE) 
