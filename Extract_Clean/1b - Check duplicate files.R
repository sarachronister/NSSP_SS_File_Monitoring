# Can only be run after you have run "DailyDQ - clean and run app.R" script - DO NOT CLEAR ENVIRONMENT

# Change text in quotes to parent org you want to check - must be verbatim and is case-sensitive
parent_org_check <- "Parent Org Name Here"

dups <- message_per_event_by_file %>%
  filter(Parent_Organization == parent_org_check) %>%
  # Enter the file names between double quotes that you suspect are duplicated, one per line
  # For multiple files, there should be a | at the end of the line for all but the last files and a ) after the last file
  filter(File_Name == "File Name 1 Here") # | 
         # File_Name == "File Name 2 Here")

dups$dup = duplicated(dups[,c("Recorded_Date","File_Name","Arrived_Date_CDT","messages","cum_messages")])

dups_true <- dups %>%
  filter(dup == TRUE) %>%
  ungroup() %>%
  select(Recorded_Date,File_Name,Arrived_Date_CDT,messages,cum_messages)
dups_false <- dups %>%
  filter(dup == FALSE) %>%
  ungroup() %>%
  select(Recorded_Date,File_Name,Arrived_Date_CDT,messages,cum_messages)

# All match_ vars should be 1 (or NA) if they are duplicates
dups_check <- bind_cols(dups_true,dups_false) %>%
  mutate(match_rec = ifelse(Recorded_Date == Recorded_Date1,"DUPLICATED","NOT DUPLICATED")) %>%
  mutate(match_file = ifelse(File_Name == File_Name1,"DUPLICATED","NOT DUPLICATED")) %>%
  mutate(match_arr = ifelse(Arrived_Date_CDT == Arrived_Date_CDT1,"DUPLICATED","NOT DUPLICATED")) %>%
  mutate(match_mes = ifelse(messages == messages1,"DUPLICATED","NOT DUPLICATED")) %>%
  mutate(match_cum = ifelse(cum_messages == cum_messages1,"DUPLICATED","NOT DUPLICATED"))

# See results in console below
# If it's a completely duplicated file, you will only see DUPLICATED
table(dups_check$match_arr)
table(dups_check$match_rec)
table(dups_check$match_file)
table(dups_check$match_mes)
table(dups_check$match_cum)
