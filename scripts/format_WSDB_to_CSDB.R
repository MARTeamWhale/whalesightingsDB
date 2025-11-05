
# Script to format data exported from WSDB and prepare for batch input to CSDB 

# INPUT:
  ## one .csv file with data records exported from WSDB

# OUTPUT OPTIONS:

  # If create_batch_files = TRUE:

    ## subfolder containing:
      ### one .xlsx file per data source for data source codes with >100 records
      ### one .xlsx file labeled "MULTI" including all data source codes with <100 records

    ## note that the criteria outlined above also apply to the "UNKNOWN" data source code ("0")

  # If create_batch_files = FALSE:

    ## one .xlsx file containing all records from the input file

# developed by M. Murphy 2024-02-16; modified by J. Stanistreet 2024-05-30

#####################################################

### TO RUN SCRIPT, MODIFY THESE LINES:

# specify csv input file (without including ".csv")
filename <- "2000export"

# select whether to create batch output files split by data source or single output matching input file
create_batch_files = FALSE

# run code below

#####################################################

##### PART 1: SET UP #####

# Install/load required packages
pacman::p_load(tidyverse, writexl)

# Set input directory
input_directory <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\CSDB\WSDB_to_CSDB\Exported_from_WSDB\)"

# Set output directory
output_directory = r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\CSDB\WSDB_to_CSDB\Formatted_for_CSDB\)"

# If create_batch is TRUE, check if output subfolder exists & create it if needed
if(create_batch_files) {
  if(!dir.exists(paste0(output_directory, filename)))
   {
  dir.create(paste0(output_directory, filename))
  }
  output_subfolder = paste0(output_directory, filename, "\\")
} else {
  output_subfolder = output_directory
}

# Set code table directory
code_table_directory <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\CSDB\WSDB_to_CSDB\Code_tables\)"

# Load code tables
Species_code_table <- read_csv(paste0(code_table_directory, "Species_code_table.csv"))
Activity_type_code_table <- read_csv(paste0(code_table_directory, "Activity_type_code_table.csv"))
Effort_code_table <- read_csv(paste0(code_table_directory, "Effort_code_table.csv"))
Data_source_code_table <- read_csv(paste0(code_table_directory, "Data_source_code_table.csv"))

# Load behaviour code table and add extra columns for CD1 through CD5 and DESC1 through DESC5
Behaviour_code_table <- read_csv(paste0(code_table_directory, "Behaviour_code_table.csv")) %>% 
  mutate(BEHAVIOUR_CD1 = BEHAVIOUR_CD,
         BEHAVIOUR_CD2 = BEHAVIOUR_CD,
         BEHAVIOUR_CD3 = BEHAVIOUR_CD,
         BEHAVIOUR_CD4 = BEHAVIOUR_CD,
         BEHAVIOUR_CD5 = BEHAVIOUR_CD,
         BEHAVIOUR_DESC_1 = BEHAVIOUR_DESC,
         BEHAVIOUR_DESC_2 = BEHAVIOUR_DESC,
         BEHAVIOUR_DESC_3 = BEHAVIOUR_DESC,
         BEHAVIOUR_DESC_4 = BEHAVIOUR_DESC)

# Load CSDB fields template
#CSDB_template<-read_csv(paste0(code_table_directory, "WSDBtoCSDB_template.csv"))

# Load WSDB data
WSDB_data <- read_csv(paste0(input_directory, filename, ".csv"),
                      locale = readr::locale(encoding = "Cp1252"))

# Turn off scientific notation
options(scipen = 999)

##### PART 2: FORMAT DATA FOR CSDB #####

# 1) Format Year Month and Day to pull from WS_DATETIME_UTC when available, otherwise use WS_DATETIME
CSDB_data <- WSDB_data %>%
  mutate(Year = ifelse(is.na(as_date(WS_DATETIME_UTC)), year(as_date(WS_DATETIME)), year(as_date(WS_DATETIME_UTC))),
         Month = ifelse(is.na(as_date(WS_DATETIME_UTC)), month(as_date(WS_DATETIME)), month(as_date(WS_DATETIME_UTC))),
         Day = ifelse(is.na(as_date(WS_DATETIME_UTC)), day(as_date(WS_DATETIME)), day(as_date(WS_DATETIME_UTC))))

# 2) Modify WS_EVENT_ID to Regional_Primary_Key with first 2 digits “11” (representing MAR Region)
# and the total number of digits is 11 (including the WS_EVENT_ID)
# example: if WS_EVENT_ID is 1234, then the Regional_Primary_Key should be 11000001234  
CSDB_data <- CSDB_data %>%
  mutate(Regional_Primary_Key = as.numeric(paste(11, sprintf("%09d", WS_EVENT_ID), sep= "")))

# 3) Change the format of UTC_Time and Reported_Time to hh:mm:ss
CSDB_data <- CSDB_data %>%
  mutate(UTC_Time = ifelse(!is.na(WS_TIME_UTC), sprintf("%04d", WS_TIME_UTC), NA),
         UTC_Time = parse_date_time(UTC_Time, orders= "HM"),
         UTC_Time = format(UTC_Time, format = "%H:%M:%S"))

CSDB_data <- CSDB_data %>%
  mutate(Reported_Time = ifelse(!is.na(WS_TIME), sprintf("%04d", WS_TIME), NA),
         Reported_Time = parse_date_time(Reported_Time, orders= "HM"),
         Reported_Time = format(Reported_Time, format = "%H:%M:%S"))

# 4) Map LCQECODE_CD to Location_Uncertainty. Note: Location_Uncertainty codes that are identical to LCQECODE_CDs remain unchanged 
# example: LCQECODE_CD = 1, Location_Uncertainty = 1
CSDB_data <- CSDB_data %>%
  mutate(Location_Uncertainty = case_when(LCQECODE_CD == 4 ~ 3, 
                                          LCQECODE_CD == 5 ~ 3, 
                                          LCQECODE_CD == 6 ~ 3, 
                                          LCQECODE_CD == 7 ~ 3,
                                          is.na(LCQECODE_CD) ~ 0,
                                          TRUE ~ as.numeric(LCQECODE_CD)))

# 5) Map LCQECODE_CD to Location_Uncertainty_Reason
CSDB_data <- CSDB_data %>%
  mutate(Location_Uncertainty_Reason = case_when(LCQECODE_CD == 3 ~ 1, 
                                                 LCQECODE_CD == 4 ~ 2, 
                                                 LCQECODE_CD == 5 ~ 3, 
                                                 LCQECODE_CD == 6 ~ 4, 
                                                 LCQECODE_CD == 7 ~ 5,
                                                 is.na(LCQECODE_CD) ~ 0,
                                                 LCQECODE_CD == 1 ~ NA_real_,
                                                 LCQECODE_CD == 2 ~ NA_real_,
                                                 TRUE ~ as.numeric(LCQECODE_CD)))

# 6) Map COMMONNAME to ITIS_Code and Species_code using Species_code_table
CSDB_data <- CSDB_data %>%
  left_join(Species_code_table, by = "COMMONNAME")

# 7) Map TRIPTYPE_CD to Activity_Type_Code using Activity_type__code_table
CSDB_data <- CSDB_data %>% 
  left_join(Activity_type_code_table, by = "TRIPTYPE_CD")

# 8) Map DATA_TYPE_CD to Effort using Effort_code_table (Null DATA_TYPE_CD maps to "0")
CSDB_data <- CSDB_data %>%
  left_join(Effort_code_table, by = "DATA_TYPE_CD")

# 9) Map Data_Source_Code using Data_source_table (Null DATASOURCE_CD maps to "0")
CSDB_data <- CSDB_data %>% 
  left_join(Data_source_code_table, by = "DATASOURCE_CD")

# 10) Map IDREL_CD to SpeciesID_Uncertainty (put 0's in for null). Note: IDREL_CDs that are identical to SpeciesID_Uncertainty_Codes remain unchanged 
# example: IDREL_CD = 1, SpeciesID_Uncertainty = 1.
CSDB_data <- CSDB_data %>%
  mutate(SpeciesID_Uncertainty = case_when(IDREL_CD == 9 ~ 0,
                                           is.na(IDREL_CD) ~ 0,
                                           TRUE ~ as.numeric(IDREL_CD)))

# 11) Map COUNT_UNCERTAINTY_CD to Count_Uncertainty (put 0’s in for null)
CSDB_data <- CSDB_data %>%
  mutate(Count_Uncertainty = case_when(is.na(COUNT_UNCERTAINTY_CD) ~ 0,
                                        TRUE ~ as.numeric(COUNT_UNCERTAINTY_CD)))

# 12) Map behaviour codes to descriptions using Behaviour_code_table
CSDB_data = CSDB_data %>%
  left_join(select(Behaviour_code_table, BEHAVIOUR_CD1, BEHAVIOUR_DESC), by = "BEHAVIOUR_CD1") %>% 
  left_join(select(Behaviour_code_table, BEHAVIOUR_CD2, BEHAVIOUR_DESC_1), by = "BEHAVIOUR_CD2") %>% 
  left_join(select(Behaviour_code_table, BEHAVIOUR_CD3, BEHAVIOUR_DESC_2), by = "BEHAVIOUR_CD3") %>% 
  left_join(select(Behaviour_code_table, BEHAVIOUR_CD4, BEHAVIOUR_DESC_3), by = "BEHAVIOUR_CD4") %>%
  left_join(select(Behaviour_code_table, BEHAVIOUR_CD5, BEHAVIOUR_DESC_4), by = "BEHAVIOUR_CD5")

# 12a) Change 'NOT RECORDED' to NA from BEHAVIOUR_DESC columns
CSDB_data = CSDB_data %>%
  mutate(across(starts_with("BEHAVIOUR_DESC"),~ case_when(. == "NOT RECORDED" ~ NA_character_,
                                                          TRUE ~ .), .names = "new_{.col}"))

# 12b) Concatenate the five BEHAVIOUR_DESC columns separated by hyphens into a new column called Behaviour_Comments, removing NA's
CSDB_data <- CSDB_data %>%
  unite('Behaviour_Comments', c('new_BEHAVIOUR_DESC','new_BEHAVIOUR_DESC_1','new_BEHAVIOUR_DESC_2','new_BEHAVIOUR_DESC_3','new_BEHAVIOUR_DESC_4'), sep=" - ", na.rm = TRUE)

# 13) Map Animal_Status_Code using GEARIMPACT_CD and Behaviour_Comments
CSDB_data <- CSDB_data %>%
  mutate(Animal_Status_Code = case_when(GEARIMPACT_CD == 9 ~ 0,
                                        GEARIMPACT_CD == 5 | Behaviour_Comments == "VISIBLE INJURY"| Behaviour_Comments == "STRUCK BY VESSEL" ~ 2,
                                        GEARIMPACT_CD == 1 | GEARIMPACT_CD == 2 | Behaviour_Comments == "TANGLED IN FISHING GEAR"| Behaviour_Comments == "DISENTANGLED RELEASED ALIVE" ~ 3,
                                        GEARIMPACT_CD == 4 ~ 4,
                                        is.na(GEARIMPACT_CD) ~ 0,
                                        TRUE ~ as.numeric(GEARIMPACT_CD)))

# 14) Map BEAUFORT_CD to Reported_SeaState, replace '13' with NA
CSDB_data <- CSDB_data %>%
  mutate(Reported_SeaState = case_when(BEAUFORT_CD == 13 ~ NA,
                                       TRUE ~ BEAUFORT_CD))

# 15) Map PLATFORM_TYPE_CD to Platform_Type_Code
CSDB_data <- CSDB_data %>%
  mutate(Platform_Type_Code = case_when(PLATFORM_TYPE_CD == 4 ~ 0,
                                        is.na(PLATFORM_TYPE_CD) ~ 0,
                                        TRUE ~ as.numeric(PLATFORM_TYPE_CD)))

# 16) Rename the following columns 
CSDB_data <- CSDB_data %>%
  mutate(Suspected_Data_Issue_Reason = SUSPECTED_DATA_ISSUE,
         Species_Comments = FEATURE_DESC,
         Reported_Count = BEST_COUNT,
         Min_Count = MIN_COUNT,
         Max_Count = MAX_COUNT,
         Distance = DISTANCE,
         Latitude = LATITUDE,
         Longitude = LONGITUDE,
         Comments = COMMENTS)

# 17) Add a column called Suspected_Data_Issue before Suspected_Data_Issue_Reason, and populate with ‘Yes’ when the reason column is not null and ‘No’ when it is null 
CSDB_data <- CSDB_data %>%
  mutate(Suspected_Data_Issue = case_when(is.na(Suspected_Data_Issue_Reason) ~ "No",
                                          TRUE ~ "Yes"))

# 18) Select only the columns needed for CSDB data output in the desired order
CSDB_data_formatted <- CSDB_data %>%
  dplyr::select(Regional_Primary_Key, Year, Month, Day, UTC_Time, Reported_Time, Latitude, Longitude, 
                Location_Uncertainty, Location_Uncertainty_Reason, Species_Code, ITIS_Code, SpeciesID_Uncertainty, 
                Species_Comments, Reported_Count, Min_Count, Max_Count, Count_Uncertainty, Animal_Status_Code,
                Behaviour_Comments, Distance, Reported_SeaState, Platform_Type_Code, Activity_Type_Code, Effort, 
                Data_Source_Code, Suspected_Data_Issue, Suspected_Data_Issue_Reason, Comments)

##### PART 3: EXPORT FOR BATCH UPLOAD TO CSDB #####

# If create_batch_files = TRUE, create batch output files by data source, otherwise create single output

if (create_batch_files){
  # Filter for data sources with >= 100 records
large_data_source <- CSDB_data_formatted %>% 
  group_by(Data_Source_Code) %>% 
  filter(n() >= 100) %>% 
  ungroup()

# Split into list of data frames based on data source code
lds_list<-split(large_data_source, large_data_source$Data_Source_Code)

# Filter for data sources with <100 records
multi_source <- CSDB_data_formatted %>% 
  group_by(Data_Source_Code) %>% 
  filter(n() < 100) %>% 
  ungroup() 

# If multiple data sources with <100 records, create MULTI list for output
if(nrow(multi_source)>0 && length(unique(multi_source$Data_Source_Code))>1) {
  
  export_list <- append(lds_list, setNames(list(multi_source), paste("MULTI")))

  # If only one data source with <100 records, created named output
  } else if (nrow(multi_source)>0 && length(unique(multi_source$Data_Source_Code))==1) { 
  
  sds_list<-split(multi_source, multi_source$Data_Source_Code)
  export_list <- append(lds_list, sds_list)
  
} else {
  
  export_list <- lds_list
}

# Export .xlsx files
lapply(names(export_list), function (x) {
  write_xlsx(export_list[[x]],
             path = paste0(
               output_subfolder, format(Sys.Date(), "%Y%m%d"), "_", filename, "_", x, ".xlsx"
             ))
})

} else {
  
  write_xlsx(CSDB_data_formatted,
             path = paste0(output_subfolder, format(Sys.Date(), "%Y%m%d"), "_", str_extract(filename, "[^_]+$"), ".xlsx"))
}

