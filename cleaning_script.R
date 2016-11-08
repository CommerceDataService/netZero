################################################################################################################
# This code cleans and preprocesses labview and alternate system data 
# Be sure to run the convert_tdms_to_csv.py first, to convert TDMS files into CSVs
################################################################################################################

###############
# working directories
###############

### update these file paths to your local directory ###

# set working directories for raw data (data that was converted from tdms to csv) and for processed data (cleaned csv files)
mainWD <- "/Users/prioberoi/Documents/nist/netZero"
rawDataWD <- paste0(mainWD, "/data/raw")
rawDataAltSystemWD <- paste0(rawDataWD, "/AlternateSystem") # only use this file path if data from an alternate system needs to be imported as well
processedDataWD <- paste0(mainWD, "/data/processed")
userUpdatesWD <- paste0(mainWD, "/data/user_updates")

###############
# Get data and clean
############### 
ptm <- proc.time()

# Create object to house all data with minute-level timestamps
d <- data.frame(Timestamp = seq(ISOdatetime(2015, 02, 01, 0, 0, 0, tz = "EST"), ISOdatetime(2016, 02, 01, 0, 0, 0, tz = "EST"), "min"))

# import data from the raw files, add new channels (columns) as needed or add data to existing channels
paths <- dir(path = rawDataWD, pattern = "\\.csv$", full.names = TRUE)

for(path in paths){
  # import each csv, which has one day worth of data
  day <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
  # remove _
  names(day) <- gsub("_", "", names(day))
  # separate subsystem from channel in var names
  names(day) <- sub("\\.\\.\\.", "_", names(day))
  # remove . from variable names
  names(day) <- gsub("\\.", "", names(day))
  # remove X prefix from variable names
  day$X <- NULL
  names(day) <- sub("^X", "", names(day))
  # format timestamp as object of class POSIXlt
  day$TimeStamp_SystemTime <- strptime(substr(day$TimeStamp_SystemTime, 1, 19), "%Y-%m-%d %H:%M:%S", tz = "GMT")
  day$TimeStamp_SystemTime <- as.POSIXct(day$TimeStamp_SystemTime)
  # set to EST instead of GMT
  attr(day$TimeStamp_SystemTime, "tzone") <- "EST"
  # create new timestamp rounded to the minute
  day$Timestamp <- round(day$TimeStamp_SystemTime, units = "min")
  day <- day[!(duplicated(day$Timestamp)),]
  # Remove extraneous timestamp variables, like TimeStamp_GPSTime, TimeStamp_SystemTime, TimeStamp_SchedulerTime
  day <- day[,!(names(day) %in% c("TimeStamp_GPSTime", "TimeStamp_SystemTime", "TimeStamp_SchedulerTime"))]
  # add any new channels (columns) from day to d
  temp <- as.data.frame(matrix(NA, ncol = length(names(day)[!(names(day) %in% names(d))]), nrow = nrow(d)))
  names(temp) <- names(day)[!(names(day) %in% names(d))]
  d <- cbind(d, temp)
  # list of all variables that have data that need to be added to d from day
  vars <- names(day)[!(names(day) %in% grep("^Time", names(day), value = TRUE))]
  # add data to channels where timestamp matches
  d[as.character(d$Timestamp) %in% as.character(day$Timestamp),vars] <- day[as.character(day$Timestamp) %in% as.character(d$Timestamp),vars]
}
proc.time() - ptm

############### 
# Add data from alternate system
############### 

### only run the code below if you are also adding data from an alternate instrumentation system that uses txt files ###
### remember to update the string below to indicate which subsystem these new channels belong to ###
altSubSystem <- "HVAC"

# import data from alternate instrument acquisition system, add new channels (columns) as needed, and add data to existing channels 
paths <- dir(path = rawDataAltSystemWD, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)

for(path in paths){
  day <- read.csv(path, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
  # remove _
  names(day) <- gsub("\\_", "", names(day))
  # prefix name of subsystem for new channels
  names(day) <- paste0(altSubSystem, "_", names(day))
  # parse date from file name in path 
  start_loc <- regexpr("......\\.txt", path)[1]
  date <- paste0(strsplit(path, "")[[1]][start_loc:(start_loc+5)], collapse = '')
  year <- as.numeric(paste0("20", substr(date, 1, 2)))
  month <- as.numeric(substr(date, 3, 4))
  dom <- as.numeric(substr(date, 5, 6))
  # create new timestamp rounded to the minute
  day$Timestamp <- seq(from = ISOdatetime(year, month, dom, 0, 0, 0, tz = "EST"), by = "min", length.out = nrow(day))
  # add any new channels
  temp <- as.data.frame(matrix(NA, ncol = length(names(day)[!(names(day) %in% names(d))]), nrow = nrow(d)))
  names(temp) <- names(day)[!(names(day) %in% names(d))]
  d <- cbind(d, temp)
  # add data to channels
  vars <- names(day)
  vars <- vars[!(vars %in% grep("^Time", vars, value = TRUE))]
  d[as.character(d$Timestamp) %in% as.character(day$Timestamp),vars] <- day[,vars]
}
proc.time() - ptm # approximately 48 mins

##########
# Remove channels (variables) flagged for removal 
##########

### only run this segment of code if there are particular variables you would like removed from the dataset ###
### variable names included in the DataToBeExcluded file, will be removed from the data in the code below ###
### refer to DataToBeExcluded.csv from the user_updates folder in the repo ###

remove <- read.csv(paste0(userUpdatesWD, "/DataToBeExcluded.csv"), header = TRUE, stringsAsFactors = FALSE)
remove$Channel <- gsub("[[:punct:]]", "", remove$Channel)
remove$Channel <- gsub(" ", "", remove$Channel)
remove <- remove$Channel[!duplicated(remove$Channel)]
# remove the channels from data
for(i in 1:length(remove)){
  temp <- grep(paste0("_", remove[i], "$"), names(d), ignore.case = TRUE, value = TRUE)
  d[,temp] <- NULL
  remove[i] <- temp
}

##########
# Rename variables for interpretability
##########

### refer to newVariableNames.csv from the user_updates folder in the repo ###

# manual override of channel names
rename <- read.csv(paste0(userUpdatesWD, "/newVariableNames.csv"), header = TRUE, stringsAsFactors = FALSE)
updatedNames <- setNames(rename$NewDataLabel, rename$OldDataLabel)

# updated variable names with manual override names
names(d)[names(d) %in% rename$OldDataLabel] <- updatedNames[names(d)[names(d) %in% rename$OldDataLabel]]

##########
# More time variable processing
##########

# create variable for day of week
d$DayOfWeek <- as.factor(weekdays(d$Timestamp))
# update timestamp count
d$TimeStamp_Count <- 1:nrow(d)
# store list of time variables
timeVars <- c(names(d)[grep("time", names(d), ignore.case = TRUE)], "DayOfWeek")

proc.time() - ptm

###############
# create metadata object from d and data dictionary
###############

metadata <- data.frame(variable = names(d))
metadata$subsystem <- NA
metadata$subsystem[!(metadata$variable %in% timeVars)] <- unlist(lapply(as.character(metadata$variable[!(metadata$variable %in% timeVars)]), function(x) substr(x, 1, grep("_", strsplit(x, split = "")[[1]])-1)))
subsystems <- unique(metadata$subsystem[!is.na(metadata$subsystem)])

# import data dictionary
dic <- read.csv(paste0(userUpdatesWD, "/dataDictionary.csv"), header = TRUE, stringsAsFactors = FALSE, na.strings = c("", NA, " "))

# Pull variable descriptions, measurement location, measured parameter and units from data dictionary
metadata <- merge(metadata, dic[,c("Data.Label", "Measurement.Location", "Measured.Parameter", "Description", "Units")], by.x = 'variable', by.y = 'Data.Label', all.x = TRUE)

# rename metadata columns
names(metadata) <- c("Variable", "Subsystem", "Measurement_Location", "Parameter", "Description", "Units")

metadata[metadata$Variable %in% timeVars,'Units'] <- "Date/Time"
metadata$Units[grep("_Status", metadata$Variable)] <- "Binary Status"
metadata$max_value <- 0
metadata$min_value <- 0
for(col in names(d)){
  metadata$max_value[metadata$Variable == col] <- round(max(as.numeric(d[,col]), na.rm = TRUE), digits = 4)
  metadata$min_value[metadata$Variable == col] <- round(min(as.numeric(d[,col]), na.rm = TRUE), digits = 4)
}

###############
# write minute-level CSVs to processed folder
###############

# write entire dataset
write.csv(d, file = paste0(processedDataWD, "/data-minute.csv"), row.names = FALSE)

# write CSV for each subsystem
for(subsystem in subsystems){
  vars <- c(timeVars, names(d)[grep(paste0("^",subsystem,"_"), names(d))])
  data_subsystem <- d[,vars]
  write.csv(data_subsystem, file = paste0(processedDataWD, "/", subsystem, "-minute.csv"), row.names = FALSE)
}

proc.time() - ptm #approximately 78 mins

###############
# Create hourly-aggregates
###############

### refer to HourlyDataConversions.csv from the user_updates folder in the repo ###

# manual override of channel names
hourly <- read.csv(paste0(userUpdatesWD, "/HourlyDataConversions.csv"), header = TRUE, stringsAsFactors = FALSE)
useAvg <- hourly$Data.Label[hourly$Hourly.conversion == "Average"]
useValueAtHour <- hourly$Data.Label[hourly$Hourly.conversion == "Hour value"]
useSum <- hourly$Data.Label[hourly$Hourly.conversion == "Sum"]

# create dataframe to store hourly aggregates
d_hour <- data.frame(Timestamp = seq(ISOdatetime(2015, 02, 01, 0, 0, 0, tz = "EST"), ISOdatetime(2016, 01, 31, 0, 0, 0, tz = "EST"), "hour"))
temp <- as.data.frame(matrix(NA, nrow = nrow(d_hour), ncol = length(names(d)[!(names(d) %in% timeVars)])))
names(temp) <- names(d)[!(names(d) %in% timeVars)]
d_hour <- cbind(d_hour, temp)

# populate the aggregates based on average of values in the hour
for(hour in d_hour$Timestamp){
  temp <- apply(d[(d$Timestamp >= hour) & (d$Timestamp < (hour + 60*60)), useAvg], 
                2, 
                function(x) ifelse(length(x[is.na(x)]) == length(x), NA, mean(x, na.rm = TRUE)))
  d_hour[d_hour$Timestamp == hour,useAvg] <- temp
}

# populate the aggregates based on max value (value at the end of the hour)
for(hour in d_hour$Timestamp){
  temp <- apply(d[(d$Timestamp >= hour) & (d$Timestamp < (hour + 60*60)), useValueAtHour], 
                                                           2, 
                                                           function(x) ifelse(length(x[is.na(x)]) == length(x), NA, max(x, na.rm = TRUE)))
  d_hour[d_hour$Timestamp == hour,useValueAtHour] <- temp
}

# populate the aggregate (only one variable) based on sum of values across the hour
for(hour in d_hour$Timestamp){
  temp <- sum(d[(d$Timestamp >= hour) & (d$Timestamp < (hour + 60*60)), useSum], na.rm = TRUE)
  d_hour[d_hour$Timestamp == hour, useSum] <- temp
}

# update metadata file with aggregation method used
metadata$Aggregation_Method[metadata$Variable %in% useAvg] <- "Average"
metadata$Aggregation_Method[metadata$Variable %in% useSum] <- "Sum"
metadata$Aggregation_Method[metadata$Variable %in% useValueAtHour] <- "Hour value"

###############
# write hourly-aggregated CSVs to processed folder
###############

# write entire dataset
write.csv(d_hour, file = paste0(processedDataWD, "/data-hour.csv"), row.names = FALSE)

# write CSV for each subsystem
for(subsystem in subsystems){
  vars <- c("Timestamp", names(d_hour)[grep(paste0("^",subsystem,"_"), names(d_hour))])
  data_subsystem <- d_hour[,vars]
  write.csv(data_subsystem, file = paste0(processedDataWD, "/", subsystem, "-hour.csv"), row.names = FALSE)
}

proc.time() - ptm

###############
# Write metadata file
###############

# write metadata file
write.csv(metadata, file = paste0(processedDataWD, "/metadata.csv"), row.names = FALSE)

#save(d, d_hour, data_subsystem, day, dic, metadata, remove, altSubSystem, mainWD, path, paths, processedDataWD, rawDataWD, rawDataAltSystemWD, subsystem, subsystems, timeVars, vars, file = '/Users/prioberoi/Documents/nist/netZero/internal/cleaning_script_2016-10-18.Rda')