# Be sure to run the convert_tdms_to_csv.py first, to convert TDMS files into CSVs

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
d <- data.frame(Timestamp = seq(ISOdatetime(2015, 02, 01, 0, 0, 0, tz = "EST"), ISOdatetime(2016, 01, 31, 0, 0, 0, tz = "EST"), "min"))

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
  day$TimeStamp_SystemTime <- strptime(substr(day$TimeStamp_SystemTime, 1, 19), "%Y-%m-%d %H:%M:%S", tz = "EST")
  # create new timestamp rounded to the minute
  day$Timestamp <- round(day$TimeStamp_SystemTime, units = "min")
  day <- day[!(duplicated(day$Timestamp)),]
  # add any new channels (columns) from day to d
  temp <- as.data.frame(matrix(0, ncol = length(names(day)[!(names(day) %in% names(d))]), nrow = nrow(d)))
  names(temp) <- names(day)[!(names(day) %in% names(d))]
  d <- cbind(d, temp)
  # add data to channels
  vars <- names(day)
  vars <- vars[!(vars %in% vars[grep("^Time", vars)])]
  d[as.character(d$Timestamp) %in% as.character(day$Timestamp),vars] <- day[,vars]
}
proc.time() - ptm

############### 
# Add data from alternate system
############### 

### only run the code below if you are also adding data from an alternate instrumentation system that uses txt files ###
### note that this code runs after the code above which removes any unwanted variables ###
### additionally, update the string below to indicate which subsystem these channels belong to ###
altSubSystem <- "HVAC"

# import data from alternate instrument acquisition system, add new channels (columns) as needed, and add data to existing channels 
paths <- dir(path = rawDataAltSystemWD, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)

for(path in paths){
  day <- read.csv(path, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
  # remove _
  names(day) <- gsub("\\_", "", names(day))
  # prefix name of subsystem for new channels
  names(day) <- paste0(altSubSystem, "_", names(day))
  # create new timestamp rounded to the minute
  day$Timestamp <- seq(from = ISOdatetime(2015, 02, 01, 0, 0, 0, tz = "EST"), by = "min", length.out = nrow(day))
  # add any new channels
  temp <- as.data.frame(matrix(0, ncol = length(names(day)[!(names(day) %in% names(d))]), nrow = nrow(d)))
  names(temp) <- names(day)[!(names(day) %in% names(d))]
  d <- cbind(d, temp)
  # add data to channels
  vars <- names(day)
  vars <- vars[!(vars %in% vars[grep("^Time", vars)])]
  d[as.character(d$Timestamp) %in% as.character(day$Timestamp),vars] <- day[,vars]
}
proc.time() - ptm # approximately 48 mins

##########
# Remove channels (variables) flagged for removal 
##########

### only run this segment of code if there are particular variables you would like removed from the dataset ###
### some preprocessing was done on this csv manually to flag variables for removal, refer to DataToBeExcluded file in repo ###
### variable names included in the DataToBeExcluded file, will be removed from the data in the code below ###

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

# manual override of channel names
rename <- read.csv(paste0(userUpdatesWD, "/newVariableNames.csv"), header = TRUE, stringsAsFactors = FALSE)
updatedNames <- setNames(rename$NewDataLabel, rename$OldDataLabel)

# updated variable names with manual override names
names(d)[names(d) %in% rename$OldDataLabel] <- updatedNames[names(d)[names(d) %in% rename$OldDataLabel]]

##########
# More time variable processing
##########

# Remove extraneous timestamp variables, like TimeStamp_GPSTime, TimeStamp_SystemTime, TimeStamp_SchedulerTime
d <- d[,!(names(d) %in% c("TimeStamp_GPSTime", "TimeStamp_SystemTime", "TimeStamp_SchedulerTime"))] 
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
dic <- na.omit(dic)

# Pull variable descriptions, measurement location, measured parameter and units from data dictionary
metadata <- merge(metadata, dic[,c("Data.Label", "Measurement.Location", "Measured.Parameter", "Description", "Units")], by.x = 'variable', by.y = 'Data.Label', all.x = TRUE)

# rename metadata columns
names(metadata) <- c("Variable", "Subsystem", "Measurement_Location", "Parameter", "Description", "Units")

metadata[metadata$Variable %in% timeVars,'Units'] <- "Date/Time"
metadata$Units[grep("_Status", metadata$Variable)] <- "Binary Status"
metadata$max_value <- 0
metadata$min_value <- 0
for(col in names(d)){
  metadata$max_value[metadata$Variable == col] <- round(max(as.numeric(d[,col])), digits = 4)
  metadata$min_value[metadata$Variable == col] <- round(min(as.numeric(d[,col])), digits = 4)
}

###############
# write final CSVs to processed folder
###############

# write entire dataset
write.csv(d, file = paste0(processedDataWD, "/data.csv"), row.names = FALSE)

# write CSV for each subsystem
for(subsystem in subsystems){
  vars <- c(timeVars, names(d)[grep(paste0("^",subsystem,"_"), names(d))])
  data_subsystem <- d[,vars]
  write.csv(data_subsystem, file = paste0(processedDataWD, "/", subsystem, ".csv"), row.names = FALSE)
}

# write metadata file
write.csv(metadata, file = paste0(processedDataWD, "/metadata.csv"), row.names = FALSE)

proc.time() - ptm #approximately 78 mins

#save(d, data_subsystem, day, dic, metadata, remove, altSubSystem, channel, mainWD, naics, path, paths, processedDataWD, rawDataWD, rawDataAltSystemWD, subsystem, subsystems, timeVars, vars, file = '/Users/prioberoi/Documents/nist/netZero/internal/cleaning_script_2016-10-18.Rda')
