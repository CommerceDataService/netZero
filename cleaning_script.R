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

##########
# Remove channels (variables) flagged for removal 
##########

### only run this segment of code if there are particular variables you would like removed from the dataset ###
### some preprocessing was done on this csv manually to flag variables for removal, refer to DataToBeIncluded file in repo ###
### rows flagged with a 0 in the remove$Remove column, will be removed in the code below ###

remove <- read.csv(paste0(processedDataWD, "/DataToBeIncluded.csv"), header = TRUE, stringsAsFactors = FALSE)
remove$Channel <- gsub("[[:punct:]]", "", remove$Channel)
remove$Channel <- gsub(" ", "", remove$Channel)
remove$Remove[is.na(remove$Remove)] <- 1
remove <- remove[remove$Remove < 1,]
# remove the channels from data
for(channel in remove$Channel){
  temp <- names(d)[grep(paste0("_", channel), names(d), ignore.case = TRUE)]
  d[,temp] <- NULL
}

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
proc.time() - ptm

##########
# More time variable processing
##########

# Remove extraneous timestamp variables, like TimeStamp_GPSTime
d <- d[,!(names(d) %in% c("TimeStamp_GPSTime"))] 
# create variable for day of week
d$DayOfWeek <- as.factor(weekdays(d$Timestamp))
# store list of time variables
timeVars <- c(names(d)[grep("time", names(d), ignore.case = TRUE)], "DayOfWeek")

proc.time() - ptm

###############
# Update data dictionary subsystem and channels to match nomenclature in data
###############

dic <- read.csv(paste0(rawDataWD, "/dataDictionary.csv"), header = TRUE, stringsAsFactors = FALSE, na.strings = c("", NA, " "))
dic <- na.omit(dic)
dic$channel <- gsub("[[:punct:]]", "", dic$Data.Label)
dic$channel <- gsub("\\\xd0", "", dic$channel)
dic$channel <- gsub(" ", "", dic$channel)

# list of subsystems
subsystems <- unlist(unique(lapply(names(d[grep("_", names(d))]), function(x) substr(x, 1, grep("_", strsplit(x, split = "")[[1]])-1))))
subsystems <- subsystems[!(subsystems == "TimeStamp")]
dic$data_subsystem <- "placeholder"
for(i in max(nchar(subsystems)):min(nchar(subsystems))){
  dic$data_subsystem[!(dic$data_subsystem %in% subsystems)] <- substr(dic$Subsystem[!(dic$data_subsystem %in% subsystems)], 1, i)
}
dic$data_subsystem[dic$Subsystem == "Lighting"] <- "Elec"
dic$data_subsystem[dic$data_subsystem == "placeholder"] <- "Not mapped"
dic$channel <- paste0(dic$data_subsystem, "_", dic$channel)
# fuzzy match channel in dic to data to replace name with exact match
for(channel in dic$channel){
  temp <- names(d)[grep(channel, names(d), ignore.case = TRUE)]
  temp <- ifelse(length(temp)==0, channel, temp)
  dic$channel[dic$channel == channel] <- temp
}

write.csv(dic, file = paste0(processedDataWD, "/dataDictionary.csv"), row.names = FALSE)

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

proc.time() - ptm

###############
# create metadata object
###############

metadata <- data.frame(variable = names(d))
metadata$subsystem <- NA
metadata$subsystem[grep("_", metadata$variable)] <- unlist(lapply(as.character(metadata$variable[grep("_", metadata$variable)]), function(x) substr(x, 1, grep("_", strsplit(x, split = "")[[1]])-1)))
metadata <- merge(unique(metadata), unique(dic[,c("channel", "Description", "Measurement.Location", "Units")]), by.x = "variable", by.y = "channel", all.x = TRUE)
metadata$in_dictionary <- "N"
metadata$in_dictionary[!(metadata$description == "")] <- "Y"
metadata[metadata$variable %in% timeVars,'description'] <- "Date/Time"
metadata$description[grep("_Status", metadata$variable)] <- "Binary Status"
temp <- c()
for(i in 1:ncol(d)){
  temp[i] <- max(as.numeric(d[,i]))
}
metadata$max_value <- temp
temp <- c()
for(i in 1:ncol(d)){
  temp[i] <- min(as.numeric(d[,i]))
}
metadata$min_value <- temp

write.csv(metadata, file = paste0(processedDataWD, "/metadata.csv"), row.names = FALSE)

proc.time() - ptm #approximately 78 mins
