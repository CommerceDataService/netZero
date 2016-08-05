library(plyr)
ptm <- proc.time()

###############
# working directories
###############

# run convert_tdms_to_csv.py first, to convert tdms files into csv

mainWD <- "/Users/prioberoi/Documents/nist/netZero"
rawDataWD <- paste0(mainWD, "/data/raw")
processedDataWD <- paste0(mainWD, "/data/processed")

###############
# import data
###############

# Names of CSVs to be imported
#paths <- dir(path = rawDataWD, pattern = "2015-02-[0-9]{2}\\.csv$", full.names = TRUE)
paths <- dir(path = rawDataWD, pattern = "\\.csv$", full.names = TRUE)
names(paths) <- basename(paths)

# use plyr to combine into one dataframe
data <- ldply(.data = paths, 
              .fun = function(x){
                read.csv(x, header = TRUE, stringsAsFactors = FALSE)
              },
              .progress = "time")

###############
# variable name clean up
###############
# remove .
names(data) <- gsub("\\.", "", names(data))
# remove X prefix
names(data)[3:ncol(data)] <- sub("X", "", names(data)[3:ncol(data)])

# delineate between subsystem and channel
dataDic <- read.csv(paste0(processedDataWD, "/dataDictionary.csv"))
subsystems <- as.character(unique(dataDic$Subsystem[grep("[A-Z]+", dataDic$Subsystem)]))
subsystems <- c(subsystems, "ElecCurrent", "ElecEnergy","ElecPower", "ElecRPB", "ElecTotal", "Load", "Misc", "Vent")
for(subsystem in subsystems){
  names(data) <- sub(paste0("^",subsystem), paste0(subsystem,"_"), names(data), ignore.case = FALSE)
}

###############
# date parsing
###############

data <- data[,!(names(data) %in% c("TimeStampSchedulerTime", "TimeStampSystemTime"))] 
data$TimeStampGPSTime <- strptime(substr(data$TimeStampGPSTime, 1, 19), "%Y-%m-%d %H:%M:%S")
# create variable for day of week
data$DayOfWeek <- as.factor(weekdays(data$TimeStampGPSTime))
# store list of time variables
timeVars <- c("TimeStampGPSTime", "DayOfWeek", "TimeStampCount")

###############
# write final CSVs to processed folder
###############

# write entire dataset
write.csv(data, file = paste0(processedDataWD, "/data.csv"), row.names = FALSE)

# write CSV for each subsystem
for(subsystem in subsystems){
  vars <- c(timeVars, names(data)[grep(paste0("^",subsystem), names(data))])
  data_subsystem <- data[,vars]
  write.csv(data_subsystem, file = paste0(processedDataWD, "/", subsystem, ".csv"), row.names = FALSE)
}

proc.time() - ptm