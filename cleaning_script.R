library(plyr)

# Names of CSVs to be imported
paths <- dir(path = "/Users/prioberoi/Documents/nist/netZero/data/raw", pattern = "2015-02-[0-9]{2}\\.csv$", full.names = TRUE)
names(paths) <- basename(paths)

# use plyr to combine into one dataframe
data <- ldply(.data = paths[1:20], 
              .fun = function(x){
                read.csv(x, header = TRUE, stringsAsFactors = FALSE)
              },
              .progress = "time")

# get a dataframe of csv sizes
ptm <- proc.time()
dims <- data.frame()
for(path in paths){
  temp <- read.csv(path, stringsAsFactors = FALSE)
  dims <- rbind(dims, data.frame(file = path, rows = dim(temp)[1], columns = dim(temp)[2]))
}
proc.time() - ptm

checkVariables <- function(df1, df2){
  if(sum(!(names(df1) %in% names(df2))) > 0){
    add <- matrix(0, nrow = nrow(df2), ncol = sum(!(names(df1) %in% names(df2))))
    add <- as.data.frame(add)
    names(add) <- names(df1)[!(names(df1) %in% names(df2))]
    df2 <- cbind(df2, add)
  }
  return(df2)
}

# combine csvs into one dataframe
#paths <- c(as.character(dims$file[dims$columns == 913][1]), as.character(dims$file[dims$columns == 917][1]))
ptm <- proc.time()
data <- read.csv(paths[1], stringsAsFactors = FALSE)
data$date <- paths[1]
for(path in paths[2:length(paths)]){
  temp <- read.csv(path, stringsAsFactors = FALSE)
  temp$date <- path
  data <- checkVariables(temp, data)
  temp <- checkVariables(data, temp)
  data <- rbind(data, temp)
}
proc.time() - ptm

