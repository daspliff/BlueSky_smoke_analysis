# This code process downloaded BlueSky daily archived model predictions from AirFire’s ftp server (https://haze.airfire.org).
# The data have a grid size of 4km and temporal resolution of 1 hour (National Oceanic and Atmospheric Administration, 2006).
# The daily forecast records contained either 24 hours or 48 hours of forecast smoke data. We selected the most recent 
# forecast data hours for all analyses performed here. This code is written to be called non-interactively.

# Author - Ryan Michael
# August 2020

rm(list=ls()) 

#  Load packages
suppressPackageStartupMessages({
  library("stringr")
  library("lubridate")
  library("Hmisc")
  library("ncdf4")
  library("plyr")
  library("dplyr")
})

# Sanity check
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

# Assign time variable
yyyymm <- scan(file=args[1])

# Assign path variables
data.dir    <- file.path(args[2])
out.dir     <- file.path(args[3])

# Read in list of paths to dispersion data for time(month and year) discarding CARRYOVER data files
disper.data  <- list.files(file.path(data.dir), pattern = "*.nc", 
                           recursive = T, full.names = T)[str_which(list.files(file.path(data.dir), 
                                                               pattern = "*.nc", recursive = T), as.character(yyyymm))] %>% str_subset("carr", negate=T)

# Select COMBINED when both COMBINED and FORECAST is available for the same time interval
mod.typ <- str_split(disper.data, pattern="/", simplify=T)[,8] %>% str_split("_", simplify = T)
mod.typ <- cbind(mod.typ, diff(as.numeric(mod.typ[,1]), 1), rep(1, length(mod.typ[,1])))
for(n in 1:(nrow(mod.typ)-1)){
  if(as.numeric(mod.typ[n ,4]) == 0){
    mod.typ[n + 1, 5] <- NA
  }
}
mod.typ <- na.omit(mod.typ)[, 1:3]

# Update dispersion dataset with the processed records
disper.data  <- file.path(data.dir, paste(mod.typ[,1], mod.typ[,2], mod.typ[,3], sep="_"))
# Extract temporal identifiers from data
ymd.dat      <- date(str_replace(ymd_h(stringr::str_split(disper.data, pattern = "/", simplify = T)[,8]), " UTC", ""))
month.num    <- factor(month(str_replace(ymd_h(stringr::str_split(disper.data, pattern = "/", simplify = T)[,8]), " UTC", "")))
year.dat     <- unique(year(str_replace(ymd_h(stringr::str_split(disper.data, pattern = "/", simplify = T)[,8]), " UTC", "")))
month.name   <- unique(month(str_replace(ymd_h(stringr::str_split(disper.data, pattern = "/", simplify = T)[,8]), " UTC", ""), label = TRUE, abbr = FALSE))
# Get number of days of records in the dataset
days.month   <- day(str_replace(ymd_h(stringr::str_split(disper.data, pattern = "/", simplify = T)[,8]), " UTC", ""))
# get number of days of the calendar month
days.num     <- unique(days_in_month(as.Date(str_replace(ymd_h(stringr::str_split(disper.data, pattern = "/", simplify = T)[,8]), " UTC", ""), "%Y-%m-%d")))
# Create directory for output data if it does not exist and assign
if(dir.exists(file.path(out.dir, unique(year.dat), unique(month.name)))){
  out.path <- file.path(out.dir, unique(year.dat), unique(month.name))
} else {
  dir.create(file.path(out.dir, unique(year.dat), unique(month.name)),  recursive = T)
  out.path <- file.path(out.dir, unique(year.dat), unique(month.name))
}
# Generate grid identifiers

grid.nrows <- 751
grid.ncols <- 1751

grid.dat   <- data.frame(Grid_ID = 1:(grid.nrows*grid.ncols))

# generate sequential labels for every hour of the day and day of month.. 00 = 12:00am --> 01:00am 
day.labs     <- paste(rep(paste("DY", ifelse(1:days.num <10, paste("0", 1:days.num, sep = ""), 1:days.num), sep=""), each=24), 
                      paste("HR", ifelse(0:23<10, paste("0", 0:23, sep = ""), 0:23), sep=""), sep = "_")
# Initiate dataframe to store our data
fmt.disper.dat <- matrix(rep(-999, 1315001*days.num*24), ncol = days.num*24, dimnames = list(NULL, day.labs))

# Used as hour counter
start.date <- format(paste(paste(paste(unique(year(str_replace(ymd_h(stringr::str_split(disper.data, pattern = "/", simplify = T)[,8]), " UTC", ""))),
                                  ifelse(unique(month(str_replace(ymd_h(stringr::str_split(disper.data, pattern = "/", simplify = T)[,8]), " UTC", ""))) < 10,
                                         paste("0", unique(month(str_replace(ymd_h(stringr::str_split(disper.data, pattern = "/", simplify = T)[,8]), " UTC", ""))),sep = ""),
                                         unique(month(str_replace(ymd_h(stringr::str_split(disper.data, pattern = "/", simplify = T)[,8]), " UTC", "")))), "01", sep="-"), "00:00:00", sep=" "), "UTC", sep = " "),
                           format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# loop through days of month
for(day in 1:length(days.month)){
  ncin <- nc_open(file.path(disper.data[day]))
  globalAttributes <- ncdf4::ncatt_get(ncin, varid=0)
  if(globalAttributes$NCOLS==grid.ncols && globalAttributes$NROWS==grid.nrows){
  row  <- ncin$dim$ROW$vals
  col  <- ncin$dim$COL$vals
  lay  <- ncin$dim$LAY$vals
  XORIG <- globalAttributes[["XORIG"]] # x origin
  YORIG <- globalAttributes[["YORIG"]] # y origin
  XCENT <- globalAttributes[["XCENT"]] # x center
  YCENT <- globalAttributes[["YCENT"]] # y center
  ZLVLS <- globalAttributes[["VGLVLS"]]
  
  # Now we have enough information about the domain to figure out the n, e, s, w corners
  w <- XORIG
  e <- XORIG + 2 * abs(XCENT - XORIG)
  s <- YORIG
  n <- YORIG + 2 * (YCENT - YORIG)
  
  # Knowing the grid dimensions and the true corners we can define legitimate lat/lon dimensions
  lat <- seq(s, n, length.out=length(row))
  lon <- seq(w, e, length.out=length(col))
  lvl <- ZLVLS[1:length(lay)]
  #---------------------------------------------------------------------------
  # Temporal information is stored in the 'TFLAG' variable
  tflag <- ncdf4::ncvar_get(ncin, "TFLAG")
  time_str <- paste0(tflag[1,], sprintf(fmt="%06d", tflag[2,]))
  # We use 'strptime()' to convert our character index to a "POSIXct" value.
  time.ft <- strptime(x=time_str, format="%Y%j%H%M%S", tz="UTC")
  # Selecting data that only corresponds to the current day
  filt.tm <- time.ft[which(date(time.ft) %in% ymd.dat[day])]
  dat.yr  <- year(filt.tm)
  dat.mt  <- month(filt.tm, label = TRUE, abbr = FALSE)
  dat.dy  <- day(filt.tm)
  dat.hr  <- hour(filt.tm)
  # Get hour of the month each timestamp corresponds to.
  mt.hr <-   as.numeric(difftime(filt.tm, start.date, tz ="UTC", units = "hours"))
  # Reformat .nc data
  formatPM25 <- adply(ncdf4::ncvar_get(ncin, "PM25"), c(1,2))[, -c(1:2)]
  for (hr in 1:length(dat.hr)) {
    fmt.disper.dat[, mt.hr[hr]] <- formatPM25[, hr]
  }
  }
}
# Combine grid labels with hourly data
fmt.disper.dat <- cbind.data.frame(grid.dat, fmt.disper.dat)
write.csv(fmt.disper.dat, file = file.path(out.dir, unique(year.dat), unique(month.name), paste(unique(month.name),"_PM25.csv", sep = "")), row.names=FALSE, col.names = FALSE)
