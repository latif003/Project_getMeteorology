# Nikoula team; Nikos Tziolas and Siti H. Latifah
# Date: 26 January 2014

# clean the workspace
rm(list=ls())
setwd('D:/Project')

# we use meteorogical data from KNMI  
URL <- c("http://www.knmi.nl/climatology/daily_data/datafiles3/269/etmgeg_269.zip")

# make sure the directory
getwd()

# download and unzip the meteorogical data
unzipURL <- function(url){
  download.file(url=URL, destfile='data/url.zip', method='auto')
  unzip('data/url.zip', exdir="data/url")
  unlink('data/url.zip', recursive = TRUE)
}

unzipURL(URL)

# read the meteorogical data, the first 46 lines contain only the explanation of the data, so we start to read the data from line 47 (by defining "skip"parameter)
meteo <-read.table(file = 'data/url/etmgeg_269.txt', sep = ",", skip = 47, header = TRUE)

# list of the column head of the table
headlist <- c("STN", "YYYYMMDD", "DDVEC", "FHVEC", "FG", "FHX", "FHXH", "FHN", "FHNH", "FXX", "FXXH", "TG", "TN", "TNH", "TX", "TXH", "T10N", "T10NH", "SQ", "SP", "Q", "DR", "RH", "RHX", "RHXH", "PG", "PX", "PXH", "PN", "PNH", "VVN", "VVNH", "VVX", "VVXH", "NG", "UG", "UX", "UXH", "UN", "UNH", "EV24")

# add column names
colnames(meteo) <- headlist

# change the date format
meteo$YYYYMMDD <- as.Date(format(meteo$YYYYMMDD),'%Y%m%d')

# choose daily mean temperature (TG) and daily precipitation (RH) data from the year of 2013 and 2014
met13 <- meteo[as.numeric(format(meteo$YYYYMMDD, "%Y")) == 2013, c("YYYYMMDD", "TG", "RH")]
met14 <- meteo[as.numeric(format(meteo$YYYYMMDD, "%Y")) == 2014, c("YYYYMMDD", "TG", "RH")]

# multiply the TG and RH with 0.1 (TG is in 0.1 deg Celcius, RH is in 0.1mm)
met13$TG <- met13$TG*0.1
met13$RH <- met13$RH*0.1
met14$TG <- met14$TG*0.1
met14$RH <- met14$RH*0.1

# we will evaluate the difference in temperature and precipitation between 2013 and 2014
## apply cumulative summary for the table
head(met13) # visualise data
meteo13 <- apply(met13[,2:3],2,cumsum) # TG and RH are in column 2 and 3 
meteo14 <- apply(met14[,2:3],2,cumsum)

# visualise the result

plot(t13[,1], type = "l")
lines(t14[,1], col = "red")

