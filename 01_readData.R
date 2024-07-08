rm(list=ls())
library(oce)
library(sp) # for point.in.polygon


# define spatial box to extract data
# start at north-west corner, go counter clockwise
polyy <- c(48, 37, 37, 48)
polyx <- c(-71, -71, -48, -48)

# define which years to pull data
years <- 1999:2023
#years <- c(1999, 2001, 2009, 2017, 2023)

# define path to archived data
arcPath <- '\\\\ent.dfo-mpo.ca/ATLShares/Science/BIODataSvc/ARC/Archive/ctd'

# iterate through each year
# read in the data
# retain stations that are within the box
ctd <- NULL
for(iy in 1:length(years)){
    year <- years[iy]
    cat(paste("Reading in data for year :", year), sep = '\n')
    path <- paste(arcPath, year, sep = '/')
    files <- list.files(path = path, 
                        pattern = '^CTD.*DN\\.ODF', # only downcasts
                        full.names = TRUE)
    cat(paste("Found", length(files), "files."), sep = '\n')
    d <- lapply(files, read.ctd.odf)
    lon <- unlist(lapply(d, function(k) k[['longitude']][1]))
    lat <- unlist(lapply(d, function(k) k[['latitude']][1]))
    pip <- sp::point.in.polygon(point.x = lon,
                                point.y = lat,
                                pol.x = polyx,
                                pol.y = polyy)
    keep <- pip != 0
    cat(paste("Retaining", length(which(keep)), "profiles."), sep = '\n')
    ctd <- c(ctd, d[keep])   
}

save(ctd, file = 'ctd.rda')
