rm(list=ls())
library(oce)
# load in pre-read data
load('ctd.rda')

# Need to go through each profile and identify flurometer sensors that measure
#   chlorophyll and CDOM.
# They generally both get labelled with 'fluorometer[2]'/'fluorescence[2]'.
# We need to identify which is which is because one might
#   get removed during a mission, and the primary/secondary
#   label in the file could change. 
# We'll retain the original data for each object. If a sensor
#   is on the package that measures either of the above, we'll add
#   it to the profile. This does mean that data will be duplicated,
#   but it will make things more simple when extracting the data.
# Instruments that measure chlorophyll:
#   seapoint : flSP
#   wetlabs ecopuck : flECO-AFL
# Instruments that measure CDOM :
#   seapoint ultraUV : flSPuv0
#   wetlabs ecopuck : wetcdom
logfilename <- '02_inferChlorophyllInstrument.log'
sink(file = logfilename, append = FALSE, split = TRUE)
for(i in 1:length(ctd)){
    cat(paste("i =", i), sep = '\n')
    if('fluorescence' %in% names(ctd[[i]]@data)) cat("Fluorecence sensor present", sep = '\n')
    hist <- ctd[[i]]@metadata$header$HISTORY_HEADER
    okchannel <- grep('\\# name \\d+ \\=', hist)
    channel <- hist[okchannel]
    # two general set-ups:
    #   ultraUV (CDOM, 'primary') and seapoint (Chlorophyll, 'secondary')
    #   wetCDOM (CDOM, 'secondary') and wetlabs ecopuck (Chlorophyll, 'primary') 
    ultra <- grep('flSPuv0\\:', channel) # cdom
    seapoint <- grep('flSP\\:', channel) # chl
    fleco <- grep('flECO-AFL\\:', channel) # chl
    wetcdom <- grep('wetCDOM\\:', channel) # cdom
    # there was a switch from fluorometer to fluorescence at some point
    cat(paste('Checking fluorometers for file:', ctd[[i]][['filename']]), sep = '\n')
    # first scenario, ultra and seapoint
    if(length(ultra) != 0 & length(seapoint) != 0){
        cat('Both ultraUV and seapoint on CTD, switching', sep = '\n')
        # need to switch the fluorometer
        fl2 <- ctd[[i]][['fluorescence2']] # 20220414 not sure when it changed to fluorescence in the files, hmnn.
        fl <- ctd[[i]][['fluorescence']]
        ctd[[i]] <- oceSetData(ctd[[i]],
                                 'chlorophyll',
                                 fl2)
        ctd[[i]] <- oceSetData(ctd[[i]],
                                 'cdom',
                                 fl)
    }
    # first scenario, no cdom, chl
    if(length(ultra) == 0 & length(seapoint) != 0){
        cat('Only seapoint on CTD', sep = '\n')
        fl <- ctd[[i]][['fluorescence']]
        ctd[[i]] <- oceSetData(ctd[[i]],
                                 'chlorophyll',
                                 fl)
    }
    # first scenario, cdom, no chl
    if(length(ultra) != 0 & length(seapoint) == 0){
        cat('Only ultraUV on CTD', sep = '\n')
        fl <- ctd[[i]][['fluorescence']]
        ctd[[i]] <- oceSetData(ctd[[i]],
                                 'cdom',
                                 fl)
    }
    # second scenario, fleco (primary) (chl) and wetcdom
    if(length(wetcdom) != 0 & length(fleco) != 0){
        cat('Both wetcdom and fleco on CTD', sep = '\n')
        # need to switch the fluorometer
        fl2 <- ctd[[i]][['fluorescence2']] # 20220414 not sure when it changed to fluorescence in the files, hmnn.
        fl <- ctd[[i]][['fluorescence']]
        ctd[[i]] <- oceSetData(ctd[[i]],
                               'chlorophyll',
                               fl)
        ctd[[i]] <- oceSetData(ctd[[i]],
                               'cdom',
                               fl2)
    }
    # second scenario, no cdom, chl
    if(length(wetcdom) == 0 & length(fleco) != 0){
        cat('Only fleco on CTD', sep = '\n')
        fl <- ctd[[i]][['fluorescence']]
        ctd[[i]] <- oceSetData(ctd[[i]],
                               'chlorophyll',
                               fl)
    }
    # second scenario, cdom, no chl
    if(length(wetcdom) != 0 & length(fleco) == 0){
        cat('Only ultraUV on CTD', sep = '\n')
        fl <- ctd[[i]][['fluorescence']]
        ctd[[i]] <- oceSetData(ctd[[i]],
                                 'cdom',
                                 fl)
    }
    # no chl
    if(length(seapoint) == 0 & length(fleco) == 0){
        cat('No seapoint or flECO on CTD, adding NA values for chlorophyll', sep = '\n')
        chlfake <- rep(NA, length = length(ctd[[i]][['pressure']]))
        ctd[[i]] <- oceSetData(ctd[[i]],
                                 'chlorophyll',
                                 chlfake)
    }
    # no cdom
    if(length(ultra) == 0 & length(wetcdom) == 0){
        cat('No ultraUV or wetCDOM on CTD, adding NA values for cdom', sep = '\n')
        cdomfake <- rep(NA, length = length(ctd[[i]][['pressure']]))
        ctd[[i]] <- oceSetData(ctd[[i]],
                                 'cdom',
                                 cdomfake)
    }
}
sink()