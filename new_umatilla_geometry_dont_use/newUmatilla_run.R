###
### TempTool ReRun: UMATILLA
###

library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)

connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="MSUFLL!!")

umatillaBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 25, b=-1.39)

load("C:\\Users\\t24x137\\Desktop\\Old Tower Desktop Folders\\TempToolModelTesting2\\highHypoInitTemps.RData")

setSkeleton(firstBin = 1,
            lastBin = 18,
            odbcConnection = connect)
setParameters(firstBin = 1,
              lastBin = 18,
              odbcConnection = connect,
              initTemps = highHypoInitTemps,
              surfaceShade = 0,
              channelSurfaceArea = 1,
              channelVolume = 0.5,
              binStats = umatillaBins)
setTiming(odbcConnection = connect,
          timeStep = 10,
          yearsToRun = 4,
          outputInterval = 3600)

######################
###                ###
###                ###
### RUN IN ECLIPSE ###
###                ###
###                ###
######################

rundesc <- "new Umatilla Parameters"

assign("cTemp", tts(odbcConnection = connect,
                    holonName = "channel_0001",
                    tableName = "temp_signal_output",
                    runID = rundesc,
                    xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))

for(z in 1:18){
  if(z < 10){
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_000", z),
                                         tableName = "temp_signal_output",
                                         runID = rundesc,
                                         xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))
  } else {
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_00", z),
                                         tableName = "temp_signal_output",
                                         runID = rundesc,
                                         xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))
  }
}

# CREATE LIST OF ALL RUN DATA
objectNames <- c("cTemp", paste0("tsz", 1:18, "Temp"))
# assign(paste0("output_run", runID), lapply(objectNames, get))

names(objectNames) <- objectNames
outList <- as.list(objectNames)

for(i in 1:length(objectNames)){
  outList[[i]] <- get(objectNames[i])
}

newUmatilla <- outList

save(newUmatilla, file = "C:/Users/t24x137/Desktop/TempTool_2020/newUmatilla.RData")


