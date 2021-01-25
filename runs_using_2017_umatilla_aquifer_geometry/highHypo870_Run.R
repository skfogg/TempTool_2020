###
### TempTool ReRun
###

## Changed Specific Heat of Sediment from 2400 to 870
## ***Then Compare to old output


library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)

connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="password")
theseBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 35.94, b=-1.39)
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
              binStats = theseBins)
setTiming(odbcConnection = connect,
          timeStep = 10,
          yearsToRun = 4,
          outputInterval = 3600)


# highHypo870 <- runTempTool(odbcConnection = connect,
#                         binStats = theseBins,
#                         initTemps = highHypoInitTemps,
#                         channelVolume = 0.5,
#                         channelSurfaceArea = 1,
#                         surfaceShade = 0,
#                         firstBin = 1,
#                         lastBin = 18,
#                         timeStep = 10,
#                         yearsToRun = 4,
#                         outputInterval = 3600,
#                         runID = "high hyporheic exchange, no shade, sedimentSpHeat 870",
#                         outputIndex = NULL,
#                         internal = TRUE)
# save(highHypo, file = "d:/Users/sarah.fogg/Desktop/TempToolModelTesting2/highHypo.RData")
# load(file = "d:/Users/sarah.fogg/Desktop/TempToolModelTesting2/highHypo.RData")


assign("cTemp", tts(odbcConnection = connect,
                    holonName = "channel_0001",
                    tableName = "temp_signal_output",
                    runID = "high hyporheic exchange, no shade, sedimentSpHeat 870",
                    xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))

for(z in 1:18){
  if(z < 10){
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_000", z),
                                         tableName = "temp_signal_output",
                                         runID = "high hyporheic exchange, no shade, sedimentSpHeat 870",
                                         xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))
  } else {
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_00", z),
                                         tableName = "temp_signal_output",
                                         runID = "high hyporheic exchange, no shade, sedimentSpHeat 870",
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

highHypo870 <- outList

save(highHypo870, file = "C:/Users/t24x137/Desktop/TempTool_2020/highHypo870.RData")


