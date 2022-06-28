###
### Re-run of moderate hyporheic scenario w/ sp heat of sediment @ 870
###

library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)

connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="Kraydie")

littleHypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 11.98, b=-1.39)
load(file = "runs_using_2017_umatilla_aquifer_geometry/initTemps/littleHypoInitTemps.RData")

setSkeleton(firstBin = 1,
            lastBin = 18,
            odbcConnection = connect)
setParameters(firstBin = 1,
              lastBin = 18,
              odbcConnection = connect,
              initTemps = littleHypoInitTemps,
              surfaceShade = 0,
              channelSurfaceArea = 1,
              channelVolume = 0.5,
              binStats = littleHypoBins)
setTiming(odbcConnection = connect,
          timeStep = 10,
          yearsToRun = 4,
          outputInterval = 3600)

###
####
##### RUN IN ECLIPSE
###
###
##


assign("cTemp", tts(odbcConnection = connect,
                    holonName = "channel_0001",
                    tableName = "temp_signal_output",
                    runID = "low hyporheic exchange, no shade, sedimentSpHeat 870",
                    xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))

for(z in 1:18){
  if(z < 10){
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_000", z),
                                         tableName = "temp_signal_output",
                                         runID = "low hyporheic exchange, no shade, sedimentSpHeat 870",
                                         xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))
  } else {
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_00", z),
                                         tableName = "temp_signal_output",
                                         runID = "low hyporheic exchange, no shade, sedimentSpHeat 870",
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
littleHypo870 <- outList

save(littleHypo870, file = "C:/Users/t24x137/Desktop/TempTool_2020/littleHypo870.RData")



latentLittleHypo <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE stateVal = 'HEATLATENTEVAP';")
sensibleLittleHypo <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE stateVal = 'HEATSENSIBLE';")
longwaveLittleHypo <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE stateVal = 'LONGWAVENET';")
shortwaveLittleHypo <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE stateVal = 'SHORTWAVENET';")
# channelheatHighHypo <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE stateVal = 'HEAT';")
atmChannelHeatFlux <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE holonName = 'atm_0001-to';")

hyporheicHeat1 <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE holonName = 'bedfrom_0001-to';")
for(z in 1:18){
  if(z < 10){
    assign(paste0("tsz", z, "HeatOut"), sqlQuery(connect,
                                                 paste0("SELECT svValue FROM temptoolfour.temp_signal_output WHERE holonName = 'bedfrom_000",z,"-to';"))
    )
  } else {
    assign(paste0("tsz", z, "HeatOut"), sqlQuery(connect,
                                                 paste0("SELECT svValue FROM temptoolfour.temp_signal_output WHERE holonName = 'bedfrom_00",z,"-to';"))
    )
  }
}
objectNames <- c(paste0("tsz", 1:18, "HeatOut"))
# assign(paste0("output_run", runID), lapply(objectNames, get))

names(objectNames) <- objectNames
outList <- as.list(objectNames)

for(i in 1:length(objectNames)){
  outList[[i]] <- get(objectNames[i])
}
tszHeatOut_little <- outList

save(tszHeatOut_little, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/little/tszHeatOut_little.RData")
save(latentLittleHypo, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/little/latentLittleHypo.RData")
save(sensibleLittleHypo, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/little/sensibleLittleHypo.RData")
save(longwaveLittleHypo, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/little/longwaveLittleHypo.RData")
save(shortwaveLittleHypo, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/little/shortwaveLittleHypo.RData")
save(atmChannelLittleFlux, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/little/atmChannelHeatFlux_little.RData")

plot(shortwaveLittleHypo[,], type = "l")
plot(longwaveLittleHypo[,], type = "l")
plot(sensibleLittleHypo[,], type = "l")
plot(latentLittleHypo[,], type = "l")
plot(channelheatLittleHypo[,], type = "l")

hypoHeatIn_little <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE holonName = 'bedto_0001-from';")

save(hypoHeatIn_little, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/little/hypoHeatIn_little.RData")


