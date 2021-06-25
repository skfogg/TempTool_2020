###
### Re-run of moderate hyporheic scenario w/ sp heat of sediment @ 870
###



library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)

connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="MSUFLL!!")

mediumHypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 23.96, b=-1.39)
load(file = "C:/Users/t24x137/Desktop/Old Tower Desktop Folders/TempToolModelTesting2/medHypoInitTemps.RData")

setSkeleton(firstBin = 1,
            lastBin = 18,
            odbcConnection = connect)
setParameters(firstBin = 1,
              lastBin = 18,
              odbcConnection = connect,
              initTemps = medHypoInitTemps,
              surfaceShade = 0,
              channelSurfaceArea = 1,
              channelVolume = 0.5,
              binStats = mediumHypoBins)
setTiming(odbcConnection = connect,
          timeStep = 10,
          yearsToRun = 4,
          outputInterval = 3600)
#
##
###
#### RUN IN ECLIPSE
###
##
#


assign("cTemp", tts(odbcConnection = connect,
                    holonName = "channel_0001",
                    tableName = "temp_signal_output",
                    runID = "moderate hyporheic exchange, no shade, sedimentSpHeat 870",
                    xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))

for(z in 1:18){
  if(z < 10){
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_000", z),
                                         tableName = "temp_signal_output",
                                         runID = "moderate hyporheic exchange, no shade, sedimentSpHeat 870",
                                         xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))
  } else {
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_00", z),
                                         tableName = "temp_signal_output",
                                         runID = "moderate hyporheic exchange, no shade, sedimentSpHeat 870",
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

medHypo870 <- outList

save(medHypo870, file = "C:/Users/t24x137/Desktop/TempTool_2020/medHypo870.RData")



connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="MSUFLL!!")

latentMedHypo <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE stateVal = 'HEATLATENTEVAP';")
sensibleMedHypo <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE stateVal = 'HEATSENSIBLE';")
longwaveMedHypo <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE stateVal = 'LONGWAVENET';")
shortwaveMedHypo <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE stateVal = 'SHORTWAVENET';")
#channelheatMedHypo <- sqlQuery(connect, "SELECT svValue FROM temptoolfour.temp_signal_output WHERE stateVal = 'HEAT';")
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
tszHeatOut_med <- outList

save(tszHeatOut_med, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/med/tszHeatOut_med.RData")
save(latentMedHypo, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/med/latentMedHypo.RData")
save(sensibleMedHypo, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/med/sensibleMedHypo.RData")
save(longwaveMedHypo, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/med/longwaveMedHypo.RData")
save(shortwaveMedHypo, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/med/shortwaveMedHypo.RData")
save(atmChannelMedFlux, file = "C:/Users/t24x137/Desktop/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/med/atmChannelHeatFlux_med.RData")


plot(shortwaveMedHypo[,], type = "l")
plot(longwaveMedHypo[,], type = "l")
plot(sensibleMedHypo[,], type = "l")
plot(latentMedHypo[,], type = "l")
plot(channelheatMedHypo[,], type = "l")

