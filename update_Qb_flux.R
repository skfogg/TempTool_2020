library(zoo)
library(xts)

load("runs_using_2017_umatilla_aquifer_geometry/little/Q_b_little.RData")
load("runs_using_2017_umatilla_aquifer_geometry/little/hypoHeatIn_little.RData")
load("runs_using_2017_umatilla_aquifer_geometry/little/tszHeatOut_little.RData")

plot(Q_b_little[100:35065],
     type = "l")
abline(h = 0, col = "Red")

bedflux <- xts(zoo(Q_b_little, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600,length.out = 35065)))

plot.zoo(bedflux["2016"])
abline(h = 0, col = "Red")


library(RODBC)
connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="Kraydie")
allheat <- sqlQuery(connect, "SELECT * FROM temptoolfour.temp_signal_output WHERE stateVal = 'HEAT';")
allheat_little <- allheat
saveRDS(allheat_little, "runs_using_2017_umatilla_aquifer_geometry/little/allheat_little.RData")

allheat_little <- readRDS("runs_using_2017_umatilla_aquifer_geometry/little/allheat_little.RData")
allheat_medium <- readRDS("runs_using_2017_umatilla_aquifer_geometry/med/allheat_med.RData")
allheat_high <- readRDS("runs_using_2017_umatilla_aquifer_geometry/high/allheat_high.RData")

bedto1_little <- subset(allheat_little, holonName == "bedto_0001-from")
bedto1_medium <- subset(allheat_medium, holonName == "bedto_0001-from")
bedto1_high <- subset(allheat_high, holonName == "bedto_0001-from")

allbedfrom_little <- data.frame(x= rep(0, times = length(bedto1_little)))
for(i in 1:18){
  if(i < 10){
    assign("allbedfrom_little", cbind(allbedfrom_little, subset(allheat_little, holonName == paste0("bedfrom_000", i, "-to"))$svValue))
  }else{
    assign("allbedfrom_little", cbind(allbedfrom_little, subset(allheat_little, holonName == paste0("bedfrom_00", i, "-to"))$svValue))
  }
}

allbedfrom_medium <- data.frame(x= rep(0, times = length(bedto1_medium)))
for(i in 1:18){
  if(i < 10){
    assign("allbedfrom_medium", cbind(allbedfrom_medium, subset(allheat_medium, holonName == paste0("bedfrom_000", i, "-to"))$svValue))
  }else{
    assign("allbedfrom_medium", cbind(allbedfrom_medium, subset(allheat_medium, holonName == paste0("bedfrom_00", i, "-to"))$svValue))
  }
}

allbedfrom_high <- data.frame(x= rep(0, times = length(bedto1_high)))
for(i in 1:18){
  if(i < 10){
    assign("allbedfrom_high", cbind(allbedfrom_high, subset(allheat_high, holonName == paste0("bedfrom_000", i, "-to"))$svValue))
  }else{
    assign("allbedfrom_high", cbind(allbedfrom_high, subset(allheat_high, holonName == paste0("bedfrom_00", i, "-to"))$svValue))
  }
}

hzup_little <- rowSums(allbedfrom_little)
hzup_medium <- rowSums(allbedfrom_medium)
hzup_high <- rowSums(allbedfrom_high)

hzdwn_little <- bedto1_little$svValue
hzdwn_medium <- bedto1_medium$svValue
hzdwn_high <- bedto1_high$svValue


qhz_little <- hzup_little + hzdwn_little
qhz_medium <- hzup_medium + hzdwn_medium
qhz_high <- hzup_high + hzdwn_high

plot(qhz_little, type = "l")
abline(h = 0, col = "red")

plot(qhz_medium, type = "l")
abline(h = 0, col = "red")

plot(qhz_high, type = "l")
abline(h = 0, col = "red")

plot(hzdwn, type = "l")
plot(hzup, col = "blue")


# load("runs_using_2017_umatilla_aquifer_geometry/high/Q_b_high.RData")
# plot(Q_b_high, type = "l")
# abline(h=0, col = "red")

saveRDS(qhz_little, "runs_using_2017_umatilla_aquifer_geometry/little/qhz_little.RData")
saveRDS(qhz_medium, "runs_using_2017_umatilla_aquifer_geometry/med/qhz_med.RData")
saveRDS(qhz_high, "runs_using_2017_umatilla_aquifer_geometry/high/qhz_high.RData")




