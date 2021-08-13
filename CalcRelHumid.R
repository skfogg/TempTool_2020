# This script fits compound sin waves to relative humidity and air temperature data
# from the Iskuulpa weather station, Oregon, using data from the years 2003-2004
#
# The fitted sin coeffecients are used in the Stream Temp Model
# implemented 3/2014

#....................................................................................................................................................
AirWater <- read.csv("d:\\Users\\sarah.fogg\\Desktop\\Model_Relative_Humidity\\AirWaterTable.csv") # load csv (titled "AirWater") into workspace

#......................................................................................................................................................
plot(AirWater$rHumid, type="l") # plot of raw relative humidity

plot(AirWater$Ta[1000:1200]*3,AirWater$rHumid[1000:1200], # hysteresis of air temp and r Humid
     type="p", ylim=c(0,100))
lines(AirWater$rHumid[1000:1200], col=2)

#......................................................................................................................................................
# naming sets of data
sec <- AirWater$sec
rHumid <- AirWater$rHumid
Ta <- AirWater$Ta
SVP <- SatVapPress <- AirWater$SVP #Saturated Vapor Pressure
aHumid <- AirWater$aHumid
# jDay <- AirWater$Jday
AVP <- VapPress <- rHumid * SVP # Absolute Vapor Pressure
M <- 18.02 # molecular weight of water
R <- 8.3143 # gas constant
transparent <- adjustcolor(col="cadetblue3", alpha.f=0.5)
AirWater<-cbind(AirWater, AVP)

#................................................................................................................................................
# set max iterations of nls function
control <- nls.control(maxiter=10000, tol = 1e-4, minFactor =1e-100,printEval = FALSE, warnOnly = T)

#...................................................................................................................................................
# calculates jDay
# use cbind() to use as index
jDay = numeric(8760)
for (i in 1:8760){
  jDay[i]=ceiling((i)/24)
}
AirWater <- cbind(AirWater, jDay)

#..................................................................................................................................................
# recalculates the SVP calculation done in excel
# the excel calculation for SVP correct as shown by plot
recalcSVP <- 6.1275*exp((17.2693882*Ta)/(Ta+237.3))
plot(Ta, ylim=c(-20,65))
lines(SVP, col="gold")
lines(recalcSVP, col="seagreen")

#...................................................................................................................................................
# #SVP annual sin fit
# plot(SatVapPress)
# SVPMean <- mean(SVP)
# SVPAmp <- (max(SVP)-min(SVP))/2
# SVPFit <- nls(SVP ~ Mean+Amp*sin((2*pi/31557600)*sec+Pha),
#                       start=list(Mean=SVPMean,Amp=SVPAmp,Pha=0))
# SVPFitVal <- fitted(SVPFit)
# plot(SVP, col="violet")
# lines(SVPFitVal)

#......................................................................................................................................................
# rHumid annual sin fit (model => RHumidAnnual)
rhAnnualMean <- mean(rHumid)
rhAnnualAmp <- (max(rHumid)-min(rHumid))/2 # Amp is range divided by 2
rhAnnualFit <- nls(rHumid ~ MeanY+AmpY*sin((2*pi/31557600)*sec+PhaY),
                 start=list(MeanY=rhAnnualMean,AmpY=rhAnnualAmp,PhaY=0))
rhAnnualFitVals <- fitted(rhAnnualFit) # fitted() extracts the y values from the nls fitted function
plot(rHumid, col="seagreen")
lines(rhAnnualFitVals)
summary(rhAnnualFit) # summary() gives you the coeffiecients and other data about how nls() calculated the values

plot(resid(rhAnnualFit) ~ fitted(rhAnnualFit))

# remove annual variation from raw data by sumtracting the annual sin wave fit from the raw data
rHumidLessAnnualVar = rHumid  - rhAnnualFitVals # <- these are the residuals from the Annual Model
plot(rHumidLessAnnualVar)
points(resid(rhAnnualFit), col ="red")

# calculate the amplitude from the less annual variation data
rhAnnualAmp = numeric(max(jDay)) # creates a blank numeric vector with length of 365
for (i in 1:max(jDay)){
  rhAnnualAmp[i] = (max(rHumidLessAnnualVar[jDay==i])-min(rHumidLessAnnualVar[jDay==i]))/2
}

# fit sin wave to calculated amplitude values (in model => RHumidDayAmp)
# this will allow us to vary the amplitude annually
secAmp = c(0:364)*86400
relHumidDayAmp <- nls(rhAnnualAmp~MeanA+AmpA*sin((2*pi/31557600)*secAmp+PhaA),
                     start = list(MeanA = 20, AmpA=3, PhaA = 1))
summary(relHumidDayAmp)
dayAmp = fitted(relHumidDayAmp)
plot(rhAnnualAmp, type="l")
lines(dayAmp,col = "blue")

# fit sin wave to daily period with amplitude variation (model => RHumidDay)
rHumidDayFit <- nls(rHumidLessAnnualVar ~ MeanD +
                      (coef(relHumidDayAmp)[1]+coef(relHumidDayAmp)[2]*(sin((2*pi/31557600)*sec+coef(relHumidDayAmp)[3])))*
                         sin((2*pi/86400)*sec+PhaD),
                       start=list(PhaD=0,MeanD = 0), control = control)
summary(rHumidDayFit)
rHumidDayFitVal <- fitted(rHumidDayFit)
plot(rHumidLessAnnualVar, type="o")
lines(rHumidDayFitVal, col="red", lwd=1)

fracDay <- read.csv("d:\\Users\\sarah.fogg\\Desktop\\Model_Relative_Humidity\\jDay.csv")
rHFit <- rhAnnualFitVals+rHumidDayFitVal

# Adding the annual sin wave to the daily sine with amp variation
# results in the sin wave used (model => RelativeHumidity)
plot(fracDay$fractionalday, rHumid,type="l", main="Relative Humidity Compound Sine Fit", ylab="Relative Humidity (%)", xlab="Julian Day")
lines(fracDay$fractionalday, rHFit,col="blue")
legend("bottomright", fill=c("black", "blue"), c("Recorded", "Fitted"), bty="n")

RH <- data.frame(rHFit,jDay)
dailymaxRH <- numeric(365)
dailyminRH <- numeric(365)
dailymeanRH <- numeric(365)

for (i in 1:365){
  dailymaxRH[i]=max(RH$rHFit[RH$jDay==i])
}
for (i in 1:365){
  dailyminRH[i]=min(RH$rHFit[RH$jDay==i])
}
for (i in 1:365){
  dailymeanRH[i]=mean(RH$rHFit[RH$jDay==i])
}

plot(dailymaxRH, ylim=c(min(dailymaxRH, dailyminRH), max(dailymaxRH, dailyminRH)))
points(dailyminRH)

plot(fracDay$fractionalday, rHumid,type="l", main="Relative Humidity Compound Sine Fit", ylab="Relative Humidity (%)", xlab="Julian Day")
lines(fracDay$fractionalday, rHFit,col=transparent)
lines(c(1:365), dailymaxRH,col="darkcyan", lwd=2)
lines(c(1:365), dailyminRH,col="darkcyan", lwd=2)
legend("bottomright", fill=c("black", "cadetblue2"), c("Recorded", "Fitted"), bty="n")


#winter
strt <- 8400
fin <- 8568
plot(fracDay$fractionalday[strt:fin], rHumid[strt:fin], lwd=2,main="Winter",ylab="Relative Humidity (%)", xlab="Julian Day",type="l", ylim=c(0, max(rHumid)))
points(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin], pch=1)
legend("bottomright", bty="n",c("Recorded", "Fitted"), pch=c(32,1), lty=c(1,0), lwd=c(2,1), cex=0.8)

#spring
strt <- 2160
fin <- 2328
plot(fracDay$fractionalday[strt:fin], rHumid[strt:fin], lwd=2,main="Spring",ylab="Relative Humidity (%)", xlab="Julian Day",type="l", ylim=c(0, max(rHumid)))
points(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin])
legend("bottomright", bty="n",c("Recorded", "Fitted"), pch=c(32,1), lty=c(1,0), lwd=c(2,1), cex=0.8)

#summer
strt <- 4560
fin <- 4728
plot(fracDay$fractionalday[strt:fin], rHumid[strt:fin], lwd=2,main="Summer",ylab="Relative Humidity (%)", xlab="Julian Day",type="l", ylim=c(0, max(rHumid)))
points(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin])
legend("bottomright", bty="n",c("Recorded", "Fitted"), pch=c(32,1), lty=c(1,0), lwd=c(2,1), cex=0.8)

#fall
strt <- 6480
fin <- 6648
plot(fracDay$fractionalday[strt:fin], rHumid[strt:fin], lwd=2,main="Fall",ylab="Relative Humidity (%)", xlab="Julian Day",type="l", ylim=c(0, max(rHumid)))
points(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin])
legend("bottomright", bty="n",c("Recorded", "Fitted"), pch=c(32,1), lty=c(1,0), lwd=c(2,1), cex=0.8)

################## NEW PLOT ####################

transparent <- adjustcolor(col="black", alpha.f=0.4)
modeledcol <- "gray50"

png("d:/Users/sarah.fogg/Dropbox/PAPER1/figs/rhFit.png",
    height  = 1000*3.5, width = 2000*3.5, res = 72*5)
layout(matrix(
  c(1,2,3,
    1,4,5
  ),
  nrow = 2, ncol = 3, byrow = T),
  heights = c(1, 1),
  widths = c(1,.4,.4))
# layout.show(5)
# par(mar = c(5, 4, 4, 1), oma = c(0, 4, 0, 0), cex.axis = 2, cex.main = 2.5, lend = 2)
par(cex.axis = 2, cex.lab = 2, cex.main = 2.5, mar = c(5.5,5.5,4,1))


plot(fracDay$fractionalday, rHumid, type="l", main="Relative Humidity Annual Cycle", ylab="Relative Humidity (%)", xlab="Julian Day", ylim=c(0, max(rHumid)))
lines(fracDay$fractionalday, rHFit, col=transparent, lwd = 3)
# lines(c(1:365), dailymaxRH,col="darkcyan", lwd=2)
# lines(c(1:365), dailyminRH,col="darkcyan", lwd=2)
legend("bottomright", bty="n",c("Measured", "Modeled"), lty=c(1,1), lwd=c(1,3), cex=2, col = c("black", modeledcol))


#winter
strt <- 8400
fin <- 8568
plot(fracDay$fractionalday[strt:fin], rHumid[strt:fin], lwd=1, main="Winter", ylab="Relative Humidity (%)", xlab="Julian Day",type="l", ylim=c(0, max(rHumid)))
lines(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin], col=modeledcol, lwd = 3)
# points(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin], pch=1)
legend("bottomright", bty="n",c("Measured", "Modeled"), lty=c(1,1), lwd=c(1,3), cex=1.5, col = c("black", modeledcol))

#spring
strt <- 2160
fin <- 2328
plot(fracDay$fractionalday[strt:fin], rHumid[strt:fin], lwd=1, main="Spring", ylab="", xlab="Julian Day",type="l", ylim=c(0, max(rHumid)))
lines(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin], col=modeledcol, lwd = 3)
# points(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin])
legend("bottomright", bty="n",c("Measured", "Modeled"), lty=c(1,1), lwd=c(1,3), cex=1.5, col = c("black", modeledcol))

#summer
strt <- 4560
fin <- 4728
plot(fracDay$fractionalday[strt:fin], rHumid[strt:fin], lwd=1, main="Summer",ylab="Relative Humidity (%)", xlab="Julian Day",type="l", ylim=c(0, max(rHumid)))
lines(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin], col=modeledcol, lwd = 3)
# points(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin])
legend("bottomright", bty="n",c("Measured", "Modeled"), lty=c(1,1), lwd=c(1,3), cex=1.5, col = c("black", modeledcol))

#fall
strt <- 6480
fin <- 6648
plot(fracDay$fractionalday[strt:fin], rHumid[strt:fin], lwd=1, main="Fall", ylab="", xlab="Julian Day",type="l", ylim=c(0, max(rHumid)))
lines(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin], col=modeledcol, lwd = 3)
# points(fracDay$fractionalday[strt:fin], rhAnnualFitVals[strt:fin]+rHumidDayFitVal[strt:fin])
legend("bottomright", bty="n",c("Measured", "Modeled"), lty=c(1,1), lwd=c(1,3), cex=1.5, col = c("black", modeledcol))

dev.off()

# Compare modeled calculation with R calculation:
# works only if the model run is outputting relative humidity values for the year

# library(RODBC)
# MDBconn <- odbcDriverConnect ("DRIVER=Microsoft Access Driver (*.mdb, *.accdb);DBQ=d:\\Users\\sarah.fogg\\Desktop\\TempToolThatWorks\\linkedDatabases\\TempToolFourMySql.accdb")
# modelRHumid <- sqlQuery(MDBconn, "SELECT * FROM temp_signal_output WHERE temp_signal_output.holonName='atmosphere' AND stateVal = 'RELATIVEHUMIDITY' ORDER BY modelTime")
#
# plot((modelRHumid$modelTime)/86400, modelRHumid$svValue)
# lines(jDay, rhAnnualFitVals+rHumidDayFitVal,col="blue")


#............................................................................................................................................................
# TEMP
# fitted in the same exact way as the relative humidity
# Ta annual sin fit (model => AirTempAnnual)
TaMean <- mean(Ta)
TaAmp <- (max(Ta)-min(Ta))/2
TaFit <- nls(Ta ~ MeanY+AmpY*sin((2*pi/31557600)*sec+PhaY),
                 start=list(MeanY=TaMean,AmpY=TaAmp,PhaY=0))
TaFitVals <- fitted(TaFit)
plot(Ta, col="seagreen")
lines(TaFitVals)
summary(TaFit)


#for fitting amp variation
TaLessAnnualVar = Ta  - TaFitVals #remove some variation from real data
plot(TaLessAnnualVar)

TaAmp = numeric(max(jDay)) #calculate amplitude from the smoothed temp data
for (i in 1:max(jDay)){
  TaAmp[i] = (max(TaLessAnnualVar[jDay==i])-min(TaLessAnnualVar[jDay==i]))/2
}

TaMean = numeric(max(jDay)) #calculate mean from the smoothed temp data
for (i in 1:max(jDay)){
  TaMean[i] = mean(TaLessAnnualVar[jDay==i])
}

# # Rolling Mean Re-Calculation of Daily sin wave parameters
# yoMama <- rollmean(x = TaLessAnnualVar, k = 24, fill = NA)
# plot(yoMama[!is.na(yoMama)]-TaLessAnnualVar[!is.na(yoMama)])
# summary(yoMama[!is.na(yoMama)])
# smooth <- yoMama[!is.na(yoMama)]-TaLessAnnualVar[!is.na(yoMama)]
#
# TaAmp.smooth <- numeric(max(jDay)) #calculate amplitude from the smoothed temp data
# for (i in 1:max(jDay)){
#   TaAmp.smooth[i] = (max(smooth[jDay==i])-min(smooth[jDay==i]))/2
# }
# TaMean.smooth <- numeric(max(jDay)) #calculate amplitude from the smoothed temp data
# for (i in 1:max(jDay)){
#   TaMean.smooth[i] = mean(smooth[jDay==i])
# }
#
# secAmp = c(0:364)*86400
# TaDayAmp.smooth <- nls(TaAmp.smooth~MeanA+AmpA*sin((2*pi/31557600)*secAmp+PhaA),
#                 start = list(MeanA = 5, AmpA=7, PhaA = 1))
# summary(TaDayAmp)
# TadayAmp.smooth = fitted(TaDayAmp.smooth)
# plot(jDay[!is.na(yoMama)], smooth)
# lines(TaAmp.smooth, col="red", lwd=2)
# lines(TadayAmp.smooth,col = "blue", lwd=2)
# lines(TaMean.smooth, col="green", lwd=2)
#
# plot(TaAmp.smooth, col="red", lwd=2, type="l")
# lines(TaAmp, col="violet", lwd=2)
#
#
# TaDayFit.smooth <- nls(smooth ~ 0 +
#                   (coef(TaDayAmp.smooth)[1]+coef(TaDayAmp.smooth)[2]*(sin((2*pi/31557600)*sec+coef(TaDayAmp.smooth)[3])))*
#                   sin((2*pi/86400)*sec+PhaD),
#                 start=list(PhaD=0), control = control)
# summary(TaDayFit)
# TaDayFitVal.smooth <- fitted(TaDayFit.smooth)
# plot(smooth, type="o")
# lines(TaDayFitVal.smooth, col="red", lwd=1)
#
# plot(Ta)
# points(TaFitVals+TaDayFitVal,col="blue")
# points(TaFitVals+TaDayFitVal.smooth, col="lightblue")
#
# plot(Ta)
# points(TaFitVals+TaDayFitVal,col="blue")




length(jDay)
dailyDev <- TaLessAnnualVar - TaMean
summary(dailyDev)
summary(TaMean)
length(TaLessAnnualVar)

#fit sin wave to calculated amplitude values (model => AirTempDayAmp)
secAmp = c(0:364)*86400
TaDayAmp <- nls(TaAmp~MeanA+AmpA*sin((2*pi/31557600)*secAmp+PhaA),
                      start = list(MeanA = 20, AmpA=3, PhaA = 1))
summary(TaDayAmp)
TadayAmp = fitted(TaDayAmp)
plot(jDay, TaLessAnnualVar)
lines(TaAmp, col="red", lwd=2)
lines(TadayAmp,col = "blue", lwd=2)
lines(TaMean, col=3, lwd=2)
plot(dailyDev)

#fit sin wave on daily cycle with amp variation (model => AirTempDay)
TaDayFit <- nls(TaLessAnnualVar ~ 0 +
                      (coef(TaDayAmp)[1]+coef(TaDayAmp)[2]*(sin((2*pi/31557600)*sec+coef(TaDayAmp)[3])))*
                      sin((2*pi/86400)*sec+PhaD),
                    start=list(PhaD=0), control = control)
summary(TaDayFit)
TaDayFitVal <- fitted(TaDayFit)
plot(TaLessAnnualVar, type="o")
lines(TaDayFitVal, col="red", lwd=1)

#Adding the annual sin wave to the daily sine with amp variation results in the sin wave used (model => Temp)
plot(fracDay$fractionalday, Ta, main="Atmospheric Temperature Sine Fit",ylab="Degrees C", xlab="Julian Day",type="l", ylim=c(min(Ta), max(Ta)))
lines(fracDay$fractionalday, TaFitVals+TaDayFitVal, col="blue")
legend("bottomright", bty="n",c("Recorded", "Fitted"), fill=c("black", "blue"))

TempFitVals <- TaFitVals+TaDayFitVal
Tatm <- data.frame(TempFitVals, jDay)
dailymaxTemp <- numeric(365)
dailyminTemp <- numeric(365)
dailymeanTemp <- numeric(365)

for (i in 1:365){
  dailymaxTemp[i]=max(Tatm$TempFitVals[Tatm$jDay==i])
}
for (i in 1:365){
  dailyminTemp[i]=min(Tatm$TempFitVals[Tatm$jDay==i])
}
for (i in 1:365){
  dailymeanTemp[i]=mean(Tatm$TempFitVals[Tatm$jDay==i])
}

plot(fracDay$fractionalday, Ta, main="Atmospheric Temperature Sine Fit",ylab="Degrees C", xlab="Julian Day",type="l", ylim=c(min(Ta), max(Ta)))
lines(fracDay$fractionalday, TempFitVals, col=transparent)
lines(c(1:365), dailymaxTemp, col="darkcyan", lwd=2)
lines(c(1:365), dailyminTemp, lwd=2, col="darkcyan")
legend("bottomright", bty="n",c("Recorded", "Fitted"), fill=c("black", "cadetblue2"))

transparent <- adjustcolor(col="cadetblue3", alpha.f=0.5)



#winter
strt <- 8400
fin <- 8568
plot(fracDay$fractionalday[strt:fin], Ta[strt:fin], lwd=2,main="Winter",ylab="Degrees C", xlab="Julian Day",type="l", ylim=c(min(Ta), max(Ta)))
points(fracDay$fractionalday[strt:fin], TaFitVals[strt:fin]+TaDayFitVal[strt:fin], pch=1)
legend("bottomright", bty="n",c("Recorded", "Fitted"), pch=c(32,1), lty=c(1,0), lwd=c(2,1))

#spring
strt <- 2160
fin <- 2328
plot(fracDay$fractionalday[strt:fin], Ta[strt:fin], lwd=2,main="Spring",ylab="Degrees C", xlab="Julian Day",type="l", ylim=c(min(Ta), max(Ta)))
points(fracDay$fractionalday[strt:fin], TaFitVals[strt:fin]+TaDayFitVal[strt:fin])
legend("bottomright", bty="n",c("Recorded", "Fitted"), pch=c(32,1), lty=c(1,0), lwd=c(2,1))

#summer
strt <- 4560
fin <- 4728
plot(fracDay$fractionalday[strt:fin], Ta[strt:fin], lwd=2,main="Summer",ylab="Degrees C", xlab="Julian Day",type="l", ylim=c(min(Ta), max(Ta)))
points(fracDay$fractionalday[strt:fin], TaFitVals[strt:fin]+TaDayFitVal[strt:fin])
legend("bottomright", bty="n",c("Recorded", "Fitted"), pch=c(32,1), lty=c(1,0), lwd=c(2,1))

#fall
strt <- 6480
fin <- 6648
plot(fracDay$fractionalday[strt:fin], Ta[strt:fin], lwd=2,main="Fall",ylab="Degrees C", xlab="Julian Day",type="l", ylim=c(min(Ta), max(Ta)))
points(fracDay$fractionalday[strt:fin], TaFitVals[strt:fin]+TaDayFitVal[strt:fin])
legend("bottomright", bty="n",c("Recorded", "Fitted"), pch=c(32,1), lty=c(1,0), lwd=c(2,1))


################## NEW PLOT ####################

transparent <- adjustcolor(col="black", alpha.f=0.4)
modeledcol <- "gray50"

png("d:/Users/sarah.fogg/Dropbox/PAPER1/figs/airFit.png",
    height  = 1000*3.5, width = 2000*3.5, res = 72*5)
layout(matrix(
  c(1,2,3,
    1,4,5
  ),
  nrow = 2, ncol = 3, byrow = T),
  heights = c(1, 1),
  widths = c(1,.4,.4))
# layout.show(5)
# par(mar = c(5, 4, 4, 1), oma = c(0, 4, 0, 0), cex.axis = 2, cex.main = 2.5, lend = 2)
par(cex.axis = 2, cex.lab = 2, cex.main = 2.5, mar = c(5.5,5.5,4,1))


plot(fracDay$fractionalday, Ta, main="Atmospheric Temperature Annual Cycle",ylab = expression(paste("Air Temperature ( ", degree, "C )")), xlab="Julian Day",type="l", ylim=c(min(Ta), max(Ta)))
lines(fracDay$fractionalday, TempFitVals, col=transparent, lwd = 3)
legend("bottomright", bty="n",c("Measured", "Modeled"), lty=c(1,1), lwd=c(1,3), cex=2, col = c("black", modeledcol))


#winter
strt <- 8400
fin <- 8568
plot(fracDay$fractionalday[strt:fin], Ta[strt:fin], lwd=1,main="Winter",ylab = expression(paste("Air Temperature ( ", degree, "C )")), xlab="Julian Day",type="l", ylim=c(min(Ta), max(Ta)))
lines(fracDay$fractionalday[strt:fin], TaFitVals[strt:fin]+TaDayFitVal[strt:fin], col=modeledcol, lwd = 3)
legend("bottomright", bty="n",c("Measured", "Modeled"), lty=c(1,1), lwd=c(1,3), cex=1.5, col = c("black", modeledcol))

#spring
strt <- 2160
fin <- 2328
plot(fracDay$fractionalday[strt:fin], Ta[strt:fin], lwd=1,main="Spring",ylab = "", xlab="Julian Day",type="l", ylim=c(min(Ta), max(Ta)))
lines(fracDay$fractionalday[strt:fin], TaFitVals[strt:fin]+TaDayFitVal[strt:fin], col=modeledcol, lwd = 3)
legend("bottomright", bty="n",c("Measured", "Modeled"), lty=c(1,1), lwd=c(1,3), cex=1.5, col = c("black", modeledcol))

#summer
strt <- 4560
fin <- 4728
plot(fracDay$fractionalday[strt:fin], Ta[strt:fin], lwd=1,main="Summer",ylab = expression(paste("Air Temperature ( ", degree, "C )")), xlab="Julian Day",type="l", ylim=c(min(Ta), max(Ta)))
lines(fracDay$fractionalday[strt:fin], TaFitVals[strt:fin]+TaDayFitVal[strt:fin], col=modeledcol, lwd = 3)
legend("bottomright", bty="n",c("Measured", "Modeled"), lty=c(1,1), lwd=c(1,3), cex=1.5, col = c("black", modeledcol))

#fall
strt <- 6480
fin <- 6648
plot(fracDay$fractionalday[strt:fin], Ta[strt:fin], lwd=1,main="Fall",ylab = "", xlab="Julian Day",type="l", ylim=c(min(Ta), max(Ta)))
lines(fracDay$fractionalday[strt:fin], TaFitVals[strt:fin]+TaDayFitVal[strt:fin], col=modeledcol, lwd = 3)
legend("bottomright", bty="n",c("Measured", "Modeled"), lty=c(1,1), lwd=c(1,3), cex=1.5, col = c("black", modeledcol))

dev.off()














#Compare to Sam
plot(TaFitVals+TaDayFitVal,col="blue", lwd=2, type="l")
lines(fitted(nlm_air), type = "l")

finishedAir <- TaFitVals+TaDayFitVal

plot(sec[0:120],finishedAir[0:120], type="l", ylim=c(-8,4))
points(ATemp$modelTime[0:1440], ATemp$svValue[0:1440])











#ARCHAIC
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#rHumid daily sin fit
rHumidDay<-rHumid[2:25]
secDay<-sec[2:25]
plot(rHumidDay, type="o")

rHumidDayMean <- mean(rHumidDay)
rHumidDayAmp <- (max(rHumidDay)-min(rHumidDay))/2
rHumidDayFit <- nls(rHumidDay ~ Mean+AmpD*sin((2*pi/86400)*secDay+PhaD),
                 start=list(Mean=rHumidDayMean,AmpD=rHumidDayAmp,PhaD=0))
rHumidDayFitVal <- fitted(rHumidDayFit)
plot(rHumidDay, type="o")
lines(rHumidDayFitVal, col="red", lwd=2)

#rHumid compound sin; annual variation upon daily mean values
rHumidComp <- nls(rHumid ~ (MeanY+AmpY*sin((2*pi/31557600)*sec+PhaY)) + AmpD * sin((2*pi/86400)*sec+PhaD),
   start=list(MeanY=rhAnnualMean,AmpY=rhAnnualAmp,PhaY=0,AmpD=rHumidDayAmp,PhaD=0))
rHumidCompVal <- fitted(rHumidComp)
plot(rHumid, col="seagreen")
lines(rHumidCompVal)
summary(rHumidComp)#use parameter estimates in model equationf for relative humidity

calcAVP<-rHumidCompVal*SVP#AVP calculated from rHumid compound sin fit
plot(AVP, col="violet")
lines(calcAVP)#results in realistic AVP values!!

recalcRH<-calcAVP/SVP
plot(rHumid, col="seagreen", type="l")
lines(recalcRH)

#write.table(rHumidCompVal, file="clipboard", sep="\t", row.names=FALSE)

#AVP annual sin fit
AVPMeanY <- mean(AVP)
AVPAmpY <- (max(AVP)-min(AVP))/2
AVPFit <- nls(AVP ~ Mean+Amp*sin((2*pi/31557600)*sec+Pha),
                      start=list(Mean=AVPMeanY,Amp=AVPAmpY,Pha=0))
AVPFitVal <- fitted(AVPFit)
plot(AVP, col="violet")
lines(AVPFitVal)

#AVP compound sin; annual variation upon daily mean values
AVPDay<-AVP[2:25]
AVPDayAmp <- (max(AVPDay)-min(AVPDay))/2
AVPFitComp <- nls(AVP ~ (MeanY+AmpY*sin((2*pi/31557600)*sec+PhaY)) + AmpD * sin((2*pi/86400)*secDay+PhaD), start=list(MeanY=AVPMeanY,AmpY=AVPAmpY,PhaY=0,AmpD=AVPDayAmp,PhaD=0))
AVPFitCompVal <- fitted(AVPFitComp)
plot(AVP, col="violet")
lines(AVPFitCompVal)

RHfromAVPComp<-AVPFitCompVal/SVP #recalculate rHumid from the compound fitted values
plot(RHfromAVPComp) #returns non-realistic values of relative humidity
lines(rHumid, col="gold") #line showing recorded rHumid values

#AVP compound sin; variation in annual mean and amp; uses "VPfitty" which is a list of the long method fit of values to this sin wave
VPFitComp2 <- nls(VPfitty ~ (MeanY+AmpY*sin((2*pi/31557600)*sec+PhaY)) + (MeanY+AmpY*sin((2*pi/31557600)*sec+PhaY)) * sin((2*pi/86400)*secDay+PhaD), start=list(MeanY=VapPressMeanY,AmpY=VapPressAmpY,PhaY=0,PhaD=0))
summary(VPFitComp2)
AVPFitComp2Val <- fitted(VPFitComp2)
plot(AVPFitComp2Val, type="l")

RHfromAVPComp2<-AVPFitComp2Val/SVP #recalculate rHumid from the compound fitted values
plot(RHfromAVPComp2) #returns non-realistic values of relative humidity
lines(rHumid, col="gold") #recorded rHumid values

#Calculates the daily amplitude of AVP
AVPamp <- NA #create NA inputs
AirWater <- cbind(AirWater, AVPamp) #create column on AirWater table for the AVP amplitude values
AMP <- numeric(365)
for (i in 1:365){
  grp <- AirWater[AirWater$Jday==i, ]
  AMP[i] <- (max(grp$AVP)-min(grp$AVP))/2
  AirWater$AVPamp[AirWater$Jday==i] <- AMP[i]
}
plot(jDay, AVP, col="pink")
lines(jDay, AirWater$AVPamp, type="l") #shows the amplitude value per day

#sin fit of the daily amplitude values of AVP
MeanAmp_init <- mean(AirWater$AVPamp) #mean amplitude of AVP
AmpAmp_init <- (max(AirWater$AVPamp)-min(AirWater$AVPamp)/2) #amplitude of AVP amplitude values
AVPamp <- AirWater$AVPamp
AVPampfit <- nls((AVPamp ~ MeanAmp + AmpAmp * sin((2*pi/31557600)*sec + PhaAmp)), start=list(MeanAmp=MeanAmp_init, AmpAmp=AmpAmp_init, PhaAmp=0))
AVPampfitVal <- fitted(AVPampfit)

#Calculates the daily mean of AVP
AVPmean <- NA #create NA inputs
AirWater <- cbind(AirWater, AVPmean) #create column on AirWater table for the AVP mean values
MEAN <- numeric(365)
for (i in 1:365){
  grp <- AirWater[AirWater$Jday==i, ]
  MEAN[i] <- mean(grp$AVP)
  AirWater$AVPmean[AirWater$Jday==i] <- MEAN[i]
}
plot(jDay, AVP, col="pink")
lines(jDay, AirWater$AVPmean, type="l") #shows the mean value per day

#sin fit of the daily mean values of AVP
MeanMean_init <- mean(AirWater$AVPmean) #mean mean of AVP
MeanAmp_init <- (max(AirWater$AVPmean)-min(AirWater$AVPmean)/2) #amplitude of AVP mean values
AVPmeanfit <- nls(AirWater$AVPmean ~ MeanMean + MeanAmp * sin((2*pi/31557600)*sec + MeanPha), start=list(MeanMean=MeanMean_init, MeanAmp=MeanAmp_init, MeanPha=0))
AVPmeanfitVal <- fitted(AVPmeanfit)
plot(jDay, AVP, col="pink")
lines(jDay, AirWater$AVPmean)
lines(jDay, AVPmeanfitVal)

plot(jDay, AVP, col="pink")
lines(jDay, AirWater$AVPamp, type="l")
lines(jDay, AVPampfitVal) #sin fit of the amplitude values
lines(jDay, AirWater$AVPmean, col="chocolate")
lines(jDay, AVPmeanfitVal, col="green3") #sin fit of the mean values

AirWater<-cbind(AirWater, AVPampfitVal)


AVPfitty <- numeric(8760)
for (i in 1:8760){
  AVPfitty[i] <- AirWater$AVPmeanfitVal[i] + AirWater$AVPampfitVal[i] * sin((2*pi/86400)*sec[i]+(-1.94780))
}

plot(AVP, col="pink")
lines(VPfitty)
points(AVP, col="pink")

AVPfitty2 <- nls(AVP ~ (MeanY+AmpY*sin((2*pi/31557600)*sec+PhaY)) + AVPampfitVal * sin((2*pi/86400)*secDay+PhaD),
                 start=list(MeanY=AVPMeanY,AmpY=AVPAmpY,PhaY=0,PhaD=0))
AVPfitty2Val <- fitted(VPfitty2)

AVPfitty3 <- nls(AVP ~ AVPmeanfitVal + AVPampfitVal * sin((2*pi/86400)*secDay+PhaD),
                 start=list(PhaD=0))

plot(fitted(AVPfitty3))
points(AVPfitty, col="goldenrod")
points(AVPfitty2, col="tomato") #different methods but all the same

#recalculate and compare relative humidity from AVPfitty
RHfromAVPfitty <- VPfittyVal/SVP
plot(RHfromAVPfitty)
lines(rHumid, col="gold")


VPfittyfit<-nls(VPfitty ~ MeanY+AmpY*sin((2*pi/31557600)*sec+PhaY)) + (VPamp ~ MeanAmp + AmpAmp * sin((2*pi/31557600)*sec + PhaAmp)) * sin((2*pi/86400)*secDay+PhaD))), start=list(MeanY=VapPressMeanY,AmpY=VapPressAmpY,PhaY=0,AmpD=VapPressDayAmp,PhaD=0,MeanAmp=MeanAmp_init, AmpAmp=AmpAmp_init, PhaAmp=0))


#Vary Daily
VapPressDayMean <- mean(VapPressDay)

VPDayFit <- nls(VapPress ~ MeanD + AmpD * sin((2*pi/86400)*sec+PhaD),
                   start=list(MeanD=VapPressDayMean,AmpD=VapPressDayAmp,PhaD=0))
VPDayFitVal <- fitted(VPDayFit)
plot(VapPress, col="violet")
lines(VPDayFitVal)

#Day plus Amp
seq(from=0, to=365, by=24)


VPDayFit <- nls(VapPress ~ MeanD + (MeanA + AmpA*sin(2*pi/86400)*sec+PhaA) * sin((2*pi/86400)*sec+PhaD),
                start=list(MeanD=VapPressDayMean,PhaD=0, MeanA=7, AmpA=5, PhaA=0))
VPDayFitVal <- fitted(VPDayFit)
plot(VapPress, col="violet")
lines(VPDayFitVal)




MeanAY <- (max(VapPress)-min(VapPress))/2
AmpAY <-


VPFitComp2 <- nls(VapPress ~ (MeanY+AmpY*sin((2*pi/31557600)*sec+PhaY)) + ((MeanY+AmpY*sin((2*pi/31557600)*sec+PhaY))+AmpAY*sin((2*pi/31557600)*sec+PhaAY)) * sin((2*pi/86400)*secDay+PhaD),
                 start=list(MeanY=VapPressMeanY ,AmpY=VapPressAmpY ,PhaY=0 ,AmpAY=5 ,PhaAY=0 ,PhaD=0))
VPFitComp2Val <- fitted(VPFitComp)
plot(VapPress, col="violet", type="l")
lines(VPFitComp2Val)



















aHumidMean <- mean(aHumid)
aHumidAmp <- (max(aHumid)-min(aHumid))/2

aHumidFit <- nls(aHumid ~ Mean+Amp*sin((2*pi/31557600)*sec+Pha),
                 start=list(Mean=aHumidMean,Amp=aHumidAmp,Pha=0))
summary(aHumidFit)
aHumidFitVals <- fitted(aHumidFit)
plot(aHumidFitVals)

aHumidAmpD <- max(aHumid[2:25])-min(aHumid[2:25])
aHumidFit2 <- nls(aHumid ~ (MeanY+AmpY*sin((2*pi/31557600)*sec+PhaY)+AmpD*sin((2*pi/86400)*secDay+PhaD)),
                 start=list(MeanY=aHumidMean,AmpY=aHumidAmp,PhaY=0, AmpD=aHumidAmpD, PhaD=0))
aHumidFit2Vals <- fitted(aHumidFit2)
plot(aHumidFit2Vals, type="l")
# aHumidFitMean <- coef(aHumidFit)[1]
# aHumidFitAmp <- coef(aHumidFit)[2]
# aHumidFitPha <- coef(aHumidFit)[3]

plot(sec, aHumid, col="steelblue")
lines(sec, aHumidFit2Vals, lwd=4)

plot(aHumid[4000:4096], type="l")
lines(aHumidFit2Vals[4000:4096], col="red")

derp <- aHumid-aHumidFitVals
plot(derp)

rHumidCalculator <- (aHumidFitVals * (R * (Ta + 273.16)))/(6.1275 * M * exp((17.2693882*Ta)/(Ta+237.3)))
plot(rHumidCalculator)

Calculator <- data.frame(aHumid, aHumidFitVals, rHumid, rHumidCalculator)
write.table(Calculator, file="clipboard", sep="\t", row.names=false)
rHumidCalc <- numeric(8760)

for (i in 1:8760){
  thisTa = AirWater$Ta[i]
  thisSVP = AirWater$SVP[i]
  rHumidCalc[i] =  (aHumidFit2Vals[i] * R * (thisTa + 273.16))/(M * thisSVP)
  print(i)
}

rH2 <- (aHumidFit2Vals * R * (Ta + 273.16))/(M * SVP)

plot(rH2)
plot(rHumid)
points(rH2, col="steelblue")



#Calculted rHumid values from fitted ambient vapor pressure and saturated vapor pressure
rH <- VPFitCompVal/SVP
plot(rH, type="l")
plot(rHumid, type="l", col="goldenrod")
lines(rH)

plot(rHumid[4000:4096], type="l", col="goldenrod")
lines(rH[4000:4096])

plot(rHumid[2000:2096], type="l", col="goldenrod")
lines(rH[2000:2096])

plot(rHumid[6000:7000], type="l", col="goldenrod")
lines(rH[6000:7000])


plot(VapPress[1000:1960], type="l")
lines(VPFitCompVal[1000:1960], col="red")

plot(VapPress, type="l")
lines(VPFitCompVal, col="red")


plot(aHumid)
plot(VapPress, type="l")
plot(aHumid, VapPress)
