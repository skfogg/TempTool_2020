
library(humidity)

load("C:/Users/t24x137/Desktop/Old Tower Desktop Folders/TempToolModelTesting2/longwaveNet30.RData")
load("C:/Users/t24x137/Desktop/Old Tower Desktop Folders/TempToolModelTesting2/shortwaveNet30.RData")


plot(longwaveNet30)
plot(shortwaveNet30)
plot(apply.daily(shortwaveNet30,mean))


connect <- odbcConnect("TempToolFourANSI", "root", "MSUFLL!!")
sv <- sqlQuery(connect, "SELECT stateVal FROM temptoolfour.temp_signal_output;")
statevalues <- unique(sv)
statevalues


swgross <- sqlQuery(connect, "SELECT *
         FROM temptoolfour.temp_signal_output
         WHERE stateVal = 'SHORTWAVEGROSS'")
lwair <- sqlQuery(connect, "SELECT *
         FROM temptoolfour.temp_signal_output
         WHERE stateVal = 'LONGWAVEFLUXAIR'")

t <- seq(0, 86400*365, by = 86400)

relhumid <- 13.09056*sin(2*pi*(1/31557600)*t - 123.82405) + 65.76388   #(65.32 + 16.83*sin(2*pi*(1/31557600)*t) - 4.48) + (24.46 + 7.08*sin(2*pi*(1/31557600)*t + 1.15))*(sin(2*pi*(1/86400)*t - 2.25))
plot(relhumid, type = "l")

airtemp <- 10.353237*sin(2*pi*(1/31557600)*t + 4.418827) + 10.784492
plot(airtemp)

satVapPress <- 6.1078*exp((17.26939*airtemp)/(airtemp+273.16-35.86)) # IN MBAR!!
plot(satVapPress*100)
plot(satVapPress/relhumid/100)

specific <- SH(relhumid*satVapPress, 84500)

lw <- xts(zoo(lwair$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
sw <- xts(zoo(swgross$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

plot(apply.daily(sw, mean))
plot(apply.daily(lw, mean))

sw_mean <- apply.daily(sw,mean)
lw_mean <- apply.daily(lw,mean)
plot(sw_mean["2016"])


shortwave <- data.frame(time = seq(0, 86400*365, by = 86400), value = coredata(sw_mean["2016"])*1000)
longwave <- data.frame(time = seq(0, 86400*365, by = 86400), value = coredata(lw_mean["2016"])*1000)
airtemperature <- data.frame(time = seq(0, 86400*365, by = 86400), value = airtemp)
relativehumidity <- data.frame(time = seq(0, 86400*365, by = 86400), value = relhumid/100)
saturatedvaporpressure <- data.frame(time = seq(0, 86400*365, by = 86400), value = satVapPress*100)
specifichumidity <- data.frame(time = seq(0, 86400*365, by = 86400), value = specific)


write.csv(shortwave, "C:/Users/t24x137/Desktop/daily_atm/shortwave.csv")
write.csv(longwave, "C:/Users/t24x137/Desktop/daily_atm/longwave.csv")
write.csv(airtemperature, "C:/Users/t24x137/Desktop/daily_atm/airtemperature.csv")
write.csv(relativehumidity, "C:/Users/t24x137/Desktop/daily_atm/relativehumidity.csv")
write.csv(saturatedvaporpressure, "C:/Users/t24x137/Desktop/daily_atm/saturatedvaporpressure.csv")
write.csv(specifichumidity, "C:/Users/t24x137/Desktop/daily_atm/specifichumidity.csv")




