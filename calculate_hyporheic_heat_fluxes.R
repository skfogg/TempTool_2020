
highhypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 35.94, b=-1.39)
mediumhypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 23.96, b=-1.39)
littlehypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 11.98, b=-1.39)

load("runs_using_2017_umatilla_aquifer_geometry/high/highHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/med/medHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/little/littleHypo870.RData")

load("runs_using_2017_umatilla_aquifer_geometry/high/tszHeatOut_high.RData")
load("runs_using_2017_umatilla_aquifer_geometry/med/tszHeatOut_med.RData")
load("runs_using_2017_umatilla_aquifer_geometry/little/tszHeatOut_little.RData")

tszoutHigh_df <- data.frame(tszHeatOut_high[[1]])
for(i in 2:18){
  tszoutHigh_df <- cbind(tszoutHigh_df, tszHeatOut_high[[i]])
}
tszoutHigh <- rowSums(tszoutHigh_df)
tszoutHigh <- as.data.frame(tszoutHigh)
tszoutHigh <- xts(zoo(coredata(tszoutHigh$tszoutHigh), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

tszoutMed_df <- data.frame(tszHeatOut_med[[1]])
for(i in 2:18){
  tszoutMed_df <- cbind(tszoutMed_df, tszHeatOut_med[[i]])
}
tszoutMed <- rowSums(tszoutMed_df)
tszoutMed <- as.data.frame(tszoutMed)
tszoutMed <- xts(zoo(coredata(tszoutMed$tszoutMed), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

tszoutLittle_df <- data.frame(tszHeatOut_little[[1]])
for(i in 2:18){
  tszoutLittle_df <- cbind(tszoutLittle_df, tszHeatOut_little[[i]])
}
tszoutLittle <- rowSums(tszoutLittle_df)
tszoutLittle <- as.data.frame(tszoutLittle)
tszoutLittle <- xts(zoo(coredata(tszoutLittle$tszoutLittle), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))


binstats <- highhypoBins
T_channel <- coredata(highHypo870$cTemp$svValue)
Q_up <- coredata(tszoutHigh)


calc_Q_b <- function(binstats, T_channel, Q_up){
  rho <- 999.7
  c <- 4.192
  q_up <- binstats$returning
  q_down <- binstats$entering
  Q_down <- q_down[1] * (T_channel+273.15) * c * rho
  Q_b <- Q_up - Q_down
  return(Q_b)
}

Q_b_high <- calc_Q_b(highhypoBins, coredata(highHypo870$cTemp$svValue), coredata(tszoutHigh))
Q_b_med <- calc_Q_b(mediumhypoBins, coredata(medHypo870$cTemp$svValue), coredata(tszoutMed))
Q_b_little <- calc_Q_b(littlehypoBins, coredata(littleHypo870$cTemp$svValue), coredata(tszoutLittle))


plot(Q_b_high, type = "l", col = highhypogray)
lines(Q_b_med, col = medhypogray)
lines(Q_b_little, col = littlehypogray)
abline(h = 0, lty = 2)

save("Q_b_high", Q_b_high, file = "runs_using_2017_umatilla_aquifer_geometry/high/Q_b_high.RData")
save("Q_b_med", Q_b_med, file = "runs_using_2017_umatilla_aquifer_geometry/med/Q_b_med.RData")
save("Q_b_high", Q_b_high, file = "runs_using_2017_umatilla_aquifer_geometry/little/Q_b_little.RData")


