library(RODBC)
library(lubridate)
library(zoo)
library(xts)
source('CalcReturnFracForModel_3.R')
source("D:/Users/sarah.fogg/Desktop/R_scripts/createnames.R")
source('D:/Users/sarah.fogg/Desktop/R_scripts/createEdgeNames.R')
source('d:/Users/sarah.fogg/Desktop/Contract Goals/Sensitivity Analysis/NEOStateValSpec.R')
source('d:/Users/sarah.fogg/Desktop/Contract Goals/Sensitivity Analysis/plottingFunctions.R')



par(bg = "black", col.axis = "white", col.lab = "white", col.main = "white", col = "white", fg = "white",
    mfrow=c(1,1), cex.axis=1.5, cex.main=2, mar = c(5,5,1,1) + 0.3, oma = c(0,0,0,0), cex.lab=1.5)

plot(theseRuns[[1]]$to/86400, log(theseRuns[[1]]$returnFlow*86400), type="l", col=paired2[1],
     ylim=c(-10, max(log(theseRuns[[6]]$returnFlow*86400))+2), lwd=2,
     ylab = "log Q (m3 day-1)", xlab = "Residence Time (days)")
for (i in 2:6){
  lines(theseRuns[[i]]$to/86400, log(theseRuns[[i]]$returnFlow*86400), col=paired2[i], lwd=2)
}
legend("topright", c("0.1", "0.01", "0.001", "0.0001", "0.00001", "0.000001"), title = "a Values",
       col=paired2[seq(6, 1, -1)], lty=rep(1, times = 6), lwd=rep(2, times=6), cex = 1.5)



plot(theseRuns[[1]]$to/86400, theseRuns[[1]]$returnFlow*86400, log="y",
     type="l", col=paired2[1], ylim = c(0.0001, 1400), lwd=2,
     ylab = "Q (m3 day-1)", xlab = "Residence Time (days)", yaxt = "n")
text(160, theseRuns[[1]]$returnFlow[10]*86400,
     paste0("a = ",as.character(signif(attributes(theseRuns[[1]])$a, 1))),
     cex=1.4)
for (i in 2:6){
  lines(theseRuns[[i]]$to/86400, theseRuns[[i]]$returnFlow*86400, col=paired2[i], lwd=2)
  text(160, theseRuns[[i]]$returnFlow[10]*86400,
       paste0("a = ",as.character(signif(attributes(theseRuns[[i]])$a, 1))),
       cex=1.4)
}
axis(2, at = c(0.0001, 0.01, 1, 100), labels = c(0.0001, 0.01, 1, 100), line=0)

legend("topright", c("0.1", "0.01", "0.001", "0.0001", "0.00001", "0.000001"), title = "a Values",
       col=paired2[seq(6, 1, -1)], lty=rep(1, times = 6), lwd=rep(2, times=6), cex = 1.5)


theseRuns_1.7 <- mapply(
  getHyporheicBinStats3,
  aquiferVol = forbplot$aqVol,
  hyporheicQ = forbplot$hypQ,
  MoreArgs = list(
    nbins = nbins,
    factor = factor,
    minRT = minRT,
    maxRT = maxRT,
    porosity = porosity),
  SIMPLIFY = F
)

plot(theseRuns[[1]]$to/86400, theseRuns[[1]]$returnFlow*86400, type="l", col=paired2[2],
     ylim=c(0, max(theseRuns_1.7[[1]]$returnFlow*86400)))
lines(theseRuns_1.7[[1]]$to/86400, theseRuns_1.7[[1]]$returnFlow*86400, col=paired2[5])

plot(theseRuns[[1]]$to/86400, theseRuns[[1]]$returnFlow*86400, type="l", col=paired2[2],
     xlim=c(0,4),
     ylim=c(0, max(theseRuns_1.7[[1]]$returnFlow*86400)))
lines(theseRuns_1.7[[1]]$to/86400, theseRuns_1.7[[1]]$returnFlow*86400, col=paired2[5])


plot(theseRuns[[1]]$to/86400, theseRuns[[1]]$returnFlow*86400, type="l", col=paired2[2],
     xlim=c(150,160),
     ylim=c(0,0.001))
lines(theseRuns_1.7[[1]]$to/86400, theseRuns_1.7[[1]]$returnFlow*86400, col=paired2[5])


##### FOR ANIMATION: 3 HYSTERESIS LOOPS #####
plotDateForward2 <- c(plotDatesForward[10:53], plotDatesForward[1:10])
months2 <- c(rep("March", times = 4), rep("April", times = 5), rep(c("May", "June"), each = 4), rep("July", times = 5), rep(c("August", "September"), each = 4),
             rep("October", times = 5),rep("November", times = 4), rep("December", times = 5), rep("January", times = 5), rep("February", times = 4), "March")
threeCol <- c("gold2", "mediumorchid4","dodgerblue")
for (i in length(plotDateForward2)){
#   png(filename = paste0("d:/Users/sarah.fogg/Desktop/Contract Goals/Sensitivity Analysis/SensitivityAnalysisData/Plots/ToAnimate/Animation4/Slide", i, "WithSlope2.png"),
#       width = 580,
#       height = 580)
  par(bg = "black", col.axis = "white", col.lab = "white", col.main = "white", col = "white", fg = "white", mar = c(5, 5, 2, 2) + 0.1, cex.lab = 2, cex.axis = 1.5)
  hystPlot(experimentalTemp = list(output_run4[[1]], output_run5[[1]], output_run6[[1]]), referenceTemp = output_run1[[1]], samePlot = T, index = plotDateForward2[i], type="l",
           lwd=2,  xlim = c(-5, 26), ylim = c(-5, 26), colors = "black")
  legend("topleft", c("10000 m3", "1000 m3", "100 m3"), title = "Aquifer Volume", col = c("dodgerblue", "mediumorchid4", "darkgoldenrod2"), pch = "O", pt.cex=1.5, cex = 1.5)
  for (j in 1:i){
    lines(coredata(output_run4[[1]]$svValue[plotDateForward2[j]]) ~ coredata(output_run1[[1]]$svValue[plotDateForward2[j]]), col = adjustcolor("gold1", alpha.f = 0.2), lwd=2)
    lines(coredata(output_run5[[1]]$svValue[plotDateForward2[j]]) ~ coredata(output_run1[[1]]$svValue[plotDateForward2[j]]), col = adjustcolor("mediumorchid3", alpha.f = 0.25), lwd=2)
    lines(coredata(output_run6[[1]]$svValue[plotDateForward2[j]]) ~ coredata(output_run1[[1]]$svValue[plotDateForward2[j]]), col = adjustcolor(threeCol[3], alpha.f = 0.4), lwd=2)
  }
#   lines(coredata(output_run4[[1]]$svValue[plotDateForward2[i]]) ~ coredata(output_run1[[1]]$svValue[plotDateForward2[i]]), lwd=2, col = "darkgoldenrod2")
#   lines(coredata(output_run5[[1]]$svValue[plotDateForward2[i]]) ~ coredata(output_run1[[1]]$svValue[plotDateForward2[i]]), lwd=2, col = threeCol[2])
#   lines(coredata(output_run6[[1]]$svValue[plotDateForward2[i]]) ~ coredata(output_run1[[1]]$svValue[plotDateForward2[i]]), lwd=2, col = threeCol[3])

  # Annual Slopes
  lm_gold <- lm(coredata(output_run4[[1]]$svValue) ~ coredata(output_run1[[1]]$svValue))
  lm_purp <- lm(coredata(output_run5[[1]]$svValue) ~ coredata(output_run1[[1]]$svValue))
  lm_blue <- lm(coredata(output_run6[[1]]$svValue) ~ coredata(output_run1[[1]]$svValue))
  abline(lm_gold, col = "darkgoldenrod", lty = 2)
  abline(lm_purp, col = "mediumorchid4", lty = 2)
  abline(lm_blue, col = "dodgerblue3", lty = 2)

#   points(mean(coredata(output_run1[[1]]$svValue)), mean(coredata(output_run4[[1]]$svValue)), pch = 16, col = "darkgoldenrod")
#   points(mean(coredata(output_run1[[1]]$svValue)), mean(coredata(output_run5[[1]]$svValue)), pch = 16, col = "mediumorchid4")
#   points(mean(coredata(output_run1[[1]]$svValue)), mean(coredata(output_run6[[1]]$svValue)), pch = 16, col = "dodgerblue3")


#   if (i %in% c(1:22)){
#     text(mean(coredata(output_run1[[1]]$svValue[plotDateForward2[i]])) + 2, mean(coredata(output_run6[[1]]$svValue[plotDateForward2[i]])) - 2, labels = months2[i], col="gray95", cex = 1.5)
#   }
#   if (i %in% c(23:44)){
#     text(mean(coredata(output_run1[[1]]$svValue[plotDateForward2[i]])) - 2, mean(coredata(output_run6[[1]]$svValue[plotDateForward2[i]])) + 2.5, labels = months2[i], col="gray95", cex = 1.5)
#   }
#   if (i %in% c(45:46)){
#     text(mean(coredata(output_run1[[1]]$svValue[plotDateForward2[i]])) + 2.5, mean(coredata(output_run6[[1]]$svValue[plotDateForward2[i]])) - 5, labels = months2[i], col="gray95", cex = 1.5)
#   }
#   if (i %in% c(47:49)){
#     text(mean(coredata(output_run1[[1]]$svValue[plotDateForward2[i]])) + 2.5, mean(coredata(output_run6[[1]]$svValue[plotDateForward2[i]])) - 5, labels = months2[i], col="gray95", cex = 1.5)
#   }
#   if (i %in% c(50,51)){
#     text(mean(coredata(output_run1[[1]]$svValue[plotDateForward2[i]])) + 1.5, mean(coredata(output_run6[[1]]$svValue[plotDateForward2[i]])) - 4.5, labels = months2[i], col="gray95", cex = 1.5)
#   }
#   if (i %in% c(52,53)){
#     text(mean(coredata(output_run1[[1]]$svValue[plotDateForward2[i]])) + 1, mean(coredata(ctemp2$svValue[plotDateForward2[i]])) - 3, labels = months2[i], col="gray95", cex = 1.5)
#   }
#   if (i == 54){
#     text(mean(coredata(output_run1[[1]]$svValue[plotDateForward2[i]])) + 2, mean(coredata(ctemp2$svValue[plotDateForward2[i]])) - 2, labels = months2[i], col="gray95", cex = 1.5)
#   }
  # dev.off()
}


##### Hyst and Sine Feb May Aug #####
par(bg = "black", col.axis = "white", col.lab = "white", col.main = "white", col = "white", fg = "white",
    mar = c(0, 0, 4, 2) + 0.3, cex.lab = 2, cex.axis = 1.5, mfrow = c(3,3), oma = c(5, 5, 0, 0), cex.main = 2)
hystPlot(experimentalTemp = output_run4[[1]], referenceTemp = output_run1[[1]], index = aug, type="l",
         lwd=2, xlim = c(15, 25), ylim = c(15, 25), colors = threeCol[1], main = "100 m3")
hystPlot(experimentalTemp = output_run5[[1]], referenceTemp = output_run1[[1]], index = aug, type="l",
         lwd=2, xlim = c(15, 25), ylim = c(15, 25), colors = "mediumorchid2", main = "1000 m3")
hystPlot(experimentalTemp = output_run6[[1]], referenceTemp = output_run1[[1]], index = aug, type="l",
         lwd=2, xlim = c(15, 25), ylim = c(15, 25), colors = threeCol[3], main = "10000 m3")
mtext("Treatment Stream Temperature", 2, line=3, outer=T, cex = 1.5)
mtext("Reference Stream Temperature", 1, line=3, outer=T, cex = 1.5)


hystPlot(experimentalTemp = output_run4[[1]], referenceTemp = output_run1[[1]], index = may, type="l",
         lwd=2, xlim = c(10, 20), ylim = c(10, 20), colors = threeCol[1], main = "")
hystPlot(experimentalTemp = output_run5[[1]], referenceTemp = output_run1[[1]], index = may, type="l",
         lwd=2, xlim = c(10, 20), ylim = c(10, 20), colors = "mediumorchid2", main = "")
hystPlot(experimentalTemp = output_run6[[1]], referenceTemp = output_run1[[1]], index = may, type="l",
         lwd=2, xlim = c(10, 20), ylim = c(10, 20), colors = threeCol[3], main = "")
mtext("Treatment Stream Temperature", 2, line=3, outer=T, cex = 1.5)
mtext("Reference Stream Temperature", 1, line=3, outer=T, cex = 1.5)



hystPlot(experimentalTemp = output_run4[[1]], referenceTemp = output_run1[[1]], index = feb, type="l",
         lwd=2, xlim = c(-5, 5), ylim = c(-5, 5), colors = threeCol[1], main = "")
hystPlot(experimentalTemp = output_run5[[1]], referenceTemp = output_run1[[1]], index = feb, type="l",
         lwd=2, xlim = c(-5, 5), ylim = c(-5, 5), colors = "mediumorchid2", main = "")
hystPlot(experimentalTemp = output_run6[[1]], referenceTemp = output_run1[[1]], index = feb, type="l",
         lwd=2, xlim = c(-5, 5), ylim = c(-5, 5), colors = threeCol[3], main = "")
mtext("Treatment Stream Temperature", 2, line=3, outer=T, cex = 1.5)
mtext("Reference Stream Temperature", 1, line=3, outer=T, cex = 1.5)




par(bg = "black", col.axis = "white", col.lab = "white", col.main = "white", col = "white", fg = "white",
    mar = c(0, 0, 4, 2) + 0.3, cex.lab = 2, cex.axis = 1.5, mfrow = c(1,3), oma = c(5, 5, 0, 0), cex.main = 2)
# FEB SINE
plot(output_run1[[1]][feb], type="l", ylim = c(-4, 5), auto.grid = F, xaxt = "n", main = "100 m3")
lines(output_run4[[1]][feb], lwd=1, col = threeCol[1], lty=2)
axis(1, at=index(output_run1[[1]][feb])[c(1, 25, 49, 73, 96)] ,
     labels=c(0,6,12,18,24), cex=1.5)

plot(output_run1[[1]][feb], type="l", ylim = c(-4, 5), auto.grid = F, xaxt = "n", main = "1000 m3")
lines(output_run5[[1]][feb], lwd=1, col = "mediumorchid2", lty=2)
axis(1, at=index(output_run1[[1]][feb])[c(1, 25, 49, 73, 96)] ,
     labels=c(0,6,12,18,24), cex=1.5)

plot(output_run1[[1]][feb], type="l", ylim = c(-4, 5), auto.grid = F, xaxt = "n", main = "10000 m3")
lines(output_run6[[1]][feb], lwd=1, col = threeCol[3], lty=2)
axis(1, at=index(output_run1[[1]][feb])[c(1, 25, 49, 73, 96)] ,
     labels=c(0,6,12,18,24), cex=1.5)
mtext("Stream Temperature", 2, line=3, outer=T, cex = 1.5)
mtext("Hour", 1, line=3, outer=T, cex = 1.5)

# MAY SINE
plot(output_run1[[1]][may], type="l", ylim = c(15, 25), auto.grid = F, xaxt = "n", main = "100 m3")
lines(output_run4[[1]][may], lwd=1, col = threeCol[1], lty=2)
axis(1, at=index(output_run1[[1]][may])[c(1, 25, 49, 73, 96)] ,
     labels=c(0,6,12,18,24), cex=1.5)
legend("bottomleft", c("Reference ", "Treatment"), lty = c(1, 2), col = c("gray95", threeCol),ncol = 2)

plot(output_run1[[1]][may], type="l", ylim = c(15, 25), auto.grid = F, xaxt = "n", main = "1000 m3")
lines(output_run5[[1]][may], lwd=1, col = "mediumorchid2", lty=2)
axis(1, at=index(output_run1[[1]][may])[c(1, 25, 49, 73, 96)] ,
     labels=c(0,6,12,18,24), cex=1.5)
legend("bottomleft", c("Reference ", "Treatment"), lty = c(1, 2), col = c("gray95", "mediumorchid2"),ncol = 2)

plot(output_run1[[1]][may], type="l", ylim = c(15, 25), auto.grid = F, xaxt = "n", main = "10000 m3")
lines(output_run6[[1]][may], lwd=1, col = threeCol[3], lty=2)
axis(1, at=index(output_run1[[1]][may])[c(1, 25, 49, 73, 96)] ,
     labels=c(0,6,12,18,24), cex=1.5)
legend("bottomleft", c("Reference ", "Treatment"), lty = c(1, 2), col = c("gray95", threeCol[3]),ncol = 2)
mtext("Stream Temperature", 2, line=3, outer=T, cex = 1.5)
mtext("Hour", 1, line=3, outer=T, cex = 1.5)





###### FINAL SIMULATED MEAN ANNUAL AND DIEL FROM RUN2  #####

par(bg = "black", col.axis = "white", col.lab = "white", col.main = "white", col = "white", fg = "white",
    mfrow=c(1,2), cex.axis=1.5, cex.main=2, mar = c(5,1,4,1) + 0.3, oma = c(0,4,0,2), cex.lab=1.5)
plot(output_run5[[1]]["2014-07-18"], xaxt = "n", auto.grid = F, main = "Simulated Diel",
     ylim = c(17.5, 23.5), xlab = "Hour")
for (j in seq(10, 1, -1)){
  lines(output_run5[[j]]["2014-07-18"], col=rainbow13[j], lwd=2)
}
lines(output_run5[[1]]["2014-07-18"], lwd=4, col = rainbow13[1])
axis(1, at=index(output_run5[[1]]["2014-07-18"])[c(1, 25, 49, 73, 96)] ,
     labels=c(0,6,12,18,24), cex=1.5)
legend("topleft", "Channel", col="gray70", lwd = 4, bty="n")

meanDaily <- apply.daily(output_run5[[1]], mean)["2014-07-01/2014-12-31"]
meanDaily2 <- apply.daily(output_run5[[1]], mean)["2014-01-01/2014-06-30"]
year(index(meanDaily2)) <- 2015

new <- rbind(meanDaily, meanDaily2)

plot(as.zoo(new$svValue), xlab= "Month",
     main = "Simulated Mean Annual", col="gray70", ylim = c(-3,25))

# plot(c(1:184), coredata(apply.daily(output_run5[[1]]$svValue["2014-07-01/2014-12-31"], mean)), col="orange", xlim = c(1, (181+184)))
# lines(c(185:(181+184)), coredata(apply.daily(output_run5[[1]]$svValue["2014-01-01/2014-06-30"], mean)))

for (j in seq(13, 10, -1)){
  meanDailyHyst <- apply.daily(output_run5[[j]], mean)["2014-07-01/2014-12-31"]
  meanDaily2Hyst <- apply.daily(output_run5[[j]], mean)["2014-01-01/2014-06-30"]
  year(index(meanDaily2Hyst)) <- 2015
  newHyst <- rbind(meanDailyHyst, meanDaily2Hyst)
  lines(newHyst$svValue, col=rainbow13[j], lwd=2)
}
lines(new$svValue, lwd=4, col = rainbow13[1])
legend("topleft", "Channel Daily Mean", col="gray70", lwd = 4, bty="n")
mtext("Temperature", 2, line=2, outer=T, cex=1.6)
######
##### FEB MAY AUG SAME PLOT ######
par(mfrow = c(1,4), mar = c(1,1,3,1), oma = c(5,5,0,0), cex.axis = 2)
plot(coredata(output_run3[[1]]$svValue[feb]), type ="l", xaxt = "n", ylim=c(-2, 26), main = "10 m3")
for (i in seq(10, 1, -1)){
  lines(coredata(output_run3[[i]]$svValue[feb]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run3[[i]]$svValue[may]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run3[[i]]$svValue[aug]), col = rainbow13[i], lwd=2)
}
lines(coredata(output_run3[[1]]$svValue[feb]), lwd=3)
lines(coredata(output_run3[[1]]$svValue[may]), lwd=3)
lines(coredata(output_run3[[1]]$svValue[aug]), lwd=3)
text(88, last(coredata(output_run3[[1]]$svValue[feb]) + 1.5), "Feb", cex=2)
text(88, last(coredata(output_run3[[1]]$svValue[may]) + 3), "May", cex=2)
text(88, last(coredata(output_run3[[1]]$svValue[aug]) + 3), "Aug", cex=2)
axis(1, at=c(0,48,96), labels =c(0,12,24))

plot(coredata(output_run4[[1]]$svValue[feb]), type ="l",
     xaxt = "n", ylim=c(-2, 26), main = "100 m3", yaxt = "n")
for (i in seq(10, 1, -1)){
  lines(coredata(output_run4[[i]]$svValue[feb]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run4[[i]]$svValue[may]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run4[[i]]$svValue[aug]), col = rainbow13[i], lwd=2)
}
lines(coredata(output_run4[[1]]$svValue[feb]), lwd=3)
lines(coredata(output_run4[[1]]$svValue[may]), lwd=3)
lines(coredata(output_run4[[1]]$svValue[aug]), lwd=3)
text(88, last(coredata(output_run4[[1]]$svValue[feb]) + 1.5), "Feb", cex=2)
text(88, last(coredata(output_run4[[1]]$svValue[may]) + 3), "May", cex=2)
text(88, last(coredata(output_run4[[1]]$svValue[aug]) + 3), "Aug", cex=2)
axis(1, at=c(0,48,96), labels =c(0,12,24))


plot(coredata(output_run5[[1]]$svValue[feb]), type ="l", xaxt = "n", yaxt = "n", ylim=c(-2, 26), main = "1000 m3")
for (i in seq(10, 1, -1)){
  lines(coredata(output_run5[[i]]$svValue[feb]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run5[[i]]$svValue[may]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run5[[i]]$svValue[aug]), col = rainbow13[i], lwd=2)
}
lines(coredata(output_run5[[1]]$svValue[feb]), lwd=3)
lines(coredata(output_run5[[1]]$svValue[may]), lwd=3)
lines(coredata(output_run5[[1]]$svValue[aug]), lwd=3)
text(88, last(coredata(output_run5[[1]]$svValue[feb]) + 1.5), "Feb", cex=2)
text(88, last(coredata(output_run5[[1]]$svValue[may]) + 3), "May", cex=2)
text(88, last(coredata(output_run5[[1]]$svValue[aug]) + 3), "Aug", cex=2)
axis(1, at=c(0,48,96), labels =c(0,12,24))


plot(coredata(output_run6[[1]]$svValue[feb]), type ="l", xaxt = "n", yaxt = "n", ylim=c(-2, 26), main = "10000 m3")
for (i in seq(10, 1, -1)){
  lines(coredata(output_run6[[i]]$svValue[feb]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run6[[i]]$svValue[may]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run6[[i]]$svValue[aug]), col = rainbow13[i], lwd=2)
}
lines(coredata(output_run6[[1]]$svValue[feb]), lwd=3)
lines(coredata(output_run6[[1]]$svValue[may]), lwd=3)
lines(coredata(output_run6[[1]]$svValue[aug]), lwd=3)
text(88, last(coredata(output_run6[[1]]$svValue[feb]) + 1.5), "Feb", cex=2)
text(88, last(coredata(output_run6[[1]]$svValue[may]) + 2), "May", cex=2)
text(88, last(coredata(output_run6[[1]]$svValue[aug]) + 2), "Aug", cex=2)
axis(1, at=c(0,48,96), labels =c(0,12,24))

mtext("Temperature", 2, line=3, outer=T, cex=1.5)
mtext("Hour", 1, line=3, outer=T, cex=1.5)


##### FEB MAY AUG NOV SAME PLOT #####
par(mfrow = c(1,4), mar = c(1,1,3,1), oma = c(5,5,0,0), cex.axis = 2)
plot(coredata(output_run3[[1]]$svValue[feb]), type ="l", xaxt = "n", ylim=c(-2, 26), main = "10 m3")
for (i in seq(10, 1, -1)){
  lines(coredata(output_run3[[i]]$svValue[feb]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run3[[i]]$svValue[may]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run3[[i]]$svValue[aug]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run3[[i]]$svValue[nov]), col = rainbow13[i], lwd=2)
}
lines(coredata(output_run3[[1]]$svValue[feb]), lwd=3)
lines(coredata(output_run3[[1]]$svValue[may]), lwd=3)
lines(coredata(output_run3[[1]]$svValue[aug]), lwd=3)
lines(coredata(output_run3[[1]]$svValue[nov]), lwd=3)
text(88, last(coredata(output_run3[[1]]$svValue[feb]) + 1.5), "Feb", cex=2)
text(88, last(coredata(output_run3[[1]]$svValue[may]) + 3), "May", cex=2)
text(88, last(coredata(output_run3[[1]]$svValue[aug]) + 3), "Aug", cex=2)
text(88, last(coredata(output_run3[[1]]$svValue[nov]) + 2.5), "Nov", cex=2)
axis(1, at=c(0,48,96), labels =c(0,12,24))

plot(coredata(output_run4[[1]]$svValue[feb]), type ="l",
     xaxt = "n", ylim=c(-2, 26), main = "100 m3", yaxt = "n")
for (i in seq(10, 1, -1)){
  lines(coredata(output_run4[[i]]$svValue[feb]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run4[[i]]$svValue[may]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run4[[i]]$svValue[aug]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run4[[i]]$svValue[nov]), col = rainbow13[i], lwd=2)
}
lines(coredata(output_run4[[1]]$svValue[feb]), lwd=3)
lines(coredata(output_run4[[1]]$svValue[may]), lwd=3)
lines(coredata(output_run4[[1]]$svValue[aug]), lwd=3)
lines(coredata(output_run4[[1]]$svValue[nov]), lwd=3)
text(88, last(coredata(output_run4[[1]]$svValue[feb]) + 1.5), "Feb", cex=2)
text(88, last(coredata(output_run4[[1]]$svValue[may]) + 3), "May", cex=2)
text(88, last(coredata(output_run4[[1]]$svValue[aug]) + 3), "Aug", cex=2)
text(88, last(coredata(output_run4[[1]]$svValue[nov]) + 2.5), "Nov", cex=2)
axis(1, at=c(0,48,96), labels =c(0,12,24))


plot(coredata(output_run5[[1]]$svValue[feb]), type ="l", xaxt = "n", yaxt = "n", ylim=c(-2, 26), main = "1000 m3")
for (i in seq(10, 1, -1)){
  lines(coredata(output_run5[[i]]$svValue[feb]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run5[[i]]$svValue[may]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run5[[i]]$svValue[aug]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run5[[i]]$svValue[nov]), col = rainbow13[i], lwd=2)
}
lines(coredata(output_run5[[1]]$svValue[feb]), lwd=3)
lines(coredata(output_run5[[1]]$svValue[may]), lwd=3)
lines(coredata(output_run5[[1]]$svValue[aug]), lwd=3)
lines(coredata(output_run5[[1]]$svValue[nov]), lwd=3)
text(88, last(coredata(output_run5[[1]]$svValue[feb]) + 1.5), "Feb", cex=2)
text(88, last(coredata(output_run5[[1]]$svValue[may]) + 3), "May", cex=2)
text(88, last(coredata(output_run5[[1]]$svValue[aug]) + 3), "Aug", cex=2)
text(88, last(coredata(output_run5[[1]]$svValue[nov]) + 3), "Nov", cex=2)
axis(1, at=c(0,48,96), labels =c(0,12,24))


plot(coredata(output_run6[[1]]$svValue[feb]), type ="l", xaxt = "n", yaxt = "n", ylim=c(-2, 26), main = "10000 m3")
for (i in seq(10, 1, -1)){
  lines(coredata(output_run6[[i]]$svValue[feb]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run6[[i]]$svValue[may]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run6[[i]]$svValue[aug]), col = rainbow13[i], lwd=2)
}
for (i in seq(10, 1, -1)){
  lines(coredata(output_run6[[i]]$svValue[nov]), col = rainbow13[i], lwd=2)
}
lines(coredata(output_run6[[1]]$svValue[feb]), lwd=3)
lines(coredata(output_run6[[1]]$svValue[may]), lwd=3)
lines(coredata(output_run6[[1]]$svValue[aug]), lwd=3)
lines(coredata(output_run6[[1]]$svValue[nov]), lwd=3)
text(88, last(coredata(output_run6[[1]]$svValue[feb]) + 1.5), "Feb", cex=2)
text(88, last(coredata(output_run6[[1]]$svValue[may]) + 2), "May", cex=2)
text(88, last(coredata(output_run6[[1]]$svValue[aug]) + 2), "Aug", cex=2)
text(88, last(coredata(output_run6[[1]]$svValue[nov]) - 0.8), "Nov", cex=2)
axis(1, at=c(0,48,96), labels =c(0,12,24))

mtext("Temperature", 2, line=3, outer=T, cex=1.5)
mtext("Hour", 1, line=3, outer=T, cex=1.5)








##########################


yo <- getHyporheicBinStats3(nbins = 10,
                        factor = factor,
                        minRT = minRT,
                        maxRT = 20*86400,
                        porosity = porosity,
                        aquiferVol = 100,
                        hyporheicQ = 0.0002314814815)

yo5 <- getHyporheicBinStats3(nbins = 5,
                            factor = factor,
                            minRT = minRT,
                            maxRT = 31*86400,
                            porosity = porosity,
                            aquiferVol = 100,
                            hyporheicQ = 0.0002314814815)



par(mfrow=c(1,1))
plot(yo$to, yo$returnFlow, type="l")
lines(yo$to[1:3], yo$returnFlow[1:3], col= "red", lwd=2)
lines(yo$to[3:5], yo$returnFlow[3:5], col= "gold", lwd=2)
lines(yo$to[5:7], yo$returnFlow[5:7], col= "forestgreen", lwd=2)
lines(yo$to[7:9], yo$returnFlow[7:9], col= "dodgerblue", lwd=2)
lines(yo$to[9:10], yo$returnFlow[9:10], col= "purple", lwd=2)

par(mar = c(3, 3, 1, 1), oma = c(0,0,0,0))
plot(yo$to[1:9], yo$returnFlow[1:9], type="l", lwd=2, xaxt = "n", yaxt = "n", ylab = "", xlab = "")
points(yo$to[1], yo$returnFlow[1], col="red", pch=15, cex=2.5)
points(yo$to[3], yo$returnFlow[3], col="gold", pch=15, cex=2.5)
points(yo$to[5], yo$returnFlow[5], col="forestgreen", pch=15, cex=2.5)
points(yo$to[7], yo$returnFlow[7], col="dodgerblue", pch=15, cex=2.5)
points(yo$to[9], yo$returnFlow[9], col="purple", pch=15, cex=2.5)

for(i in c(1, 3, 5, 7, 9)){
  points(yo$to[i], yo$returnFlow[i], col="gray95", pch=22, cex=3)
}

mtext("Residence Time", 1, line = 1, cex = 2)
mtext("Q", 2, line = 1, cex = 2)



points(mean(c(yo$to[2], yo$to[1])), mean(c(yo$returnFlow[2], yo$returnFlow[1])), col= "red", pch=16)
points(mean(c(yo$to[2], yo$to[1])), mean(c(yo$returnFlow[2], yo$returnFlow[1])), col= "red", pch=16)






