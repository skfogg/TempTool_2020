library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)

connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="MSUFLL!!")
highHypo <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 35.94, b=-1.39)
medHypo <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 23.96, b=-1.39)
lowHypo <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 11.98, b=-1.39)

qdwn <- signif(highHypo$entering/highHypo$entering[1],3)
qup <- signif(highHypo$returning/sum(highHypo$returning),3)
vol <- signif(highHypo$aquiferStorage/sum(highHypo$aquiferStorage),3)

plot(qdwn ~ highHypo$meanWaterAge, log = "x")
points(qup ~ highHypo$meanWaterAge, col = "red")
points(vol ~ highHypo$meanWaterAge, col = "blue")
abline(v = highHypo$from)
abline(v = highHypo$to)

table2 <- matrix(data = c(qdwn, qup, vol), ncol = 3, nrow = 18, byrow = F,
       dimnames = list(c(as.character(trunc(highHypo$meanWaterAge))),
                       c("Qup", "Qdwn", "Volume")))
ylabels <- paste0(c(round(highHypo$from/60)[1:5],
                    paste0(round(highHypo$from/60)[6], " mins"),
                    round(highHypo$from/3600,1)[7:10],
                    paste0(round(highHypo$from/3600,1)[11], " hrs"),
                    round(highHypo$from/86400,1)[12:18]),
                  " - ",
                  c(paste0(round(highHypo$to/60)[1:5], " mins"),
                    paste0(round(highHypo$to/3600,1)[6:10], " hrs"),
                    paste0(round(highHypo$to/86400,1)[11:18], " days"))
)
table2_v2 <- matrix(data = c(qdwn, qup, vol), ncol = 3, nrow = 18, byrow = F,
                    dimnames = list(ylabels,
                                    c("Qup", "Qdwn", "Volume")))


png("plots/table2_graphic.png",
    height = 900*5,
    width = 1000*5,
    res = 72*5)
par(mar = c(12,4,2,2),
    cex.axis = 1.3)
barplot(height = t(table2_v2),
        beside = T,
        # log = "y",
        # legend.text = T,
        las = 3,
        args.legend = list("top"),
        col = c("gray25", "gray60", "gray90"))
legend("top", c("q down", "q up", "volume"),
       fill = c("gray25", "gray60", "gray90"),
       cex = 1.5)
dev.off()


c(paste0(round(highHypo$from), " - ", round(highHypo$to)))


lowHypo$returning/sum(lowHypo$returning)


plot(returning ~ meanWaterAge, data = highHypo,
     type = "l",
     log = "x")
lines(returning ~ meanWaterAge, data = medHypo, col = "red")
lines(returning ~ meanWaterAge, data = lowHypo, col = "blue")


plot(returning ~ meanWaterAge, data = highHypo,
     type = "l")
lines(entering ~ meanWaterAge, data = highHypo, lty = 2)

plot(aquiferStorage ~ meanWaterAge, data = highHypo, lty = 3, type = "l")
