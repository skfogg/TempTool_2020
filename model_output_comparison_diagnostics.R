yearandhalf <- "2014-01-01 00:00:00/2015-07-01 00:00:00"
timing <- yearandhalf

diagnostics <- data.frame(runConfig = c("control", "hypo low", "hypo moderate","hypo high", "shade low", "shade moderate", "shade high"))
diagnostics$annualmean <- c(mean(control[timing]),
                            mean(hypolit[timing]),
                            mean(hypomed[timing]),
                            mean(hypohigh[timing]),
                            mean(shade30[timing]),
                            mean(shade60[timing]),
                            mean(shade90[timing]))

## Difference in annual mean tempreature from the control
diagnostics$meandiff <- c(mean(control[timing]) - mean(control[timing]),
                          mean(hypolit[timing]) - mean(control[timing]),
                          mean(hypomed[timing]) - mean(control[timing]),
                          mean(hypohigh[timing]) - mean(control[timing]),
                          mean(shade30[timing]) - mean(control[timing]),
                          mean(shade60[timing]) - mean(control[timing]),
                          mean(shade90[timing]) - mean(control[timing]))


## Max stuff
annualmaxdailymean <- function(x){
  xmd <- apply.daily(x, mean)
  return(xmd[coredata(xmd) == max(xmd)])
}

diagnostics$annualmax <- c(coredata(annualmaxdailymean(control[timing])),
                           coredata(annualmaxdailymean(hypolit[timing])),
                           coredata(annualmaxdailymean(hypomed[timing])),
                           coredata(annualmaxdailymean(hypohigh[timing])),
                           coredata(annualmaxdailymean(shade30[timing])),
                           coredata(annualmaxdailymean(shade60[timing])),
                           coredata(annualmaxdailymean(shade90[timing])))

diagnostics$maxdiff <- c(coredata(annualmaxdailymean(control[timing])) - coredata(annualmaxdailymean(control[timing])),
                         coredata(annualmaxdailymean(hypolit[timing])) - coredata(annualmaxdailymean(control[timing])),
                         coredata(annualmaxdailymean(hypomed[timing])) - coredata(annualmaxdailymean(control[timing])),
                         coredata(annualmaxdailymean(hypohigh[timing])) - coredata(annualmaxdailymean(control[timing])),
                         coredata(annualmaxdailymean(shade30[timing])) - coredata(annualmaxdailymean(control[timing])),
                         coredata(annualmaxdailymean(shade60[timing])) - coredata(annualmaxdailymean(control[timing])),
                         coredata(annualmaxdailymean(shade90[timing])) - coredata(annualmaxdailymean(control[timing])))


# timing of annual max
diagnostics$jdayofmax <- c(yday(annualmaxdailymean(control[timing])),
                          yday(annualmaxdailymean(hypolit[timing])),
                          yday(annualmaxdailymean(hypomed[timing])),
                          yday(annualmaxdailymean(hypohigh[timing])),
                          yday(annualmaxdailymean(shade30[timing])),
                          yday(annualmaxdailymean(shade60[timing])),
                          yday(annualmaxdailymean(shade90[timing])))

diagnostics$dayofmax <- c(index(annualmaxdailymean(control[timing])),
                          index(annualmaxdailymean(hypolit[timing])),
                          index(annualmaxdailymean(hypomed[timing])),
                          index(annualmaxdailymean(hypohigh[timing])),
                          index(annualmaxdailymean(shade30[timing])),
                          index(annualmaxdailymean(shade60[timing])),
                          index(annualmaxdailymean(shade90[timing])))



diagnostics$maxtimingdiff <- c(yday(annualmaxdailymean(control[timing])) - yday(annualmaxdailymean(control[timing])),
                               yday(annualmaxdailymean(hypolit[timing])) - yday(annualmaxdailymean(control[timing])),
                               yday(annualmaxdailymean(hypomed[timing])) - yday(annualmaxdailymean(control[timing])),
                               yday(annualmaxdailymean(hypohigh[timing])) - yday(annualmaxdailymean(control[timing])),
                               yday(annualmaxdailymean(shade30[timing])) - yday(annualmaxdailymean(control[timing])),
                               yday(annualmaxdailymean(shade60[timing])) - yday(annualmaxdailymean(control[timing])),
                               yday(annualmaxdailymean(shade90[timing])) - yday(annualmaxdailymean(control[timing])))



## Min stuff
annualmindailymean <- function(x){
  xmd <- apply.daily(x, mean)
  return(xmd[coredata(xmd) == min(xmd)])
}

diagnostics$annualmin <- c(coredata(annualmindailymean(control[timing])),
                           coredata(annualmindailymean(hypolit[timing])),
                           coredata(annualmindailymean(hypomed[timing])),
                           coredata(annualmindailymean(hypohigh[timing])),
                           coredata(annualmindailymean(shade30[timing])),
                           coredata(annualmindailymean(shade60[timing])),
                           coredata(annualmindailymean(shade90[timing])))

diagnostics$mindiff <- c(coredata(annualmindailymean(control[timing])) - coredata(annualmindailymean(control[timing])),
                         coredata(annualmindailymean(hypolit[timing])) - coredata(annualmindailymean(control[timing])),
                         coredata(annualmindailymean(hypomed[timing])) - coredata(annualmindailymean(control[timing])),
                         coredata(annualmindailymean(hypohigh[timing])) - coredata(annualmindailymean(control[timing])),
                         coredata(annualmindailymean(shade30[timing])) - coredata(annualmindailymean(control[timing])),
                         coredata(annualmindailymean(shade60[timing])) - coredata(annualmindailymean(control[timing])),
                         coredata(annualmindailymean(shade90[timing])) - coredata(annualmindailymean(control[timing])))

# timing of annual min
diagnostics$jdayofmin <- c(yday(annualmindailymean(control[timing])),
                          yday(annualmindailymean(hypolit[timing])),
                          yday(annualmindailymean(hypomed[timing])),
                          yday(annualmindailymean(hypohigh[timing])),
                          yday(annualmindailymean(shade30[timing])),
                          yday(annualmindailymean(shade60[timing])),
                          yday(annualmindailymean(shade90[timing])))

diagnostics$dayofmin <- c(index(annualmindailymean(control[timing])),
                          index(annualmindailymean(hypolit[timing])),
                          index(annualmindailymean(hypomed[timing])),
                          index(annualmindailymean(hypohigh[timing])),
                          index(annualmindailymean(shade30[timing])),
                          index(annualmindailymean(shade60[timing])),
                          index(annualmindailymean(shade90[timing])))


diagnostics$mintimingdiff <- c(yday(annualmindailymean(control[timing])) - yday(annualmindailymean(control[timing])),
                               yday(annualmindailymean(hypolit[timing])) - yday(annualmindailymean(control[timing])),
                               yday(annualmindailymean(hypomed[timing])) - yday(annualmindailymean(control[timing])),
                               yday(annualmindailymean(hypohigh[timing])) - yday(annualmindailymean(control[timing])),
                               yday(annualmindailymean(shade30[timing])) - yday(annualmindailymean(control[timing])),
                               yday(annualmindailymean(shade60[timing])) - yday(annualmindailymean(control[timing])),
                               yday(annualmindailymean(shade90[timing])) - yday(annualmindailymean(control[timing])))

diagnostics$daysofmeanwarming <- diagnostics$jdayofmax - diagnostics$jdayofmin

diagnostics$daysofmeancooling <- (365 + diagnostics$jdayofmin) - diagnostics$daysofmeanwarming

diagnostics[,c(2,3,4,5,9,10)] <- round(diagnostics[,c(2,3,4,5,9,10)], 1)

#write.csv(diagnostics, "d:/Users/sarah.fogg/Dropbox/PAPER1/diagnosticsTable.csv")
