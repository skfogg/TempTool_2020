calc_budget <- function(sw, lw, sens, lat, hypo = 0){
  total <- sum(c(sum(abs(sw)), sum(abs(lw)), sum(abs(sens)), sum(abs(lat)), sum(abs(hypo))))
  swannual <- sum(abs(sw))/total
  lwannual <- sum(abs(lw))/total
  sensannual <- sum(abs(sens))/total
  latannual <- sum(abs(lat))/total
  hypoannual <- sum(abs(hypo))/total
  frame <- data.frame(sw = swannual, lw = lwannual, sensible = sensannual, latent = latannual, hyporheic = hypoannual)

  return(frame)
}
