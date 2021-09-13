calc_absolute_annual <- function(sw, lw, sens, lat, hypo = 0){
  # total <- sum(c(sum(abs(sw)), sum(abs(lw)), sum(abs(sens)), sum(abs(lat)), sum(abs(hypo))))
  swannual <- sum(abs(sw))
  lwannual <- sum(abs(lw))
  sensannual <- sum(abs(sens))
  latannual <- sum(abs(lat))
  hypoannual <- sum(abs(hypo))
  frame <- data.frame(sw = swannual, lw = lwannual, sensible = sensannual, latent = latannual, hyporheic = hypoannual)

  return(frame)
}
