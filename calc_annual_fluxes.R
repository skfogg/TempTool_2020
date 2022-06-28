calc_annual_fluxes <- function(sw, lw, sens, lat, hypo = NA){

  if(is.na(hypo)){
    sw_gain <- sum(subset(sw, x > 0)*10)
    lw_gain <- sum(subset(lw, x > 0)*10)
    sens_gain <- sum(subset(sens, x > 0)*10)
    lat_gain <- sum(subset(lat, x > 0)*10)

    sw_loss <- sum(subset(sw, x < 0)*10)
    lw_loss <- sum(subset(lw, x < 0)*10)
    sens_loss <- sum(subset(sens, x < 0)*10)
    lat_loss <- sum(subset(lat, x < 0)*10)

    frame <- data.frame(sw = c(sw_gain, sw_loss),
                        lw = c(lw_gain, lw_loss),
                        sensible = c(sens_gain, sens_loss),
                        latent = c(lat_gain, lat_loss),
                        hyporheic = c(0, 0),
                        row.names = c("gain", "loss"))
  }else{
    sw_gain <- sum(subset(sw, x > 0)*10)
    lw_gain <- sum(subset(lw, x > 0)*10)
    sens_gain <- sum(subset(sens, x > 0)*10)
    lat_gain <- sum(subset(lat, x > 0)*10)
    hypo_gain <- sum(subset(hypo, x > 0)*10)

    sw_loss <- sum(subset(sw, x < 0)*10)
    lw_loss <- sum(subset(lw, x < 0)*10)
    sens_loss <- sum(subset(sens, x < 0)*10)
    lat_loss <- sum(subset(lat, x < 0)*10)
    hypo_loss <- sum(subset(hypo, x < 0)*10)

    frame <- data.frame(sw = c(sw_gain, sw_loss),
                        lw = c(lw_gain, lw_loss),
                        sensible = c(sens_gain, sens_loss),
                        latent = c(lat_gain, lat_loss),
                        hyporheic = c(hypo_gain, hypo_loss),
                        row.names = c("gain", "loss"))
  }

    return(frame)
}
