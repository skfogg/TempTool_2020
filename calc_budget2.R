calc_budget2 <- function(sw, lw, sens, lat, hypo = 0){


  if (hypo != 0){
    total_gain <- sum(c(sum(subset(sw, x > 0)),
                        sum(subset(lw, x > 0)),
                        sum(subset(sens, x > 0)),
                        sum(subset(lat, x > 0)),
                        sum(subset(hypo, x > 0))))
    total_loss <- sum(c(sum(subset(sw, x < 0)),
                        sum(subset(lw, x < 0)),
                        sum(subset(sens, x < 0)),
                        sum(subset(lat, x < 0)),
                        sum(subset(hypo, x < 0))))

    sw_gain <- sum(subset(sw, x > 0))/total_gain
    lw_gain <- sum(subset(lw, x > 0))/total_gain
    sens_gain <- sum(subset(sens, x > 0))/total_gain
    lat_gain <- sum(subset(lat, x > 0))/total_gain
    hypo_gain <- sum(subset(hypo, x > 0))/total_gain

    sw_loss <- sum(subset(sw, x < 0))/total_loss
    lw_loss <- sum(subset(lw, x < 0))/total_loss
    sens_loss <- sum(subset(sens, x < 0))/total_loss
    lat_loss <- sum(subset(lat, x < 0))/total_loss
    hypo_loss <- sum(subset(hypo, x < 0))/total_loss

    frame <- data.frame(sw = c(sw_gain, sw_loss),
                        lw = c(lw_gain, lw_loss),
                        sensible = c(sens_gain, sens_loss),
                        latent = c(lat_gain, lat_loss),
                        hyporheic = c(hypo_gain, hypo_loss),
                        row.names = c("gain", "loss"))
  }else{
    total_gain <- sum(c(sum(subset(sw, x > 0)),
                        sum(subset(lw, x > 0)),
                        sum(subset(sens, x > 0)),
                        sum(subset(lat, x > 0))))
    total_loss <- sum(c(sum(subset(sw, x < 0)),
                        sum(subset(lw, x < 0)),
                        sum(subset(sens, x < 0)),
                        sum(subset(lat, x < 0))))

    sw_gain <- sum(subset(sw, x > 0))/total_gain
    lw_gain <- sum(subset(lw, x > 0))/total_gain
    sens_gain <- sum(subset(sens, x > 0))/total_gain
    lat_gain <- sum(subset(lat, x > 0))/total_gain

    sw_loss <- sum(subset(sw, x < 0))/total_loss
    lw_loss <- sum(subset(lw, x < 0))/total_loss
    sens_loss <- sum(subset(sens, x < 0))/total_loss
    lat_loss <- sum(subset(lat, x < 0))/total_loss

    frame <- data.frame(sw = c(sw_gain, sw_loss),
                        lw = c(lw_gain, lw_loss),
                        sensible = c(sens_gain, sens_loss),
                        latent = c(lat_gain, lat_loss),
                        hyporheic = c(0, 0),
                        row.names = c("gain", "loss"))
  }

  return(frame)
}
